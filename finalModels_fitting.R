library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)
impute_tech = "hot deck"
learner = "regr.ranger"
DistrictSplit = TRUE
config = "^feature selection$"
savefile = "rangerSplitFS_"
sourcing = TRUE

if(DistrictSplit) {
 loop_district_var = c("gaya", "jamui", "nalanda", "vaishali")
} else {
 loop_district_var = "All"
}

holdout = FALSE
source("data_cleaning.R")
## upsampling of underrepresentated areas in each District
obsW_df = df %>% group_by(District) %>%
 mutate(histGroup = vapply(Yield, function(x, minv, maxv, step) {
  which(minv + 0:19 * step <= x & maxv - 19:0 * step >= x)},
  FUN.VALUE = 1, minv = min(Yield), maxv = max(Yield), step = (max(Yield) - min(Yield)) / 20)) %>%
 ungroup() %>% group_by(histGroup, District) %>%
 mutate(rep = n(), hG_min = min(Yield), hG_max = max(Yield)) %>% ungroup() %>% group_by(District) %>%
 mutate(hG_qlow = ecdf(Yield)(hG_min), hG_qhigh = ecdf(Yield)(hG_max),
        weight = 1/((rep + 1) * sqrt(rep + 1)) *
          !((hG_qlow <= 0.75 & hG_qlow >= 0.25) | (hG_qhigh <= 0.75 & hG_qhigh >= 0.25))) %>%
 ungroup() %>% select(ID, District, histGroup, weight, rep, hG_qlow, hG_qhigh)
set.seed(2024)
upsamp_gaya = sample(obsW_df %>% filter(District == "Gaya") %>% pull(ID),
  size = ceiling(obsW_df %>% filter(District == "Gaya") %>% nrow() * 0.05), replace = TRUE,
  prob = obsW_df %>% filter(District == "Gaya") %>% pull(weight))
upsamp_jamui = sample(obsW_df %>% filter(District == "Jamui") %>% pull(ID),
  size = ceiling(obsW_df %>% filter(District == "Jamui") %>% nrow() * 0.05), replace = TRUE,
  prob = obsW_df %>% filter(District == "Jamui") %>% pull(weight))
upsamp_nalanda = sample(obsW_df %>% filter(District == "Nalanda") %>% pull(ID),
  size = ceiling(obsW_df %>% filter(District == "Nalanda") %>% nrow() * 0.05), replace = TRUE,
  prob = obsW_df %>% filter(District == "Nalanda") %>% pull(weight))
upsamp_vaishali = sample(obsW_df %>% filter(District == "Vaishali") %>% pull(ID),
  size = ceiling(obsW_df %>% filter(District == "Vaishali") %>% nrow() * 0.05), replace = TRUE,
  prob = obsW_df %>% filter(District == "Vaishali") %>% pull(weight))
df = rbind(df,
  df %>% slice(vapply(c(upsamp_gaya, upsamp_jamui, upsamp_nalanda, upsamp_vaishali),
      function(x) {which(ID == x)}, FUN.VALUE = 1)))

source("imputation.R")
task_All$select(readRDS("features/feature_list_ranger.RDS"))
task_gaya$select(readRDS("features/feature_list_ranger_gaya.RDS"))
task_jamui$select(readRDS("features/feature_list_ranger_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_ranger_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_ranger_vaishali.RDS"))

hp_df = results %>%
 filter(Imputing == impute_tech & Learner == learner & DistrictSplit == DistrictSplit) %>%
 select(Seed, Hyper_Parameter, CV_Score, District) %>% rowwise %>%
 mutate(Depth = Hyper_Parameter[["depth"]], learningRate = Hyper_Parameter[["learning_rate"]],
        Iterations = Hyper_Parameter[["iterations"]],
        GrowPolicy = ifelse(is.null(Hyper_Parameter[["grow_policy"]]), "SymmetricTree",
                            Hyper_Parameter[["grow_policy"]]),
        RandomStrength = ifelse(is.null(Hyper_Parameter[["random_strength"]]), 1,
                                Hyper_Parameter[["random_strength"]]),
        mtry = ifelse(is.null(Hyper_Parameter[["mtry"]]), 4, Hyper_Parameter[["mtry"]]),
        num.trees = ifelse(is.null(Hyper_Parameter[["num.trees"]]), 1000, Hyper_Parameter[["num.trees"]]),
        max.depth = ifelse(is.null(Hyper_Parameter[["max.depth"]]), 0, Hyper_Parameter[["max.depth"]]),
        min.node.size = ifelse(is.null(Hyper_Parameter[["min.node.size"]]), 5,
                               Hyper_Parameter[["min.node.size"]]),
        bootstrap = ifelse(is.null(Hyper_Parameter[["replace"]]), FALSE,
                           Hyper_Parameter[["replace"]])) %>% ungroup() %>%
 arrange(CV_Score)

for(district in loop_district_var) {
  set.seed(hp_df %>% filter(District == district) %>% slice(1) %>% pull(Seed))
  if(learner == "regr.catboost") {
    modLearner = lrn(learner, thread_count = 6,
                     iterations = hp_df %>% filter(District == district) %>% slice(1) %>% pull(Iterations),
                     learning_rate = hp_df %>% filter(District == district) %>% slice(1) %>% pull(learningRate),
                     depth = hp_df %>% filter(District == district) %>% slice(1) %>% pull(Depth),
                     grow_policy = hp_df %>% filter(District == district) %>% slice(1) %>% pull(GrowPolicy),
                     random_strength = hp_df %>% filter(District == district) %>% slice(1) %>% pull(RandomStrength))
  } else {
    modLearner = lrn(learner, num.threads = 6,
                     mtry = hp_df %>% filter(District == district) %>% slice(1) %>% pull(mtry),
                     num.trees = hp_df %>% filter(District == district) %>% slice(1) %>% pull(num.trees),
                     max.depth = hp_df %>% filter(District == district) %>% slice(1) %>% pull(max.depth),
                     min.node.size = hp_df %>% filter(District == district) %>% slice(1) %>% pull(min.node.size),
                     replace = hp_df %>% filter(District == district) %>% slice(1) %>% pull(bootstrap))
  }
  
  modLearner$train(get(paste0("task_", district)))
  assign(paste0("modLearner_", district), modLearner$clone())
  saveRDS(get(paste0("modLearner_", district)), paste0("models/", savefile, district, ".rds"))
}

holdout = TRUE
source("data_cleaning.R")
holdoutIDs = df$ID
df = rbind(df_copy, df %>% mutate(OrgFertilizers_Pranamrit = 0, FirstTopDressFert_Other = 0,
                         FirstTopDressFert_SSP = 0))
source("imputation.R")
task_All$select(readRDS("features/feature_list_ranger.RDS"))
task_gaya$select(readRDS("features/feature_list_ranger_gaya.RDS"))
task_jamui$select(readRDS("features/feature_list_ranger_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_ranger_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_ranger_vaishali.RDS"))

modelList = data.table(Seed = integer(0), Learner = character(0), DistrictSplit = logical(0),
   District = character(0), featureSelection = logical(0), oversampling = logical(0),
   trainTime = numeric(0), trainRMSE = numeric(0), trainRMSE_comb = numeric(0),
   holdoutRMSE = numeric(0), holdoutRMSE_comb = numeric(0))

for(District in loop_district_var) {
  curr_mod = paste0("modLearner_", District)
  curr_task = paste0("task_", District)
 pred_train = get(curr_mod)$predict(get(curr_task),
    get(curr_task)$row_ids[!get(curr_task)$data(cols = "ID")[[1]] %in% holdoutIDs])
 pred_holdout = get(curr_mod)$predict(get(curr_task),
    get(curr_task)$row_ids[get(curr_task)$data(cols = "ID")[[1]] %in% holdoutIDs])
 
 modelList = rbind(modelList, data.table(
  Seed = hp_df %>% filter(District == District) %>% slice(1) %>% pull(Seed),
  Learner = learner, DistrictSplit = DistrictSplit, District = District,
  featureSelection = TRUE, oversampling = FALSE,
  trainTime = get(paste0("modLearner_", District))$state$train_time,
  trainRMSE = pred_train$score(msr("regr.rmse")),
  trainRMSE_comb = pred_train$score(msr("regr.rmse")),
  holdoutRMSE = pred_holdout$score(msr("regr.rmse")),
  holdoutRMSE_comb = pred_holdout$score(msr("regr.rmse"))
 ))
}

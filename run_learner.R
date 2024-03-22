library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)

Seed <- 11931
sourcing = TRUE
impute_tech = "hot deck"
DistrictSplit = FALSE
holdout = FALSE
source("data_cleaning.R")

## upsampling of underrepresentated areas in each District
obsW_df = df %>% group_by(District) %>%
  mutate(histGroup = vapply(Yield, function(x, minv, maxv, step) {
    which(minv + 0:19 * step <= x & maxv - 19:0 * step >= x)},
    FUN.VALUE = 1, minv = min(Yield), maxv = max(Yield), step = (max(Yield) - min(Yield)) / 20)) %>%
  ungroup() %>% group_by(histGroup, District) %>%
  mutate(rep = n(), hG_mean = mean(Yield)) %>% ungroup() %>% group_by(District) %>%
  mutate(hG_q = ecdf(Yield)(hG_mean),
         weight = 1/((rep + 1) * sqrt(rep + 1)) * (hG_q >= 0.75 | hG_q <= 0.25)) %>%
  ungroup() %>% select(ID, District, histGroup, weight, rep)
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
task_gaya$select(readRDS("features/feature_list_catboost_gaya.RDS"))
task_jamui$select(readRDS("features/feature_list_catboost_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_catboost_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_ranger_vaishali.RDS"))


# Nested Tuning
learner = lrn("regr.catboost",
              iterations = to_tune(100, 500),
              learning_rate = to_tune(0.01, 0.1),
              depth = to_tune(9, 14)
)

rr = tune_nested(
 tuner = tnr("random_search"),
 learner = learner,
 task = task_jamui,
 inner_resampling = rsmp ("cv", folds = 4),
 outer_resampling = rsmp("cv", folds = 3),
 measure = msr("regr.rmse"),
 term_evals = 15)

# Auto Tuner

#at = auto_tuner(
# tuner = tnr("mbo"),
# learner = learner,
# resampling = rsmp ("holdout"),
# measure = msr("regr.rmse"),
# term_evals = 30)

#resampling_outer = rsmp("cv", folds = 3)
#rr = resample(task, at, resampling_outer, store_models = TRUE)
#extract_inner_tuning_results(rr)
#rr$score(msr("regr.rmse"))

hp_rdist = list("gaya" = list("iterations" = c(150, 450), "depth" = c(8.5, 14.5),
      "learning_rate" = c(0.02, 0.075), "grow_policy" = c("Depthwise", "SymmetricTree")),
  "jamui" = list("iterations" = c(200, 750), "depth" = c(9.5, 14.5),
      "learning_rate" = c(0.015, 0.075), "grow_policy" = c("Depthwise", "SymmetricTree")),
  "vaishali" = list("iterations" = c(450, 750), "depth" = c(8.5, 13.5), "num.trees" = c(800, 1000),
      "learning_rate" = c(0.033, 0.1), "grow_policy" = c("Depthwise", "SymmetricTree"), "max.depth" = c(5.5, 9.5),
      "mtry" = c(5.5, 12.5), "min.node.size" = c(3.5, 5.5)),
  "nalanda" = list("iterations" = c(100, 400), "depth" = c(9.5, 15.5),
      "learning_rate" = c(0.075, 0.145), "grow_policy" = c("Depthwise", "SymmetricTree")),
  "All" = list("iterations" = c(150, 1000), "depth" = c(7.5, 13.5),
      "learning_rate" = c(0.01, 0.095), "grow_policy" = c("Depthwise", "SymmetricTree"),
      "num.trees" = c(800, 1200), "max.depth" = c(0, 0.5),
      "mtry" = c(4.5, 12.5), "min.node.size" = c(3.5, 5.5)))

# Evaluation Model
if(DistrictSplit) {
 loop_district_var = c("gaya", "jamui", "nalanda", "vaishali")
} else {
 loop_district_var = "All"
}
for(i in 1:25) {
  Seed = Seed + i
  for(District in loop_district_var) {
    hp_param = list(iterations = round(runif(1, hp_rdist[[District]]$iterations[[1]], hp_rdist[[District]]$iterations[[2]])),
                    learning_rate = runif(1, hp_rdist[[District]]$learning_rate[[1]], hp_rdist[[District]]$learning_rate[[2]]),
                    depth = round(runif(1, hp_rdist[[District]]$depth[[1]], hp_rdist[[District]]$depth[[2]])),
                    random_strength = runif(1, 0.5, 1.5),
                    grow_policy = sample(hp_rdist[[District]]$grow_policy, 1))
    #hp_param = list(num.trees = round(runif(1, hp_rdist[[District]]$num.trees[[1]], hp_rdist[[District]]$num.trees[[2]])),
    #                max.depth = round(runif(1, hp_rdist[[District]]$max.depth[[1]], hp_rdist[[District]]$max.depth[[2]])),
    #                mtry = round(runif(1, hp_rdist[[District]]$mtry[[1]], hp_rdist[[District]]$mtry[[2]])),
    #                min.node.size = round(runif(1, hp_rdist[[District]]$min.node.size[[1]], hp_rdist[[District]]$min.node.size[[2]])))
    
    set.seed(Seed)
    learner = lrn("regr.catboost",
                 thread_count = 6,
                  iterations = hp_param[["iterations"]],
                  learning_rate = hp_param[["learning_rate"]],
                  depth = hp_param[["depth"]],
                  random_strength = hp_param[["random_strength"]],
                  grow_policy = hp_param[["grow_policy"]])
    
    #learner = lrn("regr.ranger", num.threads = 6, respect.unordered.factors = "partition",
    #              replace = TRUE, num.trees = hp_param[["num.trees"]],
    #              max.depth = hp_param[["max.depth"]], mtry = hp_param[["mtry"]],
    #              min.node.size = hp_param[["min.node.size"]])
    
    cv = rsmp("cv", folds = 5)
    
    rr = resample(get(paste0("task_", District)), learner, cv)
    
    rmse = rr$aggregate(msr("regr.rmse"))[[1]]
    
    Comment = "no feature selection"
    
    learner$train(get(paste0("task_", District)))
    
    results = rbind(readRDS("data/results_nils.RDS"),data.table(
      Date = Sys.Date(),
      Seed = Seed,
      Learner = learner$id,
      Hyper_Parameter = list(learner$param_set$values),
      Features = list(get(paste0("task_", District))[["feature_names"]]),
      Imputing = impute_tech,
      Time = learner$timings[[1]],
      Resampling_folds = cv$param_set$values[[1]],
      CV_Score = rmse,
      Comment = Comment,
      DistrictSplit = DistrictSplit,
      District = District,
      Combined_Score = rmse
    ))
    saveRDS(results, "data/results_nils.RDS")
  }
  if(DistrictSplit) {
    # get the overall rmse for district models
    first_obs = nrow(results) - 3
    results$Combined_Score[first_obs + 0:3] = sqrt((results[first_obs, ]$CV_Score^2 * task_gaya$nrow +
      results[first_obs + 1, ]$CV_Score^2 * task_jamui$nrow + results[first_obs + 2, ]$CV_Score^2 *
      task_nalanda$nrow + results[first_obs + 3, ]$CV_Score^2 * task_vaishali$nrow)/nrow(df_copy))
    
    saveRDS(results, "data/results_nils.RDS")
  }
}


# Predicitons on test Data
pr = learner$predict_newdata(newdata = tdf)
Yield = round(pr$response, digits = 2)
ID = tdf$ID
PR = cbind(ID, Yield)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)



library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)

Seed <- 112233
set.seed(Seed)
sourcing = TRUE
impute_tech = "hot deck"
DistrictSplit = FALSE
source("data_cleaning.R")
source("imputation.R")
task_All$select(readRDS("features/feature_list_ranger.RDS"))
task_gaya$select(readRDS("features/feature_list_ranger_gaya.RDS"))
task_jamui$select(readRDS("features/feature_list_ranger_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_ranger_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_ranger_vaishali.RDS"))

if(DistrictSplit) {
 loop_district_var = c("gaya", "jamui", "nalanda", "vaishali")
} else {
 loop_district_var = "All"
}

for(District in loop_district_var) {
 set.seed(Seed)
 learner = lrn("regr.ranger", 
               num.trees = to_tune(500, 1000),
               mtry = to_tune(2, 10),
               num.threads = 6)
 
 #learner = lrn("regr.catboost",
 #              iterations = to_tune(200, 800),
 #              learning_rate = to_tune(0.01, 0.1),
 #              depth = to_tune(6, 15),
 #              grow_policy = to_tune(c("SymmetricTree", "Depthwise", "Lossguide")),
 #              random_strength = to_tune(0.5, 1.5),
 #              thread_count = 6)
 
 instance = tune(
  tuner = tnr("mbo"),
  task = get(paste0("task_", District)),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 200)
 )
 
 learner$param_set$values = instance$result_learner_param_vals
 
 cv = rsmp("cv", folds = 5)
 
 rr = resample(get(paste0("task_", District)), learner, cv)
 
 rmse = rr$aggregate(msr("regr.rmse"))[[1]]
 
 Comment = "Tuning + Feature selection Ranger"
 
 learner$train(get(paste0("task_", District)))
 
 results = rbind(readRDS("data/results_adrian.RDS"),data.table(
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
 saveRDS(results, "data/results_adrian.RDS")
}
if(DistrictSplit) {
 # get the overall rmse for district models
 first_obs = nrow(results) - 3
 results$Combined_Score[first_obs + 0:3] = sqrt((results[first_obs, ]$CV_Score^2 * task_gaya$nrow +
                                                  results[first_obs + 1, ]$CV_Score^2 * task_jamui$nrow + results[first_obs + 2, ]$CV_Score^2 *
                                                  task_nalanda$nrow + results[first_obs + 3, ]$CV_Score^2 * task_vaishali$nrow)/3096)
 
 saveRDS(results, "data/results_adrian.RDS")
}
results

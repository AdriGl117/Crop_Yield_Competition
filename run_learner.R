library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)

Seed <- 1603
set.seed(Seed)
sourcing = TRUE
impute_tech = "hot deck"
DistrictSplit = TRUE
source("data_cleaning.R")
source("imputation.R")
#task$select(readRDS("data/feature_list_catboost.RDS"))

# Nested Tuning
#learner = lrn("regr.catboost",
#              iterations = to_tune(500, 1500),
#              learning_rate = to_tune(0.01, 0.1),
#              depth = to_tune(1, 10)
#)

#rr = tune_nested(
# tuner = tnr("mbo"),
# learner = learner,
# task = task,
# inner_resampling = rsmp ("holdout"),
# outer_resampling = rsmp("cv", folds = 5),
# measure = msr("regr.rmse"),
# term_evals = 20)

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

# Evaluation Model
if(DistrictSplit) {
 loop_district_var = c("gaya", "jamui", "nalanda", "vaishali")
} else {
 loop_district_var = "All"
}

for(District in loop_district_var) {
 set.seed(Seed)
 learner = lrn("regr.catboost",
               iterations = 320,
               learning_rate = 0.08948363,
               depth = 10)
 
 #learner = lrn("regr.ranger", num.trees = 750, num.threads = 6)
 
 cv = rsmp("cv", folds = 5)
 
 rr = resample(get(paste0("task_", District)), learner, cv)
 
 rmse = rr$aggregate(msr("regr.rmse"))[[1]]
 
 Comment = "Target is Yield/Acre"
 
 learner$train(get(paste0("task_", District)))
 
 results = rbind(readRDS("data/results.RDS"),data.table(
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
 saveRDS(results, "data/results.RDS")
}
if(DistrictSplit) {
  # get the overall rmse for district models
  first_obs = nrow(results) - 3
  results$Combined_Score[first_obs + 0:3] = sqrt((results[first_obs, ]$CV_Score^2 * task_gaya$nrow +
    results[first_obs + 1, ]$CV_Score^2 * task_jamui$nrow + results[first_obs + 2, ]$CV_Score^2 *
    task_nalanda$nrow + results[first_obs + 3, ]$CV_Score^2 * task_vaishali$nrow)/3096)
  
  saveRDS(results, "data/results.RDS")
}

# Predicitons on test Data
pr = learner$predict_newdata(newdata = tdf)
Yield = round(pr$response, digits = 2)
ID = tdf$ID
PR = cbind(ID, Yield)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)



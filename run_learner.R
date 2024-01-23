library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)

Seed <- 1234
set.seed(Seed)
sourcing = TRUE
#impute_tech = "hot deck"
#DistrictSplit = TRUE
source("data_cleaning.R")
source("imputation.R")
task$select(readRDS("data/feature_list_catboost.RDS"))
task$set_col_roles("ID", add_to = "name", remove_from = "feature")

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

learner = lrn("regr.catboost",
              iterations = 965,
              learning_rate = 0.08948363,
              depth = 6)

cv = rsmp("cv", folds = 5)

rr = resample(task, learner, cv)

rmse = rr$aggregate(msr("regr.rmse"))[[1]]

Comment = "Target is Yield/Acre"

Distict = "All"

learner$train(task)

results = rbind(readRDS("data/results.RDS"),data.table(
                Date = Sys.Date(),
                Seed = Seed,
                Learner = learner$id,
                Hyper_Parameter = list(learner$param_set$values),
                Features = list(task$feature_names),
                Imputing = impute_tech,
                Time = learner$timings[[1]],
                Resampling_folds = cv$param_set$values[[1]],
                CV_Score = rmse,
                Comment = Comment,
                DistrictSplit = DistrictSplit,
                District = District
                ))
saveRDS(results, "data/results.RDS")

# Predicitons on test Data
pr = learner$predict_newdata(newdata = tdf)
Yield = round(pr$response, digits = 2)
ID = tdf$ID
PR = cbind(ID, Yield)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)



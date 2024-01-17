library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)

impute_tech = "impute mean"
Seed <- 1234
set.seed(Seed)
source("imputation.R", skip = 4)
task$select(readRDS("data/feature_list.RDS"))
task$set_col_roles("ID", add_to = "name", remove_from = "feature")

learner = lrn("regr.catboost",
              iterations = to_tune(500, 1500),
              learning_rate = to_tune(0.01, 0.1),
              depth = to_tune(1, 10)
)

#rr = tune_nested(
# tuner = tnr("mbo"),
# learner = learner,
# task = task,
# inner_resampling = rsmp ("holdout"),
# outer_resampling = rsmp("cv", folds = 5),
# measure = msr("regr.rmse"),
# term_evals = 20)

learner = lrn("regr.catboost",
              iterations = 965,#965
              learning_rate = 0.08948363,#0.08948363
              depth = 6)#6



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

cv = rsmp("cv", folds = 5)

rr = resample(task, learner, cv)

rmse = rr$aggregate(msr("regr.rmse"))[[1]]

Comment = "Hyperparameter Choosen by Bayesian HPO"

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
                Comment = Comment
                ))
saveRDS(results, "data/results.RDS")

# Predicitons on test Data
learner$train(task)
pr = learner$predict_newdata(newdata = tdf)
Yield = round(pr$response, digits = 2)
ID = tdf$ID
PR = cbind(ID, Yield)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)



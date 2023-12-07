library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(mlr3learners.catboost)

df = df %>% subset(Yield/Acre <= 6000)

task = as_task_regr(df, target = "Yield", id = "task")
task$set_col_roles("ID", add_to = "name", remove_from = "feature")

po_hist = po("imputehist")

learner = lrn("regr.catboost",
              iterations = to_tune(500, 1500),
              learning_rate = to_tune(0.01, 0.1),
              depth = to_tune(1, 10)
)

learner = as_learner(po_hist %>>% learner)

#rr = tune_nested(
# tuner = tnr("mbo"),
# learner = learner,
# task = task,
# inner_resampling = rsmp ("holdout"),
# outer_resampling = rsmp("cv", folds = 5),
# measure = msr("regr.rmse"),
# term_evals = 20)

learner = lrn("regr.catboost",
              iterations = 1241,#965
              learning_rate = 0.04592393,#0.08948363
              depth = 9)#6

learner$train(task)

at = auto_tuner(
 tuner = tnr("mbo"),
 learner = learner,
 resampling = rsmp ("holdout"),
 measure = msr("regr.rmse"),
 term_evals = 30)

resampling_outer = rsmp("cv", folds = 3)
rr = resample(task, at, resampling_outer, store_models = TRUE)
extract_inner_tuning_results(rr)
rr$score(msr("regr.rmse"))

at$train(task)
pr = learner$predict_newdata(newdata = tdf)
Yield = round(pr$response, digits = 2)
ID = tdf$ID
PR = cbind(ID, Yield)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)



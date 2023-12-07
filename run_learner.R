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
              iterations = to_tune(1, 1000),
              learning_rate = to_tune(0.001, 0.1),
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

#learner = lrn("regr.catboost",
#              iterations = 965,
#              learning_rate = 0.08948363,
#              depth = 6)

#learner$train(task)

at = auto_tuner(
 tuner = tnr("mbo"),
 learner = learner,
 resampling = rsmp ("cv", folds = 3),
 measure = msr("regr.rmse"),
 term_evals = 30)

at$train(task)
pr = at$predict_newdata(newdata = tdf)
Yield = round(pr$response, digits = 2)
ID = tdf$ID
PR = cbind(ID, Yield)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)



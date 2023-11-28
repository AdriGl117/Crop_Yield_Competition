library(glmnet)
library(kknn)
library(DiceKriging)
library(e1071)
library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(caTools)
library(ROSE)
library(mlr3measures)
source("get_data.R")

df = get_data(src = "data/Train.csv")
df[] <- lapply(df, function(x) {
 if(is.character(x)) as.factor(x) else x
})

df = df %>% mutate(Outlier = ifelse(Yield >= 4000, 1, 0))
df <- ovun.sample(Outlier ~ ., data = df, 
                          method = "over")$data
df = df %>% select(-Outlier)

task = as_task_regr(df, target = "Yield", id = "task")

learner_glmnet = lrn("regr.glmnet")
learner_kknn = lrn("regr.kknn")
learner_km = lrn("regr.km")
learner_lm = lrn("regr.lm")
learner_nnet = lrn("regr.nnet")
learner_ranger = lrn("regr.ranger")
learner_svm = lrn("regr.svm")
learner_xgboost = lrn("regr.xgboost",
                      booster = "gbtree",
                      eta = 0.1,
                      max_depth = 20,
                      nrounds = 250
)

#impute
po_hist = po("imputehist")

#Factor encoding
fencoder = po("encode", affect_columns = selector_type("factor"))

#Graph Learner
learner_glmnet = as_learner(po_hist %>>% fencoder %>>% learner_glmnet)
learner_kknn = as_learner(po_hist %>>% fencoder %>>% learner_kknn)
learner_km = as_learner(po_hist %>>% fencoder %>>% learner_km)
learner_lm = as_learner(po_hist %>>% fencoder %>>% learner_lm)
#learner_nnet = as_learner(po_hist %>>% lencoder %>>% learner_nnet)
learner_svm = as_learner(po_hist %>>% fencoder %>>% learner_svm)
learner_xgboost = as_learner(po_hist %>>% fencoder %>>% learner_xgboost)

resampling = rsmp("cv", folds = 5)

# run experiments
rr = resample(task = task, learner = learner_xgboost, resampling = resampling)

# access results
rmse_boost <- rr$score(msr("regr.rmse"))[, .(task_id, learner_id, iteration, regr.rmse)]
rmse_boost


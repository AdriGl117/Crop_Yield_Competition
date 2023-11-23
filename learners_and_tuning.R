library(mlr3verse)
library(mlr3mbo)
library(mlr3tuning)
library(mlr3hyperband)
library(paradox)
library(mlr3fselect)
library(dplyr)
install.packages("lubridate")
library(lubridate)
data = read.csv("data/Train.csv") %>%
 mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                 Harv_date, Threshing_date), as.Date))

data$CropTillageMonth = month(data$CropTillageDate)

data$CropTillageDate = as.factor(difftime(data$CropTillageDate, min(data$CropTillageDate), units = "days"))
data$RcNursEstDate = as.factor(difftime(data$RcNursEstDate, min(data$RcNursEstDate), units = "days"))
data$SeedingSowingTransplanting = as.factor(difftime(data$SeedingSowingTransplanting, min(data$SeedingSowingTransplanting), units = "days"))
data$Harv_date = as.factor(difftime(data$Harv_date, min(data$Harv_date), units = "days"))
data$Threshing_date = as.factor(difftime(data$Threshing_date, min(data$Threshing_date), units = "days"))

data[] <- lapply(data, function(x) {
 if(is.character(x)) as.factor(x) else x
})
data_test = read.csv("data/Test.csv")
task = as_task_regr(data, target = "Yield")

instance_fs = fsi(
 task = task,
 learner = learner_rpart,
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse"),
 terminator = trm("evals", n_evals = 500)
)
fselector = fs("random_search", batch_size = 100)
fselector$optimize(instance_fs)
task$select(instance_fs$result_feature_set)
task$feature_names
#Base learners
learner_ranger = lrn("regr.ranger", 
                     num.trees = to_tune(1, 500),
                     mtry= to_tune(1,10))

learner_xgboost = lrn("regr.xgboost",
                      booster = "gbtree",
                      eta = to_tune(0, 1),
                      max_depth = to_tune(1, 30),
                      nrounds = to_tune(p_int(10, 250, tags = "budget")))

learner_rpart = lrn("regr.rpart",
                    cp  = to_tune(0.00001, 0.1))

#Impute
po_impute = po("imputemean")
#fencoder 
fencoder = po("encode", affect_columns = selector_type("factor"))

learner_xgboost = as_learner(fencoder %>>% learner_xgboost)

learner_ranger = as_learner(po_impute %>>% learner_ranger)
learner_xgboost = as_learner(po_impute %>>% learner_xgboost)
learner_rpart = as_learner(po_impute %>>% learner_rpart)

instance_ranger = tune(
 tuner = tnr("mbo"),
 task = task,
 learner = learner_ranger ,
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse"),
 terminator = trm("run_time", secs = 300)
)
learner_ranger$param_set$values = instance_ranger$result_learner_param_vals
autoplot(instance_ranger)

instance_xgboost = tune(
 tuner = tnr("hyperband", eta = 3),
 task = task,
 learner = learner_xgboost ,
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse"),
)
learner_xgboost$param_set$values = instance_xgboost$result_learner_param_vals
autoplot(instance_xgboost)

instance_rpart = tune(
 tuner = tnr("mbo"),
 task = task,
 learner = learner_rpart ,
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse"),
 terminator = trm("run_time", secs = 300)
)
learner_rpart$param_set$values = instance_rpart$result_learner_param_vals
autoplot(instance_rpart)

lrns = c(learner_ranger, learner_xgboost, learner_rpart)

resampling = rsmp("cv", folds = 5)

#Define the Benchmark Grid
d = benchmark_grid(task = task, learner = lrns, resampling = resampling)
#Run the Benchmark
bmr = benchmark(design = d)
mse = bmr$aggregate(msr("regr.rmse"))
mse
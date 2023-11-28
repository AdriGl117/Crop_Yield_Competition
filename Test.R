library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(caTools)
library(ROSE)
library(mlr3measures)
source("get_data.R")

data = get_data(src = "data/Train.csv")
data[] <- lapply(data, function(x) {
 if(is.character(x)) as.factor(x) else x
})

data_Gaya = data %>% filter(District == "Gaya")
data_Jamui = data %>% filter(District == "Jamui")
data_Nalanda = data %>% filter(District == "Nalanda")
data_Vaishali = data %>% filter(District == "Vaishali")

data_Jamui = data_Jamui %>% mutate(Outlier = ifelse(Yield >= 4000, 1, 0))
data_Jamui <- ovun.sample(Outlier ~ ., data = data_Jamui, 
                          method = "over")$data
data_Jamui = data_Jamui %>% select(-Outlier)


task_Gaya = as_task_regr(data_Gaya, target = "Yield", id = "task_Gaya")
task_Jamui = as_task_regr(data_Jamui, target = "Yield", id = "task_Jamui")
task_Nalanda = as_task_regr(data_Nalanda, target = "Yield", id = "task_Nalanda")
task_Vaishali = as_task_regr(data_Vaishali, target = "Yield", id = "task_Vaishali")

po_hist = po("imputehist")

learner = lrn("regr.xgboost")

learner = lrn("regr.xgboost",
                      booster = "gbtree",
                      eta = 0.1,
                      max_depth = 20,
                      nrounds = 250
)

fencoder = po("encode", affect_columns = selector_type("factor"))


learner = as_learner(po_hist %>>% fencoder %>>% learner)

tsks = c(task_Gaya, task_Jamui, task_Nalanda, task_Vaishali)

resampling = rsmp("cv", folds = 5)

#Define the Benchmark Grid
d = benchmark_grid(task = tsks, learner = learner, resampling = resampling)
#Run the Benchmark
bmr = benchmark(design = d)
rmse = bmr$aggregate(msr("regr.rmse"))
rmse


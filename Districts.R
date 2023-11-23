library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
source("get_data.R")

data = get_data(src = "data/Train.csv")
data[] <- lapply(data, function(x) {
 if(is.character(x)) as.factor(x) else x
})
table(data$District)
data_Gaya = data %>% filter(District == "Gaya")
data_Jamui = data %>% filter(District == "Jamui")
data_Nalanda = data %>% filter(District == "Nalanda")
data_Vaishali = data %>% filter(District == "Vaishali")

task_Gaya = as_task_regr(data_Gaya, target = "Yield", id = "task_Gaya")
task_Jamui = as_task_regr(data_Jamui, target = "Yield", id = "task_Jamui")
task_Nalanda = as_task_regr(data_Nalanda, target = "Yield", id = "task_Nalanda")
task_Vaishali = as_task_regr(data_Vaishali, target = "Yield", id = "task_Vaishali")

tsks = c(task_Gaya, task_Jamui, task_Nalanda, task_Vaishali)

po_hist = po("imputehist")

learner = lrn("regr.ranger",
              mtry = 13,
              num.trees = 500,
              max.depth = 300
)

learner = as_learner(po_hist %>>% learner)

#g_ppl = ppl("targettrafo", graph = learner_hist)
#g_ppl$param_set$values$targetmutate.trafo = function(x) log(x)
#g_ppl$param_set$values$targetmutate.inverter = function(x) list(response = exp(x$response))

lrns = c(learner)

resampling = rsmp("cv", folds = 5)

#Define the Benchmark Grid
d = benchmark_grid(task = tsks, learner = lrns, resampling = resampling)
#Run the Benchmark
bmr = benchmark(design = d)
rmse = bmr$aggregate(msr("regr.rmse"))
rmse

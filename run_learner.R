library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
source("get_data.R")

data = get_data(src = "data/Train.csv")
data[] <- lapply(data, function(x) {
 if(is.character(x)) as.factor(x) else x
})

task = as_task_regr(data, target = "Yield", id = "task")

#Replace outliers with NA
Yield <- data %>% select((Yield))
data <- data %>% select(-Yield)
data <- data %>% 
 mutate_if(is.numeric, replace_outliers)
data$Yield <- Yield

task_without_outliers = as_task_regr(data, target = "Yield", id = "without outliers")

tsks = c(task, task_without_outliers)

po_hist = po("imputehist")
po_mean = po("imputemean")
po_median = po("imputemedian")

learner = lrn("regr.ranger",
              mtry = 13,
              num.trees = 500,
              max.depth = 300
              )

learner_hist = as_learner(po_hist %>>% learner)
learner_mean = as_learner(po_mean %>>% learner)
learner_median = as_learner(po_median %>>% learner)

g_ppl = ppl("targettrafo", graph = learner_hist)
g_ppl$param_set$values$targetmutate.trafo = function(x) log(x)
g_ppl$param_set$values$targetmutate.inverter = function(x) list(response = exp(x$response))

lrns = c(learner_hist, learner_mean, learner_median, g_ppl)

resampling = rsmp("cv", folds = 5)

#Define the Benchmark Grid
d = benchmark_grid(task = tsks, learner = lrns, resampling = resampling)
#Run the Benchmark
bmr = benchmark(design = d)
rmse = bmr$aggregate(msr("regr.rmse"))
rmse


# run experiments
rr1 = resample(task = task, learner = g_ppl, resampling = resampling)

# access results
rmse2 <- rr1$score(msr("regr.rmse"))[, .(task_id, learner_id, iteration, regr.rmse)]
rmse2
mean(rmse$regr.rmse)

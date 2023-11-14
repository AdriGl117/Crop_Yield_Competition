library(mlr3verse)
library(dplyr)
library(lubridate)
source("get_task.R")

task = get_task()

learner = lrn("regr.ranger")

resampling = rsmp("cv", folds = 5)

#Define the Benchmark Grid
d = benchmark_grid(task = task, learner = learner, resampling = resampling)
#Run the Benchmark
bmr = benchmark(design = d)
mse = bmr$aggregate(msr("regr.rmse"))
mse

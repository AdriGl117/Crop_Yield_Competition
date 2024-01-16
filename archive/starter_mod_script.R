library(mlr3)
library(mlr3learners)
learner <- lrn("regr.ranger", num.trees = 1000, mtry = 10, num.threads = 15,
               num.random.splits = 5, splitrule = "extratrees",
               respect.unordered.factors = "partition")
df_subset <- df %>% select(-CropTillageDate, -RcNursEstDate,
                           -SeedingSowingTransplanting, -Harv_date, -Threshing_date)

task <- as_task_regr(df_subset, target = "Yield", id = "Yield Crop")
task$set_col_roles("ID", add_to = c("name", "order"), remove_from = "feature")
task

rpo = mlr3pipelines::po("imputelearner", lrn("regr.rpart"))

task = rpo$train(list(task = task))[[1]]
tempdata <- task$data()
tempdata[, `:=` (UreaTotal = BasalUrea + X1tdUrea + X2tdUrea,
                 OrganicFertAmount = Ganaura + CropOrgFYM,
                 BasalLP = BasalUrea + BasalDAP)]
task$cbind(tempdata[, c("UreaTotal", "OrganicFertAmount", "BasalLP")])

cv5 = rsmp("cv", folds = 5)
cv5$instantiate(task)

rr = resample(task, learner, cv5)
rr$score(msr("regr.rmse"), predict_sets = "test")
rr$aggregate(msr("regr.rmse"))



for (i in 1:10) {
 task = rpo$train(list(task = task))[[1]]
 tempdata <- task$data()
 tempdata[, `:=` (UreaTotal = BasalUrea + X1tdUrea + X2tdUrea,
                  OrganicFertAmount = Ganaura + CropOrgFYM,
                  BasalLP = BasalUrea + BasalDAP)]
 task$cbind(tempdata[, c("UreaTotal", "OrganicFertAmount", "BasalLP")])
 
 cv5 = rsmp("cv", folds = 5)
 cv5$instantiate(task)
 
 rr = resample(task, learner, cv5)
 rr$score(msr("regr.rmse"))
 print(rr$aggregate(msr("regr.rmse")))
}

xg_learner = lrn("regr.xgboost")

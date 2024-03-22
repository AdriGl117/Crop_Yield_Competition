library(mlr3fselect)
library(mlr3filters)
library(genalg)

# Information Gain
flt_gain = flt("information_gain")
flt_gain$calculate(task_All)
as.data.table(flt_gain)

# Pearson Correlation
flt_perm = flt("permutation")
flt_perm$param_set
flt_perm$calculate(task_All)

# Feature Importance
ttask = task_All
lrn_ranger = lrn("regr.ranger", importance = "impurity")
ttask$filter(ttask$row_ids[complete.cases(ttask$data())])
flt_importance = flt("importance", learner = lrn_ranger)
flt_importance$calculate(ttask)
as.data.table(flt_importance)

# Feature Selection
instance_ranger = fselect(
 fselector = fs("sequential"),
 task = task_All,
 learner = lrn("regr.ranger"),
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse")
)

autoplot(instance_ranger, type = "performance") + 
 theme(axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold")) + 
 ggtitle("RMSE per Batch and Feature Combination") + 
 theme(plot.title = element_text(hjust = 0.5, size=40)) +
 theme(legend.text = element_text(size = 20))

saveRDS(c(instance_ranger$result_feature_set),
        "features/feature_list_ranger.RDS")

instance_catboost = fselect(
 fselector = fs("sequential", strategy = "sbs"), #sfs forward, sbs backwards
 task = task_All,
 learner = lrn("regr.catboost",
               thread_count = 6),
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse")
)

saveRDS(c(instance_catboost$result_feature_set),#, "ID"), 
        "features/feature_list_catboost_sequential.RDS")
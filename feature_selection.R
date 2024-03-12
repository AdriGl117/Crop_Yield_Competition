library(mlr3fselect)
library(mlr3filters)
library(genalg)

# Information Gain
flt_gain = flt("information_gain")
flt_gain$calculate(task)
as.data.table(flt_gain)

# Pearson Correlation
flt_perm = flt("permutation")
flt_perm$param_set
flt_perm$calculate(task)

# Feature Importance
ttask = task
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
 measures = msr("regr.rmse")#,
 #terminator = trm("evals", n_evals = 50)
)

autoplot(instance_ranger, type = "performance") + 
 theme(axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold")) + 
 ggtitle("RMSE per Batch and Feature Combination") + 
 theme(plot.title = element_text(hjust = 0.5, size=40)) +
 theme(legend.text = element_text(size = 20))

#fselector = fs("sequential")

#fselector$optimize(instance_ranger)

saveRDS(c(instance_ranger$result_feature_set),
        "features/feature_list_ranger.RDS")

instance_catboost = fselect(
 fselector = fs("sequential", strategy = "sfs"), #sfs forward, sbs backwards
 task = task_All,
 learner = lrn("regr.catboost",
               thread_count = 6),
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse")#,
 #terminator = trm("evals", n_evals = 1000)
)

#fselector = fs("genetic_search")#genetic_search

#fselector$optimize(instance_catboost)

saveRDS(c(instance_catboost$result_feature_set),#, "ID"), 
        "features/feature_list_catboost_vaishali.RDS")

#dt = as.data.table(instance$archive)

#task$select(instance$result_feature_set)
library(mlr3fselect)
library(mlr3filters)
library(genalg)

task = as_task_regr(df, target = "Yield", id = "task")
task$set_col_roles("ID", add_to = "name", remove_from = "feature")

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
instance = fsi(
 task = task,
 learner = as_learner(po("imputehist") %>>% lrn("regr.ranger")),
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse"),
 terminator = trm("evals", n_evals = 50)
)

fselector = fs("genetic_search")

fselector$optimize(instance)

saveRDS(c(instance$result_feature_set, "ID"), "data/feature_list.RDS")

#dt = as.data.table(instance$archive)

#task$select(instance$result_feature_set)


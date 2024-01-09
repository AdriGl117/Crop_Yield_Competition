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
instance = fsi(
 task = task,
 learner = as_learner(po("imputehist") %>>% lrn("regr.catboost")),
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse"),
 terminator = trm("evals", n_evals = 50)
)
instance

fselector = fs("genetic_search")
fselector

fselector$optimize(instance)

instance$result_feature_set

instance$result_y

dt = as.data.table(instance$archive)

task$select(instance$result_feature_set)

library(mlr3fselect)

instance = fsi(
 task = task,
 learner = as_learner(po("imputehist") %>>% lrn("regr.catboost")),
 resampling = rsmp("cv", folds = 3),
 measures = msr("regr.rmse"),
 terminator = trm("evals", n_evals = 200)
)
instance

fselector = fs("random_search", batch_size = 5)
fselector

fselector$optimize(instance)

instance$result_feature_set

instance$result_y

task$select(instance$result_feature_set)
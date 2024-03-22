Date <- Sys.Date()
Learner <- learner$id
Hyper_Parameter <- list(learner$param_set$values)
Features <- list(task$feature_names)
Resampling_folds <- cv$param_set$values[[1]]
CV_Score <- rmse
Comment <- "Hyperparameters optimized by Bayesian Optimization with nested resampling"
Imputing <- impute_tech
Time <- learner$timings[[1]]
DistrictSplit = FALSE
District = "All"
fselector <- "genetic_search"

results_Adrian <- data.table::data.table(Date, Seed, Learner, Hyper_Parameter, Features, Imputing, Time, Resampling_folds, CV_Score, Comment, DistrictSplit, District)

saveRDS(results, file = "data/results.RDS")
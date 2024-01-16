Date <- Sys.Date()
Learner <- learner$id
Hyper_Parameter <- format_row(as.list(as.data.frame(learner$param_set$values)[1, ]))
Features <- paste(task$feature_names, collapse = ", ")
resampling_folds <- cv$param_set$values[[1]]
cv_score <- rmse
comment <- "Hyperparameters optimized by Bayesian Optimization with nested resampling"
results <- data.frame(Date,Features, Learner, Hyper_Parameter, resampling_folds, cv_score, comment)

# Funktion zur Formatierung einer Zeile des Data Frames
format_row <- function(row) {
 paste(paste(names(row), row, sep = ":"), collapse = ", ")
}

saveRDS(results, file = "data/results.RDS")

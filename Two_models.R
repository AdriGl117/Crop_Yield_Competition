source("get_data.R")
df = get_data(src = "data/Train.csv")
df[] <- lapply(df, function(x) {
 if(is.character(x)) as.factor(x) else x
})

df = df %>% mutate(Outlier = ifelse(Yield - 5000*Acre > 500, 1, 0))
df$Outlier = as.factor(df$Outlier)
df_norm = df %>% filter(Outlier == "0")
df_norm = df_norm %>% select(-Outlier)
task_norm = as_task_regr(df_norm, target = "Yield", id = "task")

df_out = df %>% filter(Outlier == "1")
df_out = df_out %>% select(-Outlier)
task_out = as_task_regr(df_out, target = "Yield", id = "task")
df = df %>% select(-Yield)

task = as_task_classif(df, target = "Outlier", id = "task")

po = po("imputelearner", lrn("regr.rpart"))
task = po$train(list(task = task))[[1]]

df = task$data()
df = themis::smotenc(df, var = "Outlier", k = 2, over_ratio = 0.1)

##Model that predicts the Outlier Variable for the Test Data##
#df = df %>% select(-Yield)
task_Outlier = as_task_classif(df, target = "Outlier", id = "task")
task$set_col_roles("ID", add_to = "name", remove_from = "feature")

learner_Outlier = lrn("classif.rpart")

po_hist = po("imputehist")

learner_Outlier = as_learner(po_hist %>>% learner_Outlier)

learner_Outlier$train(task)

source("get_test_data.R")
tdf = get_test_data("data/Test.csv")
tdf$Outlier = as.factor(tdf$Outlier)

predicts = learner_Outlier$predict_newdata(newdata = tdf)
Outlier = predicts$response
table(Outlier)
tdf$Outlier = Outlier
tdf$Outlier = as.factor(ifelse(is.na(tdf$Outlier) == TRUE, 1, 0))


tdf_norm = tdf %>% filter(Outlier == "0")
tdf_out = tdf %>% filter(Outlier == "1")
tdf_out[] <- lapply(tdf_out, function(x) {
 if(is.character(x)) as.factor(x) else x
})

learner_norm = lrn("regr.ranger",
                  mtry = 20,
                  num.trees = 500,
                  max.depth = 200
)

learner_norm = as_learner(po_hist %>>% learner_norm)

learner_out = lrn("regr.kknn", k = 2)

learner_out = as_learner(po_hist %>>% learner_out)

learner_norm$train(task_norm)

learner_out$train(task_out)

pr_norm = learner_norm$predict_newdata(newdata = tdf_norm)
Yield = round(pr_norm$response, digits = 2)
ID = tdf_norm$ID
norm = cbind(ID, Yield)

pr_out = learner_out$predict_newdata(newdata = tdf_out)
Yield = round(pr_out$response, digits = 2)
ID = tdf_out$ID
out = cbind(ID, Yield)

PR = rbind(norm, out)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

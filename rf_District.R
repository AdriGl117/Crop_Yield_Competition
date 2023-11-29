library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3filters)
library(dplyr)

#prepare data
df = df %>% mutate(dataType = 0,
                   ob_weight = ifelse(Yield/Acre > 8000 || Yield/Acre < 100, 2, 1))
tdf = tdf %>% mutate(dataType = 1, ob_weight = 1)
df_subset = rbind(df, tdf)
df_subset <- df_subset %>% select(-ob_weight)

#prepare task
task = as_task_regr(df_subset, target = "Yield", id = "Yield Crop")
task$set_col_roles("ID", add_to = "name", remove_from = "feature")
#task$set_col_roles("ob_weight", add_to = "weight", remove_from = "feature")

# define learner
rf_learner = lrn("regr.ranger", num.trees = 600, mtry = 20, num.threads = 15,
   respect.unordered.factors = "partition", importance = "impurity")

# define filter
flt_importance = flt("importance", learner = rf_learner)

# pipelines for prep_graph
targettrans = po("targetmutate", "YieldAcre",
                 param_vals = list(trafo = function(x) x / acre,
                 inverter = function(x) list(response = x$response * acre)))

addvar = po("mutate", "addvariables")
addvar$param_set$values$mutation = list(UreaTotal = ~ BasalUrea + X1tdUrea + X2tdUrea,
   OrganicFertAmount = ~ Ganaura + CropOrgFYM, BasalLP = ~ BasalUrea + BasalDAP)

# build prep_graph
prep_graph = Graph$new()
#prep_graph$add_pipeop(targettrans)
prep_graph$add_pipeop(po("imputelearner", lrn("regr.rpart")))
prep_graph$add_pipeop(addvar)
#prep_graph$add_edge(src_id = "YieldAcre", dst_id = "imputelearner",
#   src_channel = 2, dst_channel = 1)
prep_graph$add_edge(src_id = "imputelearner", dst_id = "addvariables")


# execute prep_graph and split task
task = prep_graph$train(task)[["addvariables.output"]]
task_test = task$clone()
task_test = task_test$filter(which(df_subset$dataType == 1))
task_train = task$clone()
task_train = task_train$filter(which(df_subset$dataType == 0))

task_gaya = task_train$clone()
task_gaya = task_gaya$filter(which(df_subset$District == "Gaya"))
task_gaya$set_col_roles("Block_Gurua", add_to = "stratum")
flt_importance$calculate(task_gaya)
task_gaya$select(names(flt_importance$scores)[1:75])

task_jamui = task_train$clone()
task_jamui = task_jamui$filter(which(df_subset$District == "Jamui"))
task_jamui$set_col_roles("Block_Jamui", add_to = "stratum")
flt_importance$calculate(task_jamui)
task_jamui$select(names(flt_importance$scores)[1:75])

task_nalanda = task_train$clone()
task_nalanda = task_nalanda$filter(which(df_subset$District == "Nalanda"))
task_nalanda$set_col_roles("Block_Rajgir", add_to = "stratum")
flt_importance$calculate(task_nalanda)
task_nalanda$select(names(flt_importance$scores)[1:75])

task_vaishali = task_train$clone()
task_vaishali = task_vaishali$filter(which(df_subset$District == "Vaishali"))
task_vaishali$set_col_roles("Block_Garoul", add_to = "stratum")
task_vaishali$set_col_roles("Block_Mahua", add_to = "stratum")
flt_importance$calculate(task_gaya)
task_gaya$select(names(flt_importance$scores)[1:75])

# build modelling graph
targetinverse = po("targetmutate", "YieldAcre",
   param_vals = list(trafo = function(x) x,
   inverter = function(x) list(response = x$response)))

gr_single_pred = po("subsample", frac = 1, replace = TRUE) %>>%
   rf_learner

gr_pred_set = ppl("greplicate", graph = gr_single_pred, n = 10)
gr_bagging = gr_pred_set %>>% po("regravg", innum = 10)


gr_mod = gunion(list(po("copy", outnum = 10) %>>% gr_bagging, PipeOpNOP$new()))
gr_mod$add_pipeop(targetinverse)
gr_mod$add_edge(src_id = "YieldAcre", dst_id = "copy", src_channel = 2, dst_channel = 1)
gr_mod$add_edge(src_id = "YieldAcre", dst_id = "nop", src_channel = 1, dst_channel = 1)
gr_mod$add_pipeop(PipeOpTargetInvert$new())
gr_mod$add_edge(src_id = "nop", dst_id = "targetinvert", src_channel = 1, dst_channel = 1)
gr_mod$add_edge(src_id = "regravg", dst_id = "targetinvert", src_channel = 1, dst_channel = 2)
gr_mod$keep_results = TRUE

# model
rf_bagging = as_learner(gr_mod)

rf_gaya = rf_bagging$clone()
acre = df %>% filter(District == "Gaya") %>% select(Acre) %>% pull()
rf_gaya$train(task_gaya)
result_gaya = rf_gaya$predict(task_gaya)

rf_jamui = rf_bagging$clone()
acre = df %>% filter(District == "Jamui") %>% select(Acre) %>% pull()
rf_jamui$train(task_jamui)
result_jamui = rf_jamui$predict(task_jamui)

rf_nalanda = rf_bagging$clone()
acre = df %>% filter(District == "Nalanda") %>% select(Acre) %>% pull()
rf_nalanda$train(task_nalanda)
result_nalanda = rf_nalanda$predict(task_nalanda)

rf_vaishali = rf_bagging$clone()
acre = df %>% filter(District == "Vaishali") %>% select(Acre) %>% pull()
rf_vaishali$train(task_vaishali)
result_vaishali = rf_vaishali$predict(task_vaishali)

# scores
result_gaya$score(msr("regr.rmse"))
result_jamui$score(msr("regr.rmse"))
result_nalanda$score(msr("regr.rmse"))
result_vaishali$score(msr("regr.rmse"))

results = PredictionRegr$new(
   row_ids = c(result_gaya$data$row_ids, result_jamui$data$row_ids,
      result_nalanda$data$row_ids, result_vaishali$data$row_ids),
   truth = c(result_gaya$data$truth, result_jamui$data$truth,
      result_nalanda$data$truth, result_vaishali$data$truth),
   response = c(result_gaya$data$response, result_jamui$data$response,
      result_nalanda$data$response, result_vaishali$data$response),
   se = c(result_gaya$se, result_jamui$se, result_nalanda$se, result_vaishali$se))
results$score(msr("regr.rmse"))



#### predicition on test data ####
# execute prep_graph and split task
ttask_gaya = task_test$clone()
ttask_gaya = ttask_gaya$filter(which(df_subset$District == "Gaya"))

ttask_jamui = task_test$clone()
ttask_jamui = ttask_jamui$filter(which(df_subset$District == "Jamui"))

ttask_nalanda = task_test$clone()
ttask_nalanda = ttask_nalanda$filter(which(df_subset$District == "Nalanda"))

ttask_vaishali = task_test$clone()
ttask_vaishali = ttask_vaishali$filter(which(df_subset$District == "Vaishali"))

#acre = tdf %>% filter(District == "Gaya") %>% select(Acre) %>% pull()
result_gaya_test = rf_gaya$predict_newdata(ttask_gaya$data(), task = task_gaya)
result_jamui_test = rf_jamui$predict_newdata(ttask_jamui$data(), task = task_jamui)
result_nalanda_test = rf_nalanda$predict_newdata(ttask_nalanda$data(), task = task_nalanda)
result_vaishali_test = rf_vaishali$predict_newdata(ttask_vaishali$data(), task = task_vaishali)

results_test = PredictionRegr$new(
   row_ids = c(ttask_gaya$row_ids, ttask_jamui$row_ids,
               ttask_nalanda$row_ids, ttask_vaishali$row_ids),
   truth = c(result_gaya_test$data$truth, result_jamui_test$data$truth,
             result_nalanda_test$data$truth, result_vaishali_test$data$truth),
   response = c(result_gaya_test$data$response, result_jamui_test$data$response,
                result_nalanda_test$data$response, result_vaishali_test$data$response),
   se = c(result_gaya_test$se, result_jamui_test$se, result_nalanda_test$se, result_vaishali_test$se))

abgabe = data.frame(ID = df_subset[results_test$row_ids, "ID"] %>% pull(),
                    Yield = round(results_test$data$response, 2))

write.table(abgabe, file = "pred_rf_District.csv", row.names = FALSE, dec = ".", sep = ",")

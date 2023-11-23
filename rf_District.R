library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(dplyr)

#prepare data
df = df %>% arrange(ID)
acre = df$Acre
df_subset <- df %>% select(-CropTillageDate, -RcNursEstDate, -Acre, -ID,
                           -SeedingSowingTransplanting, -Harv_date, -Threshing_date)

#prepare task
task = as_task_regr(df_subset, target = "Yield", id = "Yield Crop")
#task$set_col_roles("ID", add_to = c("name", "order"), remove_from = "feature")

# define learner
rf_learner = lrn("regr.ranger", num.trees = 600, mtry = 10, num.threads = 15,
   respect.unordered.factors = "partition")

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
task = prep_graph$train(task)

task_gaya = task$clone()
task_gaya = task_gaya$filter(which(df_subset$District == "Gaya"))

task_jamui = task$clone()
task_jamui = task_jamui$filter(which(df_subset$District == "Jamui"))

task_nalanda = task$clone()
task_nalanda = task_nalanda$filter(which(df_subset$District == "Nalanda"))

task_vaishali = task$clone()
task_vaishali = task_vaishali$filter(which(df_subset$District == "Vaishali"))

# build modelling graph
targetinverse = po("targetmutate", "YieldAcre",
   param_vals = list(trafo = function(x) x / acre,
   inverter = function(x) list(response = x$response * acre)))

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

# predicition on test data
tdf = tdf %>% arrange(ID)
tdf_subset <- tdf %>% select(-CropTillageDate, -RcNursEstDate, -Acre, -ID,
                           -SeedingSowingTransplanting, -Harv_date, -Threshing_date)
test_task = as_task_regr(tdf_subset, target = "Yield", id = "Yield Crop")

# execute prep_graph and split task
test_task = prep_graph$train(test_task)[["addvariables.output"]]

ttask_gaya = test_task$clone()
ttask_gaya = ttask_gaya$filter(which(df_subset$District == "Gaya"))

ttask_jamui = test_task$clone()
ttask_jamui = ttask_jamui$filter(which(df_subset$District == "Jamui"))

ttask_nalanda = test_task$clone()
ttask_nalanda = ttask_nalanda$filter(which(df_subset$District == "Nalanda"))

ttask_vaishali = test_task$clone()
ttask_vaishali = ttask_vaishali$filter(which(df_subset$District == "Vaishali"))



acre = tdf %>% filter(District == "Gaya") %>% select(Acre) %>% pull()
result_gaya = rf_gaya$predict_newdata(ttask_gaya$data())

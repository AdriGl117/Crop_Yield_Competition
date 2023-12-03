library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(dplyr)

#prepare data
df_subset <- df %>% select(-CropTillageDate, -RcNursEstDate, -ID,
                           -SeedingSowingTransplanting, -Harv_date, -Threshing_date)

#prepare task
task = as_task_regr(df_subset, target = "Yield", id = "Yield Crop")
#task$set_col_roles("ID", add_to = "name", remove_from = "feature")

# define learner
rf_learner = lrn("regr.ranger", num.trees = 600, mtry = 10, num.threads = 15,
                 respect.unordered.factors = "partition")

# pipelines for prep_graph
addvar = po("mutate", "addvariables")
addvar$param_set$values$mutation = list(UreaTotal = ~ BasalUrea + X1tdUrea + X2tdUrea,
    OrganicFertAmount = ~ Ganaura + CropOrgFYM, BasalLP = ~ BasalUrea + BasalDAP)

# build prep_graph
prep_graph = Graph$new()
prep_graph$add_pipeop(po("imputelearner", lrn("regr.rpart")))
prep_graph$add_pipeop(addvar)
prep_graph$add_edge(src_id = "imputelearner", dst_id = "addvariables")

task = prep_graph$train(task)[["addvariables.output"]]
# sampling
weightedBootstrap = function(rows_id, n = 10, ratio = 1, weights = NULL) {
  bootstrap_sample = list(train_set = list(), test_set = list())
  for (i in seq_len(n)) {
   bootstrap_sample[["train_set"]][[i]] = sample(rows_id, round(length(rows_id) * ratio), TRUE, weights)
   bootstrap_sample[["test_set"]][[i]] = rows_id[!rows_id %in% bootstrap_sample[["train_set"]][[i]]]
  }
  bootstrap_sample
}
# possible sampling weights 
df %>% mutate(weights = sqrt(abs(Yield/Acre - mean(df$Yield/df$Acre)))) %>% pull()
df %>% mutate(q_YieldAcre = ecdf(df$Yield/df$Acre)(Yield/Acre),
              weights = ifelse(q_YieldAcre > 0.99 & q_YieldAcre < 0.01, 1.5, 1)) %>% pull()

rsmp_bootstrap = rsmp("bootstrap", ratio = 1, repeats = 10)

#
task_gaya = task$clone()
task_gaya = task_gaya$filter(which(df_subset$District == "Gaya"))
btsets_gaya = weightedBootstrap(which(df_subset$District == "Gaya"))
rsmp_bootstrap$instantiate(task_gaya)

rr_gaya = resample(task_gaya, rf_learner, rsmp_bootstrap)

task_jamui = task$clone()
task_jamui = task_jamui$filter(which(df_subset$District == "Jamui"))
btsets_jamui = weightedBootstrap(which(df_subset$District == "Jamui"))
rsmp_bootstrap$instantiate(task_jamui)

rr_jamui = resample(task_jamui, rf_learner, rsmp_bootstrap)

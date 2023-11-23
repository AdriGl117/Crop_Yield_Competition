library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(caTools)
library(mlr3measures)
source("get_data.R")

data = get_data(src = "data/Train.csv")
data[] <- lapply(data, function(x) {
 if(is.character(x)) as.factor(x) else x
})

data_Gaya = data %>% filter(District == "Gaya")
data_Jamui = data %>% filter(District == "Jamui")
data_Nalanda = data %>% filter(District == "Nalanda")
data_Vaishali = data %>% filter(District == "Vaishali")

split_Gaya = sample.split(data_Gaya$Yield, SplitRatio = 0.8)
train_Gaya = subset(data_Gaya, split_Gaya == TRUE)
test_Gaya = subset(data_Gaya, split_Gaya == FALSE)

split_Jamui = sample.split(data_Jamui$Yield, SplitRatio = 0.8)
train_Jamui = subset(data_Jamui, split_Jamui == TRUE)
test_Jamui = subset(data_Jamui, split_Jamui == FALSE)

split_Nalanda = sample.split(data_Nalanda$Yield, SplitRatio = 0.8)
train_Nalanda = subset(data_Nalanda, split_Nalanda == TRUE)
test_Nalanda = subset(data_Nalanda, split_Nalanda == FALSE)

split_Vaishali = sample.split(data_Vaishali$Yield, SplitRatio = 0.8)
train_Vaishali = subset(data_Vaishali, split_Vaishali == TRUE)
test_Vaishali = subset(data_Vaishali, split_Vaishali == FALSE)

#train = train %>% mutate(Yield = case_when(Yield > 1000 ~ 1000,
#                                           Yield <= 1000 ~ Yield))

train_Jamui = train_Jamui %>% mutate(Yield = ifelse(Yield/Acre > 10000, Acre * 10000, Yield))

task_Gaya = as_task_regr(train_Gaya, target = "Yield", id = "task_Gaya")
task_Gaya_test = as_task_regr(test_Gaya, target = "Yield", id = "task_Gaya_test")

task_Jamui = as_task_regr(train_Jamui, target = "Yield", id = "task_Jamui")
task_Jamui_test = as_task_regr(test_Jamui, target = "Yield", id = "task_Jamui_test")

task_Nalanda = as_task_regr(train_Nalanda, target = "Yield", id = "task_Nalanda")
task_Nalanda_test = as_task_regr(test_Nalanda, target = "Yield", id = "task_Nalanda_test")

task_Vaishali = as_task_regr(train_Vaishali, target = "Yield", id = "task_Vaishali")
task_Vaishali_test = as_task_regr(test_Vaishali, target = "Yield", id = "task_Vaishali_test")

po_hist = po("imputehist")

learner = lrn("regr.ranger",
              mtry = 13,
              num.trees = 500,
              max.depth = 300
)

learner_Gaya = as_learner(po_hist %>>% learner)
learner_Jamui = as_learner(po_hist %>>% learner)
learner_Nalanda = as_learner(po_hist %>>% learner)
learner_Vaishali = as_learner(po_hist %>>% learner)

learner_Gaya$train(task_Gaya)
learner_Jamui$train(task_Jamui)
learner_Nalanda$train(task_Nalanda)
learner_Vaishali$train(task_Vaishali)

pr_Gaya = learner_Gaya$predict(task_Gaya_test)
pr_Jamui = learner_Jamui$predict(task_Jamui_test)
pr_Nalanda = learner_Nalanda$predict(task_Nalanda_test)
pr_Vaishali = learner_Vaishali$predict(task_Vaishali_test)

rmse_Gaya = rmse(pr_Gaya$truth, pr_Gaya$response)
rmse_Jamui = rmse(pr_Jamui$truth, pr_Jamui$response)
rmse_Nalanda = rmse(pr_Nalanda$truth, pr_Nalanda$response)
rmse_Vaishali = rmse(pr_Vaishali$truth, pr_Vaishali$response)



source("get_data.R")
library(ROSE)

data = get_data(src = "data/Train.csv")

data_Gaya = data %>% filter(District == "Gaya")
data_Jamui = data %>% filter(District == "Jamui")
data_Nalanda = data %>% filter(District == "Nalanda")
data_Vaishali = data %>% filter(District == "Vaishali")

data_Jamui = data_Jamui%>% mutate(Outlier = as.factor(ifelse(Yield > 4000, 1, 0)))
data_Jamui <- ovun.sample(Outlier ~ ., data = data_Jamui, 
                     method = "over")$data
data_Jamui = data_Jamui %>% select(-Outlier)

task_Gaya = as_task_regr(data_Gaya, target = "Yield", id = "task_Gaya")
task_Jamui = as_task_regr(data_Jamui, target = "Yield", id = "task_Jamui")
task_Nalanda = as_task_regr(data_Nalanda, target = "Yield", id = "task_Nalanda")
task_Vaishali = as_task_regr(data_Vaishali, target = "Yield", id = "task_Vaishali")

po_hist = po("imputehist")

learner = lrn("regr.ranger",
              mtry = 25,
              num.trees = 500,
              max.depth = 200
)

learner_Gaya = as_learner(po_hist %>>% learner)
learner_Jamui = as_learner(po_hist %>>% learner)
learner_Nalanda = as_learner(po_hist %>>% learner)
learner_Vaishali = as_learner(po_hist %>>% learner)

learner_Gaya$train(task_Gaya)
learner_Jamui$train(task_Jamui)
learner_Nalanda$train(task_Nalanda)
learner_Vaishali$train(task_Vaishali)

data_test = get_data(src = "data/Test.csv")
data_test$Yield = as.numeric(0)
data_test$OF_Jeevamrit = FALSE
data_test$CF_MoP = FALSE
data_test$FTDF_NPK = FALSE

data_test_Gaya = data_test %>% filter(District == "Gaya")
data_test_Jamui = data_test %>% filter(District == "Jamui")
data_test_Nalanda = data_test %>% filter(District == "Nalanda")
data_test_Vaishali = data_test %>% filter(District == "Vaishali")

task_Gaya_test = as_task_regr(data_test_Gaya, target = "Yield", id = "task_test_Gaya")
task_Jamui_test = as_task_regr(data_test_Jamui, target = "Yield", id = "task_test_Jamui")
task_Nalanda_test = as_task_regr(data_test_Nalanda, target = "Yield", id = "task_test_Nalanda")
task_Vaishali_test = as_task_regr(data_test_Vaishali, target = "Yield", id = "task_test_Vaishali")

pr_Gaya = learner_Gaya$predict(task_Gaya_test)
Yield = round(pr_Gaya$response, digits = 2)
ID = data_test_Gaya$ID
Gaya = cbind(ID, Yield)

pr_Jamui = learner_Jamui$predict(task_Jamui_test)
Yield = round(pr_Jamui$response, digits = 2)
ID = data_test_Jamui$ID
Jamui = cbind(ID, Yield)

pr_Nalanda = learner_Nalanda$predict(task_Nalanda_test)
Yield = round(pr_Nalanda$response, digits = 2)
ID = data_test_Nalanda$ID
Nalanda = cbind(ID, Yield)

pr_Vaishali = learner_Vaishali$predict(task_Vaishali_test)
Yield = round(pr_Vaishali$response, digits = 2)
ID = data_test_Vaishali$ID
Vaishali = cbind(ID, Yield)

PR = rbind(Gaya, Jamui, Nalanda, Vaishali)

write.table(PR, "~/Desktop/Predicts.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

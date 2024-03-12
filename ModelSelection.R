library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)

Seed <- 1234
sourcing = TRUE
impute_tech = "hot deck"
DistrictSplit = TRUE
holdout = FALSE
source("data_cleaning.R")
source("imputation.R")
DistrictSplit = FALSE
source("data_cleaning.R")
source("imputation.R")
task_All$select(readRDS("features/feature_list_catboost.RDS"))
task_gaya$select(readRDS("features/feature_list_catboost_gaya.RDS"))
task_jamui$select(readRDS("features/feature_list_catboost_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_catboost_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_catboost_vaishali.RDS"))
task_train_gaya = task_gaya
task_train_jamui = task_jamui
task_train_nalanda = task_nalanda
task_train_vaishali = task_vaishali

holdout = TRUE
source("data_cleaning.R")
DistrictSplit = T
source("imputation.R")
task_gaya$select(readRDS("features/feature_list_catboost_gaya.RDS"))
task_jamui$select(readRDS("features/feature_list_catboost_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_catboost_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_catboost_vaishali.RDS"))
task_test_gaya = task_gaya
task_test_jamui = task_jamui
task_test_nalanda = task_nalanda
task_test_vaishali = task_vaishali

best_result_gaya = readRDS("data/results.RDS") %>% filter(District == "gaya" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
learner_gaya = lrn(paste0(best_result_gaya$Learner))
learner_gaya$param_set$values = best_result_gaya$Hyper_Parameter[[1]]

best_result_nalanda = readRDS("data/results.RDS") %>% filter(District == "nalanda" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
learner_nalanda = lrn(paste0(best_result_nalanda$Learner))
learner_nalanda$param_set$values = best_result_nalanda$Hyper_Parameter[[1]]

best_result_jamui = readRDS("data/results.RDS") %>% filter(District == "jamui" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
learner_jamui = lrn(paste0(best_result_jamui$Learner))
learner_jamui$param_set$values = best_result_jamui$Hyper_Parameter[[1]]

best_result_vaishali = readRDS("data/results.RDS") %>% filter(District == "vaishali" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
learner_vaishali = lrn(paste0(best_result_vaishali$Learner))
learner_vaishali$param_set$values = best_result_vaishali$Hyper_Parameter[[1]]

learner_gaya$train(task_train_gaya)
learner_jamui$train(task_train_jamui)
learner_nalanda$train(task_train_nalanda)
learner_vaishali$train(task_train_vaishali)


pr_gaya_test = learner_gaya$predict(task_test_gaya)
pr_gaya_train = learner_gaya$predict(task_train_gaya)
pr_gaya_test = data.table(pr_gaya_test$truth, pr_gaya_test$response)
pr_gaya_train = data.table(pr_gaya_train$truth, pr_gaya_train$response)

pr_nalanda_test = learner_nalanda$predict(task_test_nalanda)
pr_nalanda_train = learner_nalanda$predict(task_train_nalanda)
pr_nalanda_test = data.table(pr_nalanda_test$truth, pr_nalanda_test$response)
pr_nalanda_train = data.table(pr_nalanda_train$truth, pr_nalanda_train$response)

pr_jamui_test = learner_jamui$predict(task_test_jamui)
pr_jamui_train = learner_jamui$predict(task_train_jamui)
pr_jamui_test = data.table(pr_jamui_test$truth, pr_jamui_test$response)
pr_jamui_train = data.table(pr_jamui_train$truth, pr_jamui_train$response)

pr_vaishali_test = learner_vaishali$predict(task_test_vaishali)
pr_vaishali_train = learner_vaishali$predict(task_train_vaishali)
pr_vaishali_test = data.table(pr_vaishali_test$truth, pr_vaishali_test$response)
pr_vaishali_train = data.table(pr_vaishali_train$truth, pr_vaishali_train$response)

library(ggplot2)
library(ggpubr)

p1_gaya = ggplot(pr_gaya_train) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Gaya (Train)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 

p2_gaya = ggplot(pr_gaya_test) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Gaya (Holdout)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 


p1_nalanda = ggplot(pr_nalanda_train) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Nalanda (Train)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 


p2_nalanda = ggplot(pr_nalanda_test) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Nalanda (Holdout)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 


p1_jamui = ggplot(pr_jamui_train) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Jamui (Train)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 


p2_jamui = ggplot(pr_jamui_test) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Jamui (Holdout)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 


p1_vaishali = ggplot(pr_vaishali_train) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Vaishali (Train)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 
 

p2_vaishali = ggplot(pr_vaishali_test) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 labs(x = NULL, y = NULL) +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Vaishali (Holdout)") + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5, size = 20)) 

plot1 = ggarrange(p1_gaya, p1_nalanda, p2_gaya, p2_nalanda, nrow = 2, ncol = 2)
annotate_figure(plot1, left = text_grob("Predicted Yield/Acre", size = 20, rot = 90),
                bottom = text_grob("Yield/Acre", size = 20))

plot2 = ggarrange(p1_jamui, p1_vaishali, p2_jamui, p2_vaishali, nrow = 2, ncol = 2)
annotate_figure(plot2, left = text_grob("Predicted Yield/Acre", size = 20, rot = 90),
                bottom = text_grob("Yield/Acre", size = 20))

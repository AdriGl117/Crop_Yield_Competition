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

best_result = readRDS("data/results.RDS") %>% filter(District == "All" & Date != "2024-01-16" & Learner == "regr.catboost") %>% filter(CV_Score == min(CV_Score))
learner_All = lrn(paste0(best_result$Learner))
learner_All$param_set$values = best_result$Hyper_Parameter[[1]]

best_result_gaya = readRDS("data/results.RDS") %>% filter(District == "gaya" & Date != "2024-01-16" & Learner == "regr.catboost") %>% filter(CV_Score == min(CV_Score))
learner_gaya = lrn(paste0(best_result_gaya$Learner))
learner_gaya$param_set$values = best_result_gaya$Hyper_Parameter[[1]]

best_result_nalanda = readRDS("data/results.RDS") %>% filter(District == "nalanda" & Date != "2024-01-16" & Learner == "regr.catboost") %>% filter(CV_Score == min(CV_Score))
learner_nalanda = lrn(paste0(best_result_nalanda$Learner))
learner_nalanda$param_set$values = best_result_nalanda$Hyper_Parameter[[1]]

best_result_jamui = readRDS("data/results.RDS") %>% filter(District == "jamui" & Date != "2024-01-16" & Learner == "regr.catboost") %>% filter(CV_Score == min(CV_Score))
learner_jamui = lrn(paste0(best_result_jamui$Learner))
learner_jamui$param_set$values = best_result_jamui$Hyper_Parameter[[1]]

best_result_vaishali = readRDS("data/results.RDS") %>% filter(District == "vaishali" & Date != "2024-01-16" & Learner == "regr.catboost") %>% filter(CV_Score == min(CV_Score))
learner_vaishali = lrn(paste0(best_result_vaishali$Learner))
learner_vaishali$param_set$values = best_result_vaishali$Hyper_Parameter[[1]]

learner_All$train(task_All)
learner_gaya$train(task_gaya)
learner_jamui$train(task_jamui)
learner_nalanda$train(task_nalanda)
learner_vaishali$train(task_vaishali)

pr_All = learner_All$predict(task_All)
rmse_All = round(pr_All$score(msr("regr.rmse")))
pr_All = data.table(pr_All$truth, pr_All$response)
pr_gaya = learner_gaya$predict(task_gaya)
rmse_gaya = round(pr_gaya$score(msr("regr.rmse")))
pr_gaya = data.table(pr_gaya$truth, pr_gaya$response)
pr_jamui = learner_jamui$predict(task_jamui)
rmse_jamui = round(pr_jamui$score(msr("regr.rmse")))
pr_jamui = data.table(pr_jamui$truth, pr_jamui$response)
pr_nalanda = learner_nalanda$predict(task_nalanda)
rmse_nalanda = round(pr_nalanda$score(msr("regr.rmse")))
pr_nalanda = data.table(pr_nalanda$truth, pr_nalanda$response)
pr_vaishali = learner_vaishali$predict(task_vaishali)
rmse_vaishali = round(pr_vaishali$score(msr("regr.rmse")))
pr_vaishali = data.table(pr_vaishali$truth, pr_vaishali$response)


ggplot(pr_All) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 xlab("Yield/Acre") +
 ylab("Predicted Yield/Acre") +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Combined Model") + 
 theme(plot.title = element_text(hjust = 0.5))

p1 = ggplot(pr_gaya) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 xlab("Yield/Acre") +
 ylab("Predicted Yield/Acre") +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Gaya") + 
 theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(pr_nalanda) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 xlab("Yield/Acre") +
 ylab("Predicted Yield/Acre") +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Nalanda") + 
 theme(plot.title = element_text(hjust = 0.5))

p3 = ggplot(pr_jamui) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 xlab("Yield/Acre") +
 ylab("Predicted Yield/Acre") +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Jamui") + 
 theme(plot.title = element_text(hjust = 0.5))

p4 = ggplot(pr_vaishali) + 
 aes(x = V1, y = V2) +
 geom_point() +
 geom_abline(slope = 1) +
 xlab("Yield/Acre") +
 ylab("Predicted Yield/Acre") +
 xlim(0, 5000) +
 ylim(0, 5000) +
 ggtitle("Vaishali") + 
 theme(plot.title = element_text(hjust = 0.5))

plot = ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
annotate_figure(plot, top = text_grob("District Models"))

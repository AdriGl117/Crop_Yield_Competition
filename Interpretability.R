library(iml)
library(ggplot2)
library(ggpubr)
library(mlr3verse)
library(dplyr)
library(lubridate)
library(mlr3mbo)
library(data.table)
library(mlr3learners.catboost)

sourcing = TRUE
impute_tech = "hot deck"
DistrictSplit = TRUE
holdout = FALSE
source("data_cleaning.R")
source("imputation.R")
task_gaya$select(readRDS("features/feature_list_catboost_gaya.RDS"))
task_jamui$select(readRDS("features/feature_list_ranger_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_ranger_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_catboost_vaishali.RDS"))
DistrictSplit = FALSE
source("imputation.R")
task_All$select(readRDS("features/feature_list_ranger.RDS"))
holdout = TRUE
source("data_cleaning.R")

#######FEATURE IMPORTANCE#######
#All
best_result = readRDS("data/results.RDS") %>% filter(District == "All" & Date != "2024-01-16" & Learner == "regr.ranger") %>% filter(CV_Score == min(CV_Score))
seed = best_result$Seed
set.seed(seed)
learner = lrn(paste0(best_result$Learner))
learner$param_set$values = best_result$Hyper_Parameter[[1]]
learner$train(task_All)
X = df_copy %>% select(task_All$feature_names)
y = df_copy %>% select(task_All$target_names)
predictor = Predictor$new(learner, data = X, y = y)
importance = FeatureImp$new(predictor, loss = "rmse", n.repetitions = 100)
plot(importance) +
 theme_minimal() +
 theme(axis.text=element_text(size=30), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))
ggsave("graphics/Feature_imp_All.jpeg", dpi = 300, width = 64, height = 40, units = "cm")

#Gaya
best_result_gaya = readRDS("data/results.RDS") %>% filter(District == "gaya" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
seed = best_result_gaya$Seed
set.seed(seed)
learner_gaya = lrn(paste0(best_result_gaya$Learner))
learner_gaya$param_set$values = best_result_gaya$Hyper_Parameter[[1]]
learner_gaya$train(task_gaya)
X_gaya = df_copy %>% filter(District == "Gaya") %>% select(task_gaya$feature_names)
y_gaya = df_copy %>% filter(District == "Gaya") %>% select(task_gaya$target_names)
predictor_gaya = Predictor$new(learner_gaya, data = X_gaya, y = y_gaya)
importance_gaya = FeatureImp$new(predictor_gaya, loss = "rmse", n.repetitions = 100)
plot(importance_gaya) +
 theme_minimal() +
 #ggtitle("Gaya")+
 theme(axis.text=element_text(size=30), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))
ggsave("graphics/Feature_imp_Gaya.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#Nalanda
best_result_nalanda = readRDS("data/results.RDS") %>% filter(District == "nalanda" & Date != "2024-01-16" & Learner == "regr.ranger") %>% filter(CV_Score == min(CV_Score))
seed = best_result_nalanda$Seed
set.seed(seed)
learner_nalanda = lrn(paste0(best_result_nalanda$Learner))
learner_nalanda$param_set$values = best_result_nalanda$Hyper_Parameter[[1]]
learner_nalanda$train(task_nalanda)
X_nalanda = df_copy %>% filter(District == "Nalanda") %>% select(task_nalanda$feature_names)
y_nalanda = df_copy %>% filter(District == "Nalanda") %>% select(task_nalanda$target_names)
predictor_nalanda = Predictor$new(learner_nalanda, data = X_nalanda, y = y_nalanda)
importance_nalanda = FeatureImp$new(predictor_nalanda, loss = "rmse", n.repetitions = 100)
plot(importance_nalanda) +
 theme_minimal() +
 #ggtitle("Nalanda") +
 theme(axis.text=element_text(size=30), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))
ggsave("graphics/Feature_imp_Nalanda.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#Jamui
best_result_jamui = readRDS("data/results.RDS") %>% filter(District == "jamui" & Date != "2024-01-16" & Learner == "regr.ranger") %>% filter(CV_Score == min(CV_Score))
seed = best_result_jamui$Seed
set.seed(seed)
learner_jamui = lrn(paste0(best_result_jamui$Learner))
learner_jamui$param_set$values = best_result_jamui$Hyper_Parameter[[1]]
learner_jamui$train(task_jamui)
X_jamui = df_copy %>% filter(District == "Jamui") %>% select(task_jamui$feature_names)
y_jamui = df_copy %>% filter(District == "Jamui") %>% select(task_jamui$target_names)
predictor_jamui = Predictor$new(learner_jamui, data = X_jamui, y = y_jamui)
importance_jamui = FeatureImp$new(predictor_jamui, loss = "rmse", n.repetitions = 100)
plot(importance_jamui) +
 theme_minimal() +
 #ggtitle("Jamui")+
 theme(axis.text=element_text(size=30), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))
ggsave("graphics/Feature_imp_Jamui.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#Vaishali
best_result_vaishali = readRDS("data/results.RDS") %>% filter(District == "vaishali" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
seed = best_result_vaishali$Seed
set.seed(seed)
learner_vaishali = lrn(paste0(best_result_vaishali$Learner))
learner_vaishali$param_set$values = best_result_vaishali$Hyper_Parameter[[1]]
learner_vaishali$train(task_vaishali)
X_vaishali = df_copy %>% filter(District == "Vaishali") %>% select(task_vaishali$feature_names)
y_vaishali = df_copy %>% filter(District == "Vaishali") %>% select(task_vaishali$target_names)
predictor_vaishali = Predictor$new(learner_vaishali, data = X_vaishali, y = y_vaishali)
importance_vaishali = FeatureImp$new(predictor_vaishali, loss = "rmse", n.repetitions = 100)
plot(importance_vaishali) +
 theme_minimal() +
 #ggtitle("Vaishali")+
 theme(axis.text=element_text(size=30), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))
ggsave("graphics/Feature_imp_Vaishali.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#######FEATURE EFFECTS#######
#All
i = 1
features = importance$results[1:4, 1]
for(feature in features){
 assign(paste("effect_", feature, sep = ""), 
        FeatureEffect$new(predictor, feature = feature, method = "pdp+ice"))
 assign(paste0("p_",i), plot(get(paste0("effect_", feature)))+
         theme_minimal() +
         #ggtitle(paste0("PDP and ICE Curve \n Gaya"))+
         #theme(plot.title = element_text(hjust = 0.5))+
         theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) 
        #theme(plot.title = element_text(hjust = 0.4, size=40)))
 )
 i = i + 1
}
FE_All = ggarrange(p_1, p_2, p_3, p_4, nrow = 2, ncol = 2)
FE_All
ggsave("graphics/Feature_Effects_All.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

 #Gaya
i = 1
features_Gaya = importance_gaya$results[1:4, 1]
for(feature in features_Gaya){
  assign(paste("effect_", feature, sep = ""), 
         FeatureEffect$new(predictor_gaya, feature = feature, method = "pdp+ice"))
  assign(paste0("p_gaya_",i), plot(get(paste0("effect_", feature)))+
          theme_minimal() +
          #ggtitle(paste0("PDP and ICE Curve \n Gaya"))+
          #theme(plot.title = element_text(hjust = 0.5))+
          theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) 
          #theme(plot.title = element_text(hjust = 0.4, size=40)))
  )
  #plot(get(paste0("p_gaya_", i)))
  i = i + 1
}
p_gaya_1 = p_gaya_1 +
 xlim(0, 4000)

FE_Gaya = ggarrange(p_gaya_1, p_gaya_2, p_gaya_3, p_gaya_4, nrow = 2, ncol = 2)
FE_Gaya
ggsave("graphics/Feature_Effects_Gaya.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#Jamui
i = 1
features_Jamui = importance_jamui$results[1:4, 1]
for(feature in features_Jamui){
 assign(paste("effect_", feature, sep = ""), 
        FeatureEffect$new(predictor_jamui, feature = feature, method = "pdp+ice"))
 assign(paste0("p_jamui_",i), plot(get(paste0("effect_", feature)))+
         theme_minimal() +
         labs(y = NULL) +
         #ggtitle(paste0("PDP and ICE Curve \n Jamui"))+
         #theme(plot.title = element_text(hjust = 0.5))+
         theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) 
         #theme(plot.title = element_text(hjust = 0.4, size=40)))
 )
 i = i + 1
}
p_jamui_2 = p_jamui_2 +
 xlim(20, 30)
p_jamui_2
FE_Jamui = ggarrange(p_jamui_1, p_jamui_2, p_jamui_3, p_jamui_4, nrow = 2, ncol = 2)
FE_Jamui
ggsave("graphics/Feature_Effects_Jamui.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#Nalanda
i = 1
features_Nalanda = importance_nalanda$results[c(1,4,9,13), 1]
features_Nalanda = importance_nalanda$results[1:4, 1]
for(feature in features_Nalanda){
 assign(paste("effect_", feature, sep = ""), 
        FeatureEffect$new(predictor_nalanda, feature = feature, method = "pdp+ice"))
 assign(paste0("p_nalanda_",i), plot(get(paste0("effect_", feature)))+
         theme_minimal() +
         labs(y = NULL) +
         #ggtitle(paste0("PDP and ICE Curve \n Nalanda"))+
         #theme(plot.title = element_text(hjust = 0.5))+
         theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) 
         #theme(plot.title = element_text(hjust = 0.4, size=40)))
         )
 i = i + 1
}

FE_Nalanda = ggarrange(p_nalanda_1, p_nalanda_2, p_nalanda_3, p_nalanda_4, nrow = 2, ncol = 2)
FE_Nalanda
ggsave("graphics/Feature_Effects_Nalanda.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#Vaishali
i = 1
features_Vaishali = importance_vaishali$results[2:5, 1]
for(feature in features_Vaishali){
 assign(paste("effect_", feature, sep = ""), 
        FeatureEffect$new(predictor_vaishali, feature = feature, method = "pdp+ice"))
 assign(paste0("p_vaishali_",i), plot(get(paste0("effect_", feature)))+
         theme_minimal() +
         labs(y = NULL) +
         #ggtitle(paste0("PDP and ICE Curve \n Vaishali"))+
         #theme(plot.title = element_text(hjust = 0.5))+
         theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) 
         #theme(plot.title = element_text(hjust = 0.4, size=40)))
         )
 i = i + 1
}

FE_Vaishali = ggarrange(p_vaishali_1, p_vaishali_2, p_vaishali_3, p_vaishali_4, nrow = 2, ncol = 2)
FE_Vaishali
ggsave("graphics/Feature_Effects_Vaishali.jpeg", dpi = 300, width = 64, height = 32, units = "cm")

#######Shapley Values#######
#All
preds = predictor$predict(X)
preds_sort = sort(preds$predict.model..newdata...newdata.)
Med_All = which(preds$predict.model..newdata...newdata. == preds_sort[round(length(preds_sort)*0.5)])
q5_All = which(preds$predict.model..newdata...newdata. == preds_sort[round(length(preds_sort)*0.05)])
q95_All = which(preds$predict.model..newdata...newdata. == preds_sort[round(length(preds_sort)*0.95)])

#Gaya
preds_Gaya = predictor_gaya$predict(X_gaya)
preds_Gaya_sort = sort(preds_Gaya$predict.model..newdata...newdata.)
Med_Gaya = which(preds_Gaya$predict.model..newdata...newdata. == preds_Gaya_sort[round(length(preds_Gaya_sort)*0.5)])
q5_Gaya = which(preds_Gaya$predict.model..newdata...newdata. == preds_Gaya_sort[round(length(preds_Gaya_sort)*0.05)])
q95_Gaya = which(preds_Gaya$predict.model..newdata...newdata. == preds_Gaya_sort[round(length(preds_Gaya_sort)*0.97)])

#Nalanda
preds_Nalanda = predictor_nalanda$predict(X_nalanda)
preds_Nalanda_sort = sort(preds_Nalanda$predict.model..newdata...newdata.)
Med_Nalanda = which(preds_Nalanda$predict.model..newdata...newdata. == preds_Nalanda_sort[round(length(preds_Nalanda_sort)*0.5)])
q5_Nalanda = which(preds_Nalanda$predict.model..newdata...newdata. == preds_Nalanda_sort[round(length(preds_Nalanda_sort)*0.07)])
q95_Nalanda = which(preds_Nalanda$predict.model..newdata...newdata. == preds_Nalanda_sort[round(length(preds_Nalanda_sort)*0.9)])

#Jamui
preds_Jamui = predictor_jamui$predict(X_jamui)
preds_Jamui_sort = sort(preds_Jamui$predict.model..newdata...newdata.)
Med_Jamui = which(preds_Jamui$predict.model..newdata...newdata. == preds_Jamui_sort[round(length(preds_Jamui_sort)*0.55)])
q5_Jamui = which(preds_Jamui$predict.model..newdata...newdata. == preds_Jamui_sort[round(length(preds_Jamui_sort)*0.05)])
q95_Jamui = which(preds_Jamui$predict.model..newdata...newdata. == preds_Jamui_sort[round(length(preds_Jamui_sort)*0.94)])

#Vaishali
preds_Vaishali = predictor_vaishali$predict(X_vaishali)
preds_Vaishali_sort = sort(preds_Vaishali$predict.model..newdata...newdata.)
Med_Vaishali = which(preds_Vaishali$predict.model..newdata...newdata. == preds_Vaishali_sort[round(length(preds_Vaishali_sort)*0.45)])
q5_Vaishali = which(preds_Vaishali$predict.model..newdata...newdata. == preds_Vaishali_sort[round(length(preds_Vaishali_sort)*0.16)])
q95_Vaishali = which(preds_Vaishali$predict.model..newdata...newdata. == preds_Vaishali_sort[round(length(preds_Vaishali_sort)*0.9)])

# Plot for low Predicted Yields
shapley_q5_All = Shapley$new(predictor, x.interest = X[q5_All,], sample.size = 100)
shapley_q5_gaya = Shapley$new(predictor_gaya, x.interest = X_gaya[q5_Gaya,], sample.size = 100)
shapley_q5_nalanda = Shapley$new(predictor_nalanda, x.interest = X_nalanda[q5_Nalanda,], sample.size = 100)
shapley_q5_jamui = Shapley$new(predictor_jamui, x.interest = X_jamui[q5_Jamui,], sample.size = 100)
shapley_q5_vaishali = Shapley$new(predictor_vaishali, x.interest = X_vaishali[q5_Vaishali,], sample.size = 100)

plot_q5_gaya = plot(shapley_q5_gaya) + 
 labs(caption = "Gaya") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))


plot_q5_nalanda = plot(shapley_q5_nalanda) + 
 labs(caption = "Nalanda") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))


plot_q5_jamui = plot(shapley_q5_jamui) + 
 labs(caption = "Jamui") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))


plot_q5_vaishali = plot(shapley_q5_vaishali) + 
 labs(caption = "Vaishali") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))


ggarrange(plot_q5_gaya, plot_q5_jamui, plot_q5_nalanda, plot_q5_vaishali, nrow = 2, ncol = 2)

# Plot for average Predicted Yields
shapley_med_All = Shapley$new(predictor, x.interest = X[Med_All,], sample.size = 100)
shapley_med_gaya = Shapley$new(predictor_gaya, x.interest = X_gaya[Med_Gaya,], sample.size = 100)
shapley_med_nalanda = Shapley$new(predictor_nalanda, x.interest = X_nalanda[Med_Nalanda,], sample.size = 100)
shapley_med_jamui = Shapley$new(predictor_jamui, x.interest = X_jamui[Med_Jamui,], sample.size = 100)
shapley_med_vaishali = Shapley$new(predictor_vaishali, x.interest = X_vaishali[Med_Vaishali,], sample.size = 100)

plot_med_gaya = plot(shapley_med_gaya) + 
 labs(caption = "Gaya") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_med_nalanda = plot(shapley_med_nalanda) + 
 labs(caption = "Nalanda") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_med_jamui = plot(shapley_med_jamui) + 
 labs(caption = "Jamui") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_med_vaishali = plot(shapley_med_vaishali) + 
 labs(caption = "Vaishali") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

ggarrange(plot_med_gaya, plot_med_jamui, plot_med_nalanda, plot_med_vaishali, nrow = 2, ncol = 2)

# Plot for high Predicted Yields
shapley_q95_All = Shapley$new(predictor, x.interest = X[q95_All,], sample.size = 100)
shapley_q95_gaya = Shapley$new(predictor_gaya, x.interest = X_gaya[q95_Gaya,], sample.size = 100)
shapley_q95_nalanda = Shapley$new(predictor_nalanda, x.interest = X_nalanda[q95_Nalanda,], sample.size = 100)
shapley_q95_jamui = Shapley$new(predictor_jamui, x.interest = X_jamui[q95_Jamui,], sample.size = 100)
shapley_q95_vaishali = Shapley$new(predictor_vaishali, x.interest = X_vaishali[q95_Vaishali,], sample.size = 100)

plot_q95_gaya = plot(shapley_q95_gaya) + 
 labs(caption = "Gaya") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_q95_nalanda = plot(shapley_q95_nalanda) + 
 labs(caption = "Nalanda") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_q95_jamui = plot(shapley_q95_jamui) + 
 labs(caption = "Jamui") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_q95_vaishali = plot(shapley_q95_vaishali) + 
 labs(caption = "Vaishali") +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

ggarrange(plot_q95_gaya, plot_q95_jamui, plot_q95_nalanda, plot_q95_vaishali, nrow = 2, ncol = 2)


plot(shapley_q5_All) + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.4, size = 30, face = "bold"),
       axis.text=element_text(size=30),
       axis.title=element_text(size=30,face="bold")) #+
ggsave("graphics/Shapley_low_All.jpeg", dpi = 300, width = 60, height = 45, units = "cm")

plot(shapley_med_All) + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.4, size = 30, face = "bold"),
       axis.text=element_text(size=30),
       axis.title=element_text(size=30,face="bold"))
ggsave("graphics/Shapley_mid_All.jpeg", dpi = 300, width = 60, height = 45, units = "cm")

plot(shapley_q95_All) + 
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.4, size = 30, face = "bold"),
       axis.text=element_text(size=30),
       axis.title=element_text(size=30,face="bold"))
ggsave("graphics/Shapley_high_All.jpeg", dpi = 300, width = 60, height = 45, units = "cm")

plot_q5_nalanda
plot_med_nalanda
plot_q95_nalanda



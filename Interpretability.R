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
task_jamui$select(readRDS("features/feature_list_catboost_jamui.RDS"))
task_nalanda$select(readRDS("features/feature_list_catboost_nalanda.RDS"))
task_vaishali$select(readRDS("features/feature_list_catboost_vaishali.RDS"))
holdout = TRUE
source("data_cleaning.R")
DistrictSplit = FALSE
source("imputation.R")


#######FEATURE IMPORTANCE#######

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
 theme(axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))

#Nalanda
best_result_nalanda = readRDS("data/results.RDS") %>% filter(District == "nalanda" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
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
 theme(axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))

#Jamui
best_result_jamui = readRDS("data/results.RDS") %>% filter(District == "jamui" & Date != "2024-01-16" & Learner == "regr.catboost" & Comment == "feature selection") %>% filter(CV_Score == min(CV_Score))
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
 theme(axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))

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
 theme(axis.text=element_text(size=20), axis.title=element_text(size=30,face="bold")) + 
 theme(plot.title = element_text(hjust = 0.4, size=60))

#######FEATURE EFFECTS#######
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
p_gaya_2 = p_gaya_2 +
 xlim(0, 18)

FE_Gaya = ggarrange(p_gaya_1, p_gaya_2, p_gaya_3, p_gaya_4, nrow = 2, ncol = 2)
FE_Gaya

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

#######Shapley Values#######
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


plot_q5_nalanda = plot(shapley_q5_nalanda) + 
 theme_minimal() +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_med_nalanda = plot(shapley_med_nalanda) + 
 theme_minimal() +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_q95_nalanda = plot(shapley_q95_nalanda) + 
 theme_minimal() +
 theme(plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"))

plot_q5_nalanda
plot_med_nalanda
plot_q95_nalanda




best_result = readRDS("data/results.RDS") %>% filter(District == "All" & Date != "2024-01-16" & Learner == "regr.catboost") %>% filter(CV_Score == min(CV_Score))

if(best_result$Learner == "regr.ranger"){
 features = readRDS("features/feature_list_ranger.RDS")
 task_All$select(features)
}else{
 features = readRDS("features/feature_list_catboost.RDS")
 task_All$select(features)
}

learner = lrn(paste0(best_result$Learner))
learner$param_set$values = best_result$Hyper_Parameter[[1]]
split = partition(task_All, ratio = 0.8)
learner$train(task_All, row_ids = split$train)


# Features in test data
X = task_All$data(rows = split$test, cols = task_All$feature_names)

# Target in test data
y = task_All$data(rows = split$test, cols = task_All$target_names)

predictor = Predictor$new(learner, data = X, y = y)

# Feature Importance Plot
importance = FeatureImp$new(predictor, loss = "rmse", n.repetitions = 100)
plot(importance) +
 ggtitle("Feature Importance")+
 theme(plot.title = element_text(hjust = 0.4))

# Feature importance plot for 'features'
for(feature in features){
 if(length(unique(X[,paste0(feature)])) > 1){
  assign(paste("effect_", feature, sep = ""), 
         FeatureEffect$new(predictor, feature = feature, method = "pdp+ice"))
  assign("p", plot(get(paste0("effect_", feature)))+
          ggtitle(paste0("PDP and ICE Curve"))+
          theme(plot.title = element_text(hjust = 0.5)))
  plot(p)
 }
}
#plot = ggarrange(p_1, p_2, p_3, p_4, nrow = 2, ncol = 2)
#annotate_figure(plot, top = text_grob(paste0("Feature Effects Catboost \n District: ", District)))

#
#plot(effect_Residue_perc) +
# Adds a title
# ggtitle("Partial dependence") +
# Adds original predictions
# geom_point(
#  data = X, aes(y = predictor$predict(X)[[1]], x = Residue_perc),
#  color = "pink", size = 0.5
# )+
# xlim(9, 12)

# get position of an normal, low and high Yield Observation
preds = predictor$predict(X)
preds_sort = sort(preds$predict.model..newdata...newdata.)
Med = which(preds$predict.model..newdata...newdata. == preds_sort[round(length(preds_sort)/2)])
q5 = which(preds$predict.model..newdata...newdata. == preds_sort[round(length(preds_sort)*0.05)])
q95 = which(preds$predict.model..newdata...newdata. == preds_sort[round(length(preds_sort)*0.95)])

# Plot Shapley
shapley_q5 = Shapley$new(predictor, x.interest = X[q5,], sample.size = 100)
shapley_Med = Shapley$new(predictor, x.interest = X[Med,], sample.size = 100)
shapley_q95 = Shapley$new(predictor, x.interest = X[q95,], sample.size = 100)
shapley_q5$plot() +
 #ggtitle(paste0("Shapley Values of an Observation with low predicted Yield/Acre \n District: ", District))+
 theme(plot.title = element_text(hjust = 0.5))
shapley_Med$plot() +
 #ggtitle(paste0("Shapley Values of an Observation with average predicted Yield/Acre \n District: ", District))+
 theme(plot.title = element_text(hjust = 0.5))
shapley_q95$plot() +
 #ggtitle(paste0("Shapley Values of an Observation with high predicted Yield/Acre \n District: ", District))+
 theme(plot.title = element_text(hjust = 0.5))
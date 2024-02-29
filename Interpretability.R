library(iml)
library(ggplot2)
library(ggpubr)

# Select the District: All, gaya, nalanda, jamui or vaishali
District = "gaya"

Learner = "ranger"

task = get(paste0("task_", District))

split = partition(task, ratio = 0.67)

if(Learner == "ranger"){
 learner = lrn("regr.ranger", mtry = 6, num.trees = 777, num.threads = 6)
}else{
 learner = lrn("regr.catboost", thread_count = 6)
}
learner$train(task, row_ids = split$train)

# features in test data
X = task$data(rows = split$test, cols = task$feature_names)

# target in test data
y = task$data(rows = split$test, cols = task$target_names)

predictor = Predictor$new(learner, data = X, y = y)

# Feature Importance Plot
importance = FeatureImp$new(predictor, loss = "rmse", n.repetitions = 100)
plot(importance) +
 ggtitle(paste0("Feature Importance ", Learner, "\n District: ", District))+
 theme(plot.title = element_text(hjust = 0.5))

# read features
# features = importance$results[1:4, 1]
if(DistrictSplit){
 features = readRDS(paste0("features/feature_list_", Learner, "_", District, ".RDS"))
}else{
 features = readRDS(paste0("features/feature_list_", Learner, ".RDS"))
}

# Feature importance plot for 'features'
#i <- 1
for(feature in features){
 if(length(unique(task$data()[,get(feature)])) > 1){
 assign(paste("effect_", feature, sep = ""), 
        FeatureEffect$new(predictor, feature = feature, method = "pdp+ice"))
 #assign(paste("p_", i, sep = ""), plot(get(paste0("effect_", feature))))
 assign("p", plot(get(paste0("effect_", feature)))+
         ggtitle(paste0("PDP and ICE Curve ", Learner, "\n District: ", District))+
         theme(plot.title = element_text(hjust = 0.5)))
 plot(p)
 #i <- i + 1
 }
}
#plot = ggarrange(p_1, p_2, p_3, p_4, nrow = 2, ncol = 2)
#annotate_figure(plot, top = text_grob(paste0("Feature Effects Catboost \n District: ", District)))

#plot(effect_Harv_hand_rent) +
# # Adds a title
# ggtitle("Partial dependence") +
# # Adds original predictions
# geom_point(
#  data = X, aes(y = predictor$predict(X)[[1]], x = Harv_hand_rent),
#  color = "pink", size = 0.5
# )+
# xlim(0, 8000)

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

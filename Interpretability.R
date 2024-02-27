library(iml)
library(ggplot2)
library(ggpubr)

# Select the District: All, gaya, nalanda, jamui or vaishali
District = "All"

task = get(paste0("task_", District))

split = partition(task, ratio = 0.8)

#learner = lrn("regr.ranger", num.trees = 750, num.threads = 6)
learner = lrn("regr.catboost", thread_count = 6)
learner$train(task, row_ids = split$train)

# features in test data
X = task$data(rows = split$test, cols = task$feature_names)

# target in test data
y = task$data(rows = split$test, cols = task$target_names)

predictor = Predictor$new(learner, data = X, y = y)

# Feature Importance Plot
importance = FeatureImp$new(predictor, loss = "rmse", n.repetitions = 100)
plot(importance) +
 ggtitle(paste0("Feature Importance Catboost \n District: ", District))

# Select 4 important features
features = importance$results[1:4, 1]

# Feature importance plot for 'features'
i <- 1
for(feature in features){
 assign(paste("effect_", feature, sep = ""), 
        FeatureEffect$new(predictor, feature = feature, method = "pdp+ice"))
 assign(paste("p_", i, sep = ""), plot(get(paste0("effect_", feature))))
 i <- i + 1
}
plot = ggarrange(p_1, p_2, p_3, p_4, nrow = 2, ncol = 2)
annotate_figure(plot, top = text_grob(paste0("Feature Effects Catboost \n District: ", District)))

#plot(effect_harv) +
# # Adds a title
# ggtitle("Partial dependence") +
# # Adds original predictions
# geom_point(
#  data = X, aes(y = predictor$predict(X)[[1]], x = Harv_hand_rent),
#  color = "pink", size = 0.5
# )+
# xlim(0, 8000)

shapley = Shapley$new(predictor, x.interest = X[2,], sample.size = 1000)
shapley$plot()

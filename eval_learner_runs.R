library(ggplot2)
library(dplyr)
results = rbind(readRDS("data/results_nils.RDS"), readRDS("data/results.RDS"))
imputTech = "hot deck"
learner = "regr.catboost"
district = "vaishali"

plot_df = results %>%
  filter(District == district & Imputing == imputTech & Learner == learner & DistrictSplit == TRUE) %>%
  select(Hyper_Parameter, CV_Score) %>% rowwise %>%
  mutate(Depth = Hyper_Parameter[["depth"]], learningRate = Hyper_Parameter[["learning_rate"]],
    Iterations = Hyper_Parameter[["iterations"]],
    GrowPolicy = ifelse(is.null(Hyper_Parameter[["grow_policy"]]), "SymmetricTree",
      Hyper_Parameter[["grow_policy"]]),
    RandomStrength = ifelse(is.null(Hyper_Parameter[["random_strength"]]), 1,
                            Hyper_Parameter[["random_strength"]])) %>% ungroup()
mdpoint = plot_df %>% pull(CV_Score) %>% range() %>% mean()

plot_df %>%
 ggplot(aes(x = learningRate, y = Iterations, col = CV_Score, size = CV_Score, shape = GrowPolicy)) +
 geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                      midpoint = mdpoint) +
 scale_size(trans = "reverse") + theme_minimal()

plot_df %>%
 ggplot(aes(x = Depth, y = Iterations, col = CV_Score, size = CV_Score, shape = GrowPolicy)) +
 geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                      midpoint = mdpoint) +
 scale_size(trans = "reverse") + scale_x_continuous(breaks = c(6, 8, 10, 12, 14)) +
 theme_minimal()

plot_df %>%
 ggplot(aes(x = learningRate, y = Depth, col = CV_Score, size = CV_Score, shape = GrowPolicy)) +
 geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                      midpoint = mdpoint) +
 scale_size(trans = "reverse") + scale_y_continuous(breaks = c(6, 8, 10, 12, 14)) +
 theme_minimal()

## vaishali
# learningRate = 0.03 - 0.075
# Depth = 10 - 12
# iterations = 500 - 750
# grow_police = SymmetricTree
## jamui
# learningRate = 0.01 - 0.06 
# depth = 9-14
# iterations = 100 - 750
## gaya
# Depth = 6 - 11
# learningRate = 0.01 - 0.07
# iterations = 100 - 580
## nalanda
# GrowPolicy = Depthwise
# Depth = 6-14
# learningRate = 0.025 - 0.155
# iterations = 100 - 600

#### vaishali ranger ####
plot_df = results %>%
  filter(District == "vaishali" & Imputing == imputTech & Learner == "regr.ranger" & DistrictSplit == TRUE) %>%
  select(Hyper_Parameter, CV_Score, Date) %>% rowwise %>%
  mutate(mtry = ifelse(is.null(Hyper_Parameter[["mtry"]]), 4, Hyper_Parameter[["mtry"]]),
         num.trees = Hyper_Parameter[["num.trees"]],
         max.depth = ifelse(is.null(Hyper_Parameter[["max.depth"]]), 0, Hyper_Parameter[["max.depth"]]),
         min.node.size = ifelse(is.null(Hyper_Parameter[["min.node.size"]]), 5,
                                Hyper_Parameter[["min.node.size"]]),
         bootstrap = ifelse(is.null(Hyper_Parameter[["replace"]]), FALSE,
                            Hyper_Parameter[["replace"]])) %>% ungroup()
mdpoint = plot_df %>% filter(CV_Score < 360) %>% pull(CV_Score) %>% range() %>% mean()

plot_df %>% filter(CV_Score < 360) %>%
  ggplot(aes(x = mtry, y = num.trees, col = CV_Score, size = CV_Score, shape = bootstrap, alpha = 0.2)) +
  geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                       midpoint = mdpoint) +
  scale_size(trans = "reverse") + theme_minimal()

plot_df %>% filter(CV_Score < 360) %>%
  ggplot(aes(x = mtry, y = max.depth, col = CV_Score, size = CV_Score, shape = bootstrap, alpha = 0.2)) +
  geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                       midpoint = mdpoint) +
  scale_size(trans = "reverse") + theme_minimal()

plot_df %>% filter(CV_Score < 360) %>%
  ggplot(aes(x = mtry, y = min.node.size, col = CV_Score, size = CV_Score, shape = bootstrap, alpha = 0.2)) +
  geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                       midpoint = mdpoint) +
  scale_size(trans = "reverse") + theme_minimal()

plot_df %>% filter(CV_Score < 360) %>%
  ggplot(aes(x = max.depth, y = num.trees, col = CV_Score, size = CV_Score, shape = bootstrap)) +
  geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                       midpoint = mdpoint) +
  scale_size(trans = "reverse") + theme_minimal()

plot_df %>% filter(CV_Score < 360) %>%
  ggplot(aes(x = min.node.size, y = num.trees, col = CV_Score, size = CV_Score, shape = bootstrap)) +
  geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                       midpoint = mdpoint) +
  scale_size(trans = "reverse") + theme_minimal()

plot_df %>% filter(CV_Score < 360) %>%
  ggplot(aes(x = max.depth, y = min.node.size, col = CV_Score, size = CV_Score, shape = bootstrap)) +
  geom_point() + scale_color_gradient2(high = "#1000FF", low = "#FF0020", mid = "#fff600",
                                       midpoint = mdpoint) +
  scale_size(trans = "reverse") + theme_minimal()


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
      Hyper_Parameter[["grow_policy"]])) %>% ungroup()
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

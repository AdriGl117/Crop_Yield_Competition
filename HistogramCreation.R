library(ggplot2)
library(gridExtra)
library(grid)
holdout = FALSE
source("data_cleaning.R")

df %>% select(Yield) %>% distinct()
p2 = df %>% mutate(Yield_c = sprintf("%.2f", Yield)) %>% group_by(Yield_c) %>%
 mutate(count = n()) %>% ungroup() %>% 
 mutate(Yield_c = sprintf("%.1f", Yield)) %>% select(Yield_c, count) %>% 
 distinct() %>% arrange(-count) %>% slice(1:10) %>%
 ggplot(aes(x = reorder(Yield_c, -count), y = count)) + geom_bar(stat = "identity") +
 labs(x = NULL, title = "10 most frequent values") + scale_x_discrete() + theme_minimal() +
 theme(panel.background = element_rect(color = "white"),
       plot.background = element_rect(color = "black"),
       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
p1 = df %>% ggplot(aes(x = Yield)) +
  geom_histogram(breaks = seq(from = 0, to = 5000, by = 200)) +
  theme_minimal() + labs(x = "Yield/Acre") +
 annotation_custom(ggplotGrob(p2), xmin = 2500, xmax = 5000, ymin = 200, ymax = 800)
ggsave("graphics/YieldAcreHistogramm_All.jpeg", plot = p1, height = 9, width = 16, units = "cm", dpi = 500,
       scale = 1)
#gridExtra::grid.arrange(p1, p2, ncol = 2)
hist_gaya = df %>% filter(District == "Gaya") %>% ggplot(aes(x = Yield)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 breaks = seq(from = 0, to = 5000, by = 200)) +
  theme_minimal() + labs(x = NULL, y = NULL, title = "Gaya") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) + scale_y_continuous(breaks = seq(from = 0, to = 0.6, by = 0.1))

hist_jamui = df %>% filter(District == "Jamui") %>% ggplot(aes(x = Yield)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 breaks = seq(from = 0, to = 5000, by = 200)) +
  theme_minimal() + labs(x = NULL, y = NULL, title = "Jamui") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

hist_nalanda = df %>% filter(District == "Nalanda") %>% ggplot(aes(x = Yield)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 breaks = seq(from = 0, to = 5000, by = 200)) +
  theme_minimal() + labs(x = NULL, y = NULL, title = "Nalanda") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

hist_vaishali = df %>% filter(District == "Vaishali") %>% ggplot(aes(x = Yield)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 breaks = seq(from = 0, to = 5000, by = 200)) +
  theme_minimal() + labs(x = NULL, y = NULL, title = "Vaishali") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

hist_district = grid.arrange(hist_jamui, hist_gaya, hist_nalanda, hist_vaishali,
             bottom = textGrob("Yield/Acre", gp=gpar(fontsize=12)),
             left = textGrob("relative frequency", gp=gpar(fontsize=12), rot = 90))
ggsave("graphics/YieldAcreHistogramm_Districts.jpeg", plot = hist_district, height = 9, width = 16, units = "cm", dpi = 500,
       scale = 1)

df %>% ggplot(aes(x = Yield, group = District)) + 
        geom_histogram(aes(y = after_stat(count / sum(count)), group = District), bins = 30) +
        theme_minimal() + facet_wrap(~ District, scales = "free_y") +
        labs(y = "relative frequency", x = "Yield/Acre")
ggsave("graphics/YieldAcreHistogramm_Districts.jpeg", height = 9, width = 16, units = "cm", dpi = 500,
       scale = 1)

modLearner_gaya = readRDS("models/catboostSplitFS_gaya.rds")
modLearner_jamui = readRDS("models/catboostSplitFS_jamui.rds")
modLearner_nalanda = readRDS("models/catboostSplitFS_nalanda.rds")
modLearner_vaishali = readRDS("models/catboostSplitFS_vaishali.rds")

for(District in loop_district_var) {
  curr_mod = paste0("modLearner_", District)
  curr_task = paste0("task_", District)
  assign(paste0("pred_train_", District), get(curr_mod)$predict(get(curr_task),
    get(curr_task)$row_ids[!get(curr_task)$data(cols = "ID")[[1]] %in% holdoutIDs]))
  assign(paste0("pred_holdout_", District), get(curr_mod)$predict(get(curr_task),
   get(curr_task)$row_ids[get(curr_task)$data(cols = "ID")[[1]] %in% holdoutIDs])) 
}

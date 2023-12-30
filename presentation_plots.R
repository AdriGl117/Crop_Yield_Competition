library(dplyr)
library(ggplot2)

df <- read.csv("./data/Train.csv") %>%
 mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                 Harv_date, Threshing_date), as.Date))

ggplot(df, aes(y = Yield, x = Acre)) + geom_point(shape = 20, size = 0.75) +
 facet_wrap(~District, scales = "free") + ggtitle("Yield vs Acre per District") +
 theme_minimal()+ theme(plot.title = element_text(hjust = 0.5))
ggsave("graphics/YieldvsAcre.jpeg", dpi = 300, width = 16, height = 9, units = "cm")

ggplot(df %>% filter(Yield < 2000), aes(x = Yield)) + facet_wrap(~District) +
 geom_histogram(aes(y = stat(density) * 100), binwidth = 100) +
 labs(y = "relative frequency", title = "Yield Histogram per District") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
ggsave("graohics/rel_freq_histogram.jpeg", dpi = 300, width = 16, height = 9, units = "cm")

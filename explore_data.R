library(dplyr)
library(ggplot2)
library(corrplot)

df <- read.csv("./data/Train.csv") %>%
 mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
          Harv_date, Threshing_date), as.Date))
var_description <- read.csv("./data/VariableDescription.csv")
var_description <- var_description %>%
 mutate(class = vapply(df[, -1], class, FUN.VALUE = character(1)),
        NAs = vapply(df[, -1], function(x) {sum(is.na(x))}, FUN.VALUE = integer(1)))

str(df)
table(apply(df, 1, function(x) {sum(is.na(x))}))

# reduce the number of levels
table(df$LandPreparationMethod)
unique(unlist(strsplit(df$LandPreparationMethod, " ")))
df %>% mutate(mapply(function(x, methods_vec) {
        output <- as.integer(methods_vec %in% x)
        names(output) <- paste("LPM", methods_vec, sep = "_")
        output}, strsplit(LandPreparationMethod, " "),
        MoreArgs = list(methods_vec = unique(unlist(strsplit(LandPreparationMethod, " "))))) %>%
                t() %>% as_tibble())
        
table(df$NursDetFactor)
unique(unlist(strsplit(df$NursDetFactor, " ")))
df %>% mutate(mapply(function(x, methods_vec) {
        output <- as.integer(methods_vec %in% x)
        names(output) <- paste("NDF", methods_vec, sep = "_")
        output}, strsplit(NursDetFactor, " "),
        MoreArgs = list(methods_vec = unique(unlist(strsplit(NursDetFactor, " "))))) %>%
                t() %>% as_tibble())

table(df$TransDetFactor)
unique(unlist(strsplit(df$TransDetFactor, " ")))
df %>% mutate(mapply(function(x, methods_vec) {
        output <- as.integer(methods_vec %in% x)
        names(output) <- paste("TDF", methods_vec, sep = "_")
        output}, strsplit(TransDetFactor, " "),
        MoreArgs = list(methods_vec = unique(unlist(strsplit(TransDetFactor, " "))))) %>%
                t() %>% as_tibble())

table(df$OrgFertilizers)
unique(unlist(strsplit(df$OrgFertilizers, " ")))
df %>% mutate(mapply(function(x, methods_vec) {
        output <- as.integer(methods_vec %in% x)
        names(output) <- paste("OF", methods_vec, sep = "_")
        output}, strsplit(OrgFertilizers, " "),
        MoreArgs = list(methods_vec = unique(unlist(strsplit(OrgFertilizers, " "))))) %>%
                t() %>% as_tibble())

table(df$CropbasalFerts)
unique(unlist(strsplit(df$CropbasalFerts, " ")))
df %>% mutate(mapply(function(x, methods_vec) {
        output <- as.integer(methods_vec %in% x)
        names(output) <- paste("CF", methods_vec, sep = "_")
        output}, strsplit(CropbasalFerts, " "),
        MoreArgs = list(methods_vec = unique(unlist(strsplit(CropbasalFerts, " "))))) %>%
                t() %>% as_tibble())

table(df$FirstTopDressFert)
unique(unlist(strsplit(df$FirstTopDressFert, " ")))
df %>% mutate(mapply(function(x, methods_vec) {
        output <- as.integer(methods_vec %in% x)
        names(output) <- paste("FTDF", methods_vec, sep = "_")
        output}, strsplit(FirstTopDressFert, " "),
        MoreArgs = list(methods_vec = unique(unlist(strsplit(FirstTopDressFert, " "))))) %>%
                t() %>% as_tibble())

# transform character columns to factor columns
df <- df %>% mutate(across(c(District, Block, CropEstMethod,
                             TransplantingIrrigationSource,
                             TransplantingIrrigationPowerSource,
                             PCropSolidOrgFertAppMethod, MineralFertAppMethod,
                             MineralFertAppMethod.1, Harv_method, Threshing_method,
                             Stubble_use), as.factor))

# rename unnamed factor level
levels(df$TransplantingIrrigationSource)[
 levels(df$TransplantingIrrigationSource) == ""] <- "unknown"
levels(df$TransplantingIrrigationPowerSource)[
 levels(df$TransplantingIrrigationPowerSource) == ""] <- "unknown"
levels(df$PCropSolidOrgFertAppMethod)[
 levels(df$PCropSolidOrgFertAppMethod) == ""] <- "unknown"
levels(df$MineralFertAppMethod.1)[levels(df$MineralFertAppMethod.1) == ""] <- "unknown"

#### explore numeric/integer columns ####
summary(df$CultLand)
plot(hist(df$CultLand))
plot(hist(log10(df$CultLand)))
boxplot(df$CultLand)

summary(df$CropCultLand)
plot(hist(df$CropCultLand))
plot(hist(log10(df$CropCultLand)))
boxplot(df$CropCultLand)

summary(df$CropCultLand / df$CultLand) # potential new column

summary(df$CropTillageDepth)
plot(hist(df$CropTillageDepth))
boxplot(df$CropTillageDepth)

summary(df$SeedlingsPerPit)
boxplot(df$SeedlingsPerPit) 
df$SeedlingsPerPit[order(df$SeedlingsPerPit, decreasing = TRUE)][1:10]
# the data point with 442 seems to be an incorrect value
boxplot((df %>% filter(SeedlingsPerPit < 400))$SeedlingsPerPit)
plot(hist((df %>% filter(SeedlingsPerPit < 400))$SeedlingsPerPit))

summary(df$TransplantingIrrigationHours)
boxplot(df$TransplantingIrrigationHours)
df$TransplantingIrrigationHours[order(df$TransplantingIrrigationHours, decreasing = TRUE)][1:10]
# the data point with 2000 seems to be an incorrect value
boxplot((df %>% filter(TransplantingIrrigationHours < 500))$TransplantingIrrigationHours)
plot(hist((df %>% filter(TransplantingIrrigationHours < 500))$TransplantingIrrigationHours))

summary(df$TransIrriCost)
boxplot(df$TransIrriCost)
df$TransIrriCost[order(df$TransIrriCost, decreasing = TRUE)][1:10]
# the data point with 6000 seems to be an incorrect value
boxplot((df %>% filter(TransIrriCost < 5000))$TransIrriCost)
plot(hist((df %>% filter(TransIrriCost < 5000))$TransIrriCost))

summary(df$StandingWater)
boxplot(df$StandingWater)
plot(hist(df$StandingWater))

summary(df$Ganaura)
boxplot(df$Ganaura)
df$Ganaura[order(df$Ganaura, decreasing = TRUE)][1:10]
plot(hist(df$Ganaura))
summary(df$Ganaura / (df$NoFertilizerAppln * df$CultLand))

summary(df$CropOrgFYM)
boxplot(df$CropOrgFYM)
df$CropOrgFYM[order(df$CropOrgFYM, decreasing = TRUE)][1:10]
plot(hist(df$CropOrgFYM))
summary(df$CropOrgFYM / (df$NoFertilizerAppln * df$CultLand))

summary(df$NoFertilizerAppln)
boxplot(df$NoFertilizerAppln)
plot(hist(df$NoFertilizerAppln))

summary(df$BasalDAP)
boxplot(df$BasalDAP)
plot(hist(df$BasalDAP))

summary(df$BasalUrea)
boxplot(df$BasalUrea)
plot(hist(df$BasalUrea))

summary(df$X1tdUrea)
boxplot(df$X1tdUrea)
plot(hist(df$X1tdUrea))

summary(df$X1appDaysUrea)
boxplot(df$X1appDaysUrea)
df$X1appDaysUrea[order(df$X1appDaysUrea, decreasing = TRUE)][1:10]
# the data point with 332 seems to be an incorrect value
boxplot((df %>% filter(X1appDaysUrea < 300))$X1appDaysUrea)
plot(hist((df %>% filter(X1appDaysUrea < 300))$X1appDaysUrea))

summary(df$X2tdUrea)
boxplot(df$X2tdUrea)
plot(hist(df$X2tdUrea))

summary(df$X2appDaysUrea)
boxplot(df$X2appDaysUrea)
plot(hist(df$X2appDaysUrea))

summary(df$Harv_hand_rent)
boxplot(df$Harv_hand_rent)
df$Harv_hand_rent[order(df$Harv_hand_rent, decreasing = TRUE)][1:10]
# the data point with 60000 seems to be an incorrect value
boxplot((df %>% filter(Harv_hand_rent < 30000))$Harv_hand_rent)
plot(hist((df %>% filter(Harv_hand_rent < 30000))$Harv_hand_rent))

summary(df$Residue_length)
boxplot(df$Residue_length)
plot(hist(df$Residue_length))

summary(df$Residue_perc)
boxplot(df$Residue_perc)
plot(hist(df$Residue_perc))

summary(df$Acre)
boxplot(df$Acre)
plot(hist(df$Acre))

summary(df$Yield)
boxplot(df$Yield)
plot(hist(df$Yield))

summary(df$Yield / df$Acre)
boxplot(df$Yield / df$Acre)
plot(hist(df$Yield / df$Acre))

#### Date variables ####
ftable(df$NursDetFactor, is.na(df$RcNursEstDate))
table(df$RcNursEstDate - df$CropTillageDate)
table(df$RcNursEstDate - df$CropTillageDate < 0)
table(df$RcNursEstDate - df$SeedingSowingTransplanting < 0)
table(df$CropTillageDate - df$SeedingSowingTransplanting < 0)
table(df$SeedingSowingTransplanting - df$Harv_date < 0) # the three false are incorrect
table(df$SeedingSowingTransplanting - df$Harv_date > -40)
df[which(df$SeedingSowingTransplanting - df$Harv_date > -50 &
        df$SeedingSowingTransplanting - df$Harv_date < 0), ]
table(months(df$Harv_date))
df %>% rowwise() %>%
        mutate(LPrepDate = min(RcNursEstDate, CropTillageDate, na.rm = TRUE),
               X2Date = LPrepDate + X1appDaysUrea + X2appDaysUrea) %>%
        filter(!is.na(X2Date) & X2Date - Harv_date >= 0) %>%
        select(Harv_date, X2Date, SeedingSowingTransplanting,
               RcNursEstDate, CropTillageDate, Threshing_date, NoFertilizerAppln) %>%
        View()
table(df$Harv_date - df$Threshing_date <= 0)

plot(df$Harv_date[which(df$Yield / df$Acre < 6000)],
 df$Yield[which(df$Yield / df$Acre < 6000)] / df$Acre[which(df$Yield / df$Acre < 6000)])

View(df[which(df$Yield / df$Acre > 15000),])

ggplot(df, aes(x = Yield/Acre, group = District)) +
        geom_histogram(aes(y = stat(density) * 80), bins = 80) +
        facet_wrap(~District)

ftable(df$Block, df$Yield / df$Acre > 10000)
ftable(df$Block, df$Yield / df$Acre < 50)
df %>% group_by(Block) %>% summarise(n(), length(unique(Yield/Acre)))

#### show influence of Acre on other variables ####
jpeg("acre_cor_uncorrected.jpeg", width = 11, height = 10.5, units = "cm",
     pointsize = 9, quality = 1000, res = 170)
corrplot.mixed(cor(df %>% select(Acre, X1tdUrea, X2tdUrea, Harv_hand_rent, BasalDAP,
                                 BasalUrea, Ganaura, CropOrgFYM), use = "pairwise.complete.ob"),
        tl.pos = "lt", mar = c(0, 0, 2, 0), title = "uncorrected variables",
        tl.col = "black", tl.srt = 45)
dev.off()

jpeg("acre_cor_corrected.jpeg", width = 400, height = 410)
corrplot.mixed(cor(df %>% mutate(X1tdUrea = X1tdUrea / Acre, X2tdUrea = X2tdUrea / Acre,
                        Harv_hand_rent = Harv_hand_rent / Acre, BasalDAP = BasalDAP / Acre,
                        BasalUrea = BasalUrea / Acre, Ganaura = Ganaura / Acre,
                        CropOrgFYM = CropOrgFYM / Acre) %>%
                        select(Acre, X1tdUrea, X2tdUrea, Harv_hand_rent, BasalDAP,
                        BasalUrea, Ganaura, CropOrgFYM), use = "pairwise.complete.ob"),
               tl.pos = "lt", mar = c(0, 0, 1.5, 0), title = "corrected variables (with Acre)",
               tl.col = "black", tl.srt = 45)
dev.off()
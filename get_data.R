get_data = function(src){
 library(mlr3verse)
 library(dplyr)
 library(lubridate)
 
 df <- read.csv(src) %>%
  mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                  Harv_date, Threshing_date), as.Date))
 
 ## character columns to factor columns ##
 df <- df %>% mutate(across(c(District, Block, CropEstMethod,
                              TransplantingIrrigationSource,
                              TransplantingIrrigationPowerSource,
                              PCropSolidOrgFertAppMethod, MineralFertAppMethod,
                              MineralFertAppMethod.1, Harv_method, Threshing_method,
                              Stubble_use), as.factor))
 
 ## rename unnamed factor level ##
 levels(df$TransplantingIrrigationSource)[
  levels(df$TransplantingIrrigationSource) == ""] <- "unknown"
 levels(df$TransplantingIrrigationPowerSource)[
  levels(df$TransplantingIrrigationPowerSource) == ""] <- "unknown"
 levels(df$PCropSolidOrgFertAppMethod)[
  levels(df$PCropSolidOrgFertAppMethod) == ""] <- "unknown"
 # no second dose applied
 ftable(df$MineralFertAppMethod.1, df$NoFertilizerAppln)
 levels(df$MineralFertAppMethod.1)[levels(df$MineralFertAppMethod.1) == ""] <- "no2ndDose"
 
 ## character columns to dummy columns ##
 df <- df %>% mutate(mapply(function(x, methods_vec) {
  output <- as.logical(methods_vec %in% x)
  names(output) <- paste("LPM", methods_vec, sep = "_")
  output}, strsplit(LandPreparationMethod, " "),
  MoreArgs = list(methods_vec = unique(unlist(strsplit(LandPreparationMethod, " "))))) %>%
   t() %>% as_tibble())
 
 df <- df %>% mutate(mapply(function(x, methods_vec) {
  output <- as.logical(methods_vec %in% x)
  names(output) <- paste("NDF", methods_vec, sep = "_")
  output}, strsplit(NursDetFactor, " "),
  MoreArgs = list(methods_vec = unique(unlist(strsplit(NursDetFactor, " "))))) %>%
   t() %>% as_tibble())
 
 df <- df %>% mutate(mapply(function(x, methods_vec) {
  output <- as.logical(methods_vec %in% x)
  names(output) <- paste("TDF", methods_vec, sep = "_")
  output}, strsplit(TransDetFactor, " "),
  MoreArgs = list(methods_vec = unique(unlist(strsplit(TransDetFactor, " "))))) %>%
   t() %>% as_tibble())
 
 df <- df %>% mutate(mapply(function(x, methods_vec) {
  output <- as.logical(methods_vec %in% x)
  names(output) <- paste("OF", methods_vec, sep = "_")
  output}, strsplit(OrgFertilizers, " "),
  MoreArgs = list(methods_vec = unique(unlist(strsplit(OrgFertilizers, " "))))) %>%
   t() %>% as_tibble())
 
 df <- df %>% mutate(mapply(function(x, methods_vec) {
  output <- as.logical(methods_vec %in% x)
  names(output) <- paste("CF", methods_vec, sep = "_")
  output}, strsplit(CropbasalFerts, " "),
  MoreArgs = list(methods_vec = unique(unlist(strsplit(CropbasalFerts, " "))))) %>%
   t() %>% as_tibble())
 
 df <- df %>% mutate(mapply(function(x, methods_vec) {
  output <- as.logical(methods_vec %in% x)
  names(output) <- paste("FTDF", methods_vec, sep = "_")
  output}, strsplit(FirstTopDressFert, " "),
  MoreArgs = list(methods_vec = unique(unlist(strsplit(FirstTopDressFert, " "))))) %>%
   t() %>% as_tibble())
 
 ## replace NAs##
 df <- df %>%
  mutate(Ganaura = ifelse(OF_Ganaura == 0, 0, Ganaura),
         CropOrgFYM = ifelse(OF_FYM == 0, 0, CropOrgFYM),
         BasalUrea = ifelse(CF_Urea == 0, 0, BasalUrea),
         BasalDAP = ifelse(CF_DAP == 0, 0, BasalDAP),
         X1tdUrea = ifelse(FTDF_Urea == 0, 0, X1tdUrea),
         X2tdUrea = ifelse(NoFertilizerAppln < 3, 0, X2tdUrea))
 # no third dose of chemical fertilizer was applied
 ftable(is.na(df$X2tdUrea), df$NoFertilizerAppln)
 
 df <- df %>%
  mutate(X1appDaysUrea = ifelse(FTDF_Urea == 0, -1, X1appDaysUrea),
         X2appDaysUrea = ifelse(NoFertilizerAppln < 3, -1, X2appDaysUrea))
 
 ## adjust wrong values ##
 df <- df %>%
  mutate(X1appDaysUrea = ifelse(X1appDaysUrea == 332, NA, X1appDaysUrea),
         SeedlingsPerPit = ifelse(SeedlingsPerPit == 442, NA, SeedlingsPerPit),
         Harv_hand_rent = ifelse(Harv_hand_rent == 60000, NA, Harv_hand_rent))
 
 ## new variables ##
 df <- df %>%
  mutate(CropCultPerc = CropCultLand / CultLand,
         TIrriCost_H_Acre = TransIrriCost / (TransplantingIrrigationHours * Acre))
 
 ## adjust variables for area of land under cultivation (Acre) ##
 df <- df %>%
  mutate(X1tdUrea = X1tdUrea / Acre, X2tdUrea = X2tdUrea / Acre,
         Harv_hand_rent = Harv_hand_rent / Acre, BasalDAP = BasalDAP / Acre,
         BasalUrea = BasalUrea / Acre, Ganaura = Ganaura / Acre,
         CropOrgFYM = CropOrgFYM / Acre)
 
 data = df

 data$CropTillage_Month = month(data$CropTillageDate)
 data$RcNursEst_Month = month(data$RcNursEstDate)
 data$SeedingSowingTransplanting_Month = month(data$SeedingSowingTransplanting)
 data$Harv_Month = month(data$Harv_date)
 data$Threshing_Month = month(data$Threshing_date)
 
 data$CropTillage_Quarter = quarter(data$CropTillageDate)
 data$RcNursEst_Quarter = quarter(data$RcNursEstDate)
 data$SeedingSowingTransplanting_Quarter = quarter(data$SeedingSowingTransplanting)
 data$Harv_Quarter = quarter(data$Harv_date)
 data$Threshing_Quarter = quarter(data$Threshing_date)
 
 data$CropTillage_Month_sin = sin(2 * pi * data$CropTillage_Month / 12)
 data$CropTillage_Month_cos = cos(2 * pi * data$CropTillage_Month / 12)
 data$RcNursEst_Month_sin = sin(2 * pi * data$RcNursEst_Month / 12)
 data$RcNursEst_Month_cos = cos(2 * pi * data$RcNursEst_Month / 12)
 data$SeedingSowingTransplanting_Month_sin = sin(2 * pi * data$SeedingSowingTransplanting_Month / 12)
 data$SeedingSowingTransplanting_Month_cos = cos(2 * pi * data$SeedingSowingTransplanting_Month / 12)
 data$Harv_Month_sin = sin(2 * pi * data$Harv_Month / 12)
 data$Harv_Month_cos = cos(2 * pi * data$Harv_Month / 12)
 data$Threshing_Month_sin = sin(2 * pi * data$Threshing_Month / 12)
 data$Threshing_Month_cos = cos(2 * pi * data$Threshing_Month / 12)
 
 data$DiffCropRC = as.numeric(difftime(data$CropTillageDate, data$RcNursEstDate, units = "days"))
 data$DiffCropSeed = as.numeric(difftime(data$CropTillageDate, data$SeedingSowingTransplanting, units = "days"))
 data$DiffCropHarv = as.numeric(difftime(data$CropTillageDate, data$Harv_date, units = "days"))
 data$DiffCropThresing = as.numeric(difftime(data$CropTillageDate, data$Threshing_date, units = "days"))
 
 data$DiffRCSeed = as.numeric(difftime(data$RcNursEstDate, data$SeedingSowingTransplanting, units = "days"))
 data$DiffRCHarv = as.numeric(difftime(data$RcNursEstDate, data$Harv_date, units = "days"))
 data$DiffRCThresing = as.numeric(difftime(data$RcNursEstDate, data$Threshing_date, units = "days"))
 
 data$DiffSeedHarv = as.numeric(difftime(data$SeedingSowingTransplanting, data$Harv_date, units = "days"))
 data$DiffSeedThresing = as.numeric(difftime(data$SeedingSowingTransplanting, data$Threshing_date, units = "days"))
 
 data$DiffHarvThresing = as.numeric(difftime(data$Harv_date, data$Threshing_date, units = "days"))
 
 data$CropTillageDate = as.numeric(difftime(data$CropTillageDate, min(data$CropTillageDate), units = "days"))
 data$RcNursEstDate = as.numeric(difftime(data$RcNursEstDate, min(data$RcNursEstDate, na.rm = TRUE), units = "days"))
 data$SeedingSowingTransplanting = as.numeric(difftime(data$SeedingSowingTransplanting, min(data$SeedingSowingTransplanting), units = "days"))
 data$Harv_date = as.numeric(difftime(data$Harv_date, min(data$Harv_date), units = "days"))
 data$Threshing_date = as.numeric(difftime(data$Threshing_date, min(data$Threshing_date), units = "days"))
 
 #data = data %>% mutate(Yield = case_when(Yield > 1000 ~ 1000,
 #                                         Yield <= 1000 ~ Yield))
 
 return(data)
}

# Laden Sie die benötigten Pakete
library(dplyr)
library(purrr)

# Definieren Sie eine Funktion, um Ausreißer zu erkennen und zu NA's umzuformen
replace_outliers <- function(x) {
 qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
 H <- 3 * IQR(x, na.rm = T)
 x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
 return(x)
}

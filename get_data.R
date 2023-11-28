get_df = function(src){
 library(mlr3verse)
 library(dplyr)
 library(lubridate)
 
 df <- read.csv("df/Train.csv") %>%
  mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                  Harv_date, Threshing_date), as.Date))
 #var_description <- read.csv("./df/VariableDescription.csv")
 #var_description <- var_description %>%
 # mutate(class = vapply(df[, 2:44], class, FUN.VALUE = character(1)),
 #        NAs = vapply(df[, 2:44], function(x) {sum(is.na(x))}, FUN.VALUE = integer(1)))
 
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
 
 ## create target variable ##
 df <- df %>% mutate(Yield_Acre = Yield / Acre)
 
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
 


 df$CropTillage_Month = month(df$CropTillageDate)
 df$RcNursEst_Month = month(df$RcNursEstDate)
 df$SeedingSowingTransplanting_Month = month(df$SeedingSowingTransplanting)
 df$Harv_Month = month(df$Harv_date)
 df$Threshing_Month = month(df$Threshing_date)
 
 df$CropTillage_Quarter = quarter(df$CropTillageDate)
 df$RcNursEst_Quarter = quarter(df$RcNursEstDate)
 df$SeedingSowingTransplanting_Quarter = quarter(df$SeedingSowingTransplanting)
 df$Harv_Quarter = quarter(df$Harv_date)
 df$Threshing_Quarter = quarter(df$Threshing_date)
 
 df$CropTillage_Month_sin = sin(2 * pi * df$CropTillage_Month / 12)
 df$CropTillage_Month_cos = cos(2 * pi * df$CropTillage_Month / 12)
 df$RcNursEst_Month_sin = sin(2 * pi * df$RcNursEst_Month / 12)
 df$RcNursEst_Month_cos = cos(2 * pi * df$RcNursEst_Month / 12)
 df$SeedingSowingTransplanting_Month_sin = sin(2 * pi * df$SeedingSowingTransplanting_Month / 12)
 df$SeedingSowingTransplanting_Month_cos = cos(2 * pi * df$SeedingSowingTransplanting_Month / 12)
 df$Harv_Month_sin = sin(2 * pi * df$Harv_Month / 12)
 df$Harv_Month_cos = cos(2 * pi * df$Harv_Month / 12)
 df$Threshing_Month_sin = sin(2 * pi * df$Threshing_Month / 12)
 df$Threshing_Month_cos = cos(2 * pi * df$Threshing_Month / 12)
 
 df$DiffCropRC = as.numeric(difftime(df$CropTillageDate, df$RcNursEstDate, units = "days"))
 df$DiffCropSeed = as.numeric(difftime(df$CropTillageDate, df$SeedingSowingTransplanting, units = "days"))
 df$DiffCropHarv = as.numeric(difftime(df$CropTillageDate, df$Harv_date, units = "days"))
 df$DiffCropThresing = as.numeric(difftime(df$CropTillageDate, df$Threshing_date, units = "days"))
 
 df$DiffRCSeed = as.numeric(difftime(df$RcNursEstDate, df$SeedingSowingTransplanting, units = "days"))
 df$DiffRCHarv = as.numeric(difftime(df$RcNursEstDate, df$Harv_date, units = "days"))
 df$DiffRCThresing = as.numeric(difftime(df$RcNursEstDate, df$Threshing_date, units = "days"))
 
 df$DiffSeedHarv = as.numeric(difftime(df$SeedingSowingTransplanting, df$Harv_date, units = "days"))
 df$DiffSeedThresing = as.numeric(difftime(df$SeedingSowingTransplanting, df$Threshing_date, units = "days"))
 
 df$DiffHarvThresing = as.numeric(difftime(df$Harv_date, df$Threshing_date, units = "days"))
 
 df$CropTillageDate = as.numeric(difftime(df$CropTillageDate, min(df$CropTillageDate), units = "days"))
 df$RcNursEstDate = as.numeric(difftime(df$RcNursEstDate, min(df$RcNursEstDate, na.rm = TRUE), units = "days"))
 df$SeedingSowingTransplanting = as.numeric(difftime(df$SeedingSowingTransplanting, min(df$SeedingSowingTransplanting), units = "days"))
 df$Harv_date = as.numeric(difftime(df$Harv_date, min(df$Harv_date), units = "days"))
 df$Threshing_date = as.numeric(difftime(df$Threshing_date, min(df$Threshing_date), units = "days"))
 
 return(df)
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

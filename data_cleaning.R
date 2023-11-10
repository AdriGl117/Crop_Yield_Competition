library(dplyr)

df <- read.csv("data/Train.csv") %>%
 mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                 Harv_date, Threshing_date), as.Date))
var_description <- read.csv("./data/VariableDescription.csv")
var_description <- var_description %>%
 mutate(class = vapply(df[, 2:44], class, FUN.VALUE = character(1)),
        NAs = vapply(df[, 2:44], function(x) {sum(is.na(x))}, FUN.VALUE = integer(1)))

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

df <- df %>%
  rows_update(tibble(ID = df$ID[is.na(df$X2tdUrea) & df$X1tdUrea == 0 & df$X1appDaysUrea == -1],
                     X2tdUrea = 0, X2appDaysUrea = -1), by = "ID")
df <- df %>%
  rows_update(tibble(ID = df$ID[is.na(df$X2appDaysUrea)],
                     X2appDaysUrea = mapply(function(dt, dn, dh, maxd) {
                       max_pos <- as.numeric(max(dh - dt, dh - dn, na.rm = TRUE))
                       runif(1, 1, min(max_pos, maxd))
                     }, dt = df[is.na(df$X2appDaysUrea),]$CropTillageDate,
                     dn = df[is.na(df$X2appDaysUrea),]$RcNursEstDate,
                     dh = df[is.na(df$X2appDaysUrea),]$Harv_date,
                     MoreArgs = list(maxd = max(df$X2appDaysUrea, na.rm = TRUE)))),
              by = "ID")


## adjust wrong values ##
df <- df %>%
 mutate(X1appDaysUrea = ifelse(X1appDaysUrea == 332, NA, X1appDaysUrea),
        SeedlingsPerPit = ifelse(SeedlingsPerPit == 442, NA, SeedlingsPerPit),
        Harv_hand_rent = ifelse(Harv_hand_rent == 60000, NA, Harv_hand_rent),
        Harv_date = ifelse(SeedingSowingTransplanting - Harv_date > 0,
                           Harv_date + 365, Harv_date))

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

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

df <- df %>%
        mutate(X1appDaysUrea = ifelse(X1appDaysUrea == 332, NA, X1appDaysUrea),
               SeedlingsPerPit = ifelse(SeedlingsPerPit == 442, NA, SeedlingsPerPit),
               Harv_hand_rent = ifelse(Harv_hand_rent == 60000, NA, Harv_hand_rent),
               Harv_date = as.Date(ifelse(SeedingSowingTransplanting - Harv_date > 0,
                                          Harv_date + 365, Harv_date), origin = "1970-01-01"))

df <- df %>%
        mutate(CropCultPerc = CropCultLand / CultLand)

helper = function(x) ecdf(x)(x)
transform_col = colnames(df)[vapply(df, is.numeric, logical(1)) & colnames(df) != "Yield"]
df <- df %>%
        mutate(across(all_of(transform_col), helper))

## factor variables to dummy columns ##
df = df %>%
 fastDummies::dummy_cols(c("District", "Block", "CropEstMethod",
                           "TransplantingIrrigationSource", "TransplantingIrrigationPowerSource",
                           "PCropSolidOrgFertAppMethod", "MineralFertAppMethod", "MineralFertAppMethod.1",
                           "Harv_method", "Threshing_method", "Stubble_use"), remove_selected_columns = TRUE)

## character columns to dummy columns ##
df = df %>%
 fastDummies::dummy_cols(c("LandPreparationMethod", "NursDetFactor",
                           "TransDetFactor", "OrgFertilizers", "CropbasalFerts", "FirstTopDressFert"),
                         split = " ", remove_selected_columns = TRUE)

## create target variable ##
#df <- df %>% mutate(Yield_Acre = Yield / Acre)

## replace NAs##
df <- df %>%
 mutate(Ganaura = ifelse(OrgFertilizers_Ganaura == 0, 0, Ganaura),
        CropOrgFYM = ifelse(OrgFertilizers_FYM == 0, 0, CropOrgFYM),
        BasalUrea = ifelse(CropbasalFerts_Urea == 0, 0, BasalUrea),
        BasalDAP = ifelse(CropbasalFerts_DAP == 0, 0, BasalDAP),
        X1tdUrea = ifelse(FirstTopDressFert_Urea == 0, 0, X1tdUrea),
        X2tdUrea = ifelse(NoFertilizerAppln < 0.9, 0, X2tdUrea))

df <- df %>%
        mutate(X1appDaysUrea = ifelse(FirstTopDressFert_Urea == 0, -1, X1appDaysUrea),
               X2appDaysUrea = ifelse(NoFertilizerAppln < 0.9, -1, X2appDaysUrea))

df <- df %>%
        rows_update(tibble(ID = df$ID[is.na(df$X2tdUrea) & df$X1tdUrea == 0 & df$X1appDaysUrea == -1],
                           X2tdUrea = 0, X2appDaysUrea = -1), by = "ID")
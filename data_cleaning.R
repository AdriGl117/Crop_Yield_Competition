library(dplyr)

df <- read.csv("data/Train.csv") %>%
 mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                 Harv_date, Threshing_date), as.Date))

## character columns to factor columns ##
df <- df %>% mutate(across(c(District, Block, CropEstMethod,
    TransplantingIrrigationSource, TransplantingIrrigationPowerSource,
    PCropSolidOrgFertAppMethod, MineralFertAppMethod, MineralFertAppMethod.1,
    Harv_method, Threshing_method, Stubble_use), as.factor))

# error in District, one observation in Gurua has as District Jamui instead of Gaya
ftable(df$District, df$Block)
df = df %>%
  rows_update(tibble(ID = df$ID[df$District == "Jamui" & df$Block == "Gurua"],
                     District = "Gaya"), by = "ID")

## factor variables to dummy columns ##
df = df %>%
  fastDummies::dummy_cols(c("CropEstMethod", "TransplantingIrrigationSource",
    "TransplantingIrrigationPowerSource", "PCropSolidOrgFertAppMethod",
    "MineralFertAppMethod", "MineralFertAppMethod.1", "Block"),
    remove_selected_columns = TRUE)

## character columns to dummy columns ##
df = df %>%
  fastDummies::dummy_cols(c("LandPreparationMethod", "NursDetFactor",
      "TransDetFactor", "OrgFertilizers", "CropbasalFerts", "FirstTopDressFert"),
      split = " ", remove_selected_columns = TRUE)

## create target variable ##
#df <- df %>% mutate(Yield_Acre = Yield / Acre)

## adjust wrong values ##
# set outlier values to NA except if there is an obvious way to correct them 
df <- df %>%
  mutate(X1appDaysUrea = ifelse(X1appDaysUrea == 332, NA, X1appDaysUrea),
         SeedlingsPerPit = ifelse(SeedlingsPerPit == 442, NA, SeedlingsPerPit),
         Harv_hand_rent = ifelse(Harv_hand_rent == 60000, NA, Harv_hand_rent),
         Ganaura = ifelse(Ganaura / Acre > 500, NA, Ganaura),
         CropOrgFYM = ifelse(CropOrgFYM / Acre > 500, NA, CropOrgFYM),
         Harv_date = as.Date(ifelse(format(df$Harv_date, "%Y") == "2021",
                                    Harv_date + 365, Harv_date), origin = "1970-01-01"))


## replace NAs##
df <- df %>%
 mutate(Ganaura = ifelse(OrgFertilizers_Ganaura == 0, 0, Ganaura),
        CropOrgFYM = ifelse(OrgFertilizers_FYM == 0, 0, CropOrgFYM),
        BasalUrea = ifelse(CropbasalFerts_Urea == 0, 0, BasalUrea),
        BasalDAP = ifelse(CropbasalFerts_DAP == 0, 0, BasalDAP),
        X1tdUrea = ifelse(FirstTopDressFert_Urea == 0, 0, X1tdUrea),
        X2tdUrea = ifelse(NoFertilizerAppln < 3, 0, X2tdUrea))
# no third dose of chemical fertilizer was applied
ftable(is.na(df$X2tdUrea), df$NoFertilizerAppln)

df <- df %>%
 mutate(X1appDaysUrea = ifelse(FirstTopDressFert_Urea == 0, -1, X1appDaysUrea),
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
      MoreArgs = list(maxd = max(df$X2appDaysUrea, na.rm = TRUE)))), by = "ID")

df = df %>%
  rows_update(tibble(ID = "ID_YTZN9FE7PQUY",
      Harv_date = df$RcNursEstDate[[3134]] + runif(1, 95, 151)), by = "ID")

## new variables ##
df <- df %>%
 mutate(CropCultPerc = CropCultLand / CultLand)

df <- df %>%
  rowwise() %>%
  mutate(NoOrgFerts = sum(c_across(starts_with("OrgFertilizers"))),
         NoLPMethods = sum(c_across(starts_with("LandPreparationMethod"))),
         NoNDFactors = sum(c_across(starts_with("NursDetFactor"))),
         NoTDFactors = sum(c_across(starts_with("TransDetFactor"))),
         NoCropbFerts = sum(c_across(starts_with("CropbasalFerts"))),
         NoFTDFerts = sum(c_across(starts_with("FirstTopDressFert"))))

summary(df$StandingWater /
          as.numeric(difftime(df$Harv_date, df$SeedingSowingTransplanting, units = "days")))
## adjust variables for area of land under cultivation (Acre) ##
#df <- df %>%
# mutate(X1tdUrea = X1tdUrea / Acre, X2tdUrea = X2tdUrea / Acre,
#        Harv_hand_rent = Harv_hand_rent / Acre, BasalDAP = BasalDAP / Acre,
#        BasalUrea = BasalUrea / Acre, Ganaura = Ganaura / Acre,
#        CropOrgFYM = CropOrgFYM / Acre)

## manipulate target variable
#df <- df %>% mutate(Yield = ifelse(Yield/Acre > 10000, 10000 * Acre, Yield))

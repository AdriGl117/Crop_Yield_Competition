library(dplyr)
library(lubridate)

df <- read.csv("data/train_new.csv") %>%
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
#df = df %>%
#  fastDummies::dummy_cols(c("CropEstMethod", "TransplantingIrrigationSource",
#    "TransplantingIrrigationPowerSource", "PCropSolidOrgFertAppMethod",
#    "MineralFertAppMethod", "MineralFertAppMethod.1", "Block", "District",
#    "Stubble_use", "Threshing_method", "Harv_method"),
#     remove_selected_columns = TRUE)

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

# most similar to ID_YTZN9FE7PQUY is ID_2898YKTT7ABB according to the gower distance
# november 22 instead of march 22 seems to be more likely
df = df %>%
  rows_update(tibble(ID = "ID_YTZN9FE7PQUY", Harv_date = as.Date("2022-11-04")),
              by = "ID")
# alternativ:
#df = df %>%
#  rows_update(tibble(ID = "ID_YTZN9FE7PQUY",
#                     Harv_date = df$RcNursEstDate[[3134]] + runif(1, 95, 151)), by = "ID")

## replace NAs##
df <- df %>%
 mutate(Ganaura = ifelse(OrgFertilizers_Ganaura == 0, 0, Ganaura),
        CropOrgFYM = ifelse(OrgFertilizers_FYM == 0, 0, CropOrgFYM),
        BasalUrea = ifelse(CropbasalFerts_Urea == 0, 0, BasalUrea),
        BasalDAP = ifelse(CropbasalFerts_DAP == 0, 0, BasalDAP),
        X1tdUrea = ifelse(FirstTopDressFert_Urea == 0, 0, X1tdUrea),
        X2tdUrea = ifelse(NoFertilizerAppln < 3, 0, X2tdUrea),
        StandingWater = ifelse(is.na(StandingWater), 0, StandingWater),
        SeedlingsPerPit = ifelse(is.na(RcNursEstDate), 0, SeedlingsPerPit))
# no third dose of chemical fertilizer was applied
ftable(is.na(df$X2tdUrea), df$NoFertilizerAppln)

df <- df %>%
 mutate(X1appDaysUrea = ifelse(FirstTopDressFert_Urea == 0, -1, X1appDaysUrea),
        X2appDaysUrea = ifelse(NoFertilizerAppln < 3, -1, X2appDaysUrea))

df <- df %>%
  rows_update(tibble(ID = df$ID[is.na(df$X2tdUrea) & df$X1tdUrea == 0 & df$X1appDaysUrea == -1],
                     X2tdUrea = 0, X2appDaysUrea = -1), by = "ID")


## new variables ##
df <- df %>%
  mutate(CropCultPerc = CropCultLand / CultLand,
    StandingWaterPerc = StandingWater / as.numeric(
    difftime(Harv_date, SeedingSowingTransplanting, units = "days")) * 100,
    PlantingMethod = as.factor(ifelse(is.na(RcNursEstDate), "sowing", "tranplantingSeedlings")))

df <- df %>%
  rowwise() %>%
  mutate(NoOrgFerts = sum(c_across(starts_with("OrgFertilizers"))),
         NoLPMethods = sum(c_across(starts_with("LandPreparationMethod"))),
         NoNDFactors = sum(c_across(starts_with("NursDetFactor"))),
         NoTDFactors = sum(c_across(starts_with("TransDetFactor"))),
         NoCropbFerts = sum(c_across(starts_with("CropbasalFerts"))),
         NoFTDFerts = sum(c_across(starts_with("FirstTopDressFert")))) %>%
  ungroup()


df$CropTillage_Month = month(df$CropTillageDate)
df$RcNursEst_Month = month(df$RcNursEstDate)
df$SeedingSowingTransplanting_Month = month(df$SeedingSowingTransplanting)
df$Harv_Month = month(df$Harv_date)
df$Threshing_Month = month(df$Threshing_date)

df$CropTillage_Quarter = quarter(df$CropTillageDate)
#df$RcNursEst_Quarter = quarter(df$RcNursEstDate)
#df$SeedingSowingTransplanting_Quarter = quarter(df$SeedingSowingTransplanting)
df$Harv_Quarter = quarter(df$Harv_date)
df$Threshing_Quarter = quarter(df$Threshing_date)

#df$CropTillage_Month_sin = sin(2 * pi * df$CropTillage_Month / 12)
#df$CropTillage_Month_cos = cos(2 * pi * df$CropTillage_Month / 12)
#df$RcNursEst_Month_sin = sin(2 * pi * df$RcNursEst_Month / 12)
#df$RcNursEst_Month_cos = cos(2 * pi * df$RcNursEst_Month / 12)
#df$SeedingSowingTransplanting_Month_sin = sin(2 * pi * df$SeedingSowingTransplanting_Month / 12)
#df$SeedingSowingTransplanting_Month_cos = cos(2 * pi * df$SeedingSowingTransplanting_Month / 12)
df$Harv_Month_sin = sin(2 * pi * df$Harv_Month / 12)
#df$Harv_Month_cos = cos(2 * pi * df$Harv_Month / 12)
df$Threshing_Month_sin = sin(2 * pi * df$Threshing_Month / 12)
#df$Threshing_Month_cos = cos(2 * pi * df$Threshing_Month / 12)

df$DiffCropSeed = abs(as.numeric(difftime(df$CropTillageDate, df$SeedingSowingTransplanting, units = "days")))
df$DiffRCSeed = abs(as.numeric(difftime(df$RcNursEstDate, df$SeedingSowingTransplanting, units = "days")))
df$DiffSeedHarv = abs(as.numeric(difftime(df$SeedingSowingTransplanting, df$Harv_date, units = "days")))
df$DiffHarvThresing = abs(as.numeric(difftime(df$Harv_date, df$Threshing_date, units = "days")))

df$CropTillageDate = as.numeric(difftime(df$CropTillageDate, as.Date("2022-05-01"), units = "days"))
df$RcNursEstDate = as.numeric(difftime(df$RcNursEstDate, as.Date("2022-05-01"), units = "days"))
df$SeedingSowingTransplanting = as.numeric(difftime(df$SeedingSowingTransplanting, as.Date("2022-05-01"), units = "days"))
df$Harv_date = as.numeric(difftime(df$Harv_date, as.Date("2022-05-01"), units = "days"))
df$Threshing_date = as.numeric(difftime(df$Threshing_date, as.Date("2022-05-01"), units = "days"))

#df = df %>% mutate(Yield = case_when(Yield > 1000 ~ 1000,
#                                         Yield <= 1000 ~ Yield))
#replace NAs in RcNursEstDate
df = df %>%
  mutate(RcNursEstDate = ifelse(is.na(RcNursEstDate), -1, RcNursEstDate),
         RcNursEst_Month = ifelse(is.na(RcNursEst_Month), 0, RcNursEst_Month),
         DiffRCSeed = ifelse(is.na(DiffRCSeed), 0, DiffRCSeed))

## deselect columns + transform & adjust variables for area of land under cultivation (Acre) ##
df = df %>% select(-TransIrriCost, -Harv_Month, -Threshing_Month) %>%
  mutate(across(any_of(c("X1tdUrea", "X2tdUrea", "Harv_hand_rent", "BasalDAP", "BasalUrea",
                         "Ganaura", "CropOrgFYM", "Yield")), ~ .x / Acre))
  
## select randomly 3 of the duplicated Vaishali observations (all with Yield/Acre == 44)
dupIDs = df %>% filter(District == "Vaishali" & Yield < 45) %>% pull(ID)
set.seed(94)
dupIDs = sample(dupIDs, 91)
df = df %>% filter(!ID %in% dupIDs)

rm(dupIDs)
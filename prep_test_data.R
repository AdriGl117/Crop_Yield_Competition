library(dplyr)
library(lubridate)

tdf = read.csv("data/Test.csv") %>%
 mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                 Harv_date, Threshing_date), as.Date))

## character columns to factor columns ##
tdf = tdf %>% mutate(across(c(District, Block, CropEstMethod,
           TransplantingIrrigationSource, TransplantingIrrigationPowerSource,
           PCropSolidOrgFertAppMethod, MineralFertAppMethod, MineralFertAppMethod.1,
           Harv_method, Threshing_method, Stubble_use), as.factor))

## factor variables to dummy columns ##
#tdf = tdf %>%
#        fastDummies::dummy_cols(c("CropEstMethod", "TransplantingIrrigationSource",
#                                  "TransplantingIrrigationPowerSource", "PCropSolidOrgFertAppMethod",
#                                  "MineralFertAppMethod", "MineralFertAppMethod.1", "Block", "District",
#                                  "Stubble_use", "Threshing_method", "Harv_method"),
#                                remove_selected_columns = TRUE)

## character columns to dummy columns ##
tdf = tdf %>%
 fastDummies::dummy_cols(c("LandPreparationMethod", "NursDetFactor",
                           "TransDetFactor", "OrgFertilizers", "CropbasalFerts", "FirstTopDressFert"),
                         split = " ", remove_selected_columns = TRUE)

## adjust wrong values ##
# set outlier values to NA except if there is an obvious way to correct them 
tdf <- tdf %>%
        mutate(X1appDaysUrea = ifelse(X1appDaysUrea == 332, NA, X1appDaysUrea),
               SeedlingsPerPit = ifelse(SeedlingsPerPit == 442, NA, SeedlingsPerPit),
               Harv_hand_rent = ifelse(Harv_hand_rent == 60000, NA, Harv_hand_rent),
               Ganaura = ifelse(Ganaura / Acre > 500, NA, Ganaura),
               CropOrgFYM = ifelse(CropOrgFYM / Acre > 500, NA, CropOrgFYM),
               Harv_date = as.Date(ifelse(format(Harv_date, "%Y") == "2021",
                                          Harv_date + 365, Harv_date), origin = "1970-01-01"))

## replace NAs##
tdf = tdf %>%
 mutate(Ganaura = ifelse(OrgFertilizers_Ganaura == 0, 0, Ganaura),
        CropOrgFYM = ifelse(OrgFertilizers_FYM == 0, 0, CropOrgFYM),
        BasalUrea = ifelse(CropbasalFerts_Urea == 0, 0, BasalUrea),
        BasalDAP = ifelse(CropbasalFerts_DAP == 0, 0, BasalDAP),
        X1tdUrea = ifelse(FirstTopDressFert_Urea == 0, 0, X1tdUrea),
        X2tdUrea = ifelse(NoFertilizerAppln < 3, 0, X2tdUrea))
# no third dose of chemical fertilizer was applied
ftable(is.na(tdf$X2tdUrea), tdf$NoFertilizerAppln)

tdf = tdf %>%
 mutate(X1appDaysUrea = ifelse(FirstTopDressFert_Urea == 0, -1, X1appDaysUrea),
        X2appDaysUrea = ifelse(NoFertilizerAppln < 3, -1, X2appDaysUrea))

## new variables ##
tdf = tdf %>%
 mutate(CropCultPerc = CropCultLand / CultLand,
        StandingWaterPerc = StandingWater / as.numeric(
                difftime(Harv_date, SeedingSowingTransplanting, units = "days")) * 100)

tdf = tdf %>%
 rowwise() %>%
 mutate(NoOrgFerts = sum(c_across(starts_with("OrgFertilizers"))),
        NoLPMethods = sum(c_across(starts_with("LandPreparationMethod"))),
        NoNDFactors = sum(c_across(starts_with("NursDetFactor"))),
        NoTDFactors = sum(c_across(starts_with("TransDetFactor"))),
        NoCropbFerts = sum(c_across(starts_with("CropbasalFerts"))),
        NoFTDFerts = sum(c_across(starts_with("FirstTopDressFert"))))

## adjust variables for area of land under cultivation (Acre) ##
tdf <- tdf %>%
        mutate(X1tdUrea = X1tdUrea / Acre, X2tdUrea = X2tdUrea / Acre,
               Harv_hand_rent = Harv_hand_rent / Acre, BasalDAP = BasalDAP / Acre,
               BasalUrea = BasalUrea / Acre, Ganaura = Ganaura / Acre,
               CropOrgFYM = CropOrgFYM / Acre)

# date variables
tdf = tdf %>%
        mutate(CropTillage_Month = month(CropTillageDate),
               RcNursEst_Month = month(RcNursEstDate),
               SeedingSowingTransplanting_Month = month(SeedingSowingTransplanting),
               Harv_Month = month(Harv_date), Threshing_Month = month(Threshing_date),
               CropTillage_Quarter = quarter(CropTillageDate),
               Harv_Quarter = quarter(Harv_date), Threshing_Quarter = quarter(Threshing_date),
               CropTillage_Month_sin = sin(2 * pi * CropTillage_Month / 12),
               CropTillage_Month_cos = cos(2 * pi * CropTillage_Month / 12),
               RcNursEst_Month_sin = sin(2 * pi * RcNursEst_Month / 12),
               RcNursEst_Month_cos = cos(2 * pi * RcNursEst_Month / 12),
               SeedingSowingTransplanting_Month_sin = sin(2 * pi * SeedingSowingTransplanting_Month / 12),
               SeedingSowingTransplanting_Month_cos = cos(2 * pi * SeedingSowingTransplanting_Month / 12),
               Harv_Month_sin = sin(2 * pi * Harv_Month / 12),
               Harv_Month_cos = cos(2 * pi * Harv_Month / 12),
               Threshing_Month_sin = sin(2 * pi * Threshing_Month / 12),
               Threshing_Month_cos = cos(2 * pi * Threshing_Month / 12))

tdf = tdf %>%
        mutate(DiffCropSeed = as.numeric(difftime(CropTillageDate, SeedingSowingTransplanting, units = "days")),
               DiffCropHarv = as.numeric(difftime(CropTillageDate, Harv_date, units = "days")),
               DiffCropThresing = as.numeric(difftime(CropTillageDate, Threshing_date, units = "days")),
               DiffRCSeed = as.numeric(difftime(RcNursEstDate, SeedingSowingTransplanting, units = "days")),
               DiffRCHarv = as.numeric(difftime(RcNursEstDate, Harv_date, units = "days")),
               DiffRCThresing = as.numeric(difftime(RcNursEstDate, Threshing_date, units = "days")),
               DiffSeedHarv = as.numeric(difftime(SeedingSowingTransplanting, Harv_date, units = "days")),
               DiffSeedThresing = as.numeric(difftime(SeedingSowingTransplanting, Threshing_date, units = "days")),
               DiffHarvThresing = as.numeric(difftime(Harv_date, Threshing_date, units = "days")))

tdf = tdf %>%
        mutate(CropTillageDate = as.numeric(difftime(CropTillageDate, as.Date("2022-05-01"), units = "days")),
               RcNursEstDate = as.numeric(difftime(RcNursEstDate, as.Date("2022-05-01"), units = "days")),
               SeedingSowingTransplanting = as.numeric(difftime(SeedingSowingTransplanting, as.Date("2022-05-01"), units = "days")),
               Harv_date = as.numeric(difftime(Harv_date, as.Date("2022-05-01"), units = "days")),
               Threshing_date = as.numeric(difftime(Threshing_date, as.Date("2022-05-01"), units = "days")))

#df = df %>% mutate(Yield = case_when(Yield > 1000 ~ 1000,
#                                         Yield <= 1000 ~ Yield))
#replace NAs in RcNursEstDate
tdf = tdf %>%
        mutate(RcNursEstDate = ifelse(is.na(RcNursEstDate), -1, RcNursEstDate),
               DiffRCSeed = ifelse(is.na(DiffRCSeed), 0, DiffRCSeed),
               DiffRCHarv = ifelse(is.na(DiffRCHarv), DiffSeedHarv, DiffRCHarv),
               DiffRCThresing = ifelse(is.na(DiffRCThresing), DiffSeedThresing, DiffRCThresing))

# add missing columns & delete columns which only exist in test data
zero_mat = matrix(0, 1, sum(!colnames(df) %in% colnames(tdf)))
colnames(zero_mat) = colnames(df)[!colnames(df) %in% colnames(tdf)]
zero_mat = as_tibble(zero_mat)
tdf = tdf %>% mutate(zero_mat)

tdf = tdf %>% select(-all_of(colnames(tdf)[!colnames(tdf) %in% colnames(df)]))

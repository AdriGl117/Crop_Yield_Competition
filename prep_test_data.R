library(dplyr)

tdf = read.csv("data/Test.csv") %>%
 mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                 Harv_date, Threshing_date), as.Date))

## character columns to factor columns ##
tdf = tdf %>% mutate(across(c(District, Block, CropEstMethod,
           TransplantingIrrigationSource, TransplantingIrrigationPowerSource,
           PCropSolidOrgFertAppMethod, MineralFertAppMethod, MineralFertAppMethod.1,
           Harv_method, Threshing_method, Stubble_use), as.factor))

## factor variables to dummy columns ##
tdf = tdf %>%
 fastDummies::dummy_cols(c("CropEstMethod", "TransplantingIrrigationSource",
           "TransplantingIrrigationPowerSource", "PCropSolidOrgFertAppMethod",
                           "MineralFertAppMethod", "MineralFertAppMethod.1", "Block"),
                         remove_selected_columns = TRUE)

## character columns to dummy columns ##
tdf = tdf %>%
 fastDummies::dummy_cols(c("LandPreparationMethod", "NursDetFactor",
                           "TransDetFactor", "OrgFertilizers", "CropbasalFerts", "FirstTopDressFert"),
                         split = " ", remove_selected_columns = TRUE)

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
 mutate(CropCultPerc = CropCultLand / CultLand)

tdf = tdf %>%
 rowwise() %>%
 mutate(NoOrgFerts = sum(c_across(starts_with("OrgFertilizers"))),
        NoLPMethods = sum(c_across(starts_with("LandPreparationMethod"))),
        NoNDFactors = sum(c_across(starts_with("NursDetFactor"))),
        NoTDFactors = sum(c_across(starts_with("TransDetFactor"))),
        NoCropbFerts = sum(c_across(starts_with("CropbasalFerts"))),
        NoFTDFerts = sum(c_across(starts_with("FirstTopDressFert"))))

# add missing columns & delete columns which only exist in test data
zero_mat = matrix(0, 1, sum(!colnames(df) %in% colnames(tdf)))
colnames(zero_mat) = colnames(df)[!colnames(df) %in% colnames(tdf)]
zero_mat = as_tibble(zero_mat)
tdf = tdf %>% mutate(zero_mat)

tdf = tdf %>% select(-all_of(colnames(tdf)[!colnames(tdf) %in% colnames(df)]))
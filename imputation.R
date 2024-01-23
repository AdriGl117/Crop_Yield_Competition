library(mlr3)
library(mlr3pipelines)
library(dplyr)
if(!sourcing) {
  impute_tech = "hot deck"
  DistrictSplit = TRUE
}
source("helper_functions.R")

if(impute_tech == "hot deck") {
  df_copy = hotDeck(df, exclude = c("Yield", "Acre", "ID"),
    sameLevel = list(Ganaura = "OrgFertilizers_Ganaura", SeedlingsPerPit = "PlantingMethod",
      CropOrgFYM = "OrgFertilizers_FYM"), sameLevelALL = "District",
    excludeReplaceValue = list(X1appDaysUrea = -1, X2appDaysUrea = -1, X2tdUrea = 0))
  
  df_copy %>% mutate(UreaTotal = BasalUrea + X1tdUrea + X2tdUrea,
    OrganicFertAmount = Ganaura + CropOrgFYM, BasalLP = BasalUrea + BasalDAP)
  
  task = as_task_regr(df_copy, target = "Yield", id = "Yield Crop")
  task$set_col_roles("ID", add_to = "name", remove_from = "feature")
  task$set_col_roles("Acre", remove_from = "feature")

  if(DistrictSplit) {
    task$set_col_roles("District", remove_from = "feature")
    
    task_gaya = task$clone()
    task_gaya$filter(which(df_copy$District == "Gaya"))
    
    task_jamui = task$clone()
    task_jamui$filter(which(df_copy$District == "Jamui"))
    
    task_nalanda = task$clone()
    task_nalanda$filter(which(df_copy$District == "Nalanda"))
    
    task_vaishali = task$clone()
    task_vaishali$filter(which(df_copy$District == "Vaishali"))
  }
} else {
  if(impute_tech == "impute mean") {
    imputeMethod = po("imputemean")
  } else if(impute_tech == "impute hist") {
    imputeMethod = po("imputehist")
  } else if(impute_tech == "imputelearner regr") {
    imputeMethod = po("imputelearner", lrn("regr.rpart")) 
  }
  
  df_copy <- df %>%
    rows_update(tibble(ID = df$ID[is.na(df$X2appDaysUrea)],
                       X2appDaysUrea = mapply(function(dt, dh, maxd) {
                         max_pos <- as.numeric(dh - dt)
                         runif(1, 1, min(max_pos, maxd))
                       }, dt = df[is.na(df$X2appDaysUrea),]$CropTillageDate,
                       dh = df[is.na(df$X2appDaysUrea),]$Harv_date,
                       MoreArgs = list(maxd = max(df$X2appDaysUrea, na.rm = TRUE)))), by = "ID")
  
  
  task = as_task_regr(df_copy, target = "Yield", id = "Yield Crop")
  task$set_col_roles("ID", add_to = "name", remove_from = "feature")
  task$set_col_roles("Acre", remove_from = "feature")
 
  if(DistrictSplit) {
    task$set_col_roles("District", remove_from = "feature")
    
    task_gaya = task$clone()
    task_gaya$filter(which(df_copy$District == "Gaya"))
    task_gaya = imputeMethod$train(list(task_gaya))[[1]]
    task_gaya$cbind(task_gaya$data()[, `:=` (BasalLP = BasalUrea + BasalDAP,
      UreaTotal = BasalUrea + X1tdUrea + X2tdUrea, OrganicFertAmount = Ganaura + CropOrgFYM)] %>%
        select(BasalLP, UreaTotal, OrganicFertAmount))
    
    task_jamui = task$clone()
    task_jamui$filter(which(df_copy$District == "Jamui"))
    task_jamui = imputeMethod$train(list(task_jamui))[[1]]
    task_jamui$cbind(task_jamui$data()[, `:=` (BasalLP = BasalUrea + BasalDAP,
      UreaTotal = BasalUrea + X1tdUrea + X2tdUrea, OrganicFertAmount = Ganaura + CropOrgFYM)] %>%
        select(BasalLP, UreaTotal, OrganicFertAmount))
    
    task_nalanda = task$clone()
    task_nalanda$filter(which(df_copy$District == "Nalanda"))
    task_nalanda = imputeMethod$train(list(task_nalanda))[[1]]
    task_nalanda$cbind(task_nalanda$data()[, `:=` (BasalLP = BasalUrea + BasalDAP,
      UreaTotal = BasalUrea + X1tdUrea + X2tdUrea, OrganicFertAmount = Ganaura + CropOrgFYM)] %>%
        select(BasalLP, UreaTotal, OrganicFertAmount))
    
    task_vaishali = task$clone()
    task_vaishali$filter(which(df_copy$District == "Vaishali"))
    task_vaishali = imputeMethod$train(list(task_vaishali))[[1]]
    task_vaishali$cbind(task_vaishali$data()[, `:=` (BasalLP = BasalUrea + BasalDAP,
      UreaTotal = BasalUrea + X1tdUrea + X2tdUrea, OrganicFertAmount = Ganaura + CropOrgFYM)] %>%
        select(BasalLP, UreaTotal, OrganicFertAmount))
  } else {
    task = imputeMethod$train(list(task))[[1]]
    task$cbind(task$data()[, `:=` (BasalLP = BasalUrea + BasalDAP,
      UreaTotal = BasalUrea + X1tdUrea + X2tdUrea, OrganicFertAmount = Ganaura + CropOrgFYM)] %>%
        select(BasalLP, UreaTotal, OrganicFertAmount))
  }
}
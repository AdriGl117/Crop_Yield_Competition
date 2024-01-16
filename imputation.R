library(mlr3)
library(mlr3pipelines)
impute_tech = "hot deck"
DistrictSplit = TRUE

if(impute_tech == "hot deck") {
  df_copy = hotDeck(df, exclude = c("Yield", "Acre", "ID"),
    sameLevel = list(Ganaura = "OrgFertilizers_Ganaura", SeedlingsPerPit = "PlantingMethod",
      CropOrgFYM = "OrgFertilizers_FYM"), sameLevelALL = "District",
    excludeReplaceValue = list(X1appDaysUrea = -1, X2appDaysUrea = -1, X2tdUrea = 0))
  
  task = as_task_regr(df_copy, target = "Yield", id = "Yield Crop")
  task$set_col_roles("ID", add_to = "name", remove_from = "feature")
} else {
  if(impute_tech == "impute mean") {
    imputeMethod = po("imputemean")
  } else if(impute_tech == "impute hist") {
    imputeMethod = po("imputehist")
  } else if(impute_tech == "imputelearner regr") {
    imputeMethod = po("imputelearner", lrn("regr.rpart")) 
  }
  task = as_task_regr(df, target = "Yield", id = "Yield Crop")
  task$set_col_roles("ID", add_to = "name", remove_from = "feature")
 
  task = imputeMethod$train(list(task))[[1]]
}
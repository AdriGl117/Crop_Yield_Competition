get_task = function(){
 library(mlr3verse)
 library(dplyr)
 library(lubridate)
 data = read.csv("data/Train.csv") %>%
  mutate(across(c(CropTillageDate, RcNursEstDate, SeedingSowingTransplanting,
                  Harv_date, Threshing_date), as.Date))
 
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
 
 data$CropTillageDate = as.numeric(difftime(data$CropTillageDate, min(data$CropTillageDate), units = "days"))
 data$RcNursEstDate = as.numeric(difftime(data$RcNursEstDate, min(data$RcNursEstDate), units = "days"))
 data$SeedingSowingTransplanting = as.numeric(difftime(data$SeedingSowingTransplanting, min(data$SeedingSowingTransplanting), units = "days"))
 data$Harv_date = as.numeric(difftime(data$Harv_date, min(data$Harv_date), units = "days"))
 data$Threshing_date = as.numeric(difftime(data$Threshing_date, min(data$Threshing_date), units = "days"))
 
 #data[is.na(data)] = 0
 
 task = as_task_regr(data, target = "Yield")
 
 po = po("imputelearner", po("imputehist") %>>% lrn("regr.ranger"))
 task = po$train(list(task = task))[[1]]
 return(task)
}
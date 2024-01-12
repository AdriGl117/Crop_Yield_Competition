#' calculation of the gower distance for a single observation compared to the rest
#' of the data
#' 
#' @param obs single observation (data.frame[1, ncol(data)])
#' @param data (data.frame)
#' @param exclude vector of column names, which should be ignored for the
#' calculation of the gower distance. "" means no column is ignored
#' Default: ""
#' 
#' @return numeric vector with values representing the gower distance to the single
#' observation in order of the rows of data

gowerDist = function(obs, data, exclude = "") {
  data = data %>% select(!any_of(exclude))
  obs = obs %>% select(!any_of(exclude))
  col_dist_mat = mapply(function(obs_col, data_col) {
   if(is.numeric(data_col)) {
    abs(obs_col - data_col) / (max(data_col, na.rm = TRUE) - min(data_col, na.rm = TRUE))
   } else {
    as.numeric(data_col != obs_col)
   }
  }, obs_col = obs, data_col = data)
  apply(col_dist_mat, 1, function(x) {1/sum(!is.na(x))}) * rowSums(col_dist_mat, na.rm = TRUE)
}


#' Hot Deck Imputation function
#' used distance function: gowerDist
#' 
#' @param data the data(data.frame) to impute
#' @param exclude columns(character vector), which should be ignored for the
#' imputation and distance function
#' @param allcolsNA (logical) should all included columns be checked for NAs and
#' in case of NAs be included for the imputation. Default: TRUE
#' @param colsNA_names the column names (character vector), which should be imputed
#' if allcolsNA = TRUE, this parameter is going to be ignored
#' @param sameLevel
#' @param excludeReplaceValue
#' 
#' @return data.frame with imputed values replacing NAs
hotDeck = function(data, exclude = "", allcolsNA = TRUE, colsNA_names = "",
                   sameLevel = list(), excludeReplaceValue = list()) {
  data_copy = data %>% select(!any_of(exclude))
  
  if(allcolsNA) {
    colsNA = apply(data_copy, 2, anyNA)
    colsNA_names = names(colsNA[colsNA])
  }
  
  for(col in colsNA_names) {
    missV_index = which(is.na(data_copy[[col]]))
    
    replacementVs = vapply(missV_index,
      function(obs_id, dc) {
        gdist = gowerDist(dc[obs_id, ], dc, col)
        if(col %in% names(sameLevel)) {
          diffLevel = rep(FALSE, nrow(dc))
          
          for(i in seq_along(sameLevel[[col]])) {
            diffLevel = diffLevel |
              dc[[sameLevel[[col]][[i]]]] != dc[obs_id, sameLevel[[col]][[i]]][[1]]
          }
          gdist[diffLevel] = 1
        }
        
        if(col %in% names(excludeReplaceValue)) {
          gdist[dc[[col]] %in% excludeReplaceValue[[col]]] = 1
        }
        
        gdist_order = order(gdist)
        dc[[col]][[gdist_order[!gdist_order %in% missV_index][[1]]]]
      }, FUN.VALUE = 1, dc = data_copy)
    
    data[[col]][missV_index] = replacementVs
  }
  data
}

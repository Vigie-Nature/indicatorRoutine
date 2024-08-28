#' formatData
#'
#' A function that add a unique identifier, filter for selected year range and format variables
#' 
#' @param data a `data.frame` containing observations 
#' @param interestVar a 1 or 2-elements `vector` containing the response variable
#' @param yearRange a 2-elements `vector` containing lower and upper limits of years to be studied 
#' @param fixedEffects a `vector` containing variables that should be treated as continuous fixed effects
#' @param factorVariables a `vector` containing variables that should be treated as categorical fixed effects
#' @param randomEffects a `vector` containing variables that should be treated as random effects
#' @param nestedEffects a `list` of 2-elements `vector` containing variables that should be treated as nested effects 
#' @param slopeRandomEffects a `list` of 2-elements `vector` containing variables that should be treated as random slopes
#' @param poly a `vector` containing variables that should be treated as 2nd-order polynomial effects
#' 
#' @return
#' a `data.frame` with an additionnal column and right filtering 
formatData = function(data, yearRange, interestVar, fixedEffects, factorVariables, 
                      randomEffects, nestedEffects, slopeRandomEffects, poly){
  
  # Create unique identifier ----
  data$ID = paste(data$year, data$site)
  
  # Add the most precise spatial scale
  if("point" %in% colnames(data)){
    data$ID = paste(data$ID, data$point)
  }
  # Add the most precise temporal
  if("session" %in% colnames(data)){
    data$ID = paste(data$ID, data$session)
  }
  
  
  # Filter for selected range of years ----
  if(!is.null(yearRange)){
    data = data[data$year >= yearRange[1] & data$year <= yearRange[2], ]
  }
  
  # Select only column of interest ----
  vars = unique(c("ID", "species", "site", "year", "day", "point",
                  "longitude", "latitude", "saison", interestVar, fixedEffects, 
                  factorVariables, poly, unlist(randomEffects), 
                  unlist(nestedEffects), unlist(slopeRandomEffects))) # var "saison" has been added a posteriori and is used for SHOC trends
  
  vars = vars[vars %in% colnames(data)]
  data = data[,colnames(data) %in% vars]
  
  # Erase NA values regarding all variables ----
  rowToErase = which(apply(data[,vars], 1, function(x) any(is.na(x))))
  if(length(rowToErase) > 0){
    data = data[-rowToErase,]
    cat("Due to missing values,", length(rowToErase), "rows were removed from the analysis.\n")
  }
  
  return(data)
}

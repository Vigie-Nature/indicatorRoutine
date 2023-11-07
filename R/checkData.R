
#' checkData
#'
#' A function that checks if variables are present in the dataframe
#' 
#' @param data a `data.frame` containing observations of species.#'
#' @param interestVar a `string` containing the variable of interest
#' @param fixedEffects a `vector` containing variables that should be treated as fixed effects
#' @param factorVariables a `vector` containing variables that should be treated as fixed effects
#' @param randomEffects a `vector` containing variables that should be treated as random effects
#' @param nestedEffects a `list` of 2-elements `vector` containing variables that should be treated as nested effects 
#' @param slopeRandomEffects a `list` of 2-elements `vector` containing the random slopes that should betested
#' @param poly a `vector` containing variables that should be treated as random effects 
#' 
#' @return
#' A `boolean`. TRUE if everything is OK. FALSE if not.
#'
#' @export
#' @example
checkData <- function(data,
                      interestVar = NULL, 
                      fixedEffects = NULL, 
                      factorVariables = NULL,
                      randomEffects = NULL,
                      nestedEffects = NULL,
                      slopeRandomEffects = NULL,
                      poly = NULL){
  allIsOK = TRUE
  
  # Set of mandatory variables ----
  mandatoryVars <- c("year", "species", "site")
  
  if(!all(mandatoryVars %in% colnames(data))){
    allIsOK = FALSE
    stop("Some mandatory variables are missing in the data.frame. Make sure you have information on: ", 
         paste(mandatoryVars, collapse = ", "), ".")
  }
  
  # Set of variables included in the model ----
  modelVars <- unique(c(interestVar, fixedEffects, factorVariables, randomEffects, 
                        unlist(poly) , unlist(nestedEffects), unlist(slopeRandomEffects)))
  
  if(!all(modelVars %in% colnames(data))){
    allIsOK = FALSE
    missingVars = modelVars[which(is.na(match(modelVars, colnames(data))))]
    
    stop("Some variables specified in the model are absent: ",
         paste(missingVars, collapse = ", "), ".")
  }
  
  
  
  # If all variables are found in the dataframe ----
  if(allIsOK){
    cat("Your dataframe has proper column names\n",
        "You can proceed to further analysis !\n")
  }
  
  return(allIsOK)
  
}



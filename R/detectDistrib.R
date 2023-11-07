#' detectDistrib
#'
#' A function that affects a distribution law (e.g, gaussian, poisson, binomial) to an analysis depending on the response variable  
#'
#' @param data : a `data.frame` containing observations
#' @param interestVar : a 1 or 2-elements `vector` corresponding to the response variable(s)
#'
#' @return a `string` containing the chosen distribution
#' 
#' @example
detectDistrib <- function(data, interestVar){
  
  # Extract the response variable values ----
  abundance = data[,interestVar]
  
  # Condition 1 : is there 1 or 2 response variable ?
  if(length(interestVar) == 1){
    
    # Erase any NAs ----
    abundance <- abundance[!is.na(abundance)]
   if(all(abundance == round(abundance)) & max(abundance) > 1){
     distribution = "nbinom2"
   }else{
     distribution = "gaussian"
   }
    
  }else if(length(interestVar) == 2){
    distribution = "betabinomial"
  }else{
    stop("No distribution can be imputed from your response variable format.")
  }
  
  return(distribution)
}

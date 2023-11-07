#' identifyConvIssue
#' 
#' A function that aims to identify if there was any convergence issues
#' 
#' @param model : a 3-elements `list` containing :
#' - value, a glmmTMB object containing results from regression
#' - warnings, either NULL if no warnings raised or a `vector` specifying the warnings
#' - error, either NULL if no error encountered or a `string` specifying the error
#' 
#' @return a `boolean` indicating if there was indeed a convergence issue
#' NB : convergence issue = either an error, an absence of convergence or NA in the summary
#' 
identifyConvIssue <- function(model){
  
  # Extract potential errors
  e <- model$error
  
  # Extract warnings with "convergence issue"
  w <- model$warnings
  
  # Identify NA in the summary
  naSum <- FALSE
  if(!is.null(model$value)){
    coefs <- summary(model$value)$coefficients$cond
    naSum <- sapply(coefs, function(x) is.na(x))
  }
  
  # If error or convergence warning or NA in summary, return TRUE
  convIssue <- FALSE
  if (!is.null(e) | any(naSum)){  # | any(convW)
    convIssue <- TRUE
  }else if(model$value$fit$convergence != 0){
    convIssue <- TRUE
  }
  
  return(convIssue)
}

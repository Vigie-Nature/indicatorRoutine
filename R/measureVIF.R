#' measureVIF
#'
#' Measure the VIF associated with each variable
#'
#' @param model an 3-elements `list` (value, warnings, error) containing results of the regression
#'  
#' @importFrom stats vcov 
#'
measureVIF<- function(model){
  
  if(!is.null(model$error)){
    VIF <- NA
    
  }else{
    # Extract variance-Covariance matrix from model output
    v <- vcov(model$value)$cond
    
    # If there is only one variable treated as fixed effect, do not measure VIF.
    if (dim(v)[1] <= 2){
      VIF <- NA
      
    } else{
      
      # Exclude intercepts
      v <- v[-1,-1]
      
      # Extract names of the matrix
      cols <- colnames(v) 
      
      # Square root of the diagonal of the variance-covariance matrix
      d <- sqrt(diag(v))
      
      # Stop the measure of VIF if NAs are created
      if(any(is.na(d))) {
        VIF <- NA
      } else {
        
        # Variance-covariance matrix on outer product of d
        prodD <- v/(d %o% d)
        
        # Inverse d
        invD <- solve(prodD)
        
        # Return the diagonal of d
        VIF <- diag(invD)
        
        # Round to 2 digits
        VIF <- round(VIF,2)
      }
      
    }
  }
  
  return(VIF)
}
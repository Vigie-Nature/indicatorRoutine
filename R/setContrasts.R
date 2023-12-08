#' setContrasts
#'
#' A function that changes contrasts associated with categorical variable
#'
#' @param data a `dataframe` containing observations
#' @param factorVariables a `vector` containing variables that should be treated as categorical fixed effects
#' @param contr a `string` specifying the contrast to be considered for the 'year' either :
#' - "mean" or NULL --> mean of all levels chosen as reference
#' - levelX (e.g, 2001, 2002, ...) --> levelX chosen as reference
#'
#' @importFrom stats contrasts<-
#' @return a `data.frame` with the right contrasts set
#' 
#'
setContrasts <- function(data, factorVariables = NULL, contr = 'mean'){
  
  # Check that there are indeed factor variables to be treated ----
  if(!is.null(factorVariables)){
    
    # Loop on variables ----
    for (var in factorVariables){
      
      # Change type of the column ----
      data[,var] <- as.factor(data[,var])
      
      # Extract the levels of that variable ----
      fLevels <- levels(data[,var])
      
      # If the variable is not effectVar
      if(var != "year"){
        
        ## Measure the nb of observations for all levels ----
        tableLevels <- table(data[,var])
        
        ## Identify the level with maximum nb of obs ----
        contrVar <- names(tableLevels)[which.max(as.numeric(tableLevels))]
        
        ## Reorder levels ----
        newLevels = c(contrVar, fLevels[-which(contrVar == fLevels)])
        
        ## Affect chosen level of reference ----
        data[, var] <- factor(x = data[, var], levels = newLevels)
        
        ## Print a message ----
        message("Contrast associated with the variable '", var, 
                "' has been set to the level with maximum number of observations: ", contrVar)
        
        # If the variable is year
      }else{
        ## Extract the number of levels of the categorical variable ----
        nbLevels <- length(fLevels)
        
        ## Extract the number of columns of the contrast matrix ----
        nbCols <- nbLevels - 1
        
        # If contrast has not been specified, attribute 'mean' contrast
        if(is.null(contr)){contr <- "mean"}
        
        # If the specified contrast is one of the levels 
        # Reorder the levels
        if(contr %in% fLevels){
          ## Affect chosen level of reference ----
          data[, var] <- factor(x = data[, var], 
                                levels = c(contr, fLevels[-which(contr == fLevels)]) )
          ## Print a message ----
          message("Contrast associated with the variable '", var, 
                  "' has been set to: ", contr)
          
          # Else if contrast is 'mean' or misspecified  
          # Set the contrast matrix
        }else{
          ## Set first row of the contrasts to -1 ----
          firstRowCont <- c(rep(-1, nbCols))
          
          ## Set all following rows to a diagonal of 1 ----
          diagCont <- diag(1, nrow = nbCols, ncol = nbCols)
          
          ## Set contrasts ----
          contrasts(data[,var]) <- matrix(c(firstRowCont, diagCont), 
                                          nrow = nbLevels, 
                                          ncol = nbCols, 
                                          byrow = T)
          
          ## Print a message ----
          message("Contrast associated with the variable '", var, 
                  "' has been set to the mean of all levels")
          
        }
      }
    }
  }
  return(data)
}
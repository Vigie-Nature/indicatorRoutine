#' writeFormula
#' 
#' A function that creates a suitable formula object depending on the chosen parameters
#' 
#' @param interestVar a 1 or 2-elements `vector` containing the name of the response variable
#' @param fixedEffects a `vector` of variables that should be treated as continuous fixed effects
#' @param factorVariables a `vector` of variables that should be treated as categorical fixed effects
#' @param poly a `list` of 2-elements vectors with variable that should be treated as polynomial effects
#' @param randomEffects a `vector` of variables that should be treated as random effects 
#' @param nestedEffects a `list` of 2-elements vectors with variable names that should be treated as random nested effects 
#' @param slopeRandomEffects a `list` of 2-elements vectors with variable names that should be treated as random slopes 
#' @param raw a `boolean` specifying if polynomial terms should be raw or orthogonal (for quadraticTrends only)
#' 
#' @return a formula object
#' @example 
writeFormula <- function(interestVar = "count",
                         fixedEffects = NULL, 
                         factorVariables = NULL, 
                         poly = NULL,
                         randomEffects = NULL,
                         nestedEffects = list(),
                         slopeRandomEffects = list(),
                         raw = "raw"){
  
  ## Initialize formula ----
  if(length(interestVar) == 1){
    regrFormula <- paste0(interestVar," ~ 1")
    
  }else{
    regrFormula <- paste0("cbind(",paste(interestVar, collapse = ","),") ~ 1")
  }
  
  
  ## Add fixed effects (continuous and categorical) ----
  effects <- c(fixedEffects, factorVariables)
  
  if (!is.null(effects)){
    
    for (effect in effects){
      regrFormula <- paste0(regrFormula, " + ", effect)
    }
  }
  
  ## Add polynomial effect
  if (!is.null(poly)){
    
    for (p in poly){
      # If the effect considered is not the year, then apply orthogonal polynomial
      if(p != "year" | (p == "year" & raw == "ortho")){
        regrFormula <- paste0(regrFormula, ' + poly(', p, ", ", 2, ", raw = FALSE)")
        
      # If the effect considered is the year, then apply the `raw` parameter
      }else{
        regrFormula <- paste0(regrFormula, ' + poly(', p, ", ", 2, ", raw = TRUE)")
      }
    }
  }
  
  ## Add random effects
  if (!is.null(randomEffects)){
    
    for (effect in randomEffects){
      regrFormula <- paste0(regrFormula, " + (1|", effect, ")")
    }
  }
  
  ## Add nested effects
  if (length(nestedEffects)>0){
    
    for (nE in nestedEffects){
      nestedEffectTerm <- paste0("(1|",nE[2],") + (1|",nE[1],":",nE[2],")")
      regrFormula <- paste0(regrFormula, " + ", nestedEffectTerm)
    }
  }
  
  ## Add slope random effects
  if (length(slopeRandomEffects)>0){
    
    for (nE in slopeRandomEffects){
      nestedEffectTerm <- paste0("(0+",nE[1],"|",nE[2],")")
      regrFormula <- paste0(regrFormula, " + ", nestedEffectTerm)
    }
  }
  
  
  ## Turn character to formula type
  regrFormula <- as.formula(regrFormula)
  
  return(regrFormula)
}



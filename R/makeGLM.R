#' makeGLM
#'
#' A function that makes a GLM regarding the user specifications
#'
#' @param data a `dataframe` containing the variables used in the GLM
#' @param interestVar a 1 or 2-elements `vector` corresponding to the response variable
#' @param fixedEffects a `vector` of variables that should be treated as continuous fixed effects
#' @param factorVariables a `vector` of variables that should be treated as categorical fixed effects
#' @param randomEffects a `vector` of variables that should be treated as random effects
#' @param nestedEffects a `list` of 2-elements vector, containing variables that should be treates as nested effects 
#' @param slopeRandomEffects a `list` of 2-elements `vector` containing variables that should be tested as random slopes
#' @param poly a `vector` containing variables that should be treated as 2nd order polygon
#' @param contr a `vector` containing the contrasts that should be applied to the 'year' variable when treated as categorical
#' @param distribution a `string` containing the chosen distribution between: "gaussian", "poisson", "binomial", "betabinomial", "nbinom2" (by default : "gaussian")
#' @param raw a `string` ('raw' or 'ortho') specifying for quadratic models only, if polynoms should be raw or orthogonal
#'
#' @importFrom stats sigma
#' @return a list of 3 objects :
#' - value a `glmmTMB` object containing results of the regression
#' - warnings a `vector` of warnings encountered during the regression
#' - error a `string` containing a potential error encountered during the regression
#' 
makeGLM <- function(data, interestVar, fixedEffects = NULL,
                    factorVariables = NULL, randomEffects = NULL, 
                    nestedEffects = NULL, slopeRandomEffects = NULL, poly = NULL, 
                    contr = NULL, raw = "raw",
                    distribution = "gaussian"){
  
  ####################
  # Error management #
  ####################
  
  # Check class of data ----
  if (class(data) != "data.frame"){
    stop("'data' should be a data.frame")
  }
  
  # Check that all variables exist ----
  vars <- unique(c(interestVar, fixedEffects, factorVariables, poly, unlist(randomEffects), 
                   unlist(nestedEffects), unlist(slopeRandomEffects)))
  
  if (any(!(vars %in% colnames(data)))){
    missingVars <- vars[which(!(vars %in% colnames(data)))]
    stop("the variables '", paste(missingVars, collapse = ", "), "' are not found in dataframe.")
  }
  
  # Check that distribution exists ----
  if (!(distribution %in% c("binomial", "betabinomial", "gaussian", "poisson", "nbinom2"))){
    stop("the chosen distribution doesn't exist. \nPlease choose between : binomial, betabinomial, gaussian, poisson or nbinom2.")
  }
  
  # Check that year has a variability ----
  if(length(unique(data$year)) == 1){
    model = list(value = NULL, warnings = NULL, error = "No variability in 'year'")
  }else{
    
    ########################################
    # Check factorVariables representation #
    ########################################
    
    if(!is.null(factorVariables)){
      
      for(var in factorVariables){
        if(var != "year"){
          # Number of occurrence of each level ----
          tableComplete = table(data[,var])
          tableVar = table(data[data[interestVar[1]]>0,var])
          
          # Levels that have reasonable occurrence ----
          levelsToKeep = names(tableVar)[which(tableVar >= 5)]
          
          # Levels that don't have reasonable occurrence ----
          levelsToErase = names(tableComplete)[which(is.na(match(names(tableComplete), levelsToKeep)))]
          
          if(length(levelsToErase) > 0){
            # Number of rows to erase ----
            nToErase = length(which(data[,var] %in% levelsToErase))
            
            # Filter data for those categories ----
            data = data[-which(data[,var] %in% levelsToErase),]
            
            # Display warning ----
            cat(nToErase," rows were removed from the analysis due to low representation of levels: '", 
                paste(levelsToErase, collapse = ", "), "', from variable '", var, "'\n")
          }
          
          if(length(unique(data[!is.na(data[,var]),var])) == 1){
            factorVariables = factorVariables[factorVariables != var]
            cat("Variable '",var,"' was removed from the analysis due to the presence of 1-level only.\n")
          }
          
        }
        
      }
    }
    
    ########################################################
    # Check that continuous variable has more than 1 value #
    ########################################################
    
    for(var in fixedEffects){
      
      # Extract each value of the considered variable
      allValues = unique(data[,var])
      
      # If only one value
      if(length(allValues) == 1){
        indVar = match(var, fixedEffects) 
        fixedEffects = fixedEffects[-indVar]
        
        cat("Continuous variable '", var, ' has been removed due to only 1 value')
      }
    }
    
    # If data has more than 1 row
    if(nrow(data)==0){
      model = list(value = NULL, warnings = NULL, error = 'Not enough data')
    }else{
      
      ###############
      # Format data #
      ###############
      
      # Check that factor variables correspond to factor columns
      # And choose the right contrasts for each categorical variable
      data <- setContrasts(data = data, factorVariables = factorVariables, contr = contr)
      
      # Scale continuous variables
      for (contVar in c(fixedEffects, poly)){
        data[,contVar] = scale(data[,contVar])
      }
      
      ###############################
      # Write correctly the formula #
      ###############################
      
      # For the regression model
      formula <- writeFormula(interestVar = interestVar,
                              fixedEffects = fixedEffects,
                              factorVariables = factorVariables,
                              randomEffects = randomEffects,
                              nestedEffects = nestedEffects,
                              slopeRandomEffects = slopeRandomEffects,
                              poly = poly, 
                              raw = raw)
      
      ##################################
      # Assign correctly distributions #
      ##################################
      assign("betabinomial", glmmTMB::betabinomial)
      assign("nbinom2", glmmTMB::nbinom2)
      
      #####################
      # Tendency modeling #
      #####################
      
      # Run basic model ----
      model <- catchConditions(glmmTMB::glmmTMB(formula = formula, data = data, family = distribution))
      
      ########################################
      # Dealing with convergence issue (1/2) #
      ########################################
      
      if(is.null(model$error)){
        
        # Extract model convergence
        modelConv = model$value$fit$convergence
        
        if(modelConv == 1 || identifyConvIssue(model)){
        #if(modelConv == 1){
          # If a convergence issue has been detected 
          # Try to identify the cause, and to solve it
          gardeFou = TRUE ; compt = 0
          
          while ((modelConv == 1 || identifyConvIssue(model)) && gardeFou) {
          #while(modelConv == 1 & gardeFou){
            cat("New try\n")
            # Initialize parameter modif ----
            modif = FALSE
            
            # Compter le nombre de modifications faites ----
            compt = compt + 1
            
            # Au-delà de 5 modifications, arrêter les modifications ----
            if(compt >= 5){
              gardeFou <- FALSE
            }
            
            # 1. Check if distribution is OK ----
            ## Extract dispersion parameter ----
            dispPar = sigma(model$value)
            if(dispPar > 1e+04){
              modif = TRUE
              
              ## Assign new family if no dispersion is detected ----
              distribution <- ifelse(distribution == "nbinom2", "poisson", "binomial")
              
              ## Message relative to the change of distribution
              cat("Distribution has been changed to one with no dispersion\n")
            }
            
            
            # 2. Check if random effects are OK ----
            # if(!modif&!is.null(randomEffects)|length(nestedEffects)>0|length(slopeRandomEffects)>0){
            if (!modif && (is.null(randomEffects) || length(nestedEffects) > 0 || length(slopeRandomEffects) > 0)) {
              ## Summary related to random effects
              sumVar = summary(model$value)$varcor$cond
              
              ## Extract variance associated with each random effect ----
              varRand = sapply(sumVar, function(x) as.numeric(x))
              
              ## Extract the variable with the smallest variance ----
              varToErase = names(sumVar)[which.min(varRand)]
              
              ## Erase .1 / .2 / ... of the name if required 
              varToErase = gsub("[.0-9]+$", "", varToErase)
              
              ## Extract information about random intercept/slope
              intOrSlope = rownames(attributes(sumVar[[varToErase]])$correlation)
              
              ## If variance associated with this random effect is too low ----
              if(sqrt(varRand[which.min(varRand)]) < 0.01){
                modif = TRUE
                
                # Case of random slope to erase 
                if(intOrSlope != "(Intercept)"){
                  # Recreate the variable name 
                  slopeVarToErase = c(intOrSlope, varToErase)
                  
                  # Find position of this variable
                  matchSlope = sapply(slopeVarToErase, function(x) grep(x, slopeRandomEffects))
                  commonMatchSlope = intersect(matchSlope[[1]], matchSlope[[2]])
                  
                  # Erase it from slopeRandomEffects
                  if(length(commonMatchSlope) > 0){
                    slopeRandomEffects = slopeRandomEffects[-commonMatchSlope]
                    
                    cat("Slope random effect: ", intOrSlope, "|", varToErase, " has been erased due to low variance.\n")
                    
                  }
                  
                }     
                
                
                # Case of random intercept to erase 
                else if(varToErase %in% randomEffects){
                  ## Assign updated random effects ----
                  randomEffects <- randomEffects[-match(varToErase, randomEffects)]
                  
                  # If latest random effect, put a NULL value
                  if(length(randomEffects) == 0){randomEffects = NULL}
                  
                  cat("Random effect: ", varToErase, " has been erased due to low variance.\n")
                  
                } 
                
                # Case of random intercept to erase 
                else{
                  # try if  (":" or ".") are present in varToErase
                  if(grepl("[:.]", varToErase, fixed = F)){
                    
                    # Split on ":" or "." regarding the on present
                    splitVarToErase <- unlist(stringr::str_split(varToErase, "[:.]", simplify = T))
                    
                    # Add the englobing variable to the random effect
                    randomEffects <- c(randomEffects, splitVarToErase[2])
                    
                    # Erase the nested effect
                    indToErase <- sapply(1:length(nestedEffects), function(x){
                      nE = nestedEffects[[x]]
                      if(nE[1] == splitVarToErase[1] & nE[2] == splitVarToErase[2]){
                        return(x)
                      }
                    })
                    nestedEffects <- nestedEffects[-indToErase]
                    
                    cat("Random interaction : ", varToErase, " has been erased due to low variance.\n")
                    
                  }else{ 
                    
                    # Extract index where the varToErase is encompassing
                    indToErase = sapply(1:length(nestedEffects), function(x){
                      nE = nestedEffects[[x]]
                      if(nE[2] == varToErase){
                        return(x)
                      }}) 
                    
                    # Extract the interaction
                    
                    randomInteractions = nestedEffects[indToErase]
                    randomInteractions = sapply(randomInteractions,
                                                function(x) paste(x, collapse = ":"))
                    
                    # Add only the interaction to the randomEffects
                    randomEffects <- c(randomEffects, randomInteractions)
                    
                    # Erase the variable from the nestedEffects
                    nestedEffects = nestedEffects[-indToErase]
                    
                    cat("Encompassing nested effect: ", varToErase, " has been erased due to low variance.\n")
                    
                  }
                  
                } 
                
              }
              
            }
            
            # 3. Check if VIFs are OK ----
            if(!modif){
              # Measure VIF for all variables
              allVIF = measureVIF(model)
              
              if(all(!is.na(allVIF))){
                # Erase polynomial effects
                allVIF = allVIF[-grep("poly", names(allVIF))]
                
                # Erase year from potential erasing
                allVIF = allVIF[-grep("year", names(allVIF))]
                
                # If extreme VIF
                if(max(allVIF) > 5){
                  # Extract the name of the variable
                  varMaxVIF = names(allVIF)[which.max(allVIF)]
                  
                  # Erase effect in either continuous or categorical variables
                  indCont = match(varMaxVIF, fixedEffects)
                  indCat = match(varMaxVIF, factorVariables)
                  
                  if(!is.na(indCont)){
                    fixedEffects = fixedEffects[-indCont]
                    cat("Variable ", varMaxVIF, " was erased due to high VIF")
                  }else if(!is.na(indCat)){
                    factorVariables = factorVariables[-indCat]
                    cat("Variable ", varMaxVIF, " was erased due to high VIF")
                  }
                  
                  modif = TRUE
                }
              }
            }
            
            if (modif){
              # Re-write formula ----
              formula <- writeFormula(interestVar = interestVar,
                                      fixedEffects = fixedEffects,
                                      factorVariables = factorVariables,
                                      randomEffects = randomEffects,
                                      nestedEffects = nestedEffects,
                                      slopeRandomEffects = slopeRandomEffects,
                                      poly = poly, 
                                      raw = raw)      
              
              # Re-run model ----
              model <- catchConditions(glmmTMB::glmmTMB(formula = formula, data = data, family = distribution))
              
              # Re-extract model Convergence 
              modelConv <- ifelse(is.null(model$error), model$value$fit$convergence, -1)
            }
          }
          
          
          ########################################
          # Dealing with convergence issue (2/2) #
          ########################################
          
          # If the issue is still not solved, try to start from previous estimated parameters
          # And if the result is similar, keep it
          if(modelConv == 1){
            
            ## Find latest fitted parameters ----
            latestParams = as.numeric(model$value$fit$par)
            ## Extract their names ----
            paramNames = names(model$value$fit$par)
            
            ## Create a named list with associated values ----
            startList = list()
            for(par in unique(paramNames)){
              startList[[par]] = latestParams[which(paramNames == par)]
              
            }
            
            ## Refit model from those parameters ----
            modelCheck <- catchConditions(glmmTMB::glmmTMB(formula = formula, data = data, 
                                                           family = distribution, start = startList))
            
            if(is.null(modelCheck$error)){
              ## Extract new estimated parameters ----
              newestParams = as.numeric(modelCheck$value$fit$par)
              
              ## Extract summaries of models ----
              latestSum = summary(model$value)$coefficients$cond
              newestSum = summary(modelCheck$value)$coefficients$cond
              
              ## Extract estimate of those parameters ----
              latestEst = latestSum[grep("year", rownames(latestSum)), "Estimate"]
              newestEst = newestSum[grep("year", rownames(newestSum)), "Estimate"]
              
              ## Extract standard error of those parameters ----
              latestSE = latestSum[grep("year", rownames(latestSum)), "Std. Error"]
              newestSE = newestSum[grep("year", rownames(newestSum)), "Std. Error"]
              
              if(all(!is.na(c(newestEst, latestEst, newestSE, latestSE)))){
                ## Check that estimated parameters are close enough to old parameters ----
                cond = all(abs(newestEst - latestEst) < 0.001)
                
                ## Check that old / new SE are close enough ----
                cond = cond & all(abs(newestSE - latestSE) < 0.001)
                
                ## If all parameters are close enough, erase the warning ----
                if(cond){
                  model$value$fit$convergence = 0
                  model$warnings = "false conv --> all checked"
                  cat("Convergence OK after re-running from latest parameters\n")
                }else{
                  cat("No convergence despite re-running from latest parameters\n")
                }
              }else{
                cat("No Convergence Obtained due to NA in summary\n")
              }
            }else{
              cat("No Convergence Obtained due to Error\n")
            }
          }
        }
        
        if(modelConv == -1){
          cat("No Convergence Obtained due to Error\n")
        }
        
        
      }else{
        cat("No Convergence Obtained due to Error\n")
      }
      
    }
    
  }
  ######################
  # Save model results #
  ######################
  
  return(model)
}

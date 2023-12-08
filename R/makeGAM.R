#' makeGAM
#'
#' A function that makes a GAM with closest specifications to the GLM ones
#'
#' @param data a `dataframe` containing the variables used in the GLM
#' @param interestVar a `string` corresponding to the response variable
#' @param fixedEffects a `vector` of variables that should be treated as continuous fixed effects
#' @param factorVariables a `vector` of variables that should be treated as categorical fixed effects
#' @param randomEffects a `vector` of variables that should be treated as random effects
#' @param nestedEffects a `list` of 2-elements vector, containing nested effects 
#' @param poly a `vector` containing variables that should be treated as 2-degree polynom
#' @param distribution a `string` containing the chosen distribution between: "gaussian", "poisson", "binomial", "betabinomial", "nbinom2" (by default : "gaussian")
#'
#' @importFrom stats as.formula
#' 
#' @return a 3-elements `list` :
#' - value a `glmmTMB` object containing results of the regression
#' - warnings a `vector` of potential warnings encountered during the regression
#' - error a `string` containing a potential error encountered during the regression
#' @example
makeGAM <- function(data, interestVar, fixedEffects = NULL,
                    factorVariables = NULL, randomEffects = NULL, 
                    nestedEffects = list(), poly = NULL, 
                    distribution = "gaussian"){
  
  ####################
  # Error management #
  ####################
  
  # Check class of data ----
  if (class(data) != "data.frame"){
    stop("'data' should be a data.frame")
  }
  
  # Check that all variables exist ----
  vars <- unique(c(interestVar, fixedEffects, poly, unlist(randomEffects), unlist(nestedEffects)))
  
  if (any(!(vars %in% colnames(data)))){
    missingVars <- vars[which(!(vars %in% colnames(data)))]
    stop("the variables '", paste(missingVars, collapse = ", "), "' are not found in dataframe.")
  }
  
  # Check distribution exists ----
  if (!(distribution %in% c("binomial", "betabinomial", "gaussian", "poisson", "nbinom2"))){
    stop("the chosen distribution doesn't exist. \nPlease choose between : binomial, betabinomial, gaussian, poisson or nbinom2.")
  }
  
  ###################
  #   FORMAT DATA   #
  ###################
  
  # Check factorVariables representation ----
  
  if(!is.null(factorVariables)){
    
    for(var in factorVariables){
      # Number of occurrence of each level ----
      tableVar = table(data[,var])
      
      # Levels that have low occurrence ----
      levelsToErase = names(tableVar)[which(tableVar < 10)]
      
      if(length(levelsToErase) > 0){
        # Filter data for those categories ----
        data = data[-which(data[,var] %in% levelsToErase),]
        
        # Display warning ----
        nToErase = length(which(data[,var] %in% levelsToErase))
        message(nToErase," rows were removed from the analysis due to low representation of levels: '", 
                paste(levelsToErase, collapse = ", "), "', from variable '", var, "'")
      }
      
      
    }
    
    # Put factor variables in the right format
    data[, factorVariables] = factor(data[, factorVariables])
  }
  
  
  # Check that continuous variable has more than 1 value ----
  
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
  
  ####################
  #   DISTRIBUTION   #
  ####################
  
  if(distribution == "betabinomial"){
    distribution = "quasibinomial"
    message("'Betabinomial' distribution has been set to 'quasibinomial' for the gamm trends")
  }else if(distribution == 'nbinom2'){
    distribution = "nb"
  }
  
  
  ###############
  #   FORMULA   #
  ###############
  
  # Determine k ----
  nbYear = length(unique(data$year))
  k = 0.3 * nbYear
  k = ifelse(k == round(k), k, round(k) + 1)
  
  ## Initialize formula ----
  if(length(interestVar) == 1){
    regrFormula <- paste0(interestVar," ~ s(year, bs = 'cr', k = ", k, ")")
    
  }else{
    regrFormula <- paste0("cbind(",paste(interestVar, collapse = ","),")", 
                          " ~ s(year, bs = 'cr', k = ", k, ")")
  }
  
  ## Add explaining variables
  effects <- c(fixedEffects, factorVariables)
  effects <- effects[effects != "year"]
  
  if(length(effects) > 0){
    regrFormula = paste(regrFormula, paste(effects, collapse = " + "), sep = " + ")
  }
  
  ## Deal with polynomial effects
  if (!is.null(poly)){
    for (p in poly){
      regrFormula = paste0(regrFormula, " + poly(", p, ", 2, raw = FALSE)")
    }
    
  }
  
  ## Turn to object formula
  regrFormula = as.formula(regrFormula)
  
  ## Treat random and nested effects 
  random = unique(unlist(c(randomEffects, nestedEffects)))
  
  listRandom = list()
  if(!is.null(random)){
    for (r in random){
      listRandom[[r]] = ~1    
    }
    
  }
  
  #############
  #   MODEL   #
  #############
  
  # Run Model ----
  model = catchConditions(mgcv::gamm(formula = regrFormula,
                                     random = listRandom,
                                     method = "RMLE",
                                     data = data, 
                                     family = distribution,
                                     niterPQL = 70))
  
  
  ######################
  # Save model results #
  ######################
  
  return(model)  
}
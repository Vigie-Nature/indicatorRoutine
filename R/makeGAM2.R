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
makeGAM2 <- function(data, interestVar, fixedEffects = NULL,
                    factorVariables = NULL, randomEffects = NULL, 
                    nestedEffects = list(), poly = NULL, 
                    distribution = "gaussian", sp = "", repo = ""){
  
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
      
      if(length(unique(data[!is.na(data[,var]),var])) == 1){
        factorVariables = factorVariables[factorVariables != var]
        cat("Variable '",var,"' was removed from the analysis due to the presence of 1-level only.\n")
      }
      
      # Put factor variables in the right format
      data[, var] = factor(data[, var])
    }
    
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
  
  # Check that factor variables correspond to factor columns
  # And choose the right contrasts for each categorical variable
  data <- setContrasts(data = data, factorVariables = factorVariables, contr = contr)
  
  # Scale continuous variables
  for (contVar in c(fixedEffects, poly)){
    data[,contVar] = scale(data[,contVar])
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
  


  #############################################
  #   RANDOM EFFECTS & DISTRIBUTION & MODEL   #
  #############################################

  ############
  #   MGCV   #
  ############

  # Utiliser mgcv si aucun effet emboité et aucun effet aléatoire
  if(is.null(unlist(nestedEffects)) && is.null(randomEffects)){
    message("No random nor nested effect, using mgcv::gam to fit the gam")
    listRandom = list()

      ####################
      #   DISTRIBUTION   #
      ####################
      
      if(distribution == "betabinomial"){
        distribution = "quasibinomial"
        message("'Betabinomial' distribution has been set to 'quasibinomial' for the gamm trends")
      }else if(distribution == 'nbinom2'){
        distribution = "nb"
      }

    # Run Model ----
    model = catchConditions(mgcv::gamm(formula = regrFormula,
                                     random = listRandom,
                                     method = "REML",
                                     data = data, 
                                     family = distribution,
                                     niterPQL = 100))

  } else 
  #############
  #   GAMM4   #
  #############
  {
    # Sinon utilisation de gamm4
    message("At least one random effect detected, using gamm4::gamm4 to fit the gam")

    #######################
    #   RANDOMS EFFECTS   #
    #######################
    randomFormula = c()
    if(!is.null(randomEffects)){
      for(r in randomEffects){
        randomFormula <- c(randomFormula, paste0("(1 | ", r, ")"))
      }
    }
    
    if(!is.null(nestedEffects)){
      for(n in nestedEffects){
        randomFormula <- c(randomFormula, paste0("(1 | ", n[1], "/", n[2], ")"))
      }
    }
    
    if(!is.null(randomFormula)){
      randomFormula <- as.formula(paste("~", paste(randomFormula, collapse = " + ")))
    }

    ####################
    #   DISTRIBUTION   #
    ####################
    if(distribution == 'nbinom2'){
      # Using theta predicted in longTermTrends for the gamm
      print(paste("Chemin est", here::here("outputs", repo, "models", "longTermTrend", paste0(sp, ".rdata"))))
      load(here::here("outputs", repo, "models", "longTermTrend", paste0(sp, ".rdata")))
      theta_est <- sigma(longTermTrend$value)

      #Estimation du theta pour la negative binomiale
      #Force la conversion en data frame propre pour éviter certains problèmes
      # mod_nb <- MASS::glm.nb(interestVar ~ 1, data = data.frame(interestVar = data[[interestVar]]))

      # theta_est <- mod_nb$theta

      # distribution <- lme4::negative.binomial(theta = theta_est)
      distribution <- MASS::negative.binomial(theta = theta_est)
      
      cat("Calcul de theta terminé, theta vaut", theta_est, "\n")
    } else if(distribution == "betabinomial"){
      message("'Beta-binomial' distribution is not supported by gamm4; 
        therefore, we're unable to fit the required GAMM.")
      return(NULL)

    }

    #############
    #   MODEL   #
    #############
    
    # Run Model ----
    cat("Le model est : ", as.character(regrFormula), "\n")
    cat("Les effets aléatoires sont : ", as.character(randomFormula), "\n")
    model = catchConditions(gamm4::gamm4(formula = regrFormula, 
                                  random= randomFormula, 
                                  family = distribution,
                                  data = data, 
                                  REML = TRUE))
    
    # # Appel isolé à gamm4 dans un sous-processus via callr
    # model <- callr::r(
    #   function(regrFormula, randomFormula, distribution, data) {
        
    #     Sys.setenv(OMP_NUM_THREADS = "1")
    #     Sys.setenv(MKL_NUM_THREADS = "1")
    #     Sys.setenv(OPENBLAS_NUM_THREADS = "1")
        
    #     library(gamm4)
    #     source(file.path("R", "catchConditions.R"), local = TRUE)
        
    #     catchConditions(gamm4::gamm4(
    #       formula = regrFormula,
    #       random = randomFormula,
    #       family = distribution,
    #       data = data,
    #       REML = TRUE
    #     ))
    #   },
    #   args = list(
    #     regrFormula = regrFormula,
    #     randomFormula = randomFormula,
    #     distribution = distribution,
    #     data = data
    #   ),
    #   stderr = here::here("outputs", "stderr.log"),
    #   stdout = here::here("outputs", "stdout.log"),
    #   show = FALSE,
    #   spinner = FALSE
    # )
    # gc()


  }
  
  ######################
  # Save model results #
  ######################
  
  #print(system.time({save(gammVariations, file =  here::here("outputs", repo, "models", "gammVariations", paste0(sp, ".rdata")))}))
  return(model)  
}
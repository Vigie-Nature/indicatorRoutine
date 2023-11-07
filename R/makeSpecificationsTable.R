#' makeSpecificationsTable
#' 
#' A function that specifies for each species the final dataframe, distribution and variables used in the GLM
#' 
#' @param data a `data.frame` containing the observations
#' @param speciesList a `vector` containing the list of species to study
#' @param interestVar a 1 or 2-elements `vector` specifying the response variable
#' @param fixedEffect a `vector` containing variables that should be treated as continuous fixed effects
#' @param factorVariables a `vector` containing variables that should be treated as categorical fixed effects
#' @param randomEffects a `vector` containing variables that should be treated as random effects
#' @param nestedEffects a `list` of 2-elements vector containing variables that should be treated as nested random effects
#' @param slopeRandomEffects a `list` of 2-elements vector containing variables that should be treated as random slope effects
#' @param poly a `vector` containing variables that should be treated as 2nd order polynomial effects
#' @param repo a `string` containing the name of the output repository
#' @param modelName a `string` specifying the model to study (e.g "longTermTrend", "yearlyVariations", "shortTermTrend")
#'
makeSpecificationsTable <- function(data, speciesList, interestVar, fixedEffects,
                                    factorVariables, randomEffects, nestedEffects,
                                    slopeRandomEffects, poly, repo, modelName){
  
  ##########################
  #   REFORMAT VARIABLES   #
  ##########################
  
  # Write the original formula
  modelFormula = writeFormula(interestVar = interestVar,
                               fixedEffects = fixedEffects,
                               factorVariables = factorVariables,
                               randomEffects = randomEffects,
                               nestedEffects = nestedEffects,
                               slopeRandomEffects = slopeRandomEffects,
                               poly = poly, 
                               raw = "ortho")
  
  modelFormula = as.character(modelFormula)[[3]]
  
  # Extract variables from formula
  listVars = strsplit(modelFormula, split = " + ", fixed = T)  
  listVars = listVars[[1]]
  
  # Erase intercept
  listVars = listVars[-grep("^1", listVars)]
  
  # Erase "raw" information of polynomials
  listVars = gsub(", raw = FALSE", "", listVars)
  listVars = gsub(", raw = FALSE", "", listVars)
  
  # Regroup random slope effects 
  ind = match("(0", listVars)
  if(!is.na(ind)){
    listVars[ind+1] = paste0(listVars[ind], " + ", listVars[ind+1])
    listVars = listVars[-ind] 
  }
  
  
  ##################################
  #   IDENTIFY MISSING VARIABLES   #
  ##################################

  listOfData = lapply(speciesList, function(sp){
    # Load models
    load(here::here("outputs", repo, "models", modelName, paste0(sp, ".rdata")))
    
    # Filter data for considered species
    dataSp = data[data$species == sp, ]
    
    # Change name of the model
    model = get(modelName)
    
    if(is.null(model$error)){
      ############################
      # EXTRACT ERASED VARIABLES #
      ############################
      # Extract model info
      modInfo = model$value$modelInfo
      
      # Extract and format fixed effects
      fixEff_obj = modInfo$terms$cond$fixed
      fixEff_list = attributes(fixEff_obj)$variables
      fixEff_sp = as.character(fixEff_list)
      fixEff_sp = fixEff_sp[!(fixEff_sp %in% c(interestVar, "list"))]
      fixEff_sp = gsub(", raw = FALSE", "",fixEff_sp)
      fixEff_sp = gsub(", raw = TRUE", "",fixEff_sp)
      
      # Extract and format random effects
      randEff_sp = names(modInfo$reStruc$condReStruc)
      randEff_sp = paste0("(", randEff_sp, ")")
      
      # Put all variables in a same vector
      vars_sp = c(fixEff_sp, randEff_sp)
      
      # Identify erased variables
      varsToRemove = listVars[is.na(match(listVars, vars_sp))]
      varsToRemove = ifelse(length(varsToRemove) > 0, 
                            paste(varsToRemove, collapse = " ; "),
                            "")
      ###################
      # FILL DATA.FRAME #
      ###################
      
      data = data.frame(species = sp,
                        model = modelName,
                        nbRowsInit = nrow(dataSp),
                        nbRows = nrow(model$value$frame),
                        distribution = modInfo$family$family,
                        convergence = ifelse(model$value$fit$convergence == 1, "No : convergence issue", "Yes"),
                        removedVars = varsToRemove)
      
      
    }else{
      data = data.frame(species = sp,
                        model = modelName,
                        nbRowsInit = nrow(dataSp),
                        nbRows = NA,
                        distribution = NA,
                        convergence = "No : error erased",
                        removedVars = NA)
    }
    
    return(data)
    
  })
  
  dataSpec = do.call('rbind', listOfData)
  
  return(dataSpec)
  
}




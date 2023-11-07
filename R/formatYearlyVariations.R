#' formatYearlyVariations
#' 
#' Load all models and format estimates of yearly variations
#' 
#' @param speciesList : a `vector` containing names of species to be analyzed
#' @param repo : a `string` specifying the name of the output repository
#' @param modelName : a `string` specifying the model to be analyzed (btw : 'longTermTrend', 'yearlyVariations', 'rawQuadraticTrend' and 'orthoQuadraticTrend')
#' @param contr : a `string` specifying the reference level for the variable 'year'
#' 
formatYearlyVariations <- function(speciesList, repo, modelName, contr){
  
  dataRes = lapply(speciesList, function(sp){
    # Path to the model
    pathToModel = here::here("outputs", repo, "models", modelName, paste0(sp, ".rdata"))
    
    # If the model exists
    if(file.exists(pathToModel)){
      
      # Load model
      load(pathToModel)
      # Change the name of the object
      model = get(modelName)
      
      # Identify if there were any convergence issue
      convIssue = identifyConvIssue(model)
      
      if(!convIssue){
        # Extract basic information on the model
        model = model$value
        modelSummary = summary(model)
        modelCoefs = modelSummary$coefficients$cond
        
        # Extract information related to year
        ## The minimum value
        minYear = min(as.integer(as.character(model$frame$year))) 
        
        ## The maximum value
        maxYear = max(as.integer(as.character(model$frame$year))) 
        
        # Extract estimates and standard errors
        indCoef = grep("year", rownames(modelCoefs))
        
        estYear = modelCoefs[indCoef, "Estimate"] 
        seYear = modelCoefs[indCoef, "Std. Error"]
        
        # Introduce the reference level
        seYear = c(0, seYear)
        
        # If reference is a level of the variable
        if(contr %in% minYear:maxYear){
          # Then reference value is 0
          estYear = c(0, estYear)
          
          if(contr == minYear){
            yearLevels = minYear:maxYear
          }else if(contr == maxYear){
            yearLevels = c(contr, minYear:(maxYear - 1))
          }else{
            yearLevels = c(contr, minYear:(contr-1), (contr+1):maxYear)
          }
          
          ## If centered on the mean
        }else{
          # Order is from minimum to maximal value
          yearLevels = minYear:maxYear
          
          # Reference value is the opposite of the sum of the coefficients
          estYear = c(- 1 * sum(estYear), estYear)
        }
        
        # Calculate confidence intervals  
        infICYear = estYear - 1.96 * seYear
        supICYear = estYear + 1.96 * seYear
        
        # Extract pvalue
        pvalYear = c(NA, modelCoefs[indCoef, "Pr(>|z|)"])
        
        # Make dataframe if species has estimates for each year
        if(length(yearLevels) == length(estYear)){
          dataRes_sp = data.frame(species = sp,
                                model = modelName,
                                year = yearLevels,
                                estimate = estYear,
                                se = seYear,
                                infIC = infICYear,
                                supIC = supICYear,
                                pval = pvalYear)
          }else{
            dataRes_sp = data.frame(species = sp,
                                    model = modelName,
                                    year = NA,
                                    estimate = NA,
                                    se = NA,
                                    infIC = NA,
                                    supIC = NA,
                                    pval = NA)
        }
        
      }else{
        dataRes_sp = data.frame(species = sp, 
                                model = modelName,
                                year = NA,
                                estimate = NA,
                                se = NA,
                                infIC = NA,
                                supIC = NA,
                                pval = NA)
        
      }
      return(dataRes_sp)
    }
  })
  
  # Rbind all species
  dataRes = do.call("rbind", dataRes)
  
  # Erase rownames
  rownames(dataRes) = NULL
  
  return(dataRes)
}

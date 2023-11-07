#' formatTrendEstimates
#' 
#' Load all models and format estimates of linear / quadratic trend
#' 
#' @param data : a `data.frame` containing observations
#' @param speciesList : a `vector` containing names of species to be analyzed
#' @param repo : a `string` specifying the name of the output repository
#' @param modelName : a `string` specifying the model to be analyzed (btw : 'longTermTrend, 'rawQuadraticTrend' and 'orthoQuadraticTrend')
#' 
formatTrendEstimates <- function(data, speciesList, repo, modelName){
  
  dataRes = lapply(speciesList, function(sp){
    
    # Filter observations for species sp
    dataSp = data[data$species == sp, ]
    
    # Path to the model
    pathToModel = here::here("outputs", repo, "models", modelName, paste0(sp, ".rdata"))
    
    # If the model exists
    if(file.exists(pathToModel)){
      
      # Load model
      load(pathToModel)
      
      # Rename model
      model = get(modelName)
      
      # If convergence is OK
      convIssue = identifyConvIssue(model)
      
      if(!convIssue){
        # Extract coefficients from summary
        model = model$value
        modelSummary = summary(model)
        modelCoefs = modelSummary$coefficients$cond
        
        # Extract information related to year
        ## The mean value
        meanYear = attributes(model$frame$year)$`scaled:center`
        
        ## The mean value
        meanYear = mean(dataSp$year)
        
        ## The sd value
        sdYear = sd(dataSp$year)
        
        ## The minimum value (on the original scale)
        minYear = min(dataSp$year)
        
        ## The maximum value (on the original scale)
        maxYear = max(dataSp$year)
        
        ## Extract indices associated with year estimations
        indCoef = grep("year", rownames(modelCoefs))
        
        ## Extract estimates and standard errors, for linear trends
        if(modelName %in% c("longTermTrend", "shortTermTrend")){
          estYear = modelCoefs[indCoef, "Estimate"] / sdYear
          seYear = modelCoefs[indCoef, "Std. Error"] / sdYear
          
          # Extract estimates and standard errors, for quadratic trends
        }else{
          ## Estimate
          estYear = modelCoefs[indCoef, "Estimate"] 
          # estYear[1] = (estYear[1] / sdYear) - (2 * meanYear * estYear[2] / sdYear^2)
          # estYear[2] = estYear[2] / sdYear^2
          
          ## Standard Error
          seYear = modelCoefs[indCoef, "Std. Error"] 
          # seYear[1] = (seYear[1] / sdYear) - (2 * meanYear * seYear[2] / sdYear^2)
          # seYear[2] = seYear[2] / sdYear^2
        }
        
        # Calculate confidence intervals  
        infICYear = estYear - 1.96 * seYear
        supICYear = estYear + 1.96 * seYear
        
        # Extract pvalue
        pvalYear = modelCoefs[indCoef, "Pr(>|z|)"]
        
        # Make dataframe
        dataRes_sp = data.frame(species = sp,
                                model = modelName,
                                coef = c(1:length(indCoef)),
                                minYear = minYear,
                                maxYear = maxYear,
                                meanYear = meanYear,
                                sdYear = sdYear,
                                estimate = estYear,
                                se = seYear,
                                infIC = infICYear,
                                supIC = supICYear,
                                pval = pvalYear)
        
      }else{
        dataRes_sp = data.frame(species = sp, 
                                model = modelName,
                                coef = NA,
                                minYear = NA,
                                maxYear = NA,
                                meanYear = NA,
                                sdYear = NA,
                                estimate = NA,
                                se = NA,
                                infIC = NA,
                                supIC = NA,
                                pval = NA)
        
      }
      return(dataRes_sp)
    }
  })
  
  # Bind all data.frames
  dataRes = do.call("rbind", dataRes)
  
  return(dataRes)
}

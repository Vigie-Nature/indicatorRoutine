#' makeSlopesFromVar
#' 
#' Draw for each year N random points based on yearly variations estimates and pass linear regression into those points to create curve of uncertainty
#' 
#' @param data : a `data.frame` containing the formatted estimates of yearly variations
#' @param N : a `numeric` value specifying the number of simulations to make
#' @param limits : a `vector` containing the range values of the year
#' @param weight : a `boolean` specifying if each year value should be weighted according to its standard error
#' @param group : a `string` specifying whether the simulation is for long-term trends ('LT') or short-term trends ('ST')
#' 
#' 
makeSlopesFromVar <- function(data, N = 100, limits, weight = TRUE, group = "LT"){
  
  # Erase coefficient that has not been estimated
  data = data[!is.na(data$pval),]
  
  # Keep only the required values of effect var 
  data = data[data$year >= min(limits) & data$year <= max(limits),]
  
  # Simulate N values of yearly variations
  multipleEstimatesValues = sapply(1:nrow(data), function(x){ 
    rnorm(n = N, mean = data$estimate[x], sd = data$se[x]) 
  })
  
  # Weight : 1/standard error
  weights = 1 / data$se^2
  
  ## Fit regressions into those coefficients
  models = apply(multipleEstimatesValues, 1, function(x){
    # Extract limit values for model estimation
    minLim = min(data$year)
    maxLim = max(data$year)
    
    # Make data.frame
    regrData = data.frame(abundance = unlist(x), year = minLim:maxLim, weights = weights) 
    
    # Make regression
    if(weight){
      lm = lm(abundance ~ year, data = regrData, weights = weights)
    }else{
      lm = lm(abundance ~ year, data = regrData)
      
    }
    # Return regression
    return(lm)
  })
  
  ## Extract coefficients and turn to slope values
  slopes = lapply(models, function(x){
    # Extract coefficients
    coefs = summary(x)$coefficients[,"Estimate"]
    
    # Extract the range we want
    year = min(limits):max(limits)
    
    # Extract slope values
    slopeValues = exp(coefs[1] + coefs[2]*year)
    
    return(slopeValues)
  })
  
  # Make a result data.Frame
  dataSlopes = data.frame(year = rep(limits, times = length(slopes)),
                          estimate = unlist(slopes),
                          infIC = NA, 
                          supIC = NA,
                          rep = rep(1:N, each = length(limits)),
                          group = group)
  
  return(dataSlopes)
}
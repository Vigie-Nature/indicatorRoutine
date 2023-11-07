#' significanceToText
#' 
#' Turn pvalue to a text of ns or 1 to 3 stars
#'
#' @param pval: a `numeric` value containing the pvalue
#' 
significanceToText <- function(pval){
  if (pval < 0.001){ 
    significance <- "***"
  }else if (pval < 0.01){ 
    significance <- "**"
  }else if(pval < 0.05){ 
    significance <- "*"
  }else{
    significance <- "ns"
  }
  
  return(significance)
}

#' percEvolutionToText
#' 
#' Turn an estimate with confidence interval to a text describing the percentage of evolution 
#'
#' @param data: a `data.frame` value containing minimum and maximum year values, estimates and confidence intervals
#' @param distribution: a `string` specifying the distribution law of the model
#' 
percEvolutionToText <- function(data, distribution){
  
  # Extract percentage of change (and confidence interval)
  dataChange = apply(data[,c("estimate", "infIC", "supIC")], 2, function(x){
    
    # Calculate value for first year
    start = x * 0
    
    # Calculate Value for relative last year
    stop = x * (data$maxYear - data$minYear)
    
    # Turn to exponential if counting or occurrence data
    if(distribution != "gaussian"){
      start = exp(start)
      stop = exp(stop)
    }
    
    # Measure the percentage of difference
    perc = round(100 * (stop- start), 1)
    
    # Return the percentage of difference
    return(perc)
  })
  
  # Turn to text
  changeText = paste0(ifelse(dataChange["estimate"] > 0, "+", ""),
                      dataChange["estimate"], "% (",
                      
                      ifelse(dataChange["infIC"] > 0, "+", ""),
                      dataChange["infIC"], "% ; ",
                      
                      ifelse(dataChange["supIC"] > 0, "+", ""),
                      dataChange["supIC"], "%)")
  
  # Add a 
  changeText = paste0("Tendance linéaire ", data$min, " - ", data$max, " : ", changeText)
  
  return(changeText)
}


#' quadraticDerivative
#' 
#' Calculate 1st derivative of a 2nd order polynom
#' 
#' @param data : a `data.frame` containing the formatted estimates from raw quadratic trends
#' @param sp : a `string` specifying the species to consider
#' @param perc : a `numeric` value specifying the quantile where we want to measure the derivative 
#' 
quadraticDerivative <- function(data, sp, perc = 0.5){
  # Filter data for studied species
  dataSp = data[data$species == sp,]
  
  # Extract coefficients
  beta1 = dataSp$estimate[dataSp$coef == 1]
  beta2 = dataSp$estimate[dataSp$coef == 2]
  
  # Extract mean and se of year
  meanYear = unique(dataSp$meanYear)
  sdYear = unique(dataSp$sdYear)
  
  # Extract the quantile at 100 * perc %
  x = as.numeric(quantile(unique(dataSp$minYear):unique(dataSp$maxYear), perc))
  
  # # De-scale the quantile
  x_sc = (x - meanYear) / sdYear
  
  # Measure the derivative 
  derVal = beta1 + 2 * beta2 * x_sc
  
  return(derVal)
}

#' quadraticCurve
#' 
#' Calculate the sign of the 1st derivative of the curve function of a 2nd order polynom
#' 
#' @param data : a `data.frame` containing the formatted estimates from raw quadratic trends
#' @param sp : a `string` specifying the species to consider
#' 
quadraticCurve <- function(data, sp){
  
  # Filter data for studied species
  dataSp = data[data$species == sp,]
  
  # Extract coefficients
  beta1 = dataSp$estimate[dataSp$coef == 1]
  beta2 = dataSp$estimate[dataSp$coef == 2]
  
  # Extract mean and se of year
  meanYear = unique(dataSp$meanYear)
  sdYear = unique(dataSp$sdYear)
  
  # Extract the quantile at 100 * perc %
  x = as.numeric(median(unique(dataSp$minYear):unique(dataSp$maxYear)))
  
  # De-scale the quantile
  x_sc = (x - meanYear) / sdYear
  
  # Measure the first derivative of the curvature
  num = -12 * beta2^2 * (2 * beta2 * x_sc + beta1)
  denom = (1 + (2 * beta2 * x_sc + beta1)^2)^(5/2)
  curveDer = num / denom
  
  # Extract the sign of the curve first derivative
  signCurveDer = curveDer / abs(curveDer)
  
  return(signCurveDer)
}




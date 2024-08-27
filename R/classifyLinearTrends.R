
#' classifyLinearTrends
#' 
#' A function that takes linear trends and turns growth rates into a 7-classes classification
#' Strong increase/decline, moderate to strong increase/decline, moderate increase/decline, 
#' stable, uncertain
#' 
#' @param data : a `data.frame` containing formatted long-term or short-term trends
#' @param distribution : a `string` specifying the distribution under which models were fitted (e.g, gaussian, binomial, poisson, ...)
#' @param thresholdInf : a `numeric` value specifying the limiting growth rates (i.e, 1 - 0.036, for a decline of 30% in 10 years)
#' @param thresholdSup : a `numeric` value specifying the limiting growth rates (i.e, 1 + 0.026, for an increase of 30% in 10 years)

#' 
classifyLinearTrends = function(data, distribution, thresholdInf = 0.036, thresholdSup = 0.026){
  data$trend = NA
  
  # Erase NA values
  dataNA = data[is.na(data$estimate),]
  data = data[!is.na(data$estimate),]
  
  # Turn to exponential estimates and confidence intervals
  if(distribution != "gaussian"){
    data[,c("estimate", "infIC", "supIC")] = exp(data[,c("estimate", "infIC", "supIC")])
    limit = 1
  }else{
    limit = 0
  }
  
  # Decrease
  data$trend[data$pval < 0.05 & data$estimate < limit] = "Déclin modéré"
  data$trend[data$pval < 0.05 & data$infIC < (limit - thresholdInf)] = "Déclin modéré à fort"
  data$trend[data$pval < 0.05 & data$supIC < (limit - thresholdInf)] = "Déclin fort"
  
  # Increase
  data$trend[data$pval < 0.05 & data$estimate > limit] = "Augmentation modérée"
  data$trend[data$pval < 0.05 & data$supIC > (limit + thresholdSup)] = "Augmentation modérée à forte"
  data$trend[data$pval < 0.05 & data$infIC > (limit + thresholdSup)] = "Augmentation forte"
  
  # Stability
  data$trend[data$pval > 0.05 & (data$infIC < (limit - thresholdInf)|data$supIC > (limit + thresholdSup))] = "Incertain"
  data$trend[data$pval > 0.05 & data$infIC >= (limit - thresholdInf) & data$supIC <= (limit + thresholdSup)] = "Stable"
  
  # Turn back coefficients
  if(distribution != "gaussian"){
    data[,c("estimate", "infIC", "supIC")] = log(data[,c("estimate", "infIC", "supIC")])
  }
  
  # Bind all dataframes
  data = rbind(data, dataNA)
  
  return(data)
}


#' classifyLinearTrends
#' 
#' A function that takes linear trends and turns growth rates into a 7-classes classification
#' Strong increase/decline, moderate to strong increase/decline, moderate increase/decline, 
#' stable, uncertain
#' 
#' @param data : a `data.frame` containing formatted long-term or short-term trends
#' @param distribution : a `string` specifying the distribution under which models were fitted (e.g, gaussian, binomial, poisson, ...)
#' @param threshold : a `numeric` value specifying the limiting growth rates (i.e, 1 +/- 0.036)
#' 
classifyLinearTrends = function(data, distribution, threshold = 0.036){
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
  data$trend[data$pval < 0.05 & data$infIC < (limit - threshold)] = "Déclin modéré à fort"
  data$trend[data$pval < 0.05 & data$supIC < (limit - threshold)] = "Déclin fort"
  
  # Increase
  data$trend[data$pval < 0.05 & data$estimate > limit] = "Augmentation modérée"
  data$trend[data$pval < 0.05 & data$supIC > (limit + threshold)] = "Augmentation modérée à forte"
  data$trend[data$pval < 0.05 & data$infIC > (limit + threshold)] = "Augmentation forte"
  
  # Stability
  data$trend[data$pval > 0.05 & (data$infIC < (limit - threshold)|data$supIC > (limit + threshold))] = "Incertain"
  data$trend[data$pval > 0.05 & data$infIC >= (limit - threshold) & data$supIC <= (limit + threshold)] = "Stable"
  
  # Turn back coefficients
  if(distribution != "gaussian"){
    data[,c("estimate", "infIC", "supIC")] = log(data[,c("estimate", "infIC", "supIC")])
  }
  
  # Bind all dataframes
  data = rbind(data, dataNA)
  
  return(data)
}

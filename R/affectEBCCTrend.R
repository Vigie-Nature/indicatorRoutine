affectEBCCTrend <- function(data, sp){
  
  # Filter for species sp
  dataSp = data[data$species == sp,]
  
  # If no trend
  if(is.na(dataSp$estimate)){
    classEBCC = NA
  }
  # If significant trend
  else if(dataSp$pval > 0.05){
    # And thin confidence interval
    # Then 'stable'
    if(dataSp$infGR > 0.95 & dataSp$supGR < 1.05){
      classEBCC = "Stable"
      
      # And large confidence interval
      # Then 'uncertain'
    }else{
      classEBCC = "Incertain"
    }
    
    # Trend significant
  }else{
    # Positive trend
    if(dataSp$GR > 1){
      
      # Significant increase of more than 5% per year
      if(dataSp$infGR > 1.05){
        classEBCC = "Forte augmentation"
      }else{
        classEBCC = "Augmentation modérée"
      }
      # Negative trend
    }else{
      
      # Significant decline of more than 5% per year
      if(dataSp$supGR < 0.95){
        classEBCC = "Fort déclin"
        
        # No-significant decline of more than 5% per year
      }else{
        classEBCC = "Déclin modéré"
      }
    }
  }
  
  return(classEBCC)
}

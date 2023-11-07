#' agregateTrendsPerGroup
#'
#' A function that agregates long-term trends according to group composition
#' It is based on 
#' - Monte-Carlo method i.e, random simulation from estimated parameters
#' - Geographic Mean
#' 
#' @param data a `data.frame` containing formatted long-term trends
#' @param groupNames a `vector` containing names for each group
#' @param groupComp a `list` containing vectors of species names contained in each group
#' 
#' 
agregateTrendsPerGroup <- function(data, groupNames, groupComp){
  
  # Extract the number of groups
  groupNb = length(groupComp)
  
  # Add an "all-species" group, if multiple groups
  if(groupNb > 1){
    groupNb = groupNb +1
    groupNames[[groupNb]] = 'Toutes espèces'
    groupComp[[groupNb]] = unlist(groupComp)
  }
  
  # Add a column 'group' that states for each species the group they belong to
  listOfData = lapply(1:groupNb, function(i){
    
    # Extract name of considered group
    grName = groupNames[i]
    
    # Extract composition of considered group
    grComp = groupComp[[i]]
    
    # Filter for species inside considered group
    grData = data[data$species %in% grComp,]
    
    # Add a column "group"
    grData$group = grName
    
    return(grData)
  })
  
  ###########################
  # SIMULATIONS & INDICATOR #
  ###########################
  
  # For each group dataframe, simulate betas for each species
  simulations <- lapply(listOfData, function(df){
    sapply(1:nrow(df), function(x) rnorm(1000, df$estimate[x], df$se[x]))
  })
  
  # Calculate the arithmetic mean of the log-betas for all simulations
  index <- lapply(simulations, function(sim){
    apply(sim, 1, function(x) mean(x, na.rm = T))
  })
  
  ##########################
  # DESCRIPTIVE STATISTICS #
  ##########################
  
  # Calculate the mean of the aggregator
  meanIndex <- sapply(index, function(x) mean(x, na.rm = T))
  
  # Calculate the standard deviation of the agregator
  seIndex <- sapply(index, function(x) sd(x, na.rm = T))
  
  # Calculate the confidence intervals of the agregator
  infIndex <- exp(meanIndex - 1.96 * seIndex)
  supIndex <- exp(meanIndex + 1.96 * seIndex)
  
  # Turn mean and sd to exponential terms (geometric mean)
  meanIndex <- exp(meanIndex)
  
  # Calculate the percentage of evolution of the agregator
  ## Extract nb of year
  nbYear = max(data$maxYear, na.rm = TRUE) - min(data$minYear, na.rm = TRUE)
  
  ## Turn to percentage of evolution
  meanPerc <- 100*(meanIndex ^ nbYear - 1 )
  infPerc <- 100*(infIndex ^ nbYear - 1 )
  supPerc <- 100*(supIndex ^ nbYear - 1 )
  
  ####################
  # FORMAT DATAFRAME #
  ####################
  
  # Create data.frame and change to exponential values
  dataRes <- data.frame(group = groupNames,
                        estimate = meanIndex,
                        infIC = infIndex,
                        supIC = supIndex, 
                        perc = meanPerc,
                        infPerc = infPerc,
                        supPerc = supPerc)
  
  # Format percent columns
  dataRes[,c("perc", "infPerc", "supPerc")] <- round(dataRes[,c("perc", "infPerc", "supPerc")], 1)
  
  return(dataRes)
}

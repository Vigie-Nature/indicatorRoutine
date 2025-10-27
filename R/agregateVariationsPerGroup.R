#' agregateVariationsPerGroup
#'
#' A function that agregates yearly variations according to group composition
#' It is based on 
#' - Monte-Carlo method i.e, random simulation from estimated parameters
#' - Geographic Mean
#' 
#' @param data a `data.frame` containing formatted yearly variations
#' @param groupNames a `vector` containing names for each group
#' @param groupComp a `list` containing vectors of species names contained in each group
#' 
#' 
# agregateVariationsPerGroup <- function(data, groupNames, groupComp){
# 
#   # Extract the number of groups
#   groupNb = length(groupComp)
# 
#   # Add an "all-species" group, if multiple groups
#   if(groupNb > 1){
#     groupNb = groupNb +1
#     groupNames[[groupNb]] = 'Toutes espèces'
#     groupComp[[groupNb]] = unlist(groupComp)
#   }
# 
#   # Add a column 'group' that states for each species the group they belong to
#   listOfData = lapply(1:groupNb, function(i){
# 
#     # Extract name of considered group
#     grName = groupNames[i]
# 
#     # Extract composition of considered group
#     grComp = groupComp[[i]]
# 
#     # Filter for species inside considered group
#     grData = data[data$species %in% grComp,]
# 
#     # Add a column "group"
#     grData$group = grName
# 
#     return(grData)
#   })
# 
#   # Split each group data per year
#   listOfData = lapply(listOfData, function(df) split(df, df$year))
# 
#   #############################
#   # SIMULATIONS AND INDICATOR #
#   #############################
# 
#   # For each year x group dataframe, simulate betas for each species
#   simulations <- lapply(listOfData, function(D){
#     lapply(D, function(d){
#       sapply(1:nrow(d), function(x) {rnorm(1000, d$estimate[x], d$se[x])
#       })
#     })
#   })
# 
#   # Agregate those betas with a (weighed) geometric mean
#   index <- lapply(simulations, function(sim){
#     lapply(sim, function(s) {
#       apply(s, 1, function(x) mean(x, na.rm = T))
#     })
#   })
# 
#   ##########################
#   # DESCRIPTIVE STATISTICS #
#   ##########################
# 
#   # Calculate the mean of the agregator
#   meanIndex <- lapply(index, function(ind) {
#     sapply(ind, function(x) mean(x, na.rm = T))
#   })
# 
#   # Calculate the standard deviation of the agregator
#   seIndex <- lapply(index, function(ind) {
#     sapply(ind, function(x) sd(x, na.rm = T))
#   })
# 
# 
#   # Extract first value for each group
#   firstVal = sapply(meanIndex, function(x) x[1])
# 
#   # Calculate the lower bound of the confidence interval, and turn exponential + center on 1st value
#   infIndex <- lapply(1:length(groupNames), function(i){
#     inf = exp(meanIndex[[i]] - 1.96 * seIndex[[i]])/ exp(firstVal[i])
#     return(inf)
#   })
# 
#   # Calculate the upper bound of the confidence interval, and turn exponential + center on 1st value
#   supIndex <- lapply(1:length(groupNames), function(i){
#     sup = exp(meanIndex[[i]] + 1.96 * seIndex[[i]])/ exp(firstVal[i])
#     return(sup)
#   })
# 
#   # Turn mean index to exponential
#   meanIndex <- lapply(meanIndex, function(x) exp(x)/exp(x[1]))
# 
#   # Make resulting dataframe
#   dataRes <- data.frame(group = rep(groupNames, each = length(meanIndex[[1]])),
#                         year = rep(as.numeric(names(meanIndex[[1]])), times = groupNb),
#                         index = 100*unlist(meanIndex),
#                         infIndex = 100*unlist(infIndex),
#                         supIndex = 100*unlist(supIndex))
# 
# 
#   return(dataRes)
# 
# }


agregateVariationsPerGroup <- function(data, groupNames, groupComp, useLastYearAsReference = FALSE){
  
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
  
  # Split each group data per year
  listOfData = lapply(listOfData, function(df) split(df, df$year))
  #listOfData <- lapply(listOfData, function(x) x[-1])
  
  #############################
  # SIMULATIONS AND INDICATOR #
  #############################
  
  # For each year x group dataframe, simulate betas for each species
  simulations <- lapply(listOfData, function(D){
    lapply(D, function(d){
      sapply(1:nrow(d), function(x) {rnorm(1000, d$estimate[x], d$se[x])
      })
    })
  })
  
  # Agregate those betas with a (weighed) geometric mean
  index <- lapply(simulations, function(sim){
    lapply(sim, function(s) {
      apply(s, 1, function(x) mean(x, na.rm = T))
    })
  })
  
  ##########################
  # DESCRIPTIVE STATISTICS #
  ##########################
  
  # Calculate the mean of the agregator
  meanIndex <- lapply(index, function(ind) {
    sapply(ind, function(x) mean(x, na.rm = T))
  })
  
  # Calculate the standard deviation of the agregator
  seIndex <- lapply(index, function(ind) {
    sapply(ind, function(x) sd(x, na.rm = T))
  })
  
  # Determine the reference value based on the boolean parameter
  if (useLastYearAsReference) {
    refVal = sapply(meanIndex, function(x) x[length(x)])
  } else {
    refVal = sapply(meanIndex, function(x) x[1])
  }
  
  # Calculate the lower bound of the confidence interval, and turn exponential + center on reference value
  infIndex <- lapply(1:length(groupNames), function(i){
    inf = exp(meanIndex[[i]] - 1.96 * seIndex[[i]]) / exp(refVal[i])
    return(inf)
  })
  
  # Calculate the upper bound of the confidence interval, and turn exponential + center on reference value
  supIndex <- lapply(1:length(groupNames), function(i){
    sup = exp(meanIndex[[i]] + 1.96 * seIndex[[i]]) / exp(refVal[i])
    return(sup)
  })
  
  # Turn mean index to exponential and center on reference value
  meanIndex <- lapply(meanIndex, function(x) exp(x) / exp(x[which(names(x) == names(refVal)[1])]))
  #meanIndex <- lapply(meanIndex, function(x) exp(x)/exp(x[1]))
  
  
  # Make resulting dataframe
  dataRes <- data.frame(group = rep(groupNames, each = length(meanIndex[[1]])),
                        year = rep(as.numeric(names(meanIndex[[1]])), times = groupNb),
                        index = 100 * unlist(meanIndex),
                        infIndex = 100 * unlist(infIndex),
                        supIndex = 100 * unlist(supIndex))
  
  return(dataRes)
}

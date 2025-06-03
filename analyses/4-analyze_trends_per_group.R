# STEP 4 : CREATE TABLES / FIGURES FOR GROUPS
cat("# TABLES AND FIGURES FOR GROUPS OF SPECIES #\n")

# Make global directory 
dir.create(path = here::here("outputs", repo, "figures", "group"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "tables", "group"), showWarnings = FALSE)

# Load long term trends
pathToLT = here::here("outputs", repo, "tables", "trends", "longTermTrends.csv")
dataLongTermTrend = read.csv(pathToLT)

# Load short term trends
pathToYV = here::here("outputs", repo, "tables", "trends", "yearlyVariations.csv")
dataYearlyVariations = read.csv(pathToYV)

#############################################
#   DEAL WITH MIS-INFORMED COLORS / NAMES   #
#############################################

if(makeGroupPlot){
  if(is.null(groupComp)) {
    groupComp <- list(c(speciesList))
  }
  # Reservoirs of names and colors for groups
  allNames = c("A", "B", "C", "D", "E", "F", "G")
  allCols = c("#1FA3D4","#F3AA20", "#235D3A", "#58094F", "#FFBFC5")
  
  #   MIS-INFORMED GROUP NAMES   #
  
  # If group names are not informed, create them
  if(is.null(groupNames)){
    groupNames = paste("Groupe", allNames[1:length(groupComp)])
  }
  # If some group names are missing, add them
  else if(length(groupNames) != length(groupComp)){
    nbMissName = length(groupComp) - length(groupNames)
    groupNames = c(groupNames, paste("Groupe", allNames[1:length(nbMissName)]))
    
  }
  
  #   MIS-INFORMED GROUP COLORS   #
  
  # If group colours are not informed, create them
  if(is.null(groupCols)){
    groupCols = allCols[sample(1:length(allCols), size = length(groupComp))]
  }
  # If some group colors are missing, add them
  else if(length(groupCols) != length(groupComp)){
    nbMissCols = length(groupComp) - length(groupCols)
    groupCols = c(groupCols, allCols[sample(1:length(allCols), size = length(nbMissCols))])
  }
  
}


##########################################################
#   DEAL WITH MIS-MATCH FOR COMPOSITION & SPECIES-LIST   #
##########################################################


if(makeGroupPlot){

  # Keep species in composition, only if present in species list
  groupComp = lapply(groupComp, function(gp){
    ind = which(!is.na(match(gp, speciesList)))
    if(length(ind) > 0){
      newGp = gp[ind]
    }else{
      newGp = c()
    }
    return(newGp)
  })
  
  # Keep species in composition, only if no infinite parameter
  ## Extract species that must be removed from groups
  spToRemove = c()
  
  ## Measure for each species the median of the number of sites per species x year
  dataLowOcc = dplyr::group_by(data[data[,interestVar[1]] >0,], species, year) %>%
    dplyr::summarise(nbSite = dplyr::n()) %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(medYear = median(nbSite))
  
  ### Extract species with median occurrence inferior to 12
  spToRemove = dataLowOcc$species[dataLowOcc$medYear < 12]
  
  ## Extract species with large IC
  indInf = unique(c(which(is.infinite(dataLongTermTrend$supGR) | is.na(dataLongTermTrend$supGR)),
                    which(is.infinite(dataLongTermTrend$infGR) | is.na(dataLongTermTrend$infGR)),
                    which(is.infinite(dataLongTermTrend$GR)   | is.na(dataLongTermTrend$GR))))
  
  if(length(indInf) > 0){
    spToRemove = c(spToRemove, unique(dataLongTermTrend$species[indInf]))
  }
  
  indInfVar = unique(c(which( dataYearlyVariations$supGR > 1e+03 | is.na(dataYearlyVariations$supGR)),
                       which( dataYearlyVariations$infGR > 1e+03 | is.na(dataYearlyVariations$infGR)),
                       which( dataYearlyVariations$GR > 1e+03    | is.na(dataYearlyVariations$GR))))
  
  if(length(indInfVar) > 0){
    spToRemove = c(spToRemove, unique(dataYearlyVariations$species[indInfVar]))
  }
  
  ## Change group composition accordingly
  if(length(spToRemove)>0){
    groupComp = lapply(groupComp, function(gp){
      ind = which(!is.na(match(gp, spToRemove)))
      newGp = gp
      if(length(ind) > 0){
        newGp = gp[-ind]
      }
      return(newGp)
    })
  }
  
  
  # Check that all groups are informed (no NULL group)
  indGroup = unlist(sapply(1:length(groupComp), function(i){
    if(is.null(groupComp[[i]])){
      return(i)
    }
  }))
  
  if(!is.null(indGroup)){
    groupComp[indGroup] = NULL
    groupNames = groupNames[-indGroup]
    groupCols = groupCols[-indGroup]
  }
}

##################
#   MAKE PLOTS   #
##################

if(makeGroupPlot){
  if(!is.null(groupComp)){
    #   PLOT PER GROUPS   #
    # Agregate linear trends
    groupLongTermTrend = agregateTrendsPerGroup(dataLongTermTrend, groupNames, groupComp)
    
    # Agregate yearly variations
    groupYearlyVariations = agregateVariationsPerGroup(dataYearlyVariations, groupNames, groupComp, useLastYearAsReference = FALSE)
    
    # Plot group trends
    plotGroupTrends(groupLongTermTrend, groupYearlyVariations, data, groupCols, distribution, repo, fileName = "groupTrendPlot")
    
    # Agregate yearly variations
    # groupYearlyVariations = agregateVariationsPerGroup(dataYearlyVariations, groupNames, groupComp, useLastYearAsReference = TRUE)
    # 
    # # Plot group trends
    # plotGroupTrends(groupLongTermTrend, groupYearlyVariations, data, groupCols, distribution, repo, fileName = "groupTrendPlotLast")
    
  }else{
    
    #   PLOT FOR ALL SPECIES   #
    # Agregate linear trends
    groupLongTermTrend = agregateTrendsPerGroup(dataLongTermTrend, groupNames = "Toutes espèces", groupComp = list(speciesList))
    
    # Agregate yearly variations
    groupYearlyVariations = agregateVariationsPerGroup(dataYearlyVariations, groupNames = "Toutes espèces", groupComp = list(speciesList),
                                                       useLastYearAsReference = FALSE)
    
    # Plot group trends
    plotGroupTrends(groupLongTermTrend, groupYearlyVariations, data, groupCols = "red", distribution, repo, fileName = "groupTrendPlot")
    
  }
  
  # Save tables
  write.csv(groupLongTermTrend, here::here("outputs", repo, "tables", "group", "longTermTrend.csv"))
  write.csv(groupYearlyVariations, here::here("outputs", repo, "tables", "group", "yearlyVariations.csv"))
  
}

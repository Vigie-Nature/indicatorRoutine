#' makeSummaryTable
#' 
#' Create a table with occurrence and abundance information based on observations dataframe
#' 
#' @param data : a `data.frame` containing observations
#' @param sp : a `string` specifying the species we are interested into
#' @param interestVar : a 1 or 2-elements `vector` specifying the response variable
#' @param year : a `numeric` specifying the year that should be studied. If `NULL`, then all years are studied 
#' 
makeSummaryTable <- function(data, sp, interestVar, year = NULL){
  
  # Filter for considered year if specified
  if(!is.null(year)){
    data = data[data$year == year,]
  }

  #######################
  #   MEASURE METRICS   #
  #######################
  
  # Extract all visits (site x point x year x session)
  totVisits = length(unique(data$ID))
  
  # Extract all sites that have been visited
  if(!("point" %in% colnames(data))){
    nbSites = length(unique(data$site))
  }else{
    nbSites = length(unique(paste0(data$site, data$point)))
  }
  
  # Filter for considered species
  dataSp = data[data$species == sp,]

  # Filter for presence 
  dataPres_sp = dataSp[dataSp[,interestVar[1]]>0, ]
  
  # Extract all visits where species could have been present
  totVisits_sp = length(unique(dataSp$ID))
  
  # Extract all visits where species has been present
  presVisits_sp = length(unique(dataPres_sp$ID))
  
  # Extract all sites where species has been present
  if(!("point" %in% colnames(dataSp))){
    nbSites_sp = length(unique(dataPres_sp$site))
  }else{
    nbSites_sp = length(unique(paste0(dataPres_sp$site, dataPres_sp$point)))
  }
  
  # Occurrence
  occ <- round(100 * presVisits_sp / totVisits, 0)
  occ <- paste(occ, " %")
  
  # Total individuals
  nbInd <- sum(dataSp[,interestVar[1]])
  
  # Mean individuals
  meanInd <- round(nbInd / totVisits_sp, 1)
  
  ######################
  #   MAKE DATAFRAME   #
  ######################
  
  sumTable <- data.frame(nbSites_sp = nbSites_sp,
                         nbSites = nbSites,
                         occ = occ, 
                         nbInd = nbInd,
                         meanInd = meanInd)
  
  # Change colnames
  colnames(sumTable) <- c("Nb carrés occupés", "Total carrés", "Occurrence (a)", 
                          "Total ind. contactés", "Moyenne ind. contactés (b)")
  # Change row.names
  if(is.null(year)){
    rowName <- paste("Depuis", min(data$year))
  }else{
    rowName <- paste("En", year)
  }
  
  rownames(sumTable) = rowName
  
  return(sumTable)
}


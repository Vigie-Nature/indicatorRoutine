#' makeGroupSummaryTable
#'
#' A function that adds an information of group belonging to each species
#'
#' @param dataTrend : a `data.frame` containing formatted trends
#' @param dataObs : a `data.frame` containing observations
#' @param makeGroupPlot : a `boolean`, TRUE if groups should be made
#' @param groupComp : a `list` of vector of species contained in each group
#' @param groupNames : a `vector` containing names associated to each group
#' @param groupCols : a `vector` containing colors associated to each group
#'
#' @importFrom stats median
#'
makeGroupSummaryTable <- function(dataTrend, dataObs, makeGroupPlot, groupComp, groupNames, groupCols){
  
  # Format trend text
  dataTrend$textIC = paste0("[", dataTrend$infPerc, " ; ", dataTrend$supPerc, "]")
  
  # Initialize group and color 
  dataTrend$group = ''
  dataTrend$col = 'gray'
  
  # If a plot for grouped species has been made, attribute group to each species
  if(makeGroupPlot){
    if(is.null(groupComp)){
      dataTrend$group = ""
    }else{
      # Attribute each species to a group
      ind = sapply(groupComp, function(g){ match(g, dataTrend$species) })
      
      ind <- lapply(ind, function(x) {
        x_noNA <- na.omit(x)
        as.vector(x_noNA)
      })
      
      # Add a group column, depending on groupNames
      dataTrend$group[unlist(ind)] = rep(groupNames, times = sapply(ind, length))
      
      # Add a color column, depending on groupNames
      dataTrend$col[unlist(ind)] = rep(groupCols, times = sapply(ind, length))
    }
  }
  
  # LOW OCCURRENCE
  `%>%` = magrittr::`%>%`
  
  # Measure for each species the median of the number of sites per species x year
  dataLowOcc = dplyr::group_by(dataObs[dataObs[,interestVar[1]] >0,], species, year) %>%
    dplyr::summarise(nbSite = dplyr::n()) %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(medYear = median(nbSite))
  
  # Extract species with median occurrence inferior to 12
  lowOccSp = dataLowOcc$species[dataLowOcc$medYear < 12]
  
  # Add an * to species with low occurrence
  dataTrend$french_name[dataTrend$species %in% lowOccSp] = paste0(dataTrend$french_name[dataTrend$species %in% lowOccSp], "*")
  
  ## Extract species with NA estimates
  
  spToRemove = c()
  
  indInf = unique(c(which(is.na(dataTrend$supGR)),
                    which(is.na(dataTrend$infGR)),
                    which(is.na(dataTrend$GR))))
  
  if(length(indInf) > 0){
    spToRemove = c(spToRemove, unique(dataTrend$species[indInf]))
  }
  
  # Remove sp with NA trends
  if(length(spToRemove)>0){
    dataTrend <- dataTrend[!dataTrend$species %in% spToRemove,]
  }
  # Arrange per species name
  dataTrend = dplyr::arrange(dataTrend, french_name)
  
  # Select columns of interest
  dataTrend = dataTrend[,c("french_name", "group", "perc", "textIC", "trend", "col")]
  
  # Rename columns
  colnames(dataTrend) = c("Espèce", "Groupe", "Tendance", "IC", "Classification", "col")
  
  return(dataTrend)
  
}

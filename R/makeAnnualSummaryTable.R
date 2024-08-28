#' makeAnnualSummaryTable
#' 
#' Create a table with annual occurrence and abundance information based on observations dataframe
#' 
#' @param data : a `data.frame` containing observations
#' @param sp : a `string` specifying the species we are interested into
#' @param interestVar : a 1 or 2-elements `vector` specifying the response variable
#' 
#' 
#' 
# Make annual summary table
makeAnnualSummaryTable <- function(data, sp, interestVar){
  
if(length(interestVar) == 1){
  
  # Filter for considered species
  dataPres_Sp = data[data$species == sp & data[,interestVar[1]]>0,]
  
  if(!("point" %in% colnames(data))){
    
    # Extract nb of sites visited each year
    sites <- data %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(nbSites = length(unique(site)))
    
    # sum occurences and abundances
    sumAnnualOcc <- dataPres_Sp %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(Occurrences = length(unique(site)),
                       Abondance = sum(!!dplyr::sym(interestVar))) %>%
      dplyr::left_join(sites, by = "year") 
    
  }else{
    
    # Extract points that have been visited each year
    sites <- data %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(nbSites = length(unique(paste0(data$site, data$point))))
    
    sumAnnualOcc <- dataPres_Sp %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(Occurrences = length(unique(site)),
                       Abondance = sum(!!dplyr::sym(interestVar))) %>%
      dplyr::left_join(sites, by = "year") %>%
      dplyr::select(year, Occurrences, nbSites, Abondance)
    
  }
  
  # Change colnames
  colnames(sumAnnualOcc) <- c("Année", "Nb carrés occupés", "Total ind. contactés","Total carrés")
  
}else{
  
  if(!("point" %in% colnames(data))){
    
  # Extract points that have been visited each year
  sites <- data %>% 
    dplyr::group_by(year) %>%
    dplyr::summarise(nbSites = length(unique(site)))
  
  # sum occurences
  sumAnnualOcc <- dataPres_Sp %>% 
    dplyr::group_by(year) %>%
    dplyr::summarise(Occurrences = length(unique(site))) %>%
    dplyr::left_join(sites, by = "year") %>%
    dplyr::select(year, Occurrences, nbSites, Abondance)
  
  }else{
    
    # Extract sites that have been visited each year
    sites <- data %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(nbSites = length(unique(paste0(data$site, data$point))))
    
    # sum occurences
    sumAnnualOcc <- dataPres_Sp %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(Occurrences = length(unique(paste0(data$site, data$point)))) %>%
      dplyr::left_join(sites, by = "year") %>%
      dplyr::select(year, Occurrences, nbSites, Abondance)
    
  }
  
  # Change colnames
  colnames(sumAnnualOcc) <- c("Année", "Nb carrés occupés", "Total ind. contactés","Total carrés")
}
  
  return(sumAnnualOcc) 
  
}  
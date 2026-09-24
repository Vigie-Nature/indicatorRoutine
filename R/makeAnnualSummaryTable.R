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

  # Filter for considered species
  dataPres_Sp <- data %>% dplyr::filter(
    species == sp,
    !!dplyr::sym(interestVar[[1]]) > 0
  )

  # Define spatial structure ----
  spatialVars <- if ("point" %in% names(data)) {
    c("site", "point")
  } else {
    c("site")
  } 
  
  # Extract the number of site/point visited each year
  data <- data %>%
    dplyr::mutate(site_id = do.call(paste0, c(dplyr::across(dplyr::all_of(spatialVars)), sep = "_")))

  sites <- data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nbSites = dplyr::n_distinct(site_id), .groups = "drop")

  if("point" %in% colnames(data)){
    if(length(interestVar) == 1) {
      sumAnnualOcc <- dataPres_Sp %>% 
        dplyr::group_by(year) %>%
        dplyr::summarise(Occurrences = length(unique(paste0(site,point))),
                        Abondance = sum(!!dplyr::sym(interestVar))) %>%
        dplyr::left_join(sites, by = "year") 
      
      # Change colnames
      colnames(sumAnnualOcc) <- c("Année", "Nb points occupés", "Total ind. contactés","Total points")      
    } else {
      # sum occurences
      sumAnnualOcc <- dataPres_Sp %>% 
        dplyr::group_by(year) %>%
        dplyr::summarise(Occurrences = length(unique(paste0(site,point)))) %>%
        dplyr::left_join(sites, by = "year")

      colnames(sumAnnualOcc) <- c("Année", "Nb de points occupés", "Total de points")
    }
  } else {
    if(length(interestVar) == 1){
      # sum occurences and abundances
      sumAnnualOcc <- dataPres_Sp %>% 
        dplyr::group_by(year) %>%
        dplyr::summarise(Occurrences = length(unique(site)),
                        Abondance = sum(!!dplyr::sym(interestVar))) %>%
        dplyr::left_join(sites, by = "year") 
      
      # Change colnames
      colnames(sumAnnualOcc) <- c("Année", "Nb carrés occupés", "Total ind. contactés","Total carrés")
    } else {
      # sum occurences
      sumAnnualOcc <- dataPres_Sp %>% 
        dplyr::group_by(year) %>%
        dplyr::summarise(Occurrences = length(unique(site))) %>%
        dplyr::left_join(sites, by = "year") 

      # Change colnames
      colnames(sumAnnualOcc) <- c("Année", "Nb carrés occupés", "Total carrés")
    }
  }
  
  return(sumAnnualOcc) 
  
}  

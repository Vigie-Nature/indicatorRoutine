
#' filterData si a function to aplly filter on data based on the methodologies
#' of the differents observatories
#'
#' @param data a `data.frame` containing observations 
#' @param speciesList a `list` object containing the list of species in the study
#' @param filters a `character` either "STOC" or "VigieFlore" indicating the methodologies
#'
#' @returns speciesList a `list` object containing the list of species in the study
#' 
#'

filterData <- function(data, speciesList, filters) {

  # If no filters just making sure speciesList exist
  if(is.null(filters) && is.null(speciesList)){
    speciesList <- sort(unique(data$species))
  }

  if(is.null(filters)){
    return(speciesList)
  }
  # STOC filter
  # On garde uniquement les observations où la première variable de interestVar est positive.
  # On regroupe les données par espèce pour calculer
  # nbOcc = length(unique(ID)) le nombre d’ID uniques par espèce
  # nbYear = 5 * length(min(year):max(year)) Calcule une valeur seuil pour filtrer les espèces
  if(filters == "STOC"){

    dataFiltered <- data[data[, interestVar[1]] > 0, ]

    if(!is.null(speciesList)){
      dataFiltered <- dataFiltered[dataFiltered$species %in% speciesList, ]
    }

    grData <- dplyr::group_by(dataFiltered, species) %>%
      dplyr::summarise(nbOcc = length(unique(ID)),
                      nbYear = 5*length(min(year):max(year)))
      
      speciesList <- grData$species[grData$nbOcc > grData$nbYear]

  }

  # Filters for Vigie-Flore
  # Each species must respect this 2 conditions : 
  # - At lest 4 years of disticts observations
  # - At least 10 distinct observations
  if(filters == "VigieFlore"){
    dataFiltered <- data[data[, interestVar[1]] > 0, ]

    if(!is.null(speciesList)){
      dataFiltered <- dataFiltered[dataFiltered$species %in% speciesList, ]
    }

    grData <- dplyr::group_by(dataFiltered, species) %>%
      dplyr::summarise(
        nbSites = length(unique(ID)),
        nbYear = length(unique(year)),
        .groups = "drop"
      )

    speciesList <- grData$species[
      grData$nbSites >= 10 & grData$nbYear >= 4
    ]

  }

  if(!is.null(speciesList)){
    return(speciesList)
  }
}

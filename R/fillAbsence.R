#' fillAbsence
#'
#' A function that fills the data.frame with absences when they are not informed
#' 
#' @param data : a `data.frame` containing observations of species
#' @param interestVar : a `string` specifying the column with abundance information 
#' @param speciesList : a `vector` specifying the species that must be analyzed 
#' @param method :
#' A `string` defining the method chosen to reconstruct absences
#' "once" means reconstructing absences on site where the species was seen once at least
#' "all" means reconstructing absences for all sites
#'
#' @return
#' A `data.frame` filled with 0 for time/location where the species is absent
#' 
fillAbsence <- function(data, interestVar, speciesList, method){
  # Define spatial structure ----
  spatialVars <- if ("point" %in% names(data)) {
    c("site", "point")
  } else {
    c("site")
  } 
  
  # Filter data for presence only ----
  data <- data %>%
    dplyr::filter(interestVar[1] > 0)

  # Extract information on context only ----
  dataContext <- data %>%
    dplyr::select(-species, -dplyr::all_of(interestVar)) %>%
    dplyr::distinct()
  
  # Extract all unique identifiers ----
  id = unique(dataContext$ID)
  
  # Filter data for selected species ----
  if(!is.null(speciesList)){
    data <- data %>% 
      dplyr::filter(species %in% speciesList)
  }
  
  if (method == "once"){
    # Split data.Frame per species ----
    listData <- dplyr::group_split(data, species)
    
    # Fill with 0s when present once ----
    listData_withAbs = lapply(listData, function(df){

      # # Extract site of presence
      # site_pres <- unique(df$site)   
      
      # Extract ID of presence
      id_pres = unique(df$ID)
      
      # Dataframe where absent while visited and past present
      # dataAbsence = dataContext[dataContext$site %in% site_pres & !(dataContext$ID %in% id_pres) , ]

      # Dataframe where absent while visited and past present
      # Taking into acount spatial level (ie site ou site/point)
      dataAbsence <- dataContext %>%
        dplyr::semi_join(
          dplyr::distinct(df, dplyr::across(dplyr::all_of(spatialVars))),
          by = spatialVars
        ) %>%
        dplyr::filter(!ID %in% id_pres)
      
      # If absences, add missing columns and bind 
      if(nrow(dataAbsence)>0){
        # Add the species name
        dataAbsence$species = unique(df$species)
        
        # Add abundance information
        dataAbsence[,interestVar[1]] = 0
        
        #Specific to Vigie-Flore
        if (length(interestVar) == 2){
          dataAbsence[,interestVar[2]] = 10
        }
        
        # Rbind ----
        df = rbind(df, dataAbsence)
      }
      return(df)
      
    })
    # Rbind results ----
    dataRes = do.call("rbind", listData_withAbs)
    
  } else if(method == "all"){
    
    # Replicate these information as many times as number of species ----
    dataRes = do.call("rbind", replicate(length(speciesList), dataContext, simplify = FALSE))
    
    # Attribute a species name ----
    dataRes$species = rep(speciesList, each = nrow(dataContext))
    
    # Attribute 0 count information ----
    dataRes[,interestVar[1]] = 0
    if(length(interestVar) == 2){
      dataToAdd[,interestVar[2]] = 10
    }
    
    # Find which IDs / species already has abundance information ----
    matchId = match(paste(dataRes$ID, dataRes$species), paste(data$ID, data$species))
    
    # Change abundance information in the final data.frame ----
    dataRes[which(!is.na(matchId)), interestVar] = data[matchId[!is.na(matchId)], interestVar] 
    
  }
  cat("Absences have been correctly filled with 0s\n")
  
  return(as.data.frame(dataRes))
  
}


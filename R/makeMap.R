
#' makeMap
#' 
#' A function that returns a map with occurrence data 
#' 
#' @param data : a `data.frame` containing all species observations
#' @param speciesList : a `vector` containing names of the studied species
#' @param interest : a 1 or 2-elements `vector` containing the response variable
#' @param path : a `string` specifying the folder where the file should be saved
#' 
makeMap <- function(data, speciesList, interestVar, path){
  
  if("longitude" %in% colnames(data) & "latitude" %in% colnames(data)){
    
    listOfPlots = lapply(speciesList, function(sp){
      ###################
      # DATA FORMATTING #
      ###################
      
      # Erase NAs in coordinate information
      data = data[!is.na(data$longitude) & !is.na(data$latitude),]
      
      # Filter for where species has been present
      dataSp_Pres = data[data$species == sp & data[,interestVar[1]] > 0,]
      
      # Extract coordinates of sites where species is present
      dataSite_Pres = unique(dataSp_Pres[,c("site", "longitude", "latitude")])
      
      # Extract coordinates of all sampled sites
      dataSite_All = unique(data[,c("site", "longitude", "latitude")])
      
      # Attribute a present/absent value for each square
      dataSite_All$Type = "Absence"
      dataSite_All$Type[dataSite_All$site %in% dataSite_Pres$site] = "Présence"
      
      # Reorder presence / absence variable
      if(length(unique(dataSite_All$Type)) == 1 & unique(dataSite_All$Type)[1] == "Absence"){
        dataSite_All$Type = factor(dataSite_All$Type, c("Absence", "Présence"))
        colorValues = c("#FED98F", "#E68146")
      }else{
        dataSite_All$Type = factor(dataSite_All$Type, c("Présence", "Absence"))
        colorValues = c("#E68146", "#FED98F")
      }
      
      # Arrange according to the type of squares
      dataSite_All = dplyr::arrange(dataSite_All, dplyr::desc(Type))
      
      ############################
      # ADAPT ZOOM TO DEPARTMENT #
      ############################
      
      # Extract contours of France departments
      dataFrance <- ggplot2::map_data("france")
      
      # Turn contours to sf object
      dataFrance_sf <- sf::st_as_sf(dataFrance, coords = c("long", "lat"))
      
      # Make pipe %>% working
      `%>%` <- magrittr::`%>%`
      
      # Group by department and create polygon
      polygonFrance <- dplyr::group_by(dataFrance_sf, region) %>%
        dplyr::summarise(do_union=FALSE) %>%
        sf::st_cast("POLYGON")
      
      # Turn dataSite_All to sf object
      dataSite_All_sf <- sf::st_as_sf(dataSite_All, coords = c("longitude", "latitude"))
      
      # Create intersection between both
      dataInter <- sf::st_intersection(polygonFrance, dataSite_All_sf)
      
      # If region with low representation, don't show it
      # dataInter = group_by(dataInter, region) %>%
      #   summarise(nbSites = n())
      # dataInter = dataInter[dataInter$nbSites >= 2, ]
      
      # Filter for department of presence
      # dataFrance <- dataFrance[dataFrance$region %in% unique(dataInter$region),]
      
      #############################
      # PLOT FORMATTING ARGUMENTS #
      #############################
      
      # Transparency argument
      alpha = rep(.8, nrow(dataSite_All))
      alpha[dataSite_All$Type == "Présence"] = 1
      
      # Size argument ----
      size = 1.5
      # if(is.null(dep)){
      #   size = 1.5
      # }else{ size = 2}
      
      # Minimum/maximum x and y values
      xmin = min(dataFrance$long) ; xmax = max(dataFrance$long)
      ymin = min(dataFrance$lat) ; ymax = max(dataFrance$lat)
      
      ################
      # MAP CREATION #
      ################
      
      if(nrow(dataSite_All)>0){
        # Create the coordinate space of the map
        plot <- ggplot2::ggplot(dataFrance, ggplot2::aes(long, lat)) + 
          
          # Create the contours of France
          ggplot2::geom_polygon(ggplot2::aes(group = group), col = "darkgray", fill = "white") +
          
          # Add squares/points associated with the sites
          ggplot2::geom_point(data = dataSite_All, ggplot2::aes(x = longitude, y = latitude, col = Type), 
                              shape = 16, size = size, alpha = alpha) +
          
          # Add a simple theme as the background
          ggplot2::scale_color_manual("", values = colorValues) +
          
          # Add a simple theme as the background
          ggplot2::theme_void() +
          
          # Erase axis names
          ggplot2::ylab("") + ggplot2::xlab("") + 
          
          # Change extreme coordinates
          ggplot2::xlim(0.995*xmin, 1.005*xmax) + ggplot2::ylim(0.995*ymin, 1.005*ymax) +
          
          # Erase axis texts and ticks
          ggplot2::theme(legend.position = "bottom", 
                         legend.text = ggplot2::element_text(size = 20))  +
          
          # Avoid distorsion
          ggplot2::coord_quickmap(xlim = c(min(dataSite_All$longitude), max(dataSite_All$longitude)), 
                                  ylim = c(min(dataSite_All$latitude), max(dataSite_All$latitude)))
      }else{
        plot <- ggplot2::ggplot(dataFrance, ggplot2::aes(long, lat)) +
          
          # Create the contours of France
          ggplot2::geom_polygon(ggplot2::aes(group = group), col = "darkgray", fill = "white") +
          
          # Add a simple theme as the background
          ggplot2::theme_void() +
          
          # Erase axis names
          ggplot2::ylab("") + ggplot2::xlab("") +
          
          # Avoid distorsion
          ggplot2::coord_quickmap()
      }
      
      
      ###############
      # PLOT SAVING #
      ###############
      
      # Save the plot if required
      ggplot2::ggsave(plot = plot, filename = paste0(sp, ".png"), device = "png", 
                      path = path, width = 25, height = 20, units = "cm")
      
    })
    
    
  }
  else{
    listOfPlots = lapply(speciesList, function(sp){
      # Extract contours of France departments
      dataFrance <- ggplot2::map_data("france")
      
      # Make plot
      plot <- ggplot2::ggplot(dataFrance, ggplot2::aes(long, lat)) +
        
        # Create the contours of France
        ggplot2::geom_polygon(ggplot2::aes(group = group), col = "darkgray", fill = "white") +
        
        # Add a simple theme as the background
        ggplot2::theme_void() +
        
        # Erase axis names
        ggplot2::ylab("") + ggplot2::xlab("") +
        
        # Avoid distorsion
        ggplot2::coord_quickmap()
      
      ###############
      # PLOT SAVING #
      ###############
      
      # Save the plot if required
      ggplot2::ggsave(plot = plot, filename = paste0(sp, ".png"), device = "png", 
                      path = path, width = 25, height = 20, units = "cm")
      
    }
    
    )
  }
  
  return(listOfPlots)
  
}

#' plotLinearClassification
#' 
#' Make the plot for linear trends classification
#' 
#' @param data : a `data.frame` containing formatted long or short-term trends
#' @param type : a `string` either "barplot" or "errorbar" for the type of plot required
#' @param thresholdInf : a `numeric` value specifying the lower threshold to classify btw moderate and strong decline. 
#' NB : 0.036 corresponds to the Red List Criterion i.e, a decline of 30% over 10 years
#' #' @param thresholdSup : a `numeric` value specifying the upper threshold to classify btw moderate and strong increase. 
#' NB : 0.036 corresponds to the Red List Criterion i.e, a decline of 30% over 10 years
#' @param path : a `string` specifying where the plot should be saved
#' 
plotLinearClassification <- function(data, distribution, type = "barplot", 
                                     thresholdInf = 0.036, thresholdSup = 0.026, path = ""){
  
  # Erase NA values
  data = data[!is.na(data$trend),]
  
  # Transform coefficients according to distribution
  if(distribution != "gaussian"){
    data[,c("estimate", "infIC", "supIC")] = exp(data[,c("estimate", "infIC", "supIC")])
    zeroVal = 1
  }else{
    zeroVal = 0
  }
  
  # Order and colors for trends
  ## Names
  refOrder = c("Augmentation forte", "Augmentation modérée à forte", "Augmentation modérée",
               "Stable", "Déclin modéré", "Déclin modéré à fort", "Déclin fort", "Incertain")
  
  ## Colors
  refCols = c("#274034", "#4d8067", "#81b29a", "#F2CC8F","#f57e56","#d8400d", "#d61a0d",  "gray")
  
  ## Point types
  refTypes = c(16, 1, 3, 3, 3, 1, 16, 4)
  
  # Filter for trends that occur
  refCols = refCols[which(!is.na(match(refOrder,unique(data$trend))))]
  refTypes = refTypes[which(!is.na(match(refOrder,unique(data$trend))))]
  refOrder = refOrder[which(!is.na(match(refOrder,unique(data$trend))))]
  
  # Reorder data trends with refOrder
  data$trend = factor(data$trend, levels = refOrder)
  
  # Reorder data species with decreasing growthrate
  data = dplyr::arrange(data, estimate)
  data$species = factor(data$species, data$species)
  
  # Make barplot
  if(type == "barplot"){
    plot = ggplot2::ggplot(data, ggplot2::aes(x = trend, fill = trend)) +
      
      # Make barplot
      ggplot2::geom_bar(stat = "count") +
      
      # Adapt colors
      ggplot2::stat_count(geom = "text", colour = "white", size = 3.5,
                          ggplot2::aes(label = ggplot2::after_stat(count)), 
                          position = ggplot2::position_stack(vjust=0.5)) +
      
      # Change colors
      ggplot2::scale_fill_manual("Classification", values = refCols) +
      
      # Change theme
      ggplot2::theme_bw() + 
      
      # Set a maximum y value
      ggplot2::ylim(c(0,max(table(data$trend)))) + 
      
      # Change axis names
      ggplot2::xlab("") + ggplot2::ylab("Nombre d'espèces") +
      
      # Format theme
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(), 
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 12),
                     axis.title.y = ggplot2::element_text(size = 14))
    
  }else if(type == "errorbar"){
    plot = ggplot2::ggplot(data, ggplot2::aes(y = species, x = estimate, col = trend, shape = trend)) + 
      # Add estimates and errorbars
      ggplot2::geom_pointrange(ggplot2::aes(xmin = infIC, xmax = supIC), linewidth = 0.8, fatten = 3) +
      
      # Add vertical line for stable/uncertain effect
      ggplot2::geom_vline(xintercept = zeroVal, linetype = "dashed", col = "gray") +
      
      # Add vertical line for strong negative trend
      ggplot2::geom_vline(xintercept = zeroVal - thresholdInf, linetype = "dashed", col = "#d61a0d") +
      
      # Add vertical line for strong positive effect
      ggplot2::geom_vline(xintercept = zeroVal + thresholdSup, linetype = "dashed", col = "#274034") +
      
      # Change color
      ggplot2::scale_color_manual("Classification", values = refCols) +
      
      # Change point types
      ggplot2::scale_shape_manual("Classification", values = refTypes) +
      
      # Change names of axis
      ggplot2::ylab("Espèces") + ggplot2::xlab("Estimations") + 
      
      # Apply classic theme
      ggplot2::theme_bw() +
      
      # Format theme
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
    
  }
  
  # Save result
  ggplot2::ggsave(plot = plot, filename = paste0(type, ".png"), device = "png", path = path, 
                  width = 25, height = 20, units = "cm")
  
  return(plot)
}
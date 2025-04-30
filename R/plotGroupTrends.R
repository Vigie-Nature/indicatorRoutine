#' plotGroupTrends
#' 
#' A function that makes the plot of trends agregated per groups
#' 
#' @param dataTrend a `data.frame` containing long-term trends agregated per group
#' @param dataVar a `data.frame` containing yearly variations agregated per group
#' @param data a `data.frame` containing observations 
#' @param groupCols a `vector` containing colors associated with each group
#' @param distribution a `string` specifying the law distribution
#' @param repo a `string` specifying the data repository
#' @param fileName a `string` specifying the name under which the plot must be saved
#' 
plotGroupTrends <- function(dataTrend, dataVar, data, groupCols, distribution, repo, fileName){
  
  # Extract time range 
  minYear <- min(dataVar$year)
  maxYear <- max(dataVar$year)
  
  # Make title
  plotTitle <- paste0("Tendances par groupe (", minYear, "-", maxYear, ")")
  
  # Make y-axis
  yName <- switch(distribution,
                  "nbinom2" = "Abondance relative",
                  "poisson" = "Abondance relative",
                  "binomial" = "Rapport des chances",
                  "betabinomial" = "Rapport des chances",
                  "Indice")
  
  # Make labels for each group
  groupLabels <- paste0(dataTrend$group, " : ", dataTrend$perc, "% (", 
                        dataTrend$infPerc, "% ; ", dataTrend$supPerc, '%)')
  
  # Deal with colors
  if(length(groupCols) > 1){
    groupCols = c(groupCols, "red")
    groupNames = c(groupNames, "Toutes espèces")
  }
  
  # Associer les noms de groupes aux couleurs
  names(groupCols) <- groupNames
  
  # Vérifie que les groupes dans dataVar et dataTrend sont bien des facteurs avec les bons niveaux
  dataVar$group <- factor(dataVar$group, levels = groupNames)
  dataTrend$group <- factor(dataTrend$group, levels = groupNames)
  
  # Deal with number of rows for legend
  nGroups <- length(groupCols)
  nrowLegend <- if (nGroups <= 2) 1 else if (nGroups < 5) 2 else 3
  
  # Create a "saison" var corresponding to SHOC winter track 
  dataVar$saison <- paste(dataVar$year, dataVar$year+1, sep = "-")
  
  if("saison" %in% colnames(data)){
    saison_labels <- unique(dataVar$saison)
    x_axisName <- "Hiver"
  }  else {
    saison_labels <- unique(dataVar$year)
    x_axisName <- "Année"
  }
  
  # Make the plot
  plot <- ggplot2::ggplot(dataVar, ggplot2::aes(x = year, y = index)) +
    
    # Add reference horizontal line
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "darkgray", show.legend = T) +
    
    # Add yearly variations lines
    ggplot2::geom_line(ggplot2::aes(col = group))  +
    
    # Add yearly variations points
    ggplot2::geom_point(ggplot2::aes(col = group)) +
    
    # Add confidence intervals
    ggplot2::geom_ribbon(ggplot2::aes(ymin = infIndex, ymax = supIndex,  fill = group), alpha = .1) +
    
    # Smooth the variations as a linear trend
    #ggplot2::geom_smooth(data = dataVar, se = FALSE, method = "lm", ggplot2::aes(col = group), linetype = "dotted") +
    
    # Add a title
    ggplot2::ggtitle(plotTitle) +
    
    # Format x-axis
    ggplot2::scale_x_continuous(name = x_axisName, breaks = minYear:maxYear, labels = saison_labels) + # change year labels on x axis for winter season ) +
    
    # Add title to the y-axis
    ggplot2::ylab(yName) +
    
    # Choose a simple background theme
    ggplot2::theme_bw() +
    
    # Format x-axis text
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
                   axis.text.y = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 14),
                   title = ggplot2::element_text(size = 16),
                   legend.position = "top",
                   legend.justification = "left") +
    
    # Format color values
    ggplot2::scale_colour_manual(name = "", values = groupCols, labels = groupLabels,
                                 guide = ggplot2::guide_legend(nrow = nrow, by.row = FALSE)) +
    
    # Format fill values
    ggplot2::scale_fill_manual(name = "", values = groupCols, labels = groupLabels,
                               guide = ggplot2::guide_legend(nrow = nrow, by.row = FALSE)) 
  
  
  # Save the plot as png
  pathToPlot = here::here("outputs", repo, "figures", "group")
  
  ggplot2::ggsave(paste0(fileName, ".png"), plot, path = pathToPlot, device = "png", 
                  width = 25, height = 18,units = "cm")
  
  return(plot)
}



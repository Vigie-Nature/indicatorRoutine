#' plotGroupTrends
#' 
#' A function that makes the plot of trends agregated per groups
#' 
#' @param dataTrend a `data.frame` containing long-term trends agregated per group
#' @param dataVar a `data.frame` containing yearly variations agregated per group
#' @param groupCols a `vector` containing colors associated with each group
#' @param distribution a `string` specifying the law distribution
#' @param repo a `string` specifying the data repository
#' @param fileName a `string` specifying the name under which the plot must be saved
#' 
plotGroupTrends <- function(dataTrend, dataVar, groupCols, distribution, repo, fileName){
  
  # Extract time range 
  minYear <- min(dataVar$year)
  maxYear <- max(dataVar$year)
  
  # Make title
  plotTitle <- paste0("Tendances par groupe (", minYear, "-", maxYear, ")")
  
  # Make y-axis
  if(distribution %in% c("nbinom2", "poisson")){
    yName = "Abondance relative"
  }else if (distribution %in% c("binomial", "betabinomial")){
    yName = "Rapport des chances"
  }else{
    yName = "Indice"
  }
  
  # Make labels for each group
  groupLabels <- paste0(dataTrend$group, " : ", dataTrend$perc, "% (", 
                        dataTrend$infPerc, "% ; ", dataTrend$supPerc, '%)')
  
  # Deal with colors
  if(length(groupCols) > 1){
    groupCols = c(groupCols, "red")
  }
  
  # Deal with number of rows for legend
  nrow = 1
  if(length(groupCols) > 2 & length(groupCols) < 5){
    nrow = 2
  }else{
    nrow = 3
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
    ggplot2::geom_smooth(data = dataVar, se = FALSE, method = "lm", ggplot2::aes(col = group), linetype = "dotted") +
    
    # Add a title
    ggplot2::ggtitle(plotTitle) +
    
    # Format x-axis
    ggplot2::scale_x_continuous("Année", minYear:maxYear) +
    
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

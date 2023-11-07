#' plotQuadraticTrend
#' 
#' A plot function returning the number of species according to its quadratic trend
#' 
#' @param data : a `data.frame` containing the formatted quadratic trends for each species
#' @param path : a `string` specifying the path to the result repository
#' 
plotQuadraticTrend <- function(data, path){
  
  # Extract the number of each type of trend
  quadrTrends = table(data$trend)
  
  # Range values from -1 to 1 with equal step
  x = seq(from = -1, to = 1, by = .01)
  
  # Make data.frame with linear and quadratic values
  dataPlot = data.frame(x = x, x2 = x^2, opposite_x2 = -1 * x^2,
                        opposite_x = -1 * x, horizontal = 1)
  
  # Create all single plots
  listPlot = list()
  
  ## Déclin ralenti
  listPlot[["decDecr"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = x2)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::xlim(-1,0) + ggplot2::ylim(-0.2,1.2) +
    ggplot2::annotate(geom = "text", x = -0.1, y = 1.2 - 0.05*1.4, size = 4,  
             label = paste0("N = ", ifelse(is.na(quadrTrends["Déclin ralenti"]), 
                                           0, quadrTrends["Déclin ralenti"]))) +
   ggplot2::ggtitle("A) Déclin ralenti")
  
  ## Concave
  listPlot[["concave"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = opposite_x2)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::ylim(-1.3,0.3) +
    ggplot2::annotate(geom = "text", x = 0.8, y = 0.3 - 0.05*1.6, size = 4,  
             label = paste0("N = ",  ifelse(is.na(quadrTrends["Concave"]), 
                                            0, quadrTrends["Concave"]))) +
    ggplot2::ggtitle("B) Concave")
  
  ## Augmentation ralentie
  listPlot[["decIncr"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = opposite_x2)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::xlim(-1,0) +  ggplot2::ylim(-1.2, 0.2) +
    ggplot2::annotate(geom = "text", x = -0.1, y = 0.2 - 0.05*1.4, size = 4,  
             label = paste0("N = ",  ifelse(is.na(quadrTrends["Augmentation ralentie"]), 
                                            0, quadrTrends["Augmentation ralentie"]))) +
    ggplot2::ggtitle("C) Augmentation ralentie")
  
  ## Déclin linéaire
  listPlot[["linearDecr"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = opposite_x)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::ylim(-1.3,1.3) +
    ggplot2::annotate(geom = "text", x = 0.8, y = 1.3 - 0.05 * 2.6, size = 4,  
             label = paste0("N = ",  ifelse(is.na(quadrTrends["Déclin linéaire"]), 
                                            0, quadrTrends["Déclin linéaire"]))) +
    ggplot2::ggtitle("D) Déclin linéaire")
  
  ## Stabilité linéaire
  listPlot[["linearStable"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = horizontal)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::ylim(0.7,1.3) +
    ggplot2::annotate(geom = "text", x = 0.8, y = 1.3 - 0.05*0.6, size = 4,  
             label = paste0("N = ",  ifelse(is.na(quadrTrends["Stable linéaire"]), 
                                            0, quadrTrends["Stable linéaire"]))) +
    ggplot2::ggtitle("E) Stable linéaire")
  
  ## Augmentation linéaire
  listPlot[["linearIncr"]] =ggplot2:: ggplot(dataPlot, ggplot2::aes(x = x, y = x)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::ylim(c(-1.3,1.3)) +
    ggplot2::annotate(geom = "text", x = 0.8, y = 1.3 - 0.05 * 2.6, size = 4,  
             label = paste0("N = ", ifelse(is.na(quadrTrends["Augmentation linéaire"]), 
                                           0, quadrTrends["Augmentation linéaire"]))) +
    ggplot2::ggtitle("F) Augmentation linéaire")
  
  ## Déclin accéléré
  listPlot[["accDecr"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = opposite_x2)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::xlim(0,1) + ggplot2::ylim(-1.2,0.2) +
    ggplot2::annotate(geom = "text", x = 0.9, y = 0.2 - 0.05*1.4, size = 4, 
             label = paste0("N = ",  ifelse(is.na(quadrTrends["Déclin accéléré"]), 
                                            0, quadrTrends["Déclin accéléré"]))) +
    ggplot2::ggtitle("G) Déclin accéléré")
  
  ## Convexe
  listPlot[["convex"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = x2)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::ylim(-0.3,1.3) +
    ggplot2::annotate(geom = "text", x = 0.8, y = 1.3 - 0.05*1.6, size = 4,  
             label = paste0("N = ",  ifelse(is.na(quadrTrends["Convexe"]), 
                                            0, quadrTrends["Convexe"]))) +
    ggplot2::ggtitle("H) Convexe")
  
  
  ## Augmentation accélérée
  listPlot[["accIncr"]] = ggplot2::ggplot(dataPlot, ggplot2::aes(x = x, y = x2)) +
    ggplot2::geom_line(linewidth = 1, col = "gray25") +
    ggplot2::xlim(0,1) + ggplot2::ylim(-0.2,1.2) +
    ggplot2::annotate(geom = "text", x = 0.9, y = 1.2 - 0.05*1.4, size = 4,  
             label = paste0("N = ",  ifelse(is.na(quadrTrends["Augmentation accélérée"]), 
                                            0, quadrTrends["Augmentation accélérée"]))) +
    ggplot2::ggtitle("I) Augmentation accélérée")
    
 
  
  # Formater les plots
  listPlot = lapply(listPlot, function(p){
    p = p +
      ggplot2::theme_bw() + 
      ggplot2::xlab("") + ggplot2::ylab("") + 
      ggplot2::theme(axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            title = ggplot2::element_text(size = 10),
            plot.margin = ggplot2::unit(c(0.1,0.1,0.1,0.1), "cm"))
    
    return(p)
  })
  
  # Regrouper les plots sous la même figure
  finalPlot = cowplot::plot_grid(plotlist = listPlot, nrow = 3, ncol = 3)
  
  # Save results
  ggplot2::ggsave(filename = "quadraticTrends.png", plot = finalPlot, device = "png", path = path, 
                  width = 25, height = 20, units = "cm")
  
  return(finalPlot)
  
}







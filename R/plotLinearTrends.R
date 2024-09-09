#' plotLinearTrends
#'
#' A function that takes all estimations, and turn it to trend curves
#' 
#' @param speciesList : a `vector` containing all species names
#' @param data a `data.frame` containing observations 
#' @param dataName : a `data.frame` containing french names of the species
#' @param dataLongTerm : a `data.frame` containing formatted estimates for the long-term trend model
#' @param dataYearlyVariations : a `data.frame` containing formatted estimates for the yearly variations
#' @param dataShortTerm : a `data.frame` containing formatted estimates for the short-term trend model
#' @param distribution : a `string` specifying the law distribution
#' @param plotGamm : a `boolean` specifying if gamm variations were computed
#' @param plotST : a `boolean` specifying if short-term trends were computed
#' @param uncertainty : a `boolean` specifying if uncertainty should be shown
#' @param N : a `numeric` value that indicates how many simulations should be made (only if uncertainty == T)
#' @param weight : a `boolean` that indicates if weights should be applied in the simulation process (only if uncertainty == T)
#' @param save : a `boolean` specifying if the plot should be saved or not
#' @param path : a `string` specifying the path to the repository where to save plots
#' 
plotLinearTrends <- function(speciesList, data, dataLongTerm, dataYearlyVariations, dataShortTerm = NULL,
                      dataName = NULL, distribution, plotGamm = TRUE, plotST = TRUE,
                      uncertainty = TRUE, N = 100, weight = TRUE, save = TRUE, path = ""){
  
  plots <- sapply(speciesList, function(sp){ 
    # Filter tables for considered species
    dataLT_sp = dataLongTerm[dataLongTerm$species == sp,]
    dataVar_sp = dataYearlyVariations[dataYearlyVariations$species == sp,]
    
    if(nrow(dataLT_sp)==0|nrow(dataVar_sp)==0) {
      message('No plot for species ', sp, ': no model was found for either ',
              'long-term trend or yearly variations\n')
    plot = NULL
    
    }else if(any(is.na(c(dataLT_sp$estimate, dataVar_sp$estimate)))){
      message('No plot for species ', sp, ' due to no-convergence of either',
              " long-term trend or yearly variations\n")
      plot = NULL
      
    }else{
      # Extract values of the temporal series
      yearValues = dataLT_sp$minYear:dataLT_sp$maxYear
      
      # Create a "saison" var corresponding to SHOC winter track 
      dataVar_sp$saison <- paste(dataVar_sp$year, dataVar_sp$year+1, sep = "-")
      
      if("saison" %in% colnames(data)){
        saison_labels <- dataVar_sp$saison
        x_axisName <- "Hiver"
      }  else {
        saison_labels <- yearValues
        x_axisName <- "Année"
      }
      
      ###################
      # GAMM VARIATIONS #
      ###################
      pathToGamm = here::here("outputs", repo, "models", "gammVariations", paste0(sp, ".rdata"))
      
      if(plotGamm & file.exists(pathToGamm)){
        # Load the corresponding file
        load(here::here("outputs", repo, "models", "gammVariations", paste0(sp, ".rdata")))
        
        if(is.null(gammVariations$warnings) & is.null(gammVariations$error)){
          # Extract values of gam
          valuesToPlotGAM = plot(gammVariations$value$gam, se = TRUE, n = 50 * length(yearValues))
          
          # Make data.frame
          dataGammVariations_sp = data.frame(year = valuesToPlotGAM[[1]]$x, 
                                             estimate = valuesToPlotGAM[[1]]$fit, 
                                             infIC = valuesToPlotGAM[[1]]$fit - 1.96 * valuesToPlotGAM[[1]]$se,
                                             supIC = valuesToPlotGAM[[1]]$fit + 1.96 * valuesToPlotGAM[[1]]$se,
                                             group = "GAM",
                                             rep = 1)
          
          # Centrer sur 0
          dataGammVariations_sp[,c("estimate", "infIC", "supIC")] = dataGammVariations_sp[,c("estimate", "infIC", "supIC")] - mean(dataGammVariations_sp$estimate)
          
          # Passer à l'exponentielle si données d'occurrence ou de comptage
          if(distribution != "gaussien"){
            dataGammVariations_sp[,c("estimate", "infIC", "supIC")] = exp(dataGammVariations_sp[,c("estimate", "infIC", "supIC")])
          }
        }else{
          plotGamm = FALSE
        }
        
      }else{
        plotGamm = FALSE
      }
      
      ####################
      # SHORT-TERM TREND #
      ####################    
      if(plotST & !is.null(dataShortTerm)){
        dataST_sp = dataShortTerm[dataShortTerm$species == sp,]
        if(!is.na(dataST_sp$estimate)){
          plotST = TRUE
          
          # Extract values range of year
          yearValuesST = dataST_sp$minYear : dataST_sp$maxYear
          
          if(uncertainty){
            # Make slopes from yearly variations
            STSlopes = makeSlopesFromVar(dataVar_sp, N = N, limits = yearValuesST, weight = weight, group =  "ST")
            
            # Aesthetics
            lwdST = .8
            alphaST = .1
          }else{
            # Values of abundance for the whole years range
            estST = dataST_sp$estimate * (yearValuesST - min(yearValuesST))
            
            # Center on the middle value of the yearly variations
            midVal = mean(dataVar_sp$estimate[dataVar_sp$year >= min(yearValuesST)])
            estST = estST - mean(estST) + midVal
            
            # Turn to exponential if required
            estST = sapply(estST, function(x) ifelse(distribution == "gaussian", x, exp(x)))
            # Make dataframe
            STSlopes = data.frame(year = yearValuesST,
                                  estimate = estST,
                                  infIC = NA,
                                  supIC = NA,
                                  group = "ST",
                                  rep = 1)
            
            # Aesthetics
            lwdST = 1.1
            alphaST = 1
          }
          
        }else{
          plotST = FALSE
        }
      }else{
        plotST = FALSE
      }
      
      
      ###################
      # LONG-TERM TREND #
      ###################
      if(uncertainty){
        LTSlopes = makeSlopesFromVar(dataVar_sp, N = N, limits = yearValues, weight = weight, group =  "LT")
        # Aesthetics
        lwdLT = .8
        alphaLT = .1
        
      }else{
        # Values of abundance for the whole years range
        estLT = dataLT_sp$estimate * (yearValues - min(yearValues))
        
        # Center on middle value
        estLT = estLT - mean(estLT)
        
        # Turn to exponential if required
        estLT = sapply(estLT, function(x) ifelse(distribution == "gaussian", x, exp(x)))
        
        # Make the data.frame
        LTSlopes = data.frame(year = yearValues,
                              estimate = estLT,
                              infIC = NA,
                              supIC = NA,
                              group = "LT",
                              rep = 1)
        
        # Aesthetics
        lwdLT = 1.1
        alphaLT = 1
        
      }
      
      #####################
      # YEARLY VARIATIONS #
      #####################
      # Centrer sur le milieu de la courbe
      dataVar_sp[,c("estimate", "infIC", "supIC")] = dataVar_sp[,c("estimate", "infIC", "supIC")] - mean(dataVar_sp$estimate)
      
      # Passer à l'exponentiel, si données d'occurrence ou de comptage
      if(distribution != "gaussian"){
        dataVar_sp[,c("estimate", "infIC", "supIC")] = exp(dataVar_sp[,c("estimate", "infIC", "supIC")])
      }
      
      # Garder les colonnes d'intérêt
      dataVar_sp = dataVar_sp[,c("year","estimate", "infIC", "supIC")]
      
      # Ajouter les
      dataVar_sp$group = "VAR"
      dataVar_sp$rep = 1
      
      # FINAL DATAFRAME #
      dataPlot = rbind(LTSlopes, dataVar_sp)
      
      if(plotST){
        dataPlot = rbind(dataPlot, STSlopes) 
      }
      if(plotGamm){
        dataPlot = rbind(dataPlot, dataGammVariations_sp)
      }
      
      ###################################
      #   TITLE, SUBTITLES & AXIS NAME  #
      ###################################
      
      if(is.null(dataName)){
        title <- sp
      }else{
        # Filter french names for considered species
        dataName_sp = dataName[dataName$species == sp,]
        
        if(nrow(dataName_sp) >0){
          title <- dataName_sp$french_name
        }else{
          title <- sp
        }
        
      }
      
      # Subtitle
      legendLT = percEvolutionToText(data = dataLT_sp, distribution = distribution)
      
      if(plotST){
        legendST = percEvolutionToText(data = dataST_sp, distribution = distribution)
      }
      
      legendGAM = "Tendance non linéaire"
      legendVar = "Estimations annuelles"
      
      # Name of y axis
      if(distribution %in% c("poisson", "nbinom2")){
        yName = "Abondance relative"
      }else if(distribution %in% c("binomial", "betabinomial")){
        yName = "Rapport des chances"
      }
      
      ######################
      #   REFERENCE LINE   # 
      ######################
      
      # Identify reference intercept
      yRef = ifelse(distribution == "gaussian", 0, 1)
      
      ####################
      #   SIGNIFICANCE   #
      ####################
      
      # Turn sinificance of the trend to text ----
      signLT <- significanceToText(dataLT_sp$pval)
      
      # x-position of the significance
      xSignLT <- dataLT_sp$maxYear + 1 ; 
      
      # y-position of the significance
      ySignLT <- mean(LTSlopes$estimate[LTSlopes$year == dataLT_sp$maxYear]) 
      
      # If ST trend exists, do the same ----
      if(plotST){
        xSignST <- xSignLT
        signST <- significanceToText(dataST_sp$pval)
        ySignST <- mean(STSlopes$estimate[STSlopes$year == dataST_sp$maxYear ]) 
      }
      
      ###################
      #   GROWTH RATE   #
      ###################
      
      # Extract growth rate
      grLT = ifelse(distribution != "gaussian", exp(dataLT_sp$estimate), dataLT_sp$estimate)
      
      # Round it
      grLT = round(grLT, 3)
      
      # Extract x & y coordinate
      xGrLT = dataLT_sp$minYear - 1
      yGrLT = mean(LTSlopes$estimate[LTSlopes$year == min(LTSlopes$year)])
      
      if(plotST){
        # Extract growth rate
        grST = ifelse(distribution != "gaussian", exp(dataST_sp$estimate), dataST_sp$estimate)
        # Round it
        grST = round(grST, 3)
        # Extract x & y coordinate
        xGrST = dataST_sp$minYear - 1
        yGrST = mean(STSlopes$estimate[STSlopes$year == min(STSlopes$year)])
        
      }
      
      ##############
      #   COLORS   #
      ##############
      if (plotGamm & plotST){
        # Color values
        colValues = c( "#F8C457","#429EBD","#FF4359", "#F8C457")
        colValues_guide = c("#429EBD","#FF4359", "#F8C457", "#F8C457")
        
        # Labels for each color
        colLabels = c(legendLT, legendST, legendGAM, legendVar)
        
        # Other characteristics
        linetype = c(1, 1, 1, 0) ; shape = c(NA, NA, NA, 18)
        size = c(1.1, 1.1, 1.1, 3) ; alpha = c(1, 1, 1, 1)
        
      }else if(plotGamm & !plotST){
        # Color values
        colValues = c( "#F8C457","#429EBD", "#F8C457") 
        colValues_guide = c("#429EBD", "#F8C457", "#F8C457")
        
        # Labels for each color
        colLabels = c(legendLT, legendGAM, legendVar)
        
        # Other characteristics
        linetype = c(1, 1, 0) ; shape = c(NA, NA, 18)
        size = c(1.1, 1.1, 3) ; alpha = c(1, 1, 1)
        
      }else if(!plotGamm & plotST){
        # Color values
        colValues = colValues_guide = c("#429EBD","#FF4359", "#F8C457")
        
        # Labels for each color
        colLabels = c(legendLT, legendST, legendVar) 
        
        # Other characteristics
        linetype = c(1, 1, 0) ; shape = c(NA, NA, 18)
        size = c(1.1, 1.1, 3) ; alpha = c(1, 1, 1)
        
      }else{
        # Color values
        colValues = colValues_guide = c("#429EBD", "#F8C457")
        
        # Labels for each color
        colLabels = c(legendLT, legendVar) 
        
        # Other characteristics
        linetype = c(1, 0) ; shape = c(NA, 18)
        size = c(1.1, 3) ; alpha = c(1, 1)
      }
      
      ############
      #   PLOT   #
      ############
      if(all(dataPlot$estimate < 10^10)){
        
        # Initialiser le plot 
        plot <- ggplot2::ggplot()
        
        # ADD GAMM INFORMATION
        if(plotGamm){
          plot <- plot +  
            # Add the confidence interval
            ggplot2:: geom_ribbon(data = dataPlot[dataPlot$group == "GAM",], ggplot2::aes(x = year, ymin = infIC, ymax = supIC), fill = "#F8C457", alpha = .2) +
            
            # Add the curve associated with the GAM
            ggplot2::geom_line(data = dataPlot[dataPlot$group == "GAM",], ggplot2::aes(x = year, y = estimate, col = group), linewidth = 1.05)
          
        }
        
        # ADD YEARLY VARIATIONS & LONG-TERM TREND
        plot = plot +
          
          # Add yearlyVariations
          ggplot2::geom_point(data = dataPlot[dataPlot$group == "VAR",], ggplot2::aes(x = year, y = estimate, col = group), shape = 18, size = 2.5) + 
          
          # Add the long-term trend
          ggplot2::geom_line(data = dataPlot[dataPlot$group == "LT",], ggplot2::aes(x = year, y = estimate, group = rep, col = group), 
                    alpha = alphaLT, linewidth = lwdLT) + 
          
          # Add LT significance information
          ggplot2::annotate(geom = "text", label = signLT, color = "#429EBD", size = 5, x = xSignLT, y = ySignLT) +
          
          # Add LT growth rate
          ggplot2::annotate(geom = "text", label = grLT, color = "#429EBD", size = 5, x = xGrLT, y = yGrLT)
        
        # ADD SHORT-TERM TREND
        if(plotST){
          plot = plot +
            
            # Add the short-term trend
            ggplot2::geom_line(data = dataPlot[dataPlot$group == "ST",], ggplot2::aes(x = year, y = estimate, group = rep, col = group), 
                      alpha = alphaST, linewidth = lwdST) + 
            
            # Add LT significance information
            ggplot2::annotate(geom = "text", label = signST, color = "#FF4359", size = 5, x = xSignST, y = ySignST) +
            
            # Add LT growth rate
            ggplot2::annotate(geom = "text", label = grST, color = "#FF4359", size = 5, x = xGrST, y = yGrST)
          
          
        }
        
        # FORMAT THE PLOT : AXIS, TITLES, ...
        plot <-  plot + 
          
          # Add the reference line
          ggplot2::geom_hline(yintercept = yRef, col = "gray", linetype = "dashed", linewidth = 1.01) +
          
          # Title
          ggplot2::ggtitle(title) +
          
          # Format x and y-axis
          ggplot2::scale_x_continuous(name = x_axisName, breaks = yearValues, limits = c(dataLT_sp$minYear - 1, dataLT_sp$maxYear +1), labels = saison_labels) + # change year labels on x axis for winter season for SHOC)
          
          # Rename y-axis                            
          ggplot2::ylab(yName) +
          
          # Change theme
          ggplot2::theme_bw() +
          
          # Adapt y-axis range
          ggplot2::coord_cartesian(ylim =  c(0.5, 2)) +  
          
          # Format theme
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), 
                legend.text = ggplot2::element_text(size = 14),
                axis.text = ggplot2::element_text(size = 14),
                axis.title = ggplot2::element_text(size = 18),
                title = ggplot2::element_text(size = 20), 
                panel.grid.minor = ggplot2::element_blank(),
                legend.position = "top", 
                legend.justification = "left") +
          
          # Format type of trend colour legend
          ggplot2::scale_colour_manual(name = "", 
                                       values = colValues, 
                                       labels = colLabels, 
                                       guide = ggplot2::guide_legend(nrow = 2, 
                                                                     by.row = FALSE,
                                                                     override.aes = list(linetype = linetype,
                                                                                         color = colValues_guide,
                                                                                         shape = shape,
                                                                                         size = size,
                                                                                         alpha = alpha))) 
      } else{
        plot = ggplot2::ggplot() + ggplot2::theme_bw()
        cat("No plot for species ", sp," due to extreme values in some coefficients\n")
      }
       
    }
    
    # Save results
    if(save){
      ggplot2::ggsave(plot = plot, device = "png", filename = paste0(sp, ".png"), 
             path = path, width = 25, height = 18, units = "cm")
    }
   
    return(plot) 
  }
  )
  
  return(plots)
}

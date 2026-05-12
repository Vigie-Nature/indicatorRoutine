#' setupGammPlotV2
#'
#' A function to create tempory dataframe to add gamm in plotLinearTrends
#' 
#' @param repo : a `string` specifying the path to the repository where to save plots
#' @param sp a `string` specifying the species of interest

setGammPlot <- function (repo, sp, yearValues, distribution, sdDataSp, mnDataSp){
  pathToGamm = here::here("outputs", repo, "models", "gammVariations", paste0(sp, ".rdata"))
  
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
    
    # #Unscale year
    # dataSp <- data %>% 
    #   dplyr::filter(species == sp)
    
    # dataGammVariations_sp$year <- (dataGammVariations_sp$year * sd(dataSp$year)) + mean(dataSp$year)
    dataGammVariations_sp$year <- (dataGammVariations_sp$year * sdDataSp) + mnDataSp
    
    # Centrer sur 0
    dataGammVariations_sp[,c("estimate", "infIC", "supIC")] = dataGammVariations_sp[,c("estimate", "infIC", "supIC")] - mean(dataGammVariations_sp$estimate)
    
    # Passer à l'exponentielle si données d'occurrence ou de comptage
    if(distribution != "gaussien"){
      dataGammVariations_sp[,c("estimate", "infIC", "supIC")] = exp(dataGammVariations_sp[,c("estimate", "infIC", "supIC")])
    }
    
    #Write the csv
    pathToSave = here::here("outputs", repo, "models", "gammVariations", paste0(sp, ".csv"))
    
    data.table::fwrite(dataGammVariations_sp, file = pathToSave)
    
    return(dataGammVariations_sp)
  } else {
    return(NULL)
  }
}
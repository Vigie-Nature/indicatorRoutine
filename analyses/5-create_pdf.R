# STEP 5 : CREATE PDF WITH ALL RESULTS

# Make global directory 
pathToPdf = here::here("outputs", repo, "pdf")
dir.create(path = pathToPdf, showWarnings = FALSE)

# Load long term trends
pathToLT = here::here("outputs", repo, "tables", "trends", "longTermTrends.csv")
dataLongTermTrend = read.csv(pathToLT)

# Load short term trends
if(makeShortTrend){
  pathToST = here::here("outputs", repo, "tables", "trends", "shortTermTrends.csv")
  dataShortTermTrend = read.csv(pathToST)
}

# Load plots
pathToUncertainPlots = here::here("outputs", repo, "figures", "trends", "uncertainPlots.rdata")
load(pathToUncertainPlots)

pathToRegularPlots = here::here("outputs", repo, "figures", "trends", "regularPlots.rdata")
load(pathToRegularPlots)

################################
#   MAKE PDF FOR ALL SPECIES   #
################################

# Make species directory
pathToPdfSp = here::here("outputs", repo, "pdf", "species")
dir.create(path = pathToPdfSp, showWarnings = FALSE)


# for (sp in speciesList){
# 
#   # Extract french and scientific names if specified
#   if(!is.null(dataName)){
#     dataName_sp = dataName[dataName$species == sp,]
#     sp_french = dataName_sp$french_name
#     sp_latin = dataName_sp$scientific_name
#     sp_french_simpld = dataName_sp$french_name_simplified
#   }else{
#     sp_french = sp_latin = sp_french_simpld = sp
#   }
# 
#   # Filter long term trends
#   dataLongTermTrendSp = dataLongTermTrend[dataLongTermTrend$species == sp,]
# 
#   # Filter short term trends
#   dataShortTermTrendSp = NULL
#   if(makeShortTrend){
#     dataShortTermTrendSp = dataShortTermTrend[dataShortTermTrend$species == sp,]
#   }
# 
#   # Make occurrence summary from observations
#   sumOccurrenceSp = makeSummaryTable(data, sp, interestVar)
#   sumOccurrenceSp_lastYear = makeSummaryTable(data, sp, interestVar, year = max(data$year))
#   sumOccurrenceSp = rbind(sumOccurrenceSp, sumOccurrenceSp_lastYear)
# 
#   AnnualSummarySp = makeAnnualSummaryTable(data, sp, interestVar)
# 
#   # Path to occurrence map
#   pathToMapSp = here::here("outputs", repo, "figures", "maps", paste0(sp, ".png"))
# 
#   # Path to trend plot
#   warningUncertainty = FALSE
#   warningNoPlot = FALSE
#   if(length(uncertainPlots[[sp]]$layers) > 0){
#     pathToPlotSp = here::here("outputs", repo, "figures", "trends", "uncertainty", paste0(sp, ".png"))
#   }else{
#     pathToPlotSp = here::here("outputs", repo, "figures", "trends", "regular", paste0(sp, ".png"))
# 
#     if(length(regularPlots[[sp]]$layers)> 0){
#       warningUncertainty = TRUE
#     }else{
#       warningNoPlot = TRUE
#     }
# 
#   }
#   # Make species pdf
#   rmarkdown::render(input = here::here("Rmd", "species_analysis.Rmd"),
# 
#                     # Spécifier les paramètres
#                     params = list(sp = sp,
#                                   sp_french = sp_french,
#                                   sp_latin = sp_latin,
#                                   obs = obs,
#                                   spatialScale = spatialScale,
#                                   sumOccurrenceSp = sumOccurrenceSp,
#                                   AnnualSummarySp = AnnualSummarySp,
#                                   dataLongTermTrendSp = dataLongTermTrendSp,
#                                   dataShortTermTrendSp = dataShortTermTrendSp,
#                                   pathToMapSp = pathToMapSp,
#                                   pathToPlotSp = pathToPlotSp,
#                                   warningUncertainty = warningUncertainty,
#                                   warningNoPlot = warningNoPlot),
# 
#                     # Spécifier le répertoire
#                     output_dir = pathToPdfSp,
# 
#                     # Spécifier le fichier
#                     output_file = sp_french_simpld)
# 
#   # Erase automatically created documents
#   unlink(here::here("outputs", repo, "pdf", "species", paste0(sp_french_simpld, ".tex")))
#   unlink(here::here("Rmd", paste0(sp_french_simpld, ".log")))
# 
# }

renderSpeciesReport <- function(sp, data, dataName, dataLongTermTrend, dataShortTermTrend,
                                makeShortTrend, uncertainPlots, regularPlots,
                                repo, interestVar, obs, spatialScale, pathToPdfSp) {

  # Extraire les noms français et scientifiques
  if(!is.null(dataName)){
    dataName_sp = dataName[dataName$species == sp,]
    if(nrow(dataName_sp) > 0){
      sp_french = dataName_sp$french_name
      sp_latin = dataName_sp$scientific_name
      sp_french_simpld = dataName_sp$french_name_simplified
    } else {
      sp_french <- sp_latin <- sp_french_simpld <- sp
    }
  } else {
    sp_french <- sp_latin <- sp_french_simpld <- sp
  }

  # Filtrer les tendances long terme
  dataLongTermTrendSp = dataLongTermTrend[dataLongTermTrend$species == sp,]

  # Filtrer les tendances court terme
  dataShortTermTrendSp = NULL
  if (makeShortTrend) {
    dataShortTermTrendSp = dataShortTermTrend[dataShortTermTrend$species == sp,]
  }

  # Résumés d'occurrence
  sumOccurrenceSp = makeSummaryTable(data, sp, interestVar)
  sumOccurrenceSp_lastYear = makeSummaryTable(data, sp, interestVar, year = max(data$year))
  sumOccurrenceSp = rbind(sumOccurrenceSp, sumOccurrenceSp_lastYear)

  AnnualSummarySp = makeAnnualSummaryTable(data, sp, interestVar)

  # Chemins vers les figures
  pathToMapSp = here::here("outputs", repo, "figures", "maps", paste0(sp, ".png"))

  warningUncertainty = FALSE
  warningNoPlot = FALSE
  if (length(uncertainPlots[[sp]]$layers) > 0) {
    pathToPlotSp = here::here("outputs", repo, "figures", "trends", "uncertainty", paste0(sp, ".png"))
  } else {
    pathToPlotSp = here::here("outputs", repo, "figures", "trends", "regular", paste0(sp, ".png"))
    if (length(regularPlots[[sp]]$layers) > 0) {
      warningUncertainty = TRUE
    } else {
      warningNoPlot = TRUE
    }
  }

  # Génération du rapport PDF via RMarkdown
  rmarkdown::render(
    input = here::here("Rmd", "species_analysis.Rmd"),
    params = list(
      sp = sp,
      sp_french = sp_french,
      sp_latin = sp_latin,
      obs = obs,
      spatialScale = spatialScale,
      sumOccurrenceSp = sumOccurrenceSp,
      AnnualSummarySp = AnnualSummarySp,
      dataLongTermTrendSp = dataLongTermTrendSp,
      dataShortTermTrendSp = dataShortTermTrendSp,
      pathToMapSp = pathToMapSp,
      pathToPlotSp = pathToPlotSp,
      warningUncertainty = warningUncertainty,
      warningNoPlot = warningNoPlot
    ),
    output_dir = pathToPdfSp,
    output_file = sp_french_simpld
  )

  # Nettoyage
  unlink(here::here("outputs", repo, "pdf", "species", paste0(sp_french_simpld, ".tex")))
  unlink(here::here("Rmd", paste0(sp_french_simpld, ".log")))
}

if (TRUE) {
  cat("Computing species trends sequentially. It might take a very long time !\n")

  for (sp in speciesList){
    cat(sp, "\n")

    renderSpeciesReport(
      sp = sp,
      data = data,
      dataName = dataName,
      dataLongTermTrend = dataLongTermTrend,
      dataShortTermTrend = dataShortTermTrend,
      makeShortTrend = makeShortTrend,
      uncertainPlots = uncertainPlots,
      regularPlots = regularPlots,
      repo = repo,
      interestVar = interestVar,
      obs = obs,
      spatialScale = spatialScale,
      pathToPdfSp = pathToPdfSp
    )

  }

} else {
  
  cat("Generation of pdf in parralel\n")
  library(parallelPackage, character.only = TRUE)
  cl <- start_cluster(as.numeric(nbCores), parallelPackage)

  try_parallel <- foreach(sp = speciesList,
                          .packages = c("rmarkdown", "here", "dplyr")) %dopar% {
                            devtools::load_all(here::here()) # Charger les fonctions

                            renderSpeciesReport(
                              sp = sp,
                              data = data,
                              dataName = dataName,
                              dataLongTermTrend = dataLongTermTrend,
                              dataShortTermTrend = dataShortTermTrend,
                              makeShortTrend = makeShortTrend,
                              uncertainPlots = uncertainPlots,
                              regularPlots = regularPlots,
                              repo = repo,
                              interestVar = interestVar,
                              obs = obs,
                              spatialScale = spatialScale,
                              pathToPdfSp = pathToPdfSp
                            )
                          }

  stop_cluster(cl, parallelPackage)
}

##########################
#   MAKE A SUMMARY PDF   #
##########################

# Path to group plot
pathToGroupPlot = here::here("outputs", repo, "figures", "group", "groupTrendPlot.png")

# Group composition by french name
if(!is.null(groupComp)){
  frenchComp = lapply(groupComp, function(g){
    
    # Find which species are in each group
    ind = match(g, dataLongTermTrend$species)
    
    # Attribute french 
    french_names = dataLongTermTrend$french_name[ind]
    return(french_names)
  })
  # Dans les cas ou une des espèces du group n'a pas de tendance énère NA
  # Test de suppression des NA
  frenchComp <- lapply(frenchComp, function(x) {
    x_noNA <- na.omit(x)
    as.vector(x_noNA)
  })
}else{
  # Find which species are in each group
  ind = match(speciesList, dataLongTermTrend$species)
  
  # Attribute french 
  frenchComp = dataLongTermTrend$french_name[ind]
}



# Format summary table for all species
dataTable = makeGroupSummaryTable(dataTrend = dataLongTermTrend, dataObs = data,
                                  makeGroupPlot, groupComp, groupNames, groupCols)

# Make global pdf
rmarkdown::render(input = here::here("Rmd", "global_analysis.Rmd"),
                  
                  # Specify parameters
                  params = list(obs = obs,
                                spatialScale = spatialScale,
                                makeGroupPlot = makeGroupPlot, 
                                groupComp = groupComp,
                                frenchComp = frenchComp,
                                groupNames = groupNames,
                                pathToGroupPlot = pathToGroupPlot,
                                dataLongTermTrend = dataLongTermTrend,
                                dataTable = dataTable),
                  
                  # Specify output repertory
                  output_dir = pathToPdf,
                  
                  # Specify output file name
                  output_file = "Analyse globale")

# Erase automatically created documents
unlink(here::here("outputs", repo, "pdf", "Analyse globale.tex"))
unlink(here::here("Rmd", "Analyse globale.log"))

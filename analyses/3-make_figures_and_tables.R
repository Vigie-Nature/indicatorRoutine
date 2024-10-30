# STEP 3 : CREATE TABLES / FIGURES 
cat("# TABLES AND FIGURES FOR EACH SPECIES #\n")

# Create directories ----
dir.create(path = here::here("outputs", repo, "figures"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "tables"), showWarnings = FALSE)

#########################
#   PRE-FORMAT TRENDS   #
#########################

# Long-term trends
cat("Formatting of long-term trends in progress")
dataLongTermTrend = formatTrendEstimates(data, speciesList, repo, "longTermTrend")
cat(" --> DONE\n")

# Yearly variations
cat("Formatting of yearly variations in progress")
dataYearlyVariations = formatYearlyVariations(speciesList, repo, "yearlyVariations", contr)
cat(" --> DONE\n")

# Short-term trends
dataShortTermTrend = NULL
if(makeShortTrend){
  cat("Formatting of short-term trends in progress")
  dataShortTermTrend = formatTrendEstimates(data[data$year > max(data$year)-10,], speciesList, repo, "shortTermTrend")
  cat(" --> DONE\n")
  
}
########################
#   MAKE TREND PLOTS   #
########################

# Make repository for saving trends plots
dir.create(path = here::here("outputs", repo, "figures", "trends"), showWarnings = FALSE)

# Plot trends with uncertainty
## Create repository
pathToPlot = here::here("outputs", repo, "figures", "trends", "uncertainty")
dir.create(path = pathToPlot, showWarnings = FALSE)

## For each species, create the plot
cat("Trend plots with uncertainty in progress...")
uncertainPlots = plotLinearTrends(speciesList = speciesList, data = data, dataName = dataName, dataLongTerm = dataLongTermTrend, 
                                    dataYearlyVariations = dataYearlyVariations, dataShortTerm = dataShortTermTrend,
                                    distribution = distribution, plotST = makeShortTrend, plotGamm = makeGammTrend, 
                                    N = 100, uncertainty = TRUE, weight = TRUE, save = TRUE, path = pathToPlot)

save(uncertainPlots, file = here::here("outputs", repo, "figures", "trends", "uncertainPlots.rdata"))

# Plot trends with no uncertainty
## Create repository
pathToPlot = here::here("outputs", repo, "figures", "trends", "regular")
dir.create(path = pathToPlot, showWarnings = FALSE)

## For each species, create the plot
cat("Trend plots without uncertainty in progress...")
regularPlots = plotLinearTrends(sp = speciesList, data = data, dataName = dataName, dataLongTerm = dataLongTermTrend, 
                                dataYearlyVariations = dataYearlyVariations, dataShortTerm = dataShortTermTrend,
                                distribution = distribution, plotST = makeShortTrend, plotGamm = makeGammTrend, 
                                N = 100, uncertainty = FALSE, weight = TRUE, save = TRUE, path = pathToPlot)

save(regularPlots, file = here::here("outputs", repo, "figures", "trends", "regularPlots.rdata"))

#################################################
#   MAKE SUMMARY OF LINEAR & QUADRATIC TRENDS   #
#################################################

# Create repository
pathToPlot = here::here("outputs", repo, "figures", "classification")
dir.create(path = pathToPlot, showWarnings = FALSE)

# Create filter for species with low annual occurrences

## Measure for each species the median of the number of sites per species x year
dataLowOcc = dplyr::group_by(data[data[,interestVar[1]] >0,], species, year) %>%
  dplyr::summarise(nbSite = dplyr::n()) %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(medYear = median(nbSite))

### Extract species with median occurrence inferior to 12
spToRemove = dataLowOcc$species[dataLowOcc$medYear < 12]



###   LINEAR   ###
cat("Linear trend classification in progress")
# For each species, match a type of linear trend
dataLinearClassif_LT = classifyLinearTrends(dataLongTermTrend, distribution, thresholdInf = 0.039, thresholdSup = 0.030)

if(makeShortTrend){
  dataLinearClassif_ST = classifyLinearTrends(dataShortTermTrend, distribution, thresholdInf = 0.039, thresholdSup = 0.030)
}
cat(" --> DONE\n")

# Plot a graph that sums up those quadratic trends
## Barplot
cat("Linear trend classification (barplot) in progress")
plotLinearClassification(dataLinearClassif_LT[!dataLinearClassif_LT$species %in% spToRemove,], distribution, type = "barplot", thresholdInf = 0.039, thresholdSup = 0.030, path = pathToPlot)
cat(" --> DONE\n")

## Errorbar
cat("Linear trend classification (errorbar) in progress")
plotLinearClassification(dataLinearClassif_LT[!dataLinearClassif_LT$species %in% spToRemove,], distribution, type = "errorbar", thresholdInf = 0.039, thresholdSup = 0.030, path = pathToPlot)
cat(" --> DONE\n")

###   QUADRATIC   ###
cat("Quadratic trends in progress...")
dataQuadrTrends <- NULL
if(makeQuadraticTrend){
  # Format quadratic trends into table
  cat("Formatting")
  dataRawQuadrTrend = formatTrendEstimates(data, speciesList[!speciesList %in% spToRemove], repo, "rawQuadraticTrend")
  dataOrthoQuadrTrend = formatTrendEstimates(data, speciesList[!speciesList %in% spToRemove], repo, "orthoQuadraticTrend")
  cat(" --> DONE\n")
  
  # For each species, match a type of quadratic trend
  cat("Classification")
  dataQuadrTrends = classifyQuadraticTrends(dataRawQuadrTrend, dataOrthoQuadrTrend)
  cat(" --> DONE\n")
  
  # Plot a graph that sums up those quadratic trends
  cat("Plot")
  plotQuadraticTrend(dataQuadrTrends, path = pathToPlot)
  cat(" --> DONE\n")
  
}


###################
#   SAVE TABLES   #
###################

# TRENDS
pathToRepo = here::here("outputs", repo, "tables", "trends")
dir.create(path = pathToRepo, showWarnings = FALSE)

# Long-term trends
saveTrendTable(dataLinearClassif_LT, dataName, dataQuadrTrends =  dataQuadrTrends, distribution, repo, fileName = "longTermTrends")
cat("Long-Term Trends saved\n")

# Yearly variations
saveTrendTable(dataYearlyVariations, dataName, dataQuadrTrends = NULL, distribution, repo, fileName = "yearlyVariations")
cat("Yearly Variations saved\n")

# Short-term trends
if(makeShortTrend){
  saveTrendTable(dataLinearClassif_ST, dataName, dataQuadrTrends = NULL, distribution, repo, fileName = "shortTermTrends")
  cat("Short-Term Trends saved\n")
  
}


#################
#   MAKE MAPS   #
#################

cat("Making map of occurrence in progress")

# Create directory for maps
pathToRepo = here::here("outputs", repo, "figures", "maps")
dir.create(path = pathToRepo, showWarnings = FALSE)

# Make plots for each species
maps = makeMap(data = data, speciesList = speciesList, interestVar = interestVar, path = pathToRepo)

cat(" --> DONE\n")

# STEP 2 : ESTIMATE TRENDS FOR EACH SPECIES
cat("# ESTIMATE TRENDS FOR ALL SPECIES #\n")

# Initialize all repositories ----
dir.create(path = here::here("outputs", repo), showWarnings = FALSE, recursive = T)
dir.create(path = here::here("outputs", repo, "models"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "longTermTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "shortTermTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "rawQuadraticTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "orthoQuadraticTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "yearlyVariations"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "gammVariations"), showWarnings = FALSE)

# Loop on species & make GLM / GAM ----

if (!parallelizeSpecies) {
  cat("Computing species trends sequentially. It might take a very long time !\n")

  for (sp in speciesList){
    cat(sp, "\n")
    
    dataSp <- data %>% 
      dplyr::filter(species == sp)
    
    estimateTrends(
      sp = sp,
      data = dataSp,
      repo = repo,
      interestVar = interestVar,
      fixedEffects = fixedEffects,
      factorVariables = factorVariables,
      randomEffects = randomEffects,
      nestedEffects = nestedEffects,
      slopeRandomEffects = slopeRandomEffects,
      poly = poly,
      contr = contr,
      distribution = distribution,
      makeShortTrend = makeShortTrend,
      makeQuadraticTrend = makeQuadraticTrend,
      makeGammTrend = makeGammTrend
    )
    
  }

} else {
  cat("Computing species trends in parallel. It might take a long time !\n")
  
  library(parallelPackage, character.only = T) # load correct library
  cl <- start_cluster(as.numeric(nbCores), parallelPackage) # spawn a cluster and register it
  
  split_data <- sapply(speciesList, function(s) {
    return(list(
      dataSp = data %>% 
        dplyr::filter(species == s),
      sp = s
    ))
  }, simplify = F)

  try_parallel <- foreach (
    sp_data = split_data,
    .packages = c("glmmTMB", "dplyr")
    ) %dopar% {
      devtools::load_all(here::here()) # load routine functions
      
      estimateTrends(
        sp = sp_data$sp,
        dataSp = sp_data$dataSp,
        repo = repo,
        interestVar = interestVar,
        fixedEffects = fixedEffects,
        factorVariables = factorVariables,
        randomEffects = randomEffects,
        nestedEffects = nestedEffects,
        slopeRandomEffects = slopeRandomEffects,
        poly = poly,
        contr = contr,
        distribution = distribution,
        makeShortTrend = makeShortTrend,
        makeQuadraticTrend = makeQuadraticTrend,
        makeGammTrend = makeGammTrend
      )
  }
  
  stop_cluster(cl, parallelPackage)
}

###########################
#   SAVE SPECIFICATIONS   #
###########################

# Make table specifications

## re-build helper variables

fixedEffects_var = fixedEffects[fixedEffects != "year"]
factorVariables_var = c(factorVariables, "year")
slopeRandomEffects_var = slopeRandomEffects[slopeRandomEffects != "year"]

# Build formula a second time, this time to include it in the report
formVar = writeFormula(
  interestVar,
  fixedEffects_var,
  factorVariables_var,
  poly,
  randomEffects,
  nestedEffects,
  slopeRandomEffects_var, raw = "FALSE"
)

## Long-term
dataSpecLT = makeSpecificationsTable(data, speciesList, interestVar, fixedEffects, factorVariables, 
                                     randomEffects, nestedEffects, slopeRandomEffects, poly, 
                                     repo, modelName = "longTermTrend")
## Short-term
dataSpecST = NULL
if(makeShortTrend){
  dataSpecST = makeSpecificationsTable(data, speciesList, interestVar, fixedEffects, factorVariables, 
                                       randomEffects, nestedEffects, slopeRandomEffects, poly, 
                                       repo, modelName = "shortTermTrend")
}

## Yearly-variations
dataSpecVar = makeSpecificationsTable(
  data,
  speciesList,
  interestVar,
  fixedEffects_var,
  factorVariables_var, 
  randomEffects,
  nestedEffects,
  slopeRandomEffects_var,
  poly,
  repo,
  modelName = "yearlyVariations"
)

# Create the specifications file
pathToSpec = here::here("outputs", repo)
rmarkdown::render(input = here::here("Rmd", "specifications.Rmd"),
                  
                  # Spécifier les paramètres
                  params = list(repo = repo,
                                obs = obs,
                                spatialScale = spatialScale,
                                data = data,
                                interestVar = interestVar,
                                distribution = distribution,
                                form = form,
                                formVar = formVar,
                                makeShortTrend = makeShortTrend,
                                makeGammTrend = makeGammTrend,
                                makeQuadraticTrend = makeQuadraticTrend,
                                dataSpecLT = dataSpecLT,
                                dataSpecST = dataSpecST,
                                dataSpecVar = dataSpecVar),
                  
                  # Spécifier le répertoire
                  output_dir = pathToSpec,
                  
                  # Spécifier le fichier
                  output_file = paste0("specifications"))

# STEP 2 : ESTIMATE TRENDS FOR EACH SPECIES
cat("# ESTIMATE TRENDS FOR ALL SPECIES #\n")

# Initialize all repositories ----
dir.create(path = here::here("outputs", repo), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "longTermTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "shortTermTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "rawQuadraticTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "orthoQuadraticTrend"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "yearlyVariations"), showWarnings = FALSE)
dir.create(path = here::here("outputs", repo, "models", "gammVariations"), showWarnings = FALSE)

# Loop on species & make GLM / GAM ----
for (sp in speciesList){
  cat(sp, "\n")
  
  ## Filter for considered species
  dataSp = data[data$species == sp, ]
  
  #######################
  #   LONG-TERM TREND   #
  #######################
  longTermTrend <- makeGLM(data = dataSp, interestVar = interestVar, fixedEffects = fixedEffects,
                           factorVariables = factorVariables, randomEffects = randomEffects,
                           nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects,
                           poly = poly, contr = contr, distribution = distribution, raw = "raw")
  
  cat("Long-Term Trend --> DONE\n")
  save(longTermTrend, file = here::here("outputs", repo, "models", "longTermTrend", paste0(sp, ".rdata")))
  
  #########################
  #   YEARLY VARIATIONS   #
  #########################
  
  # Change year from continuous to categorical effect
  fixedEffects_var = fixedEffects[fixedEffects != "year"]
  if(length(fixedEffects_var) == 0){
    fixedEffects_var = NULL
  }
  factorVariables_var = c(factorVariables, "year")
  
  # Erase the slope
  indSlope = grep("year", slopeRandomEffects)
  slopeRandomEffects_var = slopeRandomEffects[-indSlope]
  
  # Save the associated formula
  formVar = writeFormula(interestVar, fixedEffects_var, factorVariables_var, poly, randomEffects, nestedEffects, slopeRandomEffects_var, raw = "FALSE")
  
  # Make the model
  yearlyVariations <- makeGLM(data = dataSp, interestVar = interestVar, fixedEffects = fixedEffects_var,
                              factorVariables = factorVariables_var, randomEffects = randomEffects,
                              nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects_var, 
                              poly = poly, contr = contr, distribution = distribution)
  
  cat("Categorical Model --> DONE\n")
  save(yearlyVariations, file = here::here("outputs", repo, "models", "yearlyVariations", paste0(sp, ".rdata")))
  
  ########################
  #   SHORT-TERM TREND   #
  ########################  
  if(makeShortTrend){
    # Filter for the latest 10 years
    dataSp_ST = dataSp[dataSp$year >= max(dataSp$year) - 10,]
    
    # Make the model
    shortTermTrend <- makeGLM(data = dataSp_ST, interestVar = interestVar, fixedEffects = fixedEffects,
                              factorVariables = factorVariables, randomEffects = randomEffects,
                              nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects,
                              poly = poly, contr = contr,
                              distribution = distribution, raw = "raw")
    
    cat("Short-Term Trend --> DONE\n")
    save(shortTermTrend, file = here::here("outputs", repo, "models", "shortTermTrend", paste0(sp, ".rdata")))
    
  }
  
  ########################
  #   QUADRATIC TRENDS   #
  ########################
  if (makeQuadraticTrend){
    
    ## Change effect of year from linear to polynomial
    if(length(fixedEffects) == 1){ 
      fixedEffects_quadr = NULL
    }else{
      fixedEffects_quadr = fixedEffects[fixedEffects != "year"]
    }
    poly_quadr = c(poly, "year")
    
    ## Make orthogonal quadratic trend
    orthoQuadraticTrend <- makeGLM(data = dataSp, interestVar = interestVar, fixedEffects = fixedEffects_quadr,
                                   factorVariables = factorVariables, randomEffects = randomEffects,
                                   nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects,
                                   poly = poly_quadr, contr = contr, distribution = distribution, raw = "ortho")
    
    cat("Orthogonal Quadratic Trend --> DONE\n")
    save(orthoQuadraticTrend, file = here::here("outputs", repo, "models", "orthoQuadraticTrend", paste0(sp, ".rdata")))
    
    ## Make raw quadratic trend
    rawQuadraticTrend <- makeGLM(data = dataSp, interestVar = interestVar, fixedEffects = fixedEffects_quadr,
                                 factorVariables = factorVariables, randomEffects = randomEffects,
                                 nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects,
                                 poly = poly_quadr, contr = contr, distribution = distribution, raw = "raw")
    cat("Raw Quadratic Trend --> DONE\n")
    save(rawQuadraticTrend, file = here::here("outputs", repo, "models", "rawQuadraticTrend", paste0(sp, ".rdata")))
    
  }
  
  #######################
  #   GAMM VARIATIONS   #
  #######################
  if(makeGammTrend){
    gammVariations <- makeGAM(data = dataSp, interestVar = interestVar, fixedEffects = fixedEffects,
                              factorVariables = factorVariables, randomEffects = randomEffects,
                              nestedEffects = nestedEffects, poly = poly, distribution = distribution)
    cat("GAM Model --> DONE\n")
    save(gammVariations, file =  here::here("outputs", repo, "models", "gammVariations", paste0(sp, ".rdata")))
  }
  
}

###########################
#   SAVE SPECIFICATIONS   #
###########################

# Make table specifications
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
dataSpecVar = makeSpecificationsTable(data, speciesList, interestVar, fixedEffects_var, factorVariables_var, 
                                     randomEffects, nestedEffects, slopeRandomEffects_var, poly,
                                     repo, modelName = "yearlyVariations")

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

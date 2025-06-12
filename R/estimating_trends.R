estimateTrends <- function(
    sp,
    dataSp,
    repo,
    interestVar,
    fixedEffects,
    factorVariables,
    randomEffects,
    nestedEffects,
    slopeRandomEffects,
    poly,
    contr,
    distribution,
    makeShortTrend,
    makeQuadraticTrend,
    makeGammTrend
) {
  
  #######################
  #   LONG-TERM TREND   #
  #######################
  longTermTrend <- makeGLM(data = dataSp, interestVar = interestVar, fixedEffects = fixedEffects,
                           factorVariables = factorVariables, randomEffects = randomEffects,
                           nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects,
                           poly = poly, contr = contr, distribution = distribution, raw = "raw")
  
  cat("Long-Term Trend --> DONE\n")
  save(longTermTrend, file = here::here("outputs", repo, "models", "longTermTrend", paste0(sp, ".rdata")))
  rm(longTermTrend)
  
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
  rm(yearlyVariations)
  
  ########################
  #   SHORT-TERM TREND   #
  ########################  
  if(makeShortTrend){
    # Filter for the latest 10 years
    dataSp_ST <- dataSp[dataSp$year > max(dataSp$year) - 10, ]
    
    # if("point" %in% colnames(data)){
    #   sites_only_0 <- dataSp_ST %>%
    #     dplyr::group_by(site, point) %>%
    #     dplyr::summarize(total_interestVar = sum(.data[[interestVar]], na.rm = TRUE), .groups = "drop") %>%
    #     dplyr::filter(total_interestVar == 0) %>%
    #     dplyr::select(site, point)
    #   
    #   # Filtrer dataSp_ST pour enlever les couples site-point à 0
    #   dataSp_ST <- dplyr::anti_join(dataSp_ST, sites_only_0, by = c("site", "point"))
    #   
    #   # Filter to remove point with only one year of observation
    #   dataSp_ST <- dataSp_ST %>%
    #     dplyr::group_by(site,point) %>%
    #     dplyr::filter(n_distinct(year) > 1) %>%
    #     dplyr::ungroup()
    #   
    # } else {
    #   # Filter to clean sites with only 0 cause observation was prior the period
    #   sites_only_0 <- dataSp_ST %>%
    #     dplyr::group_by(site) %>%
    #     dplyr::summarize(total_interestVar = sum(.data[[interestVar]], na.rm = TRUE)) %>%
    #     dplyr::filter(total_interestVar == 0) %>%
    #     dplyr::pull(site)  # extrait simplement le vecteur des noms de site
    #   
    #   dataSp_ST <- dataSp_ST %>%
    #     dplyr::filter(!site %in% sites_only_0)
    #   
    #   # Filter to remove point with only one year of observation
    #   dataSp_ST <- dataSp_ST %>%
    #     dplyr::group_by(site) %>%
    #     dplyr::filter(n_distinct(year) > 1) %>%
    #     dplyr::ungroup()
    # }
    
    # Définir dynamiquement les colonnes de regroupement
    group_vars <- if ("point" %in% colnames(data)) c("site", "point") else "site"
    
    # Identification des sites (ou couples site-point) à 0 d'abondance
    sites_only_0 <- dataSp_ST %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      dplyr::summarize(total_interestVar = sum(.data[[interestVar]], na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(total_interestVar == 0)
    
    # Exclusion de ces sites
    if ("point" %in% colnames(dataSp_ST)) {
      dataSp_ST <- dplyr::anti_join(dataSp_ST, sites_only_0, by = c("site", "point"))
    } else {
      sites_only_0_vec <- sites_only_0$site
      dataSp_ST <- dplyr::filter(dataSp_ST, !site %in% sites_only_0_vec)
    }
    
    # Filtre des site (ou couples site-point) avec 1 année d'observation
    dataSp_ST <- dataSp_ST %>%
      dplyr::group_by(across(all_of(group_vars))) %>%
      dplyr::filter(dplyr::n_distinct(year) > 1) %>%
      dplyr::ungroup()
    
    #write.csv(dataSp_ST, file = here::here("outputs", repo, "models", "shortTermTrend", paste0(sp, ".csv")))
    # Make the model
    shortTermTrend <- makeGLM(data = dataSp_ST, interestVar = interestVar, fixedEffects = fixedEffects,
                              factorVariables = factorVariables, randomEffects = randomEffects,
                              nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects,
                              poly = poly, contr = contr,
                              distribution = distribution, raw = "raw")
    
    cat("Short-Term Trend --> DONE\n")
    save(shortTermTrend, file = here::here("outputs", repo, "models", "shortTermTrend", paste0(sp, ".rdata")))
    rm(dataSp_ST, shortTermTrend)
    
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
    rm(orthoQuadraticTrend)
    
    ## Make raw quadratic trend
    rawQuadraticTrend <- makeGLM(data = dataSp, interestVar = interestVar, fixedEffects = fixedEffects_quadr,
                                 factorVariables = factorVariables, randomEffects = randomEffects,
                                 nestedEffects = nestedEffects, slopeRandomEffects = slopeRandomEffects,
                                 poly = poly_quadr, contr = contr, distribution = distribution, raw = "raw")
    cat("Raw Quadratic Trend --> DONE\n")
    save(rawQuadraticTrend, file = here::here("outputs", repo, "models", "rawQuadraticTrend", paste0(sp, ".rdata")))
    rm(rawQuadraticTrend)
    
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
    rm(gammVariations)
  }
}
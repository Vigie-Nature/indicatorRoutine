#' classifyQuadraticTrends
#'
#' Classify species between different quadratic trends, based on estimates from raw and orthogonal quadratic trends
#' 
#' @param dataRaw : a `data.frame` containing formatted estimates from raw quadratic trends for each species 
#' @param dataOrtho : a `data.frame` containing formatted estimates from orthogonal quadratic trends for each species
#'
classifyQuadraticTrends <- function(dataRaw, dataOrtho){
  ###################
  # DATA FORMATTING #
  ###################
  # Erase NA
  dataRaw = dataRaw[!is.na(dataRaw$estimate),]
  dataOrtho = dataOrtho[!is.na(dataOrtho$estimate) & 
                          dataOrtho$species %in% unique(dataRaw$species),]
  
  # Filter dataOrtho for second order coefficients
  dataOrtho = dataOrtho[dataOrtho$coef == 2,]
  
  # Separate dataRaw in 1st and 2nd order coefficients 
  dataRaw1 = dataRaw[dataRaw$coef == 1,]
  dataRaw2 = dataRaw[dataRaw$coef == 2,]
  
  ########################
  # LINEAR-TREND SPECIES #
  ########################
  
  # Sub-list of species between linear trend
  spLinear = dataOrtho$species[dataOrtho$pval >= 0.05]
  
  # Sub-list of species between linear trend
  dataRaw1 = dataRaw1[match(spLinear, dataRaw1$species),] 
  
  # Deal with no match
  if(nrow(dataRaw1) == 0){
    dataRaw1 = data.frame()
  }else{
    
    # Classify between linear increase, stable, decrease
    dataRaw1$trend = "Stable linéaire"
    dataRaw1$trend[dataRaw1$pval < 0.05 & dataRaw1$estimate > 0] = "Augmentation linéaire"
    dataRaw1$trend[dataRaw1$pval < 0.05 & dataRaw1$estimate < 0] = "Déclin linéaire"
    
    # Filter data.frame for only species and trend classification
    dataRaw1 = dataRaw1[,c("species", "trend")]
  }
  
  
  ###########################
  # QUADRATIC-TREND SPECIES #
  ###########################
  
  # Sub-list of species between quadratic trend
  spNonLinear = dataOrtho$species[dataOrtho$pval < 0.05]
  
  # DIRECTION #
  
  # Calculate derivative for at quantile 25% of the temporal trend
  derivative25 = sapply(spNonLinear, function(sp) quadraticDerivative(dataRaw, sp, perc = .25))
  # Calculate derivative for at quantile 75% of the temporal trend
  derivative75 = sapply(spNonLinear, function(sp) quadraticDerivative(dataRaw, sp, perc = .75))
  
  # Filter data.frame for non-linear trend species
  dataRaw2 = dataRaw2[match(spNonLinear, dataRaw2$species),]
  
  # Deal with no match
  if(nrow(dataRaw2) == 0){
    dataRaw2 = data.frame()
    
    }else{
    # Classify between stable / increase / decrease according to change and sign of derivatives
    dataRaw2$trend = "Stable"
    dataRaw2$trend[which(derivative25 > 0 & derivative75 > 0)] = "Augmentation"
    dataRaw2$trend[which(derivative25 < 0 & derivative75 < 0)] = "Déclin"
    
    # For stable trends, classify between convex/concave using the sign of the 1st order estimate 
    dataRaw2$trend[dataRaw2$trend == "Stable" & dataRaw2$estimate > 0] = "Convexe"
    dataRaw2$trend[dataRaw2$trend == "Stable" & dataRaw2$estimate < 0] = "Concave"
    
    # ACCELERATION #
    # Extract sign of 2nd order estimate
    coefSign = dataRaw2$estimate[dataRaw2$trend %in% c("Augmentation", "Déclin")]
    coefSign = coefSign / abs(coefSign)
    
    # Extract sign of curvature 1st derivative
    curvatureSign = sapply(dataRaw2$species[dataRaw2$trend %in% c("Augmentation", "Déclin")], 
                           function(sp) quadraticCurve(dataRaw, sp))
    
    # Calculate the product of 2nd order estimate and curvature 1st derivative
    productSign = coefSign * curvatureSign
    
    # If the sign of the product is negative, classify in acceleration
    spAcc = names(productSign)[which(productSign < 0)]
    dataRaw2$trend[dataRaw2$species %in% spAcc] = paste(dataRaw2$trend[dataRaw2$species %in% spAcc], "accéléré")
    
    # If the sign of the product is positive, classify in acceleration
    spDec = names(productSign)[which(productSign > 0)]
    dataRaw2$trend[dataRaw2$species %in% spDec] = paste(dataRaw2$trend[dataRaw2$species %in% spDec], "ralenti")
    
    # Add and 'e' to 'accéléré' and 'ralenti' when associated with 'Accélération'
    dataRaw2$trend[dataRaw2$trend == 'Augmentation accéléré'] = paste0(dataRaw2$trend[dataRaw2$trend == 'Augmentation accéléré'], "e")
    dataRaw2$trend[dataRaw2$trend == 'Augmentation ralenti'] = paste0(dataRaw2$trend[dataRaw2$trend == 'Augmentation ralenti'], "e")
    
    # Filter data.frame for only species and trend classification
    dataRaw2 = dataRaw2[,c("species", "trend")]
  }
  
  
  # Bind linear and quadratic classification
  dataRes = rbind(dataRaw1, dataRaw2)

  # Erase rownames
  rownames(dataRes) = NULL
  
  return(dataRes)
}
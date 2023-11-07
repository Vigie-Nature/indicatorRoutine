########################################
#   STEP 1 : IMPORT AND PREPARE DATA   #
########################################
cat("# IMPORT AND CLEAN DATA #\n")

#######################################
#   IMPORT AND FORMAT COUNTING DATA   #
#######################################

# Path to data ----
dataPath <- here::here("data", repo, "countingData.csv")

# Read data ----
data <- data.table::fread(file = dataPath, encoding = "UTF-8", drop = "V1")

# Turn data to dataframe ----
data <- data.frame(data)

# Check all columns of interest are in the dataframe ----
check <- checkData(data, interestVar, fixedEffects, factorVariables, 
                   randomEffects, nestedEffects, poly)

# Filter data according to species & time ----
data <- formatData(data, yearRange, interestVar, fixedEffects, factorVariables, 
                   randomEffects, nestedEffects, slopeRandomEffects, poly)

##########################
#   MISSING PARAMETERS   #
##########################

# If required, initialize speciesList ----
if(is.null(speciesList)){
  grData = dplyr::group_by(data[data[,interestVar[1]]>0,], species) %>%
    dplyr::summarise(nbOcc = length(unique(ID)),
                     nbYear = 5*length(min(year):max(year)))
  
  speciesList <- grData$species[grData$nbOcc > grData$nbYear]

}

# If required, initialize distribution ----
if(is.null(distribution)){
  distribution <- detectDistrib(data, interestVar)
  cat("DISTRIBUTIONS SPECIFICATIONS:", distribution, "\n")
}

# Synthethise the required formula ----
cat("MODEL SPECIFICATIONS:\n")
form = writeFormula(interestVar, fixedEffects, factorVariables, poly, randomEffects, 
                    nestedEffects, slopeRandomEffects, raw = "FALSE")
cat(as.character(form[2]), as.character(form[1]), as.character(form[3]), "\n")

#####################
#   FILL ABSENCES   #
#####################

data <- fillAbsence(data, interestVar, speciesList, method = "once")

################################
#   NAME CORRESPONDANCE DATA   #
################################

# Path to data ----
dataPath <- here::here("data", repo, "speciesName.csv")

dataName <- NULL
if(file.exists(dataPath)){
  # Read data ----
  dataName <- data.table::fread(file = dataPath, encoding = "UTF-8", drop = "V1", header = TRUE)
  
  # Match between the 3 mandatory columns and the dataframe
  indCols = match(c("species", "french_name", "scientific_name"), colnames(dataName))
  
  # If missing, 
  if(any(is.na(indCols))){
    dataName <- NULL
    message("speciesName.csv must contain at least 3 columns: species, french_name and scientific_name")
  }
}

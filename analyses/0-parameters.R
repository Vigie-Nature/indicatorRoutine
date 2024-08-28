# STEP 0 : CHOOSE PARAMETERS
cat("# LOAD PARAMETERS #\n")

###########################
#   Paramètres généraux   #
###########################

# Nom du répertoire où les données sont mises (sans caractères spéciaux)
# Exemple : "STOC_2023_Ile_de_France"
repo <- "test"

# Nom du jeu de données .csv à charger (sans caractères spéciaux)
# Exemple : "countingData.csv"
Data <- "test.csv"

# Nom de l'observatoire étudié (sans caractères spéciaux)
obs <- "random"

# Nom de l'échelle d'observation (sans caractères spéciaux)
# Exemple : "France" ou "Bretagne" ou "ONF" ou ...
spatialScale <- "random"

# Est-ce que les absences (0) doivent être générées par la routine ?
createAbsence <- TRUE # FALSE

# Est-ce que la tendance court-terme doit être réalisée ?
makeShortTrend <- TRUE # FALSE

# Est-ce que le gamm doit être réalisé ?
makeGammTrend <- TRUE # FALSE

# Est-ce que les tendances quadratiques doivent être réalisées ?
makeQuadraticTrend <- TRUE # FALSE

# Est-ce qu'un graphe résumé par groupe d'espèces doit être réalisé ?
makeGroupPlot <- FALSE # TRUE

# Est-ce que le pdf doit être créé ?
makePDF <- TRUE # FALSE

###########################
# Optionnel : performance #
###########################

# Les espèces doivent-elles être traitées en parallèle ?
parallelizeSpecies <- FALSE # TRUE

# Si oui, combien de processeurs sont à disposition ?
nbCores <- 2

# Quel est le package à utiliser pour foreach()%dopar%{...} ?
parallelPackage <- "doParallel" # some clusters prefer "doMPI"

#########################
# Formatter les données #
#########################

# Quelles espèces souhaitez-vous analyser ?
# Si NULL, toutes les espèces sont analysées
speciesList <- NULL # c("ERIRUB", "PASDOM")

# Quelles sont les dates de début et de fin à prendre en compte ? 
# Si NULL, toutes les années sont utilisées
yearRange <- NULL # c(2001, 2021)

##########################
# Construire les modèles #
##########################

# Quelle est la distribution à appliquer ?
# (e.g: gaussian, poisson, nbinom2, binomial, betabinomial)
# Si NULL, la routine la déduit automatiquement
distribution <- NULL

# Quelle est la variable à expliquer ?
interestVar <- "abundance" # c("abond_cumul", "fail")

# Quels sont les effets fixes continus / numériques à considérer ?
fixedEffects <-  c("year")

# Quelles sont les effets fixes catégoriels à considérer (hors year) ?
# NULL, si pas de variables catégorielles
factorVariables <- c("habitat")

# Quels est le niveau de référence pour la variable  ?
# Si NULL, automatiquement mis à la moyenne des années
# Sinon, indiquer un niveau e.g, 2001, 2010, ...
contr <- 'mean'

# Quels sont les effets aléatoires à considérer ?
# NULL si pas d'effets aléatoires simples à considérer
randomEffects <- NULL # c("r_resp")

# Quels sont les effets emboîtés à considérer ?
# list() si pas d'effets emboîtés
# NB : si une variable est renseignée dans l'effet emboîté, elle ne doit pas être mise dans l'effet aléatoire
nestedEffects <- list(c("point", "site"))

# Quels sont les effets de pentes aléatoires à considérer ?
# list() si pas de pentes aléatoires
slopeRandomEffects <- list(c("year", "point"))

# Quels sont les effets polynomiaux de degré 2 à considérer ?
# NULL si pas d'effet polynomial
# NB : si une variable est renseignée dans l'effet polynomial, elle ne doit pas être mise dans les effets fixes continus
poly <- NULL


#####################################
#   Définir les groupes d'espèces   #
#####################################
# A remplir si makeGroupPlot == TRUE

# Nom du/des groupes d'espèces
# NULL si toutes les espèces doivent être mises dans le même groupe
groupNames <- NULL # c("Généralistes", "Spécialistes agricoles", "Spécialistes forestiers", "Spécialistes urbains") 

# Composition du/des groupes d'espèces
# NULL si toutes les espèces doivent être mises dans le même groupe
groupComp <- NULL # list(c("COLPAL", "CUCCAN", "PICVIR", "SYLATR", "FRICOE", "LUSMEG",
                  #  "TURMER","PRUMOD", "ORIORI", "PARMAJ", "PARCAE", "CORCOR",
                  #  "GARGLA", "HIPPOL"),

                  # c("VANVAN", "BUTBUT", "FALTIN", "ALERUF", "PERPER", "COTCOT",
                  #  "UPUEPO", "ALAARV", "LULARB", "GALCRI", "ANTPRA", "ANTCAM",
                  #  "MOTFLA", "SYLCOM", "SAXTOR", "SAXRUB", "LANCOL",
                  #  "CORFRU", "CARCAN", "EMBCIT", "EMBCIR", "MILCAL", "EMBHOR"), # OENOEN

                  # c("DENMAJ", "DENMED", "DRYMAR", "SYLMEL", "PHYBON",
                  #   "PHYSIB", "PHYCOL", "PHYTRO", "REGREG", "REGIGN", "SITEUR",
                  #  "CERBRA", "CERFAM", "TROTRO", "TURPHI", "TURVIS", "ERIRUB",
                  #  "PARCRI", "PARATE", "PARPAL", "PARMON", "COCCOC", "PYRPYR"), # PICCAN

                  # c("STRDEC", "APUAPU", "DELURB", "HIRRUS", "PHOOCH", "PHOPHO",
                  #  "CORMON", "PICPIC", "CARCAR", "CARCHL", "SERSER", "PASDOM",
                  #  "PASMON"))

# Couleurs associées aux groupes
# NULL si pas de couleurs associées
groupCols <- NULL # c("#1FA3D4","#F3AA20", "#235D3A", "#58094F") 

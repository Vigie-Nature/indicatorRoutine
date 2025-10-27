#!/usr/bin/env Rscript


cat(here::here())
renv::restore()
devtools::load_all(here::here())
set.seed(42)


# Récupère les arguments depuis la ligne de commande
args <- commandArgs(trailingOnly = TRUE)
rds_path <- args[1] 

# Lire les paramètres tels quels
params <- readRDS(rds_path)

# On peut les passer directement
gammVariations <- makeGAM2(
  data           = get(load(params$data_path)),  # charge le data.frame
  interestVar    = params$interestVar,
  fixedEffects   = params$fixedEffects,
  factorVariables = params$factorVariables,
  randomEffects  = params$randomEffects,
  nestedEffects  = params$nestedEffects,
  poly           = params$poly,
  distribution   = params$distribution,
  sp             = params$species,
  repo           = params$repo
)

# Sauvegarde
save(gammVariations, file = params$output_path)
#' indicatorRoutine: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Mathilde Vimont \email{mathilde.vimont@gmail.com}
#' 
#' @date 2022/11/09

if(system.file(package='devtools') == ""){ install.packages("devtools") }

## DEPENDENCIES (see DESCRIPTION) ##
devtools::install_deps(upgrade = "never")

## PACKAGES AND FUNCTIONS ## 
devtools::load_all(here::here())
`%>%` = magrittr::`%>%`

## RUN PROJECT ##
# Load parameters
source(here::here("analyses", "0-parameters.R"))

# Import and format data
source(here::here("analyses", "1-import_and_clean_data.R"))

# Estimate trends
source(here::here("analyses", "2-estimate_trends.R"))

# Make figures and tables for each species
source(here::here("analyses", "3-make_figures_and_tables.R"))

# Make figures and tables for groups of species
if(makeGroupPlot){
  source(here::here("analyses", "4-analyze_trends_per_group.R"))
}

# Create summary pdf
if(makePDF){
  source(here::here("analyses", "5-create_pdf.R"))
}



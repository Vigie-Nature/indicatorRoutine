#' saveTrendTable
#'
#' Save the table containing all specific trends
#'
#' @param data : a `data.frame` containing formatted trends estimations and classification
#' @param dataName : a `data.frame` containing species french names
#' @param dataQuadrTrends : a `data.frame` containing formatted quadratic trends estimations and classification
#' @param distribution : a `string` specifying the distribution law of the model
#' @param repo : a `string` specifying the name of the repository
#' @param fileName : a `string` either 'longTermTrends', 'shortTermTrends' or 'yearlyVariations'
#'
saveTrendTable <- function(data, dataName = NULL, dataQuadrTrends, distribution, repo, fileName = ""){
  
  # Merge data with dataName if dataName exists
  if(is.null(dataName)){
    data$french_name = data$species
    
  }else{
    data = merge(data, dataName, by = "species")
  }
  if(fileName %in% c("longTermTrends", "shortTermTrends")){
    
    # Add growth rate
    data[,c("GR","infGR", "supGR")] = exp(data[,c("estimate", "infIC", "supIC")])
    
    ###########################
    # PERCENTAGE OF EVOLUTION #
    ###########################
    
    # Extract estimates
    allEst = data[,c("estimate", "infIC", "supIC")]
    
    # Calculate value for first year
    start = allEst * 0
    
    # Calculate Value for relative last year
    stop = allEst * (data$maxYear - data$minYear) 
    
    # Turn to exponential if counting or occurrence data
    if(distribution != "gaussian"){
      start = exp(start)
      stop = exp(stop)
    }
    
    # Measure the percentage of difference
    dataPerc = round(100 * (stop- start), 1)
    
    # Change to text
    dataPerc = apply(dataPerc, 2, function(x){
      return(ifelse(x > 0, paste0("+", x, "%"), paste0(x, "%")))
    })
    
    if(length(speciesList) == 1){
      dataPerc = data.frame(t(data.frame(dataPerc)))
    }
    
    # Rename columns
    colnames(dataPerc) = c("perc", "infPerc", "supPerc")
    
    # Bind both data.frames
    data = cbind(data, dataPerc)
    
    ####################
    # RANK OF ESTIMATE #
    ####################
    
    # Arrange data by decreasing estimate value
    data = dplyr::arrange(data, desc(estimate))
    
    # Initialize the rank column
    data$rank = NA
    
    # Add the rank
    data[!is.na(data$estimate),]$rank = 1:nrow(data[!is.na(data$estimate),])
    
    ###########################
    # ADD EBCC Classification #
    ###########################
    
    data$trendEBCC = sapply(data$species, function(sp) affectEBCCTrend(data, sp))
    
    #######################
    # ADD QUADRATIC TREND #
    #######################
    
    colQuadr <- c()
    if(!is.null(dataQuadrTrends) & fileName == "longTermTrends"){
      # Change colname of quadratic trends to avoid duplicates 
      colnames(dataQuadrTrends)[2] <- "quadraticTrend"
      
      # Merge both dataframes
      data <- merge(data, dataQuadrTrends, by = "species", all.x = TRUE)
      
      colQuadr <- "quadraticTrend"
    }
    
    ######################
    # COLUMNS FORMATTING #
    ######################
    
    # Names of columns to keep, in order
    colToKeep = c("french_name", "species", "minYear", "maxYear", "estimate", "se",
                  "infIC", "supIC", "GR","infGR", "supGR", "perc", "infPerc", "supPerc", 
                  "pval", "trend", "trendEBCC", colQuadr)
    
    # Indices of existing columns to keep
    indCol = match(colToKeep, colnames(data))
    
    # Reorder columns
    data = data[,indCol]
    
    
  }else if(fileName == "yearlyVariations"){
    
    # Filter for column of interest
    data = data[,c("french_name", "species", "year", "estimate", "se", "infIC", "supIC", "pval")]
    
    # Add growth rate
    data[,c("GR","infGR", "supGR")] = exp(data[,c("estimate", "infIC", "supIC")])
    
    # Reorder column names
    data = data[,c("french_name", "species", "year", "estimate", "se", "infIC", "supIC", "GR","infGR", "supGR", "pval")]
    
  }
  
  # Save the table
  write.csv(x = data, file = here::here("outputs", repo, "tables", 'trends', paste0(fileName, ".csv")))
  
  # return(data)
}




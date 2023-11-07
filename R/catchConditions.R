#' catchConditions
#'
#' A function that executes codes and saves all warnings/errors encountered
#'
#' @param expr an expression that could be raising warnings or errors
#'
#' @return a list of 3 objects :
#' - value, an object resulting from the expression when possible
#' - warnings, a `vector` of messages associated with the warnings. Can be `NULL`.
#' - error, a `string` containing the message associated with the error. Can be `NULL`.
#' 
#' @example
catchConditions <- function(expr) {
  
  # Initialisation of the results, warning and error variables
  resValue <- NULL
  warnValue <- NULL
  errValue <- NULL
  
  # Function that saves all encountered warnings 
  wHandler <- function(w) {
    warnValue <<- c(warnValue, w$message)
    invokeRestart("muffleWarning")
  }
  
  # Function that saves encountered error  
  eHandler <- function(e) {
    errValue <<- e$message
    NULL
  }
  
  # Execute expression and saves warnings/errors if encountered
  resValue <- tryCatch(withCallingHandlers(expr, warning = wHandler), error = eHandler)
  
  return(list(value = resValue, 
              warnings = warnValue, 
              error = errValue))
  
} 

########
# TEST #
########

# test_that("catchConditions handles multiple warnings", {
#   expect_equal(catchConditions())
# })
# 
# test_that("catchConditions handles simple regular, warning and error condition", {
#   expect_equal(catchConditions("this should be saved as value"), 
#                list(value = "this should be saved as value",
#                     warnings = NULL,
#                     error = NULL))
#   expect_equal(catchConditions(warning("this should be saved as value and warning")), 
#                list(value = "this should be saved as value and warning",
#                     warnings = "this should be saved as value and warning",
#                     error = NULL))
#   expect_equal(catchConditions(stop("this should be saved as error")), 
#                list(value = NULL,
#                     warnings = NULL,
#                     error = "this should be saved as error"))
# })

#' Create a cluster object with the suited parallelization package
#'
#' @param nbcores number of required cores. Not needed with `package ="doMPI"`
#' @param package `"doParallel"` for use on a local machine, `"doMPI"` for use on
#' an HPC cluster that uses the Message Passing Interface library
start_cluster <- function(nbcores, package) {
  if (package == "doParallel") {
    
    cl <- parallel::makeCluster(nbcores)
    doParallel::registerDoParallel(cl)
    
  } else if (package == "doMPI") {
    
    cl <- doMPI::startMPIcluster()
    doMPI::registerDoMPI(cl)
    
  }
  
  return(cl)
}

#' Close a cluster object, with the suited package
stop_cluster <- function(cl, package) {
  if (package == "doParallel") {
    
    parallel::stopCluster(cl)
    
  } else if (package == "doMPI") {
    
    doMPI::closeCluster(cl)
    Rmpi::mpi.finalize()
    
  }
}

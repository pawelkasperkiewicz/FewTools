JSON_to_DF <- function(col) {
  library(doSNOW) 
  library(foreach) 
  library(parallel)
  library(tidyverse)
  library(jsonlite)
  cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))
  cl<-makeCluster(cores)
  registerDoSNOW(cl)
  nr <- length(col)
  chunk <- floor(nr/cores)
  s <- rep(1:cores, each = chunk)
  s <- c(s, rep(cores,nr-length(s)))
  for(i in 1:cores) {
    col.core <- col[s==i]
    clusterExport(cl[i], 'col.core', envir = environment())
  }
  col.df <- foreach(i=1:cores, .combine = 'bind_rows', .noexport = 'col.core', .packages = c('jsonlite', 'rlist', 'magrittr')) %dopar% {
    list.stack(lapply(col.core, function(j){
      as.list(unlist(fromJSON(j)))}) , fill=TRUE)
  }
  stopCluster(cl)
  return(as.data.frame(col.df))
}

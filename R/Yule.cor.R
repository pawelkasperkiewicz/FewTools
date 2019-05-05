Yule.cor <- function(vector1, vector2) {
  f <- chisq.test(vector1, vector2, correct = FALSE, simulate.p.value = FALSE)
  
  Yule <- sqrt(f$statistic / sum(f$observed))
  
  return(Yule)

}
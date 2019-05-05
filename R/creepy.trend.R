creepy.trend <- function(vector, k, parallel = FALSE) {
  
  
  if (parallel) {
    
    library(doParallel)
    Clust <- makeCluster (detectCores(), type = "PSOCK")
    registerDoParallel(Clust)
    table <- data.frame(matrix(nrow = length(vector), ncol = (length(vector) - k + 1)))
    X <- 1:length(vector)
    smoothed.values <- c()
    
    table <- foreach (i = 1:(length(vector) - k + 1), .combine = data.frame) %dopar%  {
      
      model <- lm(vector[i:(k + i - 1)] ~ X[i:(k + i - 1)])
      begin <- sample(NA, i - 1, replace = TRUE)
      end <- sample(NA, length(vector) - k - length(begin), replace = TRUE)
      table <- c(begin, model$fitted.values, end)
    }
    
    smoothed.values <- apply(table, 1, mean, na.rm = TRUE) 
   
    return(smoothed.values)
  }
  
  
  else {
    table <- data.frame()
    X <- 1:length(vector)
    smoothed.values <- c()
    
    for (i in 1:(length(vector) - k + 1)) {
      
      model <- lm(vector[i:(k + i - 1)] ~ X[i:(k + i - 1)])
      
      table[i:(i+k-1), i] <- model$fitted.values
    }
    
    smoothed.values <- apply(table, 1, mean, na.rm = TRUE)
    
    return(smoothed.values)
  }
}

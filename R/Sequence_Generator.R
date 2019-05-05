Sequences_Generator <- function(data, target, lookback, horizon, min_index, max_index,
                                     batch_size = 128, step = 1, forecast_length = 1, shuffle = FALSE, return_target = TRUE) {
  
  #Generator for training deep neural network with keras and TensorFlow backend
  
  #data - data.frame or data.matrix with predictors [data.frame, data.matrix]
  #target - vector of corresponding targets with the same length as data [vector]
  #lookback - numbers of observations lookback for recurrent networks [int]
  #horizon - how many steps ahead forecast should be started + 1. Zero implies forecast starts from next observation after training set [int]
  #min_index - min index what should to use from data and target [int]
  #max index - max index what should to use from data and target [int, NULL]
  #batch size - batch size for network [int]
  #step - size of whole between next batch [int]
  #forecast_length - how many steps from horizon forecast should cover [int]
  
  
  data <- data.matrix(data)
  
  if (is.null(max_index))
    max_index <- nrow(data) - horizon - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    if(return_target)
      targets <- array(0, dim = c(length(rows), forecast_length))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      if(return_target)
        targets[j,] <- target[(rows[[j]] + horizon):(rows[[j]] + horizon + forecast_length - 1)]
    }           
    if(return_target){
    list(samples, targets)
    } else{
      list(samples)
      }
  }
}



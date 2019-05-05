find.best.p.logit <- function(model) {

#model - logit model to prediction
#emp_data - data to test fit
#data_for_predict - data to find best propability for best fit
#The returned p is point for using "predict_0_1 <- ifelse(predict > p, 1, 0)"
 
  
  
a.p <- 0
a.score <- 0
fit_1_0 <- 0
Ans <- data.frame(p = NA, fit = NA)


for (i in 1:100) {
  
  
  a.p <- a.p + 0.01
  
  fit_1_0 <- ifelse(model$fitted.values > a.p, 1, 0)
  
  a.score <- mean(fit_1_0 == model$y)
  
  Ans[i, c(1,2)] <- c(a.p, a.score)
  

}

Ans <- Ans[order(Ans$fit, decreasing = T), ]
Ans <- 

return(Ans[1:10, ])
}
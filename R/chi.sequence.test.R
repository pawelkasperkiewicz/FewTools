chi.sequence.test <- function(n_var_base, data_frame, Yates_correct = FALSE, Monte_Carlo = FALSE, n_iter_MC ) {
  
  for (i in 1:ncol(data_frame)) {
    b <- chisq.test(data_frame[ , n_var_base], data_frame[ ,i], correct = Yates_correct, simulate.p.value = Monte_Carlo, B = n_iter_MC)
    cat(names(data_frame)[n_var_base], " with ", names(data_frame[i]), " = ", b$p.value, "\n")
  }
}
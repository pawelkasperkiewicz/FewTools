rm_if_is_in <- function(to_clean, pattern, unique_ncol) {

# Remove values form to_clean what existing in pattern. Chcecking is only about unique_ncol (number of column)  


a = 0
czy_jest = 0
lista_jest = 0

for (i in 1:nrow(to_clean)) {
  czy_jest_nr <- match(to_clean[i, unique_ncol], pattern[ ,unique_ncol], nomatch = 0 )
  
  if (czy_jest_nr > 0) {
    a = a + 1
    lista_jest[a] <- czy_jest_nr
  }
  
}
return(to_clean[-lista_jest, ])
}
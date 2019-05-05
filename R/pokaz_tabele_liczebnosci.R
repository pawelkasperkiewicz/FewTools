a <- list(0)

for (i in 1:9) {
  
  a[[i]] <- table(MACJ1.1$Wynik.z.testu, MACJ1.1[ ,i])
}

for (j in 1:9) {
  print (a[[j]])
}
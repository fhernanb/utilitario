# Las funciones de abajo permiten generar observaciones aleatorias
# de una mezcla de dos normales

myrMixNor <- function(n, varianza) {
  var_gen <- 0.4
  d <- sqrt((varianza - var_gen)/0.25)
  mus <- c(-d * 0.5, d * 0.5)
  desvs <- sqrt(c(var_gen, var_gen))
  components <- sample(1:2, prob=c(0.5, 0.5), size=n, replace=TRUE)
  rnorm(n=n, mean=mus[components], sd=desvs[components])
}


#y <- myrMixNor(n=10000, varianza=5)
#plot(density(y))
#mean(y)
#var(y)

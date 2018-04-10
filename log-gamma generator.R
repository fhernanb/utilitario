# Las funciones de abajo permiten generar observaciones aleatorias
# de una distribucion log-gamma

mydlgamma <- function(y, k) (1/k)^(1/k)*exp((y-exp(y))/k)/gamma(1/k)

esp <- function(k) {
  aux1 <- function(x, k) x * mydlgamma(x, k)
  integrate(aux1, -Inf, Inf, k=k)$value
}

vari <- function(k) {
  aux1 <- function(x, k) x * mydlgamma(x, k)
  aux2 <- function(x, k) x^2 * mydlgamma(x, k)
  integrate(aux2, -Inf, Inf, k=k)$value - 
    integrate(aux1, -Inf, Inf, k=k)$value^2
}

vari <- Vectorize(vari)

find.k <- function(varianza) {
  aux3 <- function(k, varianza) vari(k) - varianza
  uniroot(aux3, varianza=varianza, lower=0.01, upper=10)$root
}

myrdlgamma <- function(n, varianza) {
  k <- find.k(varianza)
  log(rgamma(n=n, shape=1/k, scale=k)) - esp(k)
}

# Ejemplo
#varianza <- 3.5
#y <- myrdlgamma(n=1000, varianza=varianza)


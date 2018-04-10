# The next functions generate random effects given the variance
####
gen_ea_normal <- function(N, ni, var1, var2) {
  u1 <- rep(rnorm(n=N,mean=0,sd=sqrt(var1)), each=ni)
  u2 <- rep(rnorm(n=N,mean=0,sd=sqrt(var2)), each=ni)
  list(u1 = u1, u2 = u2)
}
####
gen_ea_unif <- function(N, ni, var1, var2) {
  b <- sqrt(12*var1)/2
  u1 <- rep(runif(n=N,min=-b,max=b), each=ni)
  b <- sqrt(12*var2)/2
  u2 <- rep(runif(n=N,min=-b,max=b), each=ni)
  list(u1 = u1, u2 = u2)
}
####
gen_ea_exp <- function(N, ni, var1, var2) {
  u1 <- rep( rexp(N,rate=1/sqrt(var1)) - sqrt(var1) , each=ni)
  u2 <- rep( rexp(N,rate=1/sqrt(var2)) - sqrt(var2) , each=ni)
  list(u1 = u1, u2 = u2)
}
####
gen_ea_lg <- function(N, ni, var1, var2) {
  u1 <- rep(myrdlgamma(N, var1), each=ni)
  u2 <- rep(myrdlgamma(N, var2), each=ni)
  list(u1 = u1, u2 = u2)
}
####
gen_ea_ln <- function(N, ni, var1, var2){
  mu <- 0
  # Suponiendo mu=0 contruyo el polinomio a*z^2 + b*z + c = 0
  # que representa vari=(exp(sigma^2)-1)*exp(sigma^2) con z=exp(sigma^2)
  # las raices del polinomio son obtenidas en roots
  roots <- polyroot(c(-var1,-1,1))
  sigma <- sqrt( log( Re(roots)[Re(roots)>0] ) )
  sigma <- sigma[sigma != 'NaN']
  # sigma es el valor que asegura una dist con varianza "vari"
  u1 <- rlnorm(n=N, meanlog = mu, sdlog = sigma)
  u1 <- u1 - exp(mu + sigma^2/2) # Trasladando la muestra aleatoria
  u1 <- rep(u1, each=ni)
  
  roots <- polyroot(c(-var2,-1,1))
  sigma <- sqrt( log( Re(roots)[Re(roots)>0] ) )
  sigma <- sigma[sigma != 'NaN']
  u2 <- rlnorm(n=N, meanlog = mu, sdlog = sigma)
  u2 <- u2 - exp(mu + sigma^2/2) # Trasladando la muestra aleatoria
  u2 <- rep(u2, each=ni)
  list(u1 = u1, u2 = u2)
}
####
gen_ea_mn <- function(N, ni, var1, var2) {
  u1 <- rep(myrMixNor(N, var1), each=ni)
  u2 <- rep(myrMixNor(N, var2), each=ni)
  list(u1 = u1, u2 = u2)
}
####
gen_ea <- function(N, ni, var1, var2, d.ea) {
  if ( ! d.ea %in% c('NORMAL','UNIF','EXP','LG','LN','MN') ) 
    stop('Cuidado con la distribucion de los EA')
  res <- switch(d.ea,
                NORMAL  = gen_ea_normal(N,ni,var1,var2),   # Normal
                UNIF    = gen_ea_unif(N,ni,var1,var2),     # Uniforme
                EXP     = gen_ea_exp(N,ni,var1,var2),      # Exponencial
                LG      = gen_ea_lg(N,ni,var1,var2),       # Log-gamma
                LN      = gen_ea_ln(N,ni,var1,var2),       # Log-normal
                MN      = gen_ea_mn(N,ni,var1,var2) )      # Mezcla de normales
  res
}



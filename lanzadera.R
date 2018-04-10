#-------------------------------------------------------------------
source('https://raw.githubusercontent.com/fhernanb/utilitario/master/Mezcla%20normales%20generator.R')
source('https://raw.githubusercontent.com/fhernanb/utilitario/master/gen%20random%20effects.R')
source('https://raw.githubusercontent.com/fhernanb/utilitario/master/gen.df.R')
source('https://raw.githubusercontent.com/fhernanb/utilitario/master/log-gamma%20generator.R')
source('https://raw.githubusercontent.com/fhernanb/utilitario/master/simulador.R')
require(gamlss)
#-------------------------------------------------------------------
nsim <- 20
N <- seq(from=5, to=50, by=5)
ni <- seq(from=5, to=20, by=5)
var1 <- c(0.5, 1.0, 1.5, 2.0)
distri <- c('NORMAL', 'UNIF', 'EXP', 'LG', 'LN', 'MN')
params <- expand.grid(N=N, ni=ni, var1=var1, nsim=nsim, distri=distri)
apply(params, 1, simulador)


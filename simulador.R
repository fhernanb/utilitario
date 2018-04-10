#-------------------------------------------------------------------
simulador <- function(x) {
  N    <- as.numeric(x[1])
  ni   <- as.numeric(x[2])
  var1 <- as.numeric(x[3])
  nsim <- as.numeric(x[4])
  d.ea <- toString(x[5])
  
  b1_0   <-  1
  b1_bet <-  2
  b1_wit <-  1
  b2_0   <- -0.5
  b2_bet <-  2.5
  b2_wit <-  3.5
  beta.sim <- matrix(NA, ncol=8, nrow=nsim)
  colnames(beta.sim) <- c('b1_0', 'b1_bet', 'b1_wit', 
                          'b2_0', 'b2_bet', 'b2_wit', 
                          'var1', 'var2')
  i <- 1
  while (i <= nsim) {
    cat(i, " ")
    datos <- gen.df(N=N, ni=ni, d.ea=d.ea, param='WEI3',
                    var1=var1, var2=var1,
                    b1_0=b1_0, b1_bet=b1_bet, b1_wit=b1_wit, 
                    b2_0=b2_0, b2_bet=b2_bet, b2_wit=b2_wit)
    mod <- try(gamlss(formula=resp~x1_bet+x1_wit+random(subject),
                      sigma.fo=~x2_bet+x2_wit+random(subject),
                      family="WEI3", data=datos,
                      control=gamlss.control(trace=FALSE, n.cyc=200),
                      i.control=glim.control(cyc=200, bf.cyc=200)),
               silent=TRUE)
    if (class(mod)[1] == "gamlss") {
      theta.est <- c(mod$mu.coefficients[1:3],
                     mod$sigma.coefficients[1:3],
                     getSmo(mod, what='mu')$sigb^2,
                     getSmo(mod, what='sigma')$sigb^2)
      beta.sim[i, ] <- theta.est
      i <- i + 1
    }
  }
  res <- as.data.frame(beta.sim)
  res <- cbind(res, dist=d.ea, N=N, ni=ni, var=var1)
  write(x=t(res) , file='resultados.txt', ncolumns=12, append=TRUE)
}
#-------------------------------------------------------------------

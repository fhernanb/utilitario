# This function is to generate a data.frame given N, ni, d.ea, param, var1 and var2
gen.df <- function(N, ni, d.ea, param='WEI3', var1, var2,
                   b1_0, b1_bet, b1_wit, b2_0, b2_bet, b2_wit) {
  subject <- factor(rep(1:N, each=ni))
  b1 <- c(b1_0, b1_bet, b1_wit)
  b2 <- c(b2_0, b2_bet, b2_wit)
  
  y.failure <- TRUE  # This is to avoid problems response variable
  while (y.failure) {
    mu.failure <- TRUE
    while (mu.failure) {
      # The mu structure
      x1_bet <- rep(runif(n=N), each=ni)
      x1_wit <- rep(runif(n=ni), times=N)
      X1 <- cbind(1, x1_bet, x1_wit)
      # The sigma structure
      x2_bet <- rep(runif(n=N), each=ni)
      x2_wit <- rep(runif(n=ni), times=N)
      X2 <- cbind(1, x2_bet, x2_wit)
      # Generating the random effects
      U  <- gen_ea(N=N, ni=ni, var1=var1, var2=var2, d.ea=d.ea)
      U1 <- U$u1
      U2 <- U$u2
      # Creating mu and sigma
      muij <- exp(drop(X1%*%b1 + U1))
      sigmaij <- exp(drop(X2%*%b2 + U2))
      mu.failure <- any(muij > 500 | sigmaij < 0.001)
    }
    
    y <- switch( param,
                 WEI = rWEI(n=N*ni, mu=muij, sigma=sigmaij),
                 WEI2= rWEI2(n=N*ni, mu=muij, sigma=sigmaij),
                 WEI3= rWEI3(n=N*ni, mu=muij, sigma=sigmaij))
    y.failure <- min(y) < 0.1
  }
  
  datos <- data.frame(resp=y,
                      x1_bet=x1_bet, x1_wit=x1_wit,
                      x2_bet=x2_bet, x2_wit=x2_wit,
                      subject=as.factor(subject),
                      muij=muij, sigmaij=sigmaij,
                      U1=U1, U2=U2)
  datos
}




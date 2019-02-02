library(MASS)
VAR.sim <- function(phi,sigma,Y0,Time) {
  n <-dim(phi)[[1]]
  res <-array(NA,c(Time+1,n))
  res[1,] <- Y0
  for(i in 2:(Time+1)) {
    res[i,] <- phi %*% res[i-1,] + mvrnorm(1,rep(0,n),sigma)
  }
      res <- res[-1,] 
    return(res)
  
}


phi <- diag(c(0.5,0.01,0.97))
sigma <- array(c(4.42e-04,6.89e-05,1.55e-05,
                 6.89e-05,3.66e-04,9.58e-05,
                 1.55e-04,9.57e-05,1.52e-04),c(3,3))
Y0 <- c(.07,-.17,-.009)

simulated.Data <- VAR.sim(phi,sigma,Y0,1000)


par(mfrow=c(3,3))
for(i in 1:3){
  plot(simulated.Data[,i],type='l',main=paste("Time series ", i))
  acf(simulated.Data[,i],main=paste("ACF for ", i))
  pacf(simulated.Data[,i],main=paste("PACF for", i))
  
}

acf(simulated.Data)
pacf(simulated.Data)


#Fit a VAR model
library(vars)

model = VAR(simulated.Data,p=1)
summary(model)

model.residuals <- resid(model)

#Test for Normality
normality.test(model)

##Testing for heterogeneouse variables
arch.test(model)

par(mfrow =c(3,2))

for(i in 1:3){
  plot(model.residuals[,i],main=paste("Residuals for series", i))
  qqnorm(model.residuals[,i], main=paste('QQNorm for series',i))
  qqline(model.residuals[,i])
}


## Independence assumption

##Residuals plots: White Noise Assumptions
acf(model.residuals)
pacf(model.residuals)

serial.test(model)

#Select Order
var_sel <-VARselect(simulated.Data)

plot(var_sel$criteria[1,],xlab='Order',ylab='AIC')

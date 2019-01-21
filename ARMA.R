#Simulate normal/exp WN
w1 <- rnorm(1000,0,1)
w2 <- rexp(1000,1)

#rescale it 
wr1 <- (w1 - mean(w1)) / sqrt(var(w1))
wr2 <- (w2 - mean(w2)) / sqrt(var(w2))

#plot ts and acf's
wt1 <- ts(wr1 , start = 1 , deltat = 1)
wt2 <- ts(wr2,start= 1, deltat = 1)

par(mfrow = c(2,2))

ts.plot(wt1,main='Normal')
ts.plot(wt2,main='Exp')

acf(wt1); acf(wt2)


#Filter to obtain the moving average process 
w1 <- rnorm(502)
w2 <- rexp(502) -1
#set coeficients
a <- c(1,-.5,.2)
a1 <- c(1,.5,.2)

##Simulate MA(3) with Normal/Exp WN

ma2.11 <- filter(w1,filter = a , side=1)
ma2.11 <- ma2.aa[3:505]
ts.plot(ma2.11)

ma2.12 <- filter(w1,filter = a1,side = 1)
ma2.12 <- ma2.12[3:505]
ts.plot(ma2.12)

ma2.21 <- filter(w2, filter=a1,side=1)
ma.21 <- ma2.21[3:505]
ts.plot(ma2.21)

ma2.22 <- filter(w2, filter=a1,side=1)
ma.22 <- ma2.22[3:505]
ts.plot(ma2.22)

#simulate MA(3) with non-stationarty noize
a4 <-c(1,.2,.8,1.2)

ma2.4 <- filter(w1*(2*(1:501)+0.5),filter=a4,side=1)
ma2.4 <- ma2.4[4:505]

ts.plot(ma2.4)


##Nonstationary AR(2)
w2 <- rnorm(1500)
a2 <- c(.8,.2)
ar2 <- filter(w2,filter=a2,method='recursive')
ar2 <- ar2[1251:1500]
ts.plot(ar2)

#Stationary AR(1) process
a1 <- 0.5
ar1 <- filter(w2,filter=a1,method = 'recursive')
ar1 <- ar1[1251:1500]
ts.plot(ar1)

acf(ar2)
acf(ar1)
 


#Causual AR(1) process (|phi| ={.9,-.9,.1,-.1})

a1 <- 0.1
ar1 <- filter(w2, filter = a1,method='recursive')
ar1.1 <- ar1[1251:1500]

a1 <- -0.1
ar1 <- filter(w2, filter = a1,method='recursive')
ar1.2 <- ar1[1251:1500]


a1 <- 0.9
ar1 <- filter(w2, filter = a1,method='recursive')
ar1.3 <- ar1[1251:1500]


a1 <- -0.9
ar1 <- filter(w2, filter = a1,method='recursive')
ar1.4 <- ar1[1251:1500]

ts.plot(ar1.1,main="0.1")
ts.plot(ar1.2,main="-0.1")
ts.plot(ar1.3,main="0.9")
ts.plot(ar1.4,main="-0.9")



#Non-statiunar AR

a5 <- 1.0 + 1e-5
ar5 <- filter(w2, filter = a5,method='recursive')
ar1.5 <- ar5[1251:1500]

a6 <- 1.0 + 1e-4
ar1 <- filter(w2, filter = a6,method='recursive')
ar1.6 <- ar1[1251:1500]

a7 <- 1.0 + 1e-3
ar1 <- filter(w2, filter = a7,method='recursive')
ar1.7 <- ar1[1251:1500]

a8 <- 1.0 + 1e-2
ar1 <- filter(w2, filter = a8,method='recursive')
ar1.8 <- ar1[1251:1500]

ts.plot(ar1.5,main=a5)
ts.plot(ar1.6,main=a5)
ts.plot(ar1.7,main=a5)
ts.plot(ar1.8,main=a5)

par(mar=c(1,1,1,1))

ts.plot(ar5,main=a5)

################################
#ARMA process 
arma22 <- arima.sim(n=500,
                    list(ar=c(.88,-.49),
                         ma=c(-.23,.25)),
                    sd = sqrt(.18))

ts.plot(arma22)
acf(arma22)
pacf(arma22)



########################
####Estimate ARMA parameters
#AR(2) process simulation
w2 <- rnorm(1500)
b <- c(1.2,-0.5)
ar2 <- filter(w2,filter=b,method='recursive')
ar2 <- ar2[1001:1500]

##Fit Linear Regression to AR(2)
data2 <- data.frame(cbind(x1=ar2[1:498],
                          x2=ar2[2:499],
                          y=ar2[3:500]))

model2 <-lm(y~x1+x2,data = data2)
summary(model2)

#Yule-Walker &AR Estimation
coef <- acf(ar2, type ='covariance',plot=FALSE)
Gamma1 <- coef$acf[2:4,,1]
Gammamatrix <- matrix(0,3,3)
for(i in 1:3) {
  if(i>1) {
    Gammamatrix[i,] <- c(coef$acf[i:2,,1],coef$acf[1:(3-i+1)])
          }
  else {
      Gammamatrix[i,] <- coef$acf[1:(3-i+1),,1]
    }
  
}
phi.estim <-solve(Gammamatrix,Gamma1)



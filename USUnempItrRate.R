intrate = read.csv('c:\\data\\Datasets\\Finance\\Maco\\3 Interest Rate Data.csv',header = T)
intrate.ts = ts(intrate$INT,start=1950, freq=12)
dates = as.Date(intrate[,1], format="%m/%d/%Y")

unemploym = read.csv('c:\\data\\Datasets\\Finance\\Maco\\3 Unemployment Data.csv',header = T)
unemploym = as.vector(t(unemploym[,-1]))
unemploym.ts = ts(unemploym[-c(809:816)],start=1950, freq=12)
data.ts = ts.union(intrate.ts,unemploym.ts)
plot(data.ts, type="l",main="")
acf(data.ts)
pacf(data.ts)
2
## Simultaneous lag correlation
n = length(intrate.ts)
cor(intrate.ts,unemploym.ts)
## lag-one correlation
cor(intrate.ts[1:(n-1)],unemploym.ts[2:n])
cor(intrate.ts[2:n],unemploym.ts[1:(n-1)])

# take the first order difference
dunrate=diff(unemploym.ts)
dintrate=diff(intrate.ts)
ddata.ts = ts.union(dintrate,dunrate)
plot(ddata.ts,xlab="time",main="",lwd=2,type="l")
acf(ddata.ts)
pacf(ddata.ts)
cor(dunrate,dintrate)
## lag-one correlation
cor(dintrate[2:(n-1)],dunrate[1:(n-2)])
cor(dunrate[2:(n-1)],dintrate[1:(n-2)])

## Fit Univariate AR Models to difference processes 
mod_un = ar(dunrate,order.max=20)
print(mod_un$order)
mod_int = ar(dintrate,order.max=20)
print(mod_int$order)
## Fit VAR model: capture relationship between change in interest rate vs change in unemplyment
library(vars)
mod_aic_1 = VAR(ddata.ts,lag.max=20,ic="AIC", type="none")
mod_aic_2 = VAR(ddata.ts,lag.max=20,ic="AIC", type="const")
mod_aic_3 = VAR(ddata.ts,lag.max=20,ic="AIC", type="trend")
mod_aic_4 = VAR(ddata.ts,lag.max=20,ic="AIC", type="both")
pord_1 = mod_aic_1$p; pord_2 = mod_aic_2$p; 
pord_3 = mod_aic_3$p; pord_4 = mod_aic_4$p
mod_hq = VAR(ddata.ts,lag.max=20,ic="HQ")
mod_sc = VAR(ddata.ts,lag.max=20,ic="SC")
mod_fpe = VAR(ddata.ts,lag.max=20,ic="FPE")
pord_hq = mod_hq$p; pord_sc = mod_sc$p; pord_fpe = mod_fpe$p

## Fit VAR Model with Selected Order
mod = VAR(ddata.ts,pord_4, type="both")

## Residual Analysis: Constant Variance Assumption
arch.test(mod) 

## Residual Analysis: Normality Assumption
normality.test(mod)

## Residual Analysis: Uncorrelated Errors Assumption
serial.test(mod)
serialtest = serial.test(mod)
plot(serialtest)

## roots analysis: is this VAR process stable?
roots_mod = roots(mod)
sum(roots_mod>=1)

## Does a smaller order fit the model equally well?  Apply Wald Test
## Coefficients for orders 3 to 12
coef.dintrate.3to12 = coefficients(mod)$dintrate[5:(2*pord_4),1]
coef.dunrate.3to12 = coefficients(mod)$dunrate[5:(2*pord_4),1]
## Covariance matrix of the coefficients
index.dintrate = 5:(2*pord_4)
var.dintrate.3to12 = vcov(mod)[index.dintrate,index.dintrate]
index.dunrate = c(((2*pord_4)+6):(4*pord_4+1))
var.dunrate.3to12 = vcov(mod)[index.dunrate,index.dunrate]
## Apply Wald Test
library(aod)
wald.test(b=coef.dintrate.3to12, var.dintrate.3to12, Terms=seq(1, 2*(pord_4-2)))
wald.test(b=coef.dunrate.3to12, var.dunrate.3to12, Terms=seq(1, 2*(pord_4-2)))
##############################################################################
## How well can we predict the change in the interest rate for beginning of 2017?

## Predict Interest Rate Change using Univariate AR
nfit = 12*67
n = length(dintrate)
dintrate.train = dintrate[1:nfit]
dintrate.test = dintrate[(nfit+1):n]
ar.train = ar(dintrate.train, order.max=12)
print(ar.train$order)
dintrate.predict = predict(ar.train,n.ahead=(n-nfit))
dintrate.fcst.1 = dintrate.predict$pred
dintrate.se.1 = dintrate.predict$se
dintrate.lo.1 = dintrate.fcst.1-1.96*dintrate.se.1
dintrate.up.1 = dintrate.fcst.1+1.96*dintrate.se.1

## Predict Interest Rate Change using VAR
ddata.train = ddata.ts[1:nfit,]
mod.train = VAR(ddata.train,pord_4)
ddata.predict = predict(mod.train,n.ahead=(n-nfit))
dintrate.fcst.2 = ddata.predict[[1]]$dintrate[,1]
dintrate.lo.2 = ddata.predict[[1]]$dintrate[,2]
dintrate.up.2 = ddata.predict[[1]]$dintrate[,3]

#ymin = min(c(dintrate.fcst.1,dintrate.fcst.2,ddata.ts[,1]))
#ymax = max(c(dintrate.fcst.1,dintrate.fcst.2,ddata.ts[,1]))
plot(dates[(n-6):n], ddata.ts[(n-6):n,1],type="l", ylim=c(-0.5,0.5), xlab="Time", ylab="Difference Interest Rate")
points(dates[(nfit+1):n],dintrate.fcst.1,col="red")
points(dates[(nfit+1):n],dintrate.fcst.2,col="green")
#lines(dates[(nfit+1):n],dintrate.lo.1,lty=3,lwd= 2, col="brown")
#lines(dates[(nfit+1):n],dintrate.up.1,lty=3,lwd= 2, col="brown")
#lines(dates[(nfit+1):n],dintrate.lo.2,lty=3,lwd= 2, col="blue")
#lines(dates[(nfit+1):n],dintrate.up.2,lty=3,lwd= 2, col="blue")

#### Granger Causality: Wald Test
library(aod)
mod = VAR(ddata.ts,pord_1)
coef.dintrate = coefficients(mod)$dintrate[-(2*pord_1+1),1]
var.dintrate = vcov(mod)[1:(2*pord_1),1:(2*pord_1)]
wald.test(b=coef.dintrate, var.dintrate, Terms=seq(2, 2*pord_1, 2))

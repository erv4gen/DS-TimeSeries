temp = read.csv("c:\\data\\Datasets\\Playground\\River Flow Example\\Temperature.csv",header=T)
rain  = read.csv("c:\\data\\Datasets\\Playground\\River Flow Example\\Precipitation.csv",header=T)
river = read.csv("c:\\data\\Datasets\\Playground\\River Flow Example\\RiverFlow.csv",header=F)

#Fill in missing value in rain for October, 1964
#using the average between September and November
rain[,11]=as.numeric(rain[,11])
rain[14,11]=0.5*(as.numeric(rain[14,10])+as.numeric(rain[14,12]))
rain[,11]=as.factor(rain[,11])

## All variables for the same time period
temp = as.vector(t(temp[,-1]))
temp = temp[-c(1:(12*6))]
temp = temp[-c(736:744)]
rain = as.vector(t(rain[,-1]))
rain = rain[-c(1:(12*6))]
rain = rain[-c(736:744)]
river = as.vector(river[,3])

temp.ts = ts(as.numeric(temp),start=1956, freq=12)
rain.ts = ts(as.numeric(rain),start=1956, freq=12)
river.ts = ts(as.numeric(river),start=1956, freq=12)

data.ts = ts.union(river.ts,temp.ts,rain.ts)
plot(data.ts, type="l",main="")
acf(data.ts, mar=c(3.5,3,1.9,0))  
pacf(data.ts, mar=c(3.5,3,1.9,0))

library(tseries)
adf.test(river.ts, alternative = "stationary")
adf.test(rain.ts, alternative = "stationary")
adf.test(temp.ts, alternative = "stationary")

library(forecast)
#Determine differencing order to achieve stationarity 
ndiffs(river.ts, alpha = 0.05, test = c("adf"))
ndiffs(rain.ts, alpha = 0.05, test = c("adf"))
ndiffs(temp.ts, alpha = 0.05, test = c("adf"))



#Log transformation
plot(log(data.ts), type="l",main="")

#Using seasonal differencing of 12 months and log transformation
dtemp=diff(log(temp.ts),differences = 12)
drain=diff(log(rain.ts),differences = 12)
driver=diff(log(river.ts),differences = 12)
ddata.ts = ts.union(driver,dtemp,drain)
plot(ddata.ts,xlab="time",main="",type="l")
acf(ddata.ts, mar=c(3.5,3,1.9,0))
pacf(ddata.ts, mar=c(3.5,3,1.9,0))

#### Data preparation for models: Testing Vs Training ###################
data=data.ts
n = nrow(data)
## Training data: 1956 to 2015
data.train=data[1:(n-15),]
## Test data: 2016 and 3 months in 2017
data.test=data[(n-14):n,]

ts_river=ts(log(data.train[,"river.ts"]),start=1956, freq=12)
ts_rain=ts(log(data.train[,"rain.ts"]),start=1956, freq=12)
ts_temp=ts(log(data.train[,"temp.ts"]),start=1956, freq=12)


ts_river2=ts(log(data.test[,"river.ts"]),start=2016, freq=12)
ts_rain2=ts(log(data.test[,"rain.ts"]),start=2016, freq=12)
ts_temp2=ts(log(data.test[,"temp.ts"]),start=2016, freq=12)


#### Univariate ARIMA model ####

final.aic = Inf
final.order = c(0,0,0,0)
for (p in 1:6) for (d in 0:1) for (q in 1:6) for(s in 0:1){
  current.aic = AIC(arima(ts_river, order=c(p, d, q), seasonal = list(order=c(0,s,0),
                                                                      period=12), method="ML"))
  if (current.aic < final.aic) {
    final.aic = current.aic
    final.order = c(p, d, q,s)
    
  }
}
# > final.order
# [1] 1 0 4 2 


model.arima = arima(ts_river, order=c(1,0,4),seasonal = list(order=c(0,2,0),
                                                             period=12), method="ML") 

## Residual analysis
par(mfrow=c(2,2))
plot(resid(model.arima), ylab='Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(resid(model.arima),main="ACF: Residuals")
hist(resid(model.arima),xlab='Residuals',main='Histogram: Residuals')
qqnorm(resid(model.arima),ylab="Sample Q",xlab="Theoretical Q")
qqline(resid(model.arima))

#We expect hight p-value to residuals be uncorrelated
Box.test(model.arima$resid, lag = (1+4+1), type = "Box-Pierce", fitdf = (1+4))
Box.test(model.arima$resid, lag = (1+4+1), type = "Ljung-Box", fitdf = (1+4))

#Predictions versus actual

plot(forecast(model.arima,h=15))

fore = forecast(model.arima,h=15)
fore=as.data.frame(fore)
point.fore = ts(fore[,1],start=2016, freq=12)
lo.fore = ts(fore[,4],start=2016, freq=12)
up.fore = ts(fore[,5],start=2016, freq=12)
ymin=min(c(log(river[(n-50):n]),lo.fore))
ymax=max(c(log(river)[(n-50):n],up.fore))
plot(ts(log(as.numeric(river[(n-50):n])),start=2012, freq=12), ylim=c(ymin,ymax), ylab="Log-River Flow", type="l",main="")
points(point.fore,lwd=2,col="red")
lines(lo.fore,lty=3,lwd= 2, col="blue")
lines(up.fore,lty=3,lwd= 2, col="blue")

##### ARIMAX model ####
final.aic = Inf
final.order = c(0,0,0,0)
for (p in 1:6) for (d in 0:1) for (q in 1:6) for(s in 0:1){
  current.aic = AIC(arima(ts_river, order=c(p, d, q), seasonal = list(order=c(0,s,0),
                                                                      period=12), method="ML",xreg=data.frame(ts_rain,ts_temp)))
  if (current.aic < final.aic) {
    final.aic = current.aic
    final.order = c(p, d, q,s)
    
  }
}
# > final.order
# [1] 1 0 4 0 

model.arima2 = arima(ts_river, order = c(1,0,4), method="ML",xreg=data.frame(ts_rain,ts_temp))
## Residual analysis
par(mfrow=c(2,2))
plot(resid(model.arima2), ylab='Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(resid(model.arima2),main="ACF: Residuals")
hist(resid(model.arima2),xlab='Residuals',main='Histogram: Residuals')
qqnorm(resid(model.arima2),ylab="Sample Q",xlab="Theoretical Q")
qqline(resid(model.arima2))

Box.test(model.arima2$resid, lag = (1+4+1), type = "Box-Pierce", fitdf = (1+4))
Box.test(model.arima2$resid, lag = (1+4+1), type = "Ljung-Box", fitdf = (1+4))

#Predictions versus actual
plot(ts(log(as.numeric(river[(n-50):n])),start=2012, freq=12), ylim=c(4,10), ylab="Log-River Flow", type="l",main="")
fore = forecast(model.arima2,h=15,xreg=data.frame(ts_rain2,ts_temp2))
fore=as.data.frame(fore)
point.fore = ts(fore[,1],start=2016, freq=12)
lo.fore = ts(fore[,4],start=2016, freq=12)
up.fore = ts(fore[,5],start=2016, freq=12)
points(point.fore,lwd=2,col="red")
lines(lo.fore,lty=3,lwd= 2, col="blue")
lines(up.fore,lty=3,lwd= 2, col="blue")



##############################################################################################
data.train=cbind(ts_river,ts_temp,ts_rain)
data.test=cbind(ts_river2,ts_temp2,ts_rain2)
####################################################################################
library(vars)
### VAR Model with Deterministic Components ####
##Model Selection
VARselect(data.train, lag.max = 20,season=12,type="both")$selection

## Model Fitting: Unrestricted VAR
model.var=VAR(data.train, p=1,type="both",season=12)
summary(model.var)

## Model Fitting: Restricted VAR
model.var.restrict=restrict(model.var)  
summary(model.var.restrict)

## Granger Causality: Wald Test
library(aod)
coef.riverflow = coefficients(model.var)$ts_river[c(1:3),1]
var.model = vcov(model.var)[1:3,1:3]
## Granger Causality: Rain & Temperature
wald.test(b=coef.riverflow, var.model, Terms=seq(2,3))
## Granger Causality: Rain
wald.test(b=coef.riverflow, var.model, Terms=seq(3, 9, 5))
## Granger Causality: Temperature
wald.test(b=coef.riverflow, var.model, Terms=seq(2, 9, 5))


pred = predict(model.var.restrict, n.ahead=15, ci=0.95)[[1]]$ts_river
point.pred = ts(pred[,1],start=2016, freq=12)
lo.pred = ts(pred[,2],start=2016, freq=12)
up.pred = ts(pred[,3],start=2016, freq=12)
pred.f = predict(model.var,n.ahead=15, ci=0.95)[[1]]$ts_river
point.pred.f = ts(pred.f[,1],start=2016, freq=12)
lo.pred.f = ts(pred.f[,2],start=2016, freq=12)
up.pred.f = ts(pred.f[,3],start=2016, freq=12)

ymin=min(c(log(river[(n-50):n]),lo.pred,lo.pred.f))
ymax=max(c(log(river)[(n-50):n],up.pred,up.pred.f))
plot(ts(log(as.numeric(river[(n-50):n])),start=2012, freq=12), ylim=c(4,10), ylab="Log-River Flow", type="l",main="")
points(point.pred,lwd=2,col="red")
lines(lo.pred,lty=3,lwd= 2, col="blue")
lines(up.pred,lty=3,lwd= 2, col="blue")
points(point.pred.f,lwd=2,col="green")
lines(lo.pred.f,lty=3,lwd= 2, col="purple")
lines(up.pred.f,lty=3,lwd= 2, col="purple")

### Another approach for prediction & visualizing predictions
predict(model.var.restrict, n.ahead=27, ci=0.95)
fcst = forecast(model.var.restrict,h=27)
plot(fcst)


###VARX Model using temperature as exogenous variable##
data.train=cbind(ts_river,ts_rain)
data.test=cbind(ts_river2,ts_rain2)
##Model Selection
VARselect(data.train, lag.max = 20,season=12,type="both", exogen=data.frame(ts_temp))$selection
## Model Fitting: Unrestricted VAR
model.var2=VAR(data.train, p=1,season=12,type="both", exogen=data.frame(ts_temp))
summary(model.var2)

## Model Fitting: Restricted VAR
model.var.restrict2=restrict(model.var2)  
summary(model.var.restrict2)

x=data.frame(ts_temp1); colnames(x)="ts_temp"
predict(model.var.restrict2, n.ahead=27, ci=0.95,dumvar=x)
fcst = forecast(model.var.restrict2,h=27,dumvar=x)
plot(fcst)


###VARX Model using rain as exogenous variable##
##Model Selection
VARselect(cbind(ts_river,ts_temp), lag.max = 20,season=12,exogen=data.frame(ts_rain))$selection
## Model Fitting: Unrestricted VAR
model.var2=VAR(cbind(ts_river,ts_temp), p=3,season=12,exogen=data.frame(ts_rain))
summary(model.var2)

## Model Fitting: Restricted VAR
model.var.restrict2=restrict(model.var2)  
summary(model.var.restrict2)

x=data.frame(ts_rain1); colnames(x)="ts_rain"
predict(model.var.restrict2, n.ahead=27, ci=0.95,dumvar=x)
fcst = forecast(model.var.restrict2,h=27,dumvar=x)
plot(fcst)



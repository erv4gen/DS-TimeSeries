#Source : https://www.youtube.com/channel/UCpviBv-De2_oeuSU_b968BQ


library(mgcv)
dat <-read.csv('c:\\data\\Datasets\\Medicine\\example_patients_in_emergency\\govhack3.csv'
               ,header = TRUE
               ,stringsAsFactors = FALSE,skip = 1)


dat <- dat[,1:2]
dat$Date <- as.Date(dat$Date,format = "%d-%b-%Y") 
dat$Attendance <- as.integer(dat$Attendance)
attach(dat)
Attendance.ts <-sqrt(Attendance+3/8)
time.pts <- c(1:length(Attendance))
time.pts <- c(time.pts-min(time.pts)/max(time.pts))

month <-as.factor(months(Date))
week <-as.factor(weekdays(Date))
Attendance.ts <-sqrt(Attendance+3/8)


## Take the difference: weekly and yearly (capture the monthly seasonality
volume.ts = ts(Attendance.ts,start=c(2013,7,1),frequency=365.25)

dvolume7=diff(volume.ts,7)

dvolume12=diff(volume.ts,12)

par(mfrow=c(2,2))


ts.plot(volume.ts,ylab="ED Volume")

ts.plot(dvolume7,ylab="Weekly difference")

ts.plot(dvolume12,ylab="Yearly difference")


mod = arima(volume.ts, order = c(5,1,5),seasonal = list(order =
                                                          c(1,0,1),period=7),method = "ML")

plot(resid(mod), ylab='Standardized Residuals',type='o',main="Residual Plot")

abline(h=0)

acf(as.vector(resid(mod)),lag.max=365*2,main="ACF: Residuals")

hist(resid(mod),xlab='Standardized Residuals',main='Histogram: Residuals')

qqnorm(resid(mod))

qqline(resid(mod))


## Forecasting with ARIMA: 2 Weeks Ahead

n = length(volume.ts); nfit = n-14

outvol = arima(volume.ts[1:nfit], order = c(5,1,5),seasonal = list(order =
                                                                     c(1,0,1),period=7),method = "ML")

out_pred = as.vector(predict(outvol,n.ahead=14))

## Compare prediction vs observed including confidence bands

timevol=time(volume.ts)

ubound = out_pred$pred+1.96*out_pred$se

lbound = out_pred$pred-1.96*out_pred$se

ymin = min(lbound)

ymax = max(ubound)

plot(timevol[(n-56):n],volume.ts[(n-56):n],type="l", ylim=c(ymin,ymax), xlab="Time",
     ylab="ED Volume")

points(timevol[(nfit+1):n],out_pred$pred,col="red")

lines(timevol[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")

lines(timevol[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")

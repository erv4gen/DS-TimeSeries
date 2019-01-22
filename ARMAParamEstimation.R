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


gam.fit.seastr.2 <- gam(Attendance.ts ~s(time.pts)+month+week)
gam.fit.seastr.2.fitted <-fitted(gam.fit.seastr.2)

resid.process <- Attendance.ts - gam.fit.seastr.2.fitted

par(mfrow = c(2,1))
acf(resid.process,lag.max = 12*4, main="ACF:Resid")
pacf(resid.process,lag.max = 12*4, main="PACF:Resid plot")


##Fit the AR(q) process for q<=order.max
mod <- ar(resid.process,order.max=20)
print(mod$order)
summary(mod)

##Plot AIC values on the log scale to easy identify minimum
plot(c(0:20),
     mod$aic,type='b',log='y',
     xlab='order',ylab='log-AIC')

#Are the roots in fitted AR within the unit circle?
roots <- polyroot(c(1,(-mod$ar)))
plot(roots,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))

resid <- mod$resid[(mod$order+1):length(mod$resid)]
#Plot the residuals
par(mfrow=c(2,2))
plot(resid,xlab='',ylab='Model Residuals')
acf(resid,main='ACF')
pacf(resid,main='PACF')
qqnorm(resid)


#Fit ARMA(1,1)
modarma <-arima(resid.process,order=c(1,0,1),method='ML')
par(mfrow=c(2,2))
plot(resid(modarma),ylab='Std Resid')
abline(h=0)
acf(resid(modarma,main='ACF'))
pacf(as.vector(resid(modarma)),main='PACF')
qqnorm(resid(modarma))
qqline(resid(modarma))

##ORDER Selection
#Use EACF
library(TSA)
eacf(resid.process,ar.max=6,ma.max = 6)

#use AICC
n <- length(resid.process)
norder <- 6
p <- c(1:norder) - 1 ; q = c(1:norder) - 1
aic <- matrix(0,norder,norder)
for(i in 1:norder) {
  for(j in 1:norder) {
    modij <- arima(resid.process,order= c(p[i],0,q[j]),method='ML')
    aic[i,j] <- modij$aic -  2*(p[i]+q[j] +1) +2 * (p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
  }
}

#Which order to select?
aicv <-as.vector(aic)
plot(aicv,ylab='AIC val')
indexp <- rep(c(1:norder),norder)
indexq <- rep(c(1:norder), each=norder)
indexaic <- which(aicv==min(aicv))
porder <- indexp[indexaic] -1
qorder <- indexq[indexaic] -1 

#Final Model 
final_model <- arima(resid.process,
                     order = c(porder,0,qorder),
                     method='ML')

par(mfrow=c(2,2))
plot(resid(final_model),ylab='Std Resid')
abline(h=0)
acf(resid(final_model,main='ACF'))
pacf(as.vector(resid(final_model)),main='PACF')
qqnorm(resid(final_model))
qqline(resid(final_model))

###Test for independence for final model

Box.test(final_model$residuals,
         lag=(porder+qorder+1),
         type='Box-Pierce',
         fitdf=(porder+qorder))


Box.test(final_model$residuals,
         lag=(porder+qorder+1),
         type='Ljung-Box',
         fitdf=(porder+qorder))

### Test for independence for smaller model


Box.test(modarma$residuals,
         lag=(porder+qorder+1),
         type='Box-Pierce',
         fitdf=(porder+qorder))


Box.test(modarma$residuals,
         lag=(porder+qorder+1),
         type='Ljung-Box',
         fitdf=(porder+qorder))



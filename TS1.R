dat <-read.csv('c:\\data\\Datasets\\Medicine\\example_patients_in_emergency\\govhack3.csv'
               ,header = TRUE
               ,stringsAsFactors = FALSE,skip = 1)

dat <- dat[,1:2]
dat$Date <- as.Date(dat$Date,format = "%d-%b-%Y") 
dat$Attendance <- as.integer(dat$Attendance)

head(dat)
attach(dat)

library("ggplot2")
ggplot(data =dat,aes(x= Date,y = Attendance))+geom_line(color = "black",size = 1)

#Apply Transformation
Attendance.ts <-sqrt(Attendance+3/8)

hist(Attendance,nclass = 20,
     xlab = "ED Volume"
     ,main = ""
     ,col='brown')
hist(Attendance.ts,nclass = 20,
     xlab = "Transformed ED Volume"
     ,main = ""
     ,col='blue')


#Plot with transformation
ggplot(data =dat,aes(x= Date,y = Attendance.ts))+geom_line(color = "black",size = 1)

time.pts <- c(1:length(Attendance))
time.pts <- c(time.pts-min(time.pts)/max(time.pts))

#Local 
loc.fit <- loess(Attendance.ts ~ time.pts)
vol.fit.loc <- fitted(loc.fit)

#Splines Trend estimation
library(mgcv)
gam.fit <- gam(Attendance.ts~ s(time.pts))
vol.fit.gam <- fitted(gam.fit)
summary(gam.fit)
#is there a trend
plot(Date,Attendance.ts,type ="l")
lines(Date,vol.fit.gam,col='brown')
lines(Date,vol.fit.loc,col='blue')

#add month-seasonality
month <-as.factor(months(Date))
gam.fit.seastr.1<- gam(Attendance.ts ~s(time.pts)+month)
gam.fit.seastr.1.fitted <- fitted(gam.fit.seastr.1)
lines(Date,gam.fit.seastr.1.fitted,col='green')

#add day-of-the-week seasonality
week <-as.factor(weekdays(Date))
gam.fit.seastr.2 <- gam(Attendance.ts ~s(time.pts)+month+week)
gam.fit.seastr.2.fitted <-fitted(gam.fit.seastr.2)

lines(Date,gam.fit.seastr.2.fitted,col='yellow')


##Compare the two fits
ggplot(dat,aes(Date,gam.fit.seastr.2.fitted))+geom_line()

plot(Date,gam.fit.seastr.2.fitted,type='l',col='blue')
lines(Date,gam.fit.seastr.1.fitted,lwd=1,col='red')


summary(gam.fit.seastr.2)


#Does the addition of seasonality of day of the week adds predictive power?
lm.fit.sasstr.1 <- lm(Attendance.ts ~ month)
lm.fit.sasstr.2 <- lm(Attendance.ts ~ month + week)
anova(lm.fit.sasstr.1,lm.fit.sasstr.2)


#Residual Process: Trend Removal
resid.1 <- Attendance.ts - gam.fit.seastr.1.fitted

##Residual Process : seasonality removal
resid.2 <- Attendance.ts - gam.fit.seastr.1.fitted

##Residual process : Trend and Seasonality removal      


resid.3 <- Attendance.ts - gam.fit.seastr.2.fitted

y.min <- min(c(resid.1,resid.2,resid.3))
y.max <- max(c(resid.1,resid.2,resid.3))

ggplot(dat,aes(Date,resid.1),ymin=y.min,ymax= y.max)+geom_line()
lines(Date,resid.2,col='blue',lwd=3)
lines(Date,resid.3,col='green',lwd=3)

#Compare auto-correlation plots
acf(resid.1,lag.max = 12*4)
acf(resid.2,lag.max = 12*4,col='blue')
acf(resid.3,lag.max = 12*4,col='green')


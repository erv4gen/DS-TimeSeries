#Read and stack data
dat <-read.csv('c:\\data\\Datasets\\example_hourly_energy_consuption\\AEP_hourly.csv'
               ,header = TRUE
               ,stringsAsFactors = FALSE)
head(dat)


temp <- ts(as.vector(dat[,2]))

ts.plot(temp,ylab='Tempirature')


#1 - Linear regression
# Create equally spaced time points for fitting trends
time.pts <- c(1:length(temp))
time.pts <- c(time.pts - min(time.pts)) / max(time.pts)

#Fit a moving average 
mav.fit <- ksmooth(time.pts,temp,kernel = "box")
temp.fit.mav <- ts(mav.fit$y)
#Check the treend


lines(temp.fit.mav,lwd=2,col="purple")
abline(temp.fit.mav[1],0,lwd=2,col='blue')

# 2 - Parametric Regression
x1 <- time.pts
x2 <- time.pts^2

lm.fit <- lm(temp ~ x1+x2)
summary(lm.fit)

#identify the trend
temp.fit.lm <- ts(fitted(lm.fit))
lines(temp.fit.lm,lwd=2,col='green')
abline(temp.fit.lm[1],0,col='blue')


#3 - Non-Parametric Regression
#Local Polynomial Trend
loc.fit <- loess(temp ~ time.pts)
temp.fit.loc <- ts(fitted(loc.fit))
#Splines Trend Estimation
library(mgcv)

gam.fit <- gam(temp ~ s(time.pts))
temp.fit.gam <-  ts(fitted(gam.fit))

#Is there a trend?

lines(temp.fit.loc,lwd=2,col='brown')
lines(temp.fit.gam,lwd=2,col='red')
abline(temp.fit.loc[1],0,col='blue')

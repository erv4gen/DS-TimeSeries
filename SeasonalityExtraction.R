#source : https://www.youtube.com/watch?v=HATh-NCWwAw&index=7&list=PL_onPhFCkVQghJC-KQkUNvkAGbriARyfX

library(TSA)
#Read and stack data
dat <-read.csv('c:\\data\\Datasets\\example_hourly_energy_consuption\\AEP_hourly.csv'
               ,header = TRUE
               ,stringsAsFactors = FALSE)


head(dat)

temp <- ts(as.vector(dat[,2]),frequency = 12)

ts.plot(temp,ylab='Tempirature')

#estimate seasonal means
month <- season(temp)

#drop jan with intersept
model1 = lm(temp ~ month)
summary(model1)

#without intersept
model2 = lm(temp ~ month-1)
summary(model2)

#cosine-sin model
har1 <-harmonic(temp,1)

model3 <-lm(temp ~ har1)
summary(model3)

har2 <-harmonic(temp,2)
model4 <- lm(temp ~ har2)
summary(model4)


#Compare models
st1 = coef(model2)
st2 = fitted(model4)[1:12]
plot(1:12,st1,type='l'
     ,lwd=2
     ,xlab = 'Month'
     ,ylab = 'Seasonality')
lines(1:12,lwd=2,type='l',col='brown')
#both models are the same, so we'll prefer the mode
#with fewer regression coeficients


#Estimating both trend and seasonality together 
#with parametric model
x1 <- c(1:length(temp))
x2 <- x1^2
lm.fit <- lm(temp ~ x1+x2+har2)
summary(lm.fit)

dif.fit.lm <- ts(temp-fitted(lm.fit),frequency = 12)
ts.plot(dif.fit.lm,ylab='Residuals')


#Estimation non-porametric trend
gam.fit <-gam(temp ~ s(x1)+har2)
dif.fit.gam <-ts(temp-fitted(gam.fit), frequency = 12)


#Compare approaches
ts.plot(dif.fit.lm,ylab='Residuals',col='brown')
lines(dif.fit.gam,col='blue')
#The difference are smalle between two aproaches  
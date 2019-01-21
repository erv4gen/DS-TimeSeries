#Read and stack data
dat <-read.csv('c:\\data\\Datasets\\example_hourly_energy_consuption\\AEP_hourly.csv'
               ,header = TRUE
               ,stringsAsFactors = FALSE)


temp <- ts(as.vector(dat[,2]))

#AFC for the tempirature time series
acf(temp,lag.max = 12*4,main="")

#AFC for residual process
acf(dif.fit.lm,max=12*4,main="")
acf(dif.fit.gam,max=12*4,main="")

##simulate norm WN
e <- rnorm(300)
## lagged time series 
e1 <- ts(e[1:250])
e2 <- ts(e[4:253])
pacf(ts.union(e1,e2))

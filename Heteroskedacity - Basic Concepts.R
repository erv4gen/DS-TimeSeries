#### Generate time series with heteroskedacticity ####
a0 <- .2
a1 <- .5
b1 <- .3
w <-rnorm(5000)
eps <-rep(0,5000)
sigsq <- rep(0,5000)
for (i in 2:5000) {
  sigsq[i] <- a0 + a1*(eps[i-1]^2) + b1*sigsq[i-1]
  eps[i] = w[i] *sqrt(sigsq[i])
}
### Plot afc
acf(eps)
acf(eps^2)

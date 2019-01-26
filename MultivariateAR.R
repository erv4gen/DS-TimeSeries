
e <- rnorm(501)
x <- rnorm(502)
xmat <- cbind(x[2:501],x[1:500])

ys <- 0
for(k in 1:500) {
  if(k==1){
    ys[k] <- e[k+1] +.6*xmat[k,1] + .3*xmat[k,2]
  }
  if(k>1){
    ys[k] <- .8*ys[k-1] +e[k+1] +.6*xmat[k,1] + .3*xmat[k,2]
  }
}
ys <- ts(ys[251:500])
xs <- ts(x[253:502])

plot(ts.union(ys,xs))
pacf(ts.union(ys,xs))

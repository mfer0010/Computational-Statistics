rm(list=ls())

N <-c(100,1000,10000,100000,1000000)
thetahat <- mat.or.vec(5,1)
variance <- mat.or.vec(5,1)
varhatthetahat <- mat.or.vec(5,1)
for (i in 1:5) {
  x <- runif(N[i],2,8)
  #Monte Carlo Estimate
  thetahat[i] <- 6*mean(sin(x)/log(x))
  #Actual Variance
  variance[i] <- ((N[i]-1)^-1)*mean(((sin(x)/log(x))+0.053792)^2)
  #variance of the Monte Carlo Integral
  varhatthetahat[i]<-((N[i]-1)^-1)*mean(((sin(x)/log(x))-thetahat)^2)
}
Table <- rbind(N,thetahat,variance, varhatthetahat)
Table
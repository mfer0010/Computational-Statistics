rm(list=ls())

K<-c(2,3,4,5,6,7,8,9,10)
N<-c(100, 1000, 10000, 100000, 1000000)
thetahat<-mat.or.vec(9,5)
varhatthetahat<-mat.or.vec(9,5)
for (i in 1:5) {
  for (j in 1:9) {
    u<-runif(N[i],0,1)
    x<-((20^(K[j]-1))/(1-u))^(1/(K[j]-1))
    thetahat[j,i]<-mean(dnorm(x)/((K[j]-1)*20^(K[j]-1)*x^(-K[j])))
    varhatthetahat[j,i]<-(N[i]-1)^(-1)*mean(dnorm(x)/((K[j]-1)*20^(K[j]-1)*x^(-K[j]))-thetahat[j,i])^2
  }
}

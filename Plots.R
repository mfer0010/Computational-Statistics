rm(list=ls())

x <- seq(-5,5,length=1000);
y = dnorm(x);
plot(x,y,type="l",xlab="x value", ylab="Density",main="Standard Normal Distribution Plot");

rm(list=ls())
x <- seq(15,35,length = 100);
Phi <- mat.or.vec(100,1)
g <- mat.or.vec(100,1)
for (i in 1:length(x)) {
  if(x[i]<=20){
    Phi[i] = 0;
    g[i] = 0;
  } else {
    Phi[i] = dnorm(x[i]);
    g[i] = 4*20^4*(x[i])^(-5);
  }
}
#plot(x,Phi,type="l",xlab="x value", ylab="Density",main="Plot of Phi for x > 20",col="red");
#lines(x,Phi,col="red")
plot(x,g,type="l",xlab="x value", ylab="Density",main="Plot of Importance Sampler g");

rm(list=ls())
require(MASS)

f <- function(x) 0.5*(x[1]^4-16*x[1]^2+5*x[1]+x[2]^4-16*x[2]^2+5*x[2])
n = 100 #no of experiments
b = 500 #no of iterations
optvalues = c()
optpoints = c()


for (k in 1:10) {
#for each experiment
  for (i in 1:n) {
    theta=c(4,6.4) # set initial theta
    #k = runif(1, min= 1, max = 5) #choose k randomly
    Lprev = f(theta)
    #iterations of each experiment
    for (j in 1:b){
      d = mvrnorm(n=1,c(0,0),k*diag(2))
      newTheta = theta+d
      L = f(newTheta)
      if(L<Lprev) {
        theta = newTheta
        Lprev = L
      }
    }
    optvalues[i] = Lprev
  }
  plot(optvalues, main = k)
  if (k==1) {
    results = rbind(c(mean(optvalues), var(optvalues),min(optvalues)))
  } else {
    results = rbind(results,c(mean(optvalues), var(optvalues),min(optvalues)))
  }
}
results
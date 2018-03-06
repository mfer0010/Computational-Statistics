rm(list=ls())

N = c(100,1000,10000,100000) #sample sizes
muy = 0.490842 #mu of Y
#vector declarations:
crudetheta = mat.or.vec(4,1)
crudevariance = mat.or.vec(4,1)
cstar = mat.or.vec(4,1)
cvtheta = mat.or.vec(4,1)
cvvariance = mat.or.vec(4,1)
sigmax = mat.or.vec(4,1)
sigmay = mat.or.vec(4,1)
sigmaxy = mat.or.vec(4,1)

#main loop over all sample sizes
for (i in 1:4) {
  U = runif(N[i],0,2) #generate N[i] uniformly (0,2) distributed RV
  X = exp(-U^2)
  Y = 2*exp(-2*U)
  #Crude Monte Carlo Integral:
  crudetheta[i] = mean(X)
  crudevariance[i] = ((N[i]-1)^-1)*mean((X-crudetheta[i])^2)
  #Calculating sigmas:
  sigmax[i] = crudevariance[i]*N[i]
  sigmay[i] = mean((Y-muy)^2)
  sigmaxy[i] = mean((X-crudetheta[i])*(Y-muy))
  #Using the control variate
  cstar[i] = -sigmaxy[i]/sigmay[i]
  cvtheta[i] = mean(X-cstar*(Y-muy))
  cvvariance[i] = (sigmax[i]-(sigmaxy[i]^2/sigmay[i]))/N[i]
}
#display results:
Table = rbind(crudetheta,cvtheta,crudevariance,cvvariance)
t(Table)

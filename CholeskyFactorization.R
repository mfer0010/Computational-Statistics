rm(list=ls())
set.seed(2)
#set mu to be the mean of the distribution
mu = c(1,0,-0.5)
#Set A to the variance covariance matrix
A = matrix(c(3,1,0.3,1,1.5,0.5,0.3,0.5,2),nrow=3,ncol=3)
#Cholesky Factorisation of matrix A
C = t(chol(A))

#Code used to check result
C
C %*% t

p = 3 #size of row of A
#Assign memory for generated random vectors
X = mat.or.vec(10000,p)
#simulate 10000 readings:
for (reading in 1:10000) {
  #generate std. normal random number
  Y = rnorm(p)
  #Work out the value of the random number following our distribution
  temp = C%*%Y+mu
  #Save the values in matrix X
  for (i in 1:p) {
    X[reading,i] = temp[i,1]
  }
}

par(mfrow=c(3,1))
hist(X[,1])
hist(X[,2])
hist(X[,3])

#Code used to generate the variance-covariance matrix of X
Ones = matrix(rep(1,len=p*10000),nrow=10000,ncol=p)
a = X-Ones*X*(1/10000)
v = t(a)%*%a/10000

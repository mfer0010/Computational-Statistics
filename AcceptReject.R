rm(list=ls())

#create a sequence of numbers beween 0 and 5 incrememnting by 0.01
x = seq(0,5,by = .01)
#create a std normal distribution
y = dnorm(x,0,1)

#create multiple exponential distributions 
e1 = dexp(x,1)
e8 = 0.8*dexp(x,1)
e65 = 0.68*dexp(x,1)
e12 = 1.2*dexp(x,1)

#plot generated distributions and add a legend to the plot
plot(c(0,5),c(0,1),type='n',
     ylab='f(x)')
lines(x,y)
lines(x,e1,col='red')
lines(x,e8,col='cyan')
lines(x,e65,col='magenta')
lines(x,e12,col='green')
legend(2,1,c('Std. Normal','exp. with a = 1'
             ,'exp. with a = 0.8',
             'exp. with a = 0.65',
             'exp. with a = 1.2'),
       lty=c(1,1,1,1,1), lwd=c(.5,.5,.5,.5,.5),
       col=c('black','red','cyan','magenta','green'))

rm(list=ls())
M = c(0.9,0.8,0.7,0.68) #majorising constant
counts = mat.or.vec(length(M),1)
 #loop counter to calculate efficiency
for (i in (1:length(M))) {
  noToGenerate = 10000 #num of points required to generate
  noGenerated = 0 #num. of points accepted, to stop when this reaches 10000
  generatedRandomNumbers=mat.or.vec(1,noToGenerate) #vector to store numbers generated
  ptr = 1 #pointer for vector
  loopCount = 0
  #main loop:
  while (noGenerated < noToGenerate) {
    x = rexp(1) #x~exp(1)
    #generate y on (0,ae^-x)
    y = runif(1,0,M[i]*exp(-x))
    #value of normal distribution at x
    normconst = (1/sqrt(2*pi))*exp(-(x^2)/2)
    if (y <= normconst) {
      #accept the value 
      #give it a sign with probability 0.5:
      sign = runif(1)
      if (sign <= 0.5) {
        generatedRandomNumbers[1,ptr] = -y
      } else {
        generatedRandomNumbers[1,ptr] = y
      }
      ptr = ptr + 1
      #increment no Generated
      noGenerated = noGenerated+1
    }
    loopCount =loopCount + 1
  }
  counts[i] = loopCount
}

hist(generatedRandomNumbers[1,])

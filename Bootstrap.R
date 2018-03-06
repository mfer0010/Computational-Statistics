rm(list=ls())

n = 100 #size of sample
b = 1000 #no. of bootstrap samples to generate
medians = mat.or.vec(b,1)
lqs = mat.or.vec(b,1)
uqs = mat.or.vec(b,1)

#Choose sample.csv
Data = read.csv(file.choose(),header=FALSE)
X = Data[,1]
rm(Data)


#generate 1000 bootstrap readings
for (i in 1:b) {
  sampleboot = sample(X,n,replace = TRUE)
  medians[i]= median(sampleboot)
  lqs[i] = quantile(sampleboot,0.25)
  uqs[i] = quantile(sampleboot,0.75)
}

#calculate bootstrap estimates:
medianBootstrap = mean(medians)
lqBootstrap = mean(lqs)
uqBootstrap = mean(uqs)

#calculate the variance of the bootstrap estimates:
varMedianBS = var(medians)
varlqBS = var(lqs)
varuqBS = var(uqs)

#calculate the normal confidence intervals
CImedian = c(medianBootstrap+qnorm(0.025)*sqrt(varMedianBS),medianBootstrap+qnorm(0.975)*sqrt(varMedianBS))
CIlq = c(lqBootstrap+qnorm(0.025)*sqrt(varlqBS),lqBootstrap+qnorm(0.975)*sqrt(varlqBS))
CIuq = c(uqBootstrap+qnorm(0.025)*sqrt(varuqBS),uqBootstrap+qnorm(0.975)*sqrt(varuqBS))

#calculate the empirical confidence interval
sortedMed = sort(medians)
sortedLq = sort(lqs)
sortedUq = sort(uqs)
ECIMedian = c(sortedMed[25],sortedMed[975])
ECILq = c(sortedLq[25],sortedLq[975])
ECIUq = c(sortedUq[25],sortedUq[975])

#code for diplaying data nicely
medinfo = c(medianBootstrap, varMedianBS, CImedian, ECIMedian)
lqinfo = c(lqBootstrap, varlqBS,CIlq,ECILq)
uqinfo = c(uqBootstrap, varuqBS,CIuq, ECIUq)

table = rbind(medinfo,lqinfo,uqinfo)
table

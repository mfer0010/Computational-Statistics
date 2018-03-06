rm(list=ls())

n = 100 #size of sample
A = 5 #no. of partitions
m = 20 #partition size
medianhat = mat.or.vec(A,1)
lqhat = mat.or.vec(A,1)
uqhat = mat.or.vec(A,1)

#Choose sample.csv
Data = read.csv(file.choose(),header=FALSE)
X = Data[,1]
rm(Data)
medianX = median(X)
lqX = quantile(X,0.25)
uqX = quantile(X,0.75)

#randomly change order of sample
Y = mat.or.vec(n,1)
Y = sample(X,n,replace=F)

temp = mat.or.vec(n-m,1) #to store each sample with a removed partition

#loop over every partition
for (i in 1:A) {
  lb = 1+(i-1)*m #first index of partition
  ub = m*i #last index of partition
  remove = Y[lb:ub]
  temp = setdiff(Y,remove) #temp is Y without the partition
  #calculate jackknife values:
  medianhat[i]=A*medianX-(A-1)*median(temp)
  lqhat[i]=A*lqX-(A-1)*quantile(temp,0.25)
  uqhat[i]=A*uqX-(A-1)*quantile(temp,0.75)
}

#calculate jackknife estimates:
medianJackknife = mean(medianhat)
lqJackknife = mean(lqhat)
uqJackknife = mean(uqhat)

#calculate the variance of the jackknife estimates:
varMedianJK = (1/A)*var(medianhat)
varlqJK = (1/A)*var(lqhat)
varuqJK = (1/A)*var(uqhat)

#calculate the confidence intervals
CImedian = c(medianJackknife+qnorm(0.025)*sqrt(varMedianJK),medianJackknife+qnorm(0.975)*sqrt(varMedianJK))
CIlq = c(lqJackknife+qnorm(0.025)*sqrt(varlqJK),lqJackknife+qnorm(0.975)*sqrt(varlqJK))
CIuq = c(uqJackknife+qnorm(0.025)*sqrt(varuqJK),uqJackknife+qnorm(0.975)*sqrt(varuqJK))

#code for diplaying data nicely
medinfo = c(medianJackknife, varMedianJK, CImedian)
lqinfo = c(lqJackknife, varlqJK,CIlq)
uqinfo = c(uqJackknife, varuqJK,CIuq)

table = rbind(medinfo,lqinfo,uqinfo)
table

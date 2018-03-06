rm(list=ls())

#declare variables
b = 100000 #no of iterations
weights = c(4,3,2,1)
values = c(15,10,9,5)
listvalues = c(b,1)
maxvalues = c(b+1,1)
maxvalues[1] = 0

for (j in 1:b) {
  weight = 0
  value = 0
  i = 1
  item = c()
  #loop to generate thetas  
  while (weight < 100) {
    item[i] = sample(1:4,1) #generate a uniform random number between 1 and 4
    weight = weight+weights[item[i]]
    value = value + values[item[i]]
    i=i+1
  }
  if (weight <= 100) {
    listvalues[j] = value 
  } else {
    #if weight is > 100, remove last element
    value = value-values[item[i-1]]
    item[i-1]=-1
    listvalues[j] = value
  }
  if (listvalues[j]>maxvalues[j]){
    maxvalues[j+1] = listvalues[j]
    thetahat = item
  } else {
    maxvalues[j+1] = maxvalues[j]
  }
}
plot(maxvalues,title="Optimal Value at each Iteration")
print("Optimal Solution:")
table(thetahat)
print("Last Iteration:")
table(item)

library(mc2d)
library(ggplot2)
ndvar(1001)
conc=10
?mcstoc
cook=mcstoc(rempiricalD,values=c(1,1/5,1/50),prob=c(0.027,0.373,0.600))
cook
serving=mcstoc(rgamma,shape=3.93,rate=0.0806)
serving
expo=conc*cook*serving
dose=mcstoc(rpois,lambda=expo)
r=0.001
risk=1-(1-r)^dose
risk
EC1=mc(cook,serving,expo,dose, risk)
print(EC1)
summary(EC1)
a=rnorm(5000, mean = 30000,sd = 3000)
b=runif(5000,min = .40,max = .75)
c=rnorm(5000,mean=3000,sd=300)
d=a*b*c
hist(d)
summary(d)
boxplot(d, range=0)
quantile(d,probs = c(0.75,0.95))
dollar(d)
?dollar
dollar(100)
dollar_format()(100)
example(dollar())
head(d,5)
str(d)
e=dollar(d)
getwd()
library(scales)
summary(d)
summary(e)
str(e)


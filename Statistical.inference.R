## Inference by Jeff Leek
# https://www.youtube.com/watch?v=M8mFArzvyRo
library(UsingR)
data(galton)
head(galton)
attach(galton)
plot(parent,child,pch=19,col="blue")
# put a linear model to the chart
lm1=lm(child~parent,data=galton)
lm1
summary(lm1)
#add the fitted values line to the plot
lines(parent,lm1$fitted,col="red",lwd=3)
## create a population of 1 million families
# create a new data frame
newG=data.frame(parent=rep(NA,1e6),child=rep(NA,1e6))
# specify the parents height as mean of galton data set
newG$parent=rnorm(1e6,mean=mean(galton$parent,sd=sd(galton$parent)))
# also specify the child's height as that derived from the previous linear model
newG$child=lm1$coeff[1]+lm1$coeff[2]*newG$parent+rnorm(1e6,sd=sd(lm1$residuals)) # adding some noise to the model
detach(galton)
attach(newG)
#doing a smooth scatter plot
smoothScatter(parent,child)
abline(lm1,col="red",lwd=3)

# perform a simulation with many different samples taken out of the million dataset and plot 
# add the result to a vector list
# first create the vector list
samplelm=vector(100,mode="list")
for (i in 1:100){
  sampleG=newG[sample(1:1e6,size=50,replace=F),]
  samplelm[i]=lm(sampleG$child~sampleG$parent,data=sampleG)
}
# plot the model as a smooth scatter
smoothScatter(newG$parent,newG$child)
for(i in 1:100){
  abline(samplelm[[i]],lwd=3,lty=2)
}
abline(lm1,col="red",lwd=3)
summary(lm1)
# now lets take a sample from the million data frame
sample4=newG[sample(1:1e6,size=50,replace=F),]
samplelm4=lm(sample4$child~sample4$parent)
summary(samplelm4)
confint(samplelm4)


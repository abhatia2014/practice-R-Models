install.packages('GGally')
library(ggplot2)
library(scales)
install.packages('memisc')
library(GGally)
data(diamonds)
disamp=diamonds[sample(1:length(diamonds$price),1000),]
head(diamonds)
ggpairs(disamp,param=c(shape=I('.'),outlier.shape=I('.')))
plot(disamp$price~disamp$carat)
ggplot(diamonds,aes(price))+theme_bw()+geom_histogram(binwidth=100)
# taking log of price
ggplot(diamonds,aes(price))+theme_bw()+geom_histogram(binwidth=0.01)+
  scale_x_log10()
#lets do price vs carat 
ggplot(diamonds,aes(carat,price))+theme_bw()+geom_point()+
  scale_y_continuous(trans=log10_trans())
# define a function for cube root of the carat
cubrootfun=function(x){
  cubrootfun=x^1/3
}
ggplot(diamonds,aes(carat,price))+theme_bw()+geom_point()+
  scale_x_continuous(trans=cubrootfun(diamonds$carat))+
  scale_y_continuous(trans=log10_trans())
head()

library(dplyr)
diamonds=mutate(diamonds,cubic=x*y*z)
diamonds=mutate(diamonds,total=x+y+z)
#use colMeans to average out the coloumns
colMeans(diamonds[,c(1,5:11)])
#lets round the carat values to 0.25 carat so that the numbers are not all over the place
diamonds$carat=round(diamonds$carat/.25)*.25
#take the means of all numeric fields and group by non numeric fields
summary=aggregate(cbind(depth,table,price,x,y,z,cubic)~cut+color+clarity+carat,data=diamonds,FUN=mean)
#lets see if we can find the average prices of diamonds for different color & clarity combinations
#using the reshape2 package
install.packages('reshape2')
library(reshape2)
# we use the dcast function to get the data in the pivot table format
pt=dcast(diamonds[,c('color','clarity','price')],color~clarity,mean)

library(xts)
library(quantmod)
data("sample_matrix")
myXts=as.xts(sample_matrix,descr="my new xts object")
class(myXts)
str(myXts)
head(myXts)
attr(myXts,'descr')
#subsetting of all 2007
myXts['2007']
#March 2007 to the end of the dataset
myXts['2007-03::']
#march 2007 to the end of 2007
myXts['2007-03::2007']
#the whole dataset
myXts['::']
#the begining of the dataset till may 2007
myXts['::2007-04']
#extracts the index from the Xts object
timeinfo=index(myXts)
timeinfo
datainfo=coredata(myXts)
datainfo
class(timeinfo)
xtsprices=myXts['2007-03::2007-05']
head(xtsprices)
xtsdummy=1.2*xtsprices[1:10,1]-xtsprices[1:10,4]
plot(xtsdummy)
xtsmerged=merge(xtsprices[,1],xtsprices[,2])
xtsmerged

#quantmod
#get prices for Goldmansach from yahoo

getSymbols("GS")
chartSeries(GS)
barChart(GS,theme='white.mono',bar.type = 'hlc')
candleChart(GS,multi.col = TRUE,theme='white')
#adding a line chart
lineChart(GS,line.type = 'h',TA=NULL,theme='white')
#do candle chart only for specific observations
candleChart(GS,subset='2007-12::2008')
candleChart(GS,subset='2008-08::2008')
#changing the x axis labelling
candleChart(GS,theme='white',type='candles')
reChart(major.ticks = 'months',subset = 'first 16 weeks')
#TA refers to the technical indicators
chartSeries(GS,TA=NULL)
#lets' apply some indicators
chartSeries(GS,theme='white',TA="addVo();addBBands();addCCI()")

#get chart symbols for IBM
getSymbols("IBM")
length(IBM)
chartSeries(IBM,subset='2008-03::2008')
addTA(OpCl(IBM),col='blue',type='h')
#it is possible to create a generic TA function
addOpCl=newTA(OpCl,col="green",type='h')
symbols=c("AAPL","QQQ")
getSymbols(symbols)
#define training sets
startT="2007-01-01"
endT="2015-01-01"
rangeT=paste(startT,"::",endT,sep = "")
taapl=AAPL[,6][rangeT]
tqqq=QQQ[,6][rangeT]
head(tqqq)

#****************************Predicting Stock Market Returns**********************

#*********************************************************************************

library(DMwR)
library(xts)
library(quantmod)
getSymbols("IBM")
data(GSPC)
head(IBM)

#create a random xts object
x=xts(rnorm(100),seq(as.POSIXct("2000-01-01"),len=100,by='day'))
x
IBM["2009-01-20"]
IBM["2010-01-01/2010-10-30"]
IBM["2010-01-01::2010-05-01"]
nrow(IBM)
nrow(GSPC)
class(IBM)
class(GSPC)
str(IBM)
getSymbols('^GSPC')
nrow(GSPC)
colnames(IBM) <- c("Open", "High", "Low", 
                    "Close","Volume","AdjClose")
head(IBM)
chartSeries(IBM)

Stock.Open <- c(102.25,102.87,102.25,100.87,103.44,103.87,103.00)
Stock.Close <- c(102.12,102.62,100.12,103.00,103.87,103.12,105.12)
Delt(Stock.Open,type = "log")
Delt(Stock.Open,Stock.Close,k=0:3)

#define a function target indicator with defaults for quotes, margin, no.days

Margin.Indicator=function(quotes,margin=0.025,nodays=5){
  #browser()
  # first get the average current price of the day
  avg.price=apply(HLC(quotes),1,mean)
  #define an emply matrix to absorb values coming in
  r=matrix(NA,ncol=nodays,nrow=nrow(quotes))
  #create a formula to calculate price change (%) using delta function 
  # between successive prices and store in a col of the matrix
  for (i in 1:nodays){
    r[,i]=Next(Delt(avg.price,k=i),i)
  }
  # sum up the rows of the matrix using apply function
  y=apply(r,1,function(x) sum(x[x > margin | x < -margin]))
  
  #output as xts object
  if (is.xts(quotes)) xts(y,time(quotes)) else y
  
}

m=Margin.Indicator(IBM,nodays = 10)
#candle chart draws the candle stick graph of the quotes
candleChart(last(IBM,'3 months'),theme = 'white',TA=NULL)

#we create a function averageprice to calculate average price of the stock over the last 3 months
averageprice=function(quotes){
  apply(HLC(quotes),1,mean)
  
}
#create a new function to add average price to the chart
addaverageprice=newTA(averageprice,col="blue",legend = "average price")
#also create a new TA function to add the Margin Indicator function developed earlier
addmarginindicator=newTA(Margin.Indicator,col="red",legend = "Target Returns")
addaverageprice(on=1)
addmarginindicator()
library(rattle)
rattle()
weather
names(weather)
# build a sample for training , test, validation
weather=weather
train=NULL
trainds=sample(nrow(weather),0.7*nrow(weather))
trainds=weather[trainds,]
validationds=weather[sample(nrow(weather),0.15*nrow(weather)),]
testds=weather[sample(nrow(weather),0.15*nrow(weather)),]
library(rpart)
#create the decision tree model
names(weather)
vars=c("RainTomorrow","Sunshine","Pressure3pm","Cloud3pm")
dectree=rpart(RainTomorrow ~ .,data=trainds[,vars],method="class")
print(dectree)
library(rpart.plot)
fancyRpartPlot(dectree,main="Decision Tree on the weather file")
asRules(dectree)
predicval=predict(dectree,newdata = validationds[,vars],type = "class")
#confusion table (error matrix)
table(predicval,validationds[,"RainTomorrow"])
round(prop.table(table(predicval,validationds[,"RainTomorrow"])),2)
describe(trainds)

library(corrplot)
names(trainds)
cor(trainds[,c(3:9)],use="pairwise",method="pearson")
summary(trainds)
str(trainds)
rc=read.table(file("clipboard"),header=TRUE)
library(fBasics)
basicStats(weather$Sunshine)
install.packages("mice")
library(mice)
md.pattern(weather[,7:10])
scale(trainds$MaxTemp)
t=summary(scale(trainds$MaxTemp))
v=summary(trainds$MaxTemp)
cbind(t,v)


#practice tidy r package from hadley wickam
library(tidyr)
library(dplyr)
head(mtcars)
mtcars$cars=rownames(mtcars)
mtcars=mtcars[,c(12,1:11)]
mtcars=data.frame(mtcars,row.names = NULL)

#Gather command
mtcarsnew=mtcars%>%
  gather(attribute,value,-cars)
#you can gather only certain columns and not all the columns 
#to gather only mpg to gear columns and leave car and carb columns
mtcarsnew=mtcars%>%
  gather(attribute,value,mpg:drat)
  

#spread does the same as cast does in reshape2 package to recreate the package


mtcarsspread=mtcarsnew%>%
  spread(attribute,value)

attack=read.csv("post2008sector.csv",stringsAsFactors = FALSE)
names(attack[500:1254])


#Using Decision trees

?state.region
?state.x77
data(state)
head(state)
head(state.x77)
state=data.frame(state.x77)

library(rattle)
rattle()
library(ROCR)
?performance
install.packages("ggmap")
library(ggmap)
library(mapproj)
library(ggplot2)
map=get_map(location='Europe',zoom=4)
map
ggmap(map)

map2=get_map(location='Germany',zoom=4)
ggmap(map2)
map3=get_map(location='World',zoom=1)
ggmap(map3)

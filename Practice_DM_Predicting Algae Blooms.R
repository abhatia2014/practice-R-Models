

# line 440 for fraudulent transactions

#***************Predicting algae bloom****************************************
#******************************************************************************
#load the data mining with R package

library(DMwR)
library(dplyr)

data(algae)
names(algae)

# As a first step in data mining, we should visualize the data

#understand the structure of the dataset


algae=tbl_df(algae)
glimpse(algae)

#see the summary statistics to understand missing values

summary(algae)

#find the distribution of some quantitative variables like mXPH

#draw a histogram first
hist(algae$mxPH,probability=TRUE)

# Roughly a normal distribution

#to check the normality we draw the qq plot for which we need to load Car package

library(car)

#draw the plots side by side
par(mfrow=c(1,1))
#par(mfrow=c(1,2))
hist(algae$mxPH,probability=TRUE,ylim=0:1)
#hist(algae$mxPH,probability = TRUE,ylim = 0:1)
#add a line in form of density plot
lines(density(algae$mxPH,na.rm=TRUE))
#lines(density(algae$mxPH,na.rm = TRUE))
#add some rug and jitter to the plot
rug(jitter(algae$mxPH))
#rug(jitter(algae$mxPH))

# Now for the second plot to find the normality QQ plot
# to find the normality, we have to load the car package first
library(car)
qqPlot(algae$mxPH,main="Normal QQ plot of maximum PH")

#qqPlot(algae$mxPH,main="normal QQ plot of maximum PH")

# also create a boxplot 

par(mfrow=c(1,1))
boxplot(algae$mxPH,range=0)

boxplot(algae$oPO4,ylab="Orthophosphate",range = 0)
# add rug and jitter to the y axis - side 2
rug(jitter(algae$oPO4),side=2)
#rug(jitter(algae$oPO4),side=2)
# add a line for the mean to the box plot
abline(h=mean(algae$oPO4,na.rm = TRUE),lty=2)
#abline(h=mean(algae$oPO4,na.rm = TRUE),lty=2)

#we look at the ways to visualize outliers
plot(algae$NH4)
#draw a line with mean
abline(h=mean(algae$NH4),na.rm= TRUE)

#draw another line with mean + sd
abline(h=mean(algae$NH4,na.rm = TRUE)+2*sd(algae$NH4,na.rm=TRUE),lty=2)
#abline(h=mean(algae$NH4,na.rm = TRUE)+sd(algae$NH4,na.rm = TRUE),lty=2)

#if we want to know the distribuion as well as the variables it depends upon it using conditioned plots

# we have to load lattice library

library(lattice)
#we create box plot for first response variable a1 for size dimension
names(algae)
print(bwplot(season~a1,data=algae))
print(bwplot(size~a1,data=algae))

# we can also plot the response variables for continous variables by converting the continuous to discrete variables
#continous variable can be converted to a factor variable by using equal count function
mincl=equal.count(na.omit(algae$Cl),number=4,overlap=1/4)
table(mincl)
min02=equal.count(na.omit(algae$mnO2),number=4,overlap=1/4)
min02

#here we condition response variable a3 against season and quantity of mn02

stripplot(season~a3|min02,data = algae[!is.na(algae$mnO2),])
stripplot(season~a1|mincl,data=algae)
#do it as a box plot
print(bwplot(season~a3|min02,data = algae[!is.na(algae$mnO2),]))
print(bwplot(season~a1|mincl,data=algae))
#***************Removing unknown values******************************

#dealing with unknown values, common strategies
#1. Remove observations with unknown values
#2. Fill in unknown values by finding correlations between variables
#3. Fill in unknown values by finding similarities between cases

# to count the number of incomplete rows
nrow(algae[!complete.cases(algae),])

nrow(algae[!complete.cases(algae),])

# removing observations with large unknown values
#manyNAs function flags rows with more than 20% missing
#manyNAs() flags rows with more than 20% missing values

library(DMwR)
manyNAs(algae)

#shows rows 62, 199 have large missing values

#removing these two rows

algae=algae[-manyNAs(algae),]

#filling in unknown with most common values

#option1 - use a measure of centrality -like mean/ mean/ mode- but using mean is best when there is 
#normal distribution

# e.g using mean to fill in the missing values in algae MXPH

summary(algae$mxPH)
data(algae)
algae=tbl_df(algae)
#finding the row with NA 


algae[is.na(algae$mxPH),]
#finding which row has the na

# filling it with mean of the other values

algae[48,'mxPH']=mean(algae$mxPH,na.rm = TRUE)


# check for the skew, if there is a skew, then median would be a better option

# lets look at algae$chla


hist(algae$Chla)# totally right skewed, in such cases much better to fill in the values with median rather than mean


#find the missing values

algae[is.na(algae$Chla),]

#11 missing values, let's fill all of them at once using median
#fill all missing values with median of other values using dplyr



algae[is.na(algae$Chla),]
algae[is.na(algae$Chla),'Chla']=median(algae$Chla,na.rm = TRUE)


#option to use centralImputation function in dmWr package to fill all unknown values with statistics of centrality
#
#The is a function centralImputation() in the book
# package which fills in all unknowns in a dataset
# using a statistic of centrality, using the
# median for numeric columns and the mode (most
# frequent value) for nominal variables:

# reload the data set

data(algae)
#remove the many NA rows
algae=algae[-manyNAs(algae),]

algae=centralImputation(algae)
#algae=centralImputation(algae)
summary(algae)

# However, these simple strategies, while seeming
# efficient are not a good idea.

#2. filling in unknown values by exploring corelations

#let's try to find correlations between mxPH and other variables in the dataset to see if we can fill 
# missing (48) value for mxPH

cor(algae[,4:18],use = "complete.obs")

#becomes more readable if we encode with symnum

symnum(cor(algae[,4:18],use = "complete.obs"))

# there is effective correlation for oPO4 an P04, so we can use linear model to fill that missing value
lm1=lm(PO4 ~ oPO4,data=algae)
names(lm1)

# we now predict the missing value of Po4

algae[is.na(algae$PO4),]
algae[28,'PO4']= lm1$coefficients[[1]]+lm1$coefficients[[2]]*algae[28,'oPO4']

#3. filling in unknown variables by exploring similarities between cases

# here instead of finding correlations between columns, we find similarities between rows to fill unknown values

# reload data set
data(algae)
# remove rows with many NAs
algae=algae[-manyNAs(algae),]

# we can use knnImputation function to find 10 similar neighbours and fill the missing values using that
#the method can be either mode,median or weighted average with weights decreasing as distance increases

algae=knnImputation(algae,k=10,meth = 'weighted average')
#algae=knnImputation(algae,k=10,meth = 'weighted average')

# no missing values, that can be checked
algae[is.na(algae),]

summary(algae)

##Obtaining prediction models***************************************************

#we explore two different prediction models
#1. Multiple linear regression
#2. Regression Trees

#linear regression cannot handle missing values while regression trees can easily

#1. Multiple linear regression

# first let's reload the dataset
data(algae)

# find and delete rows having many NAs 

algae=algae[-manyNAs(algae),]

# removed 2 records

# fill in the other missing values using KNN Imputation with k=10 and method weighted averages

algae=knnImputation(algae,k=10,meth="weighAvg")

# find if there are any missing rows
sum(is.na(algae))
# which gives 0 which is excellent

#we fit a linear regression model for a1- with first 12 variables

lm.a1=lm(a1~.,data=algae[,1:12])

summary(lm.a1)

# we can check diagnostic by calling the plot function
par(mfrow=c(2,2))

plot(lm.a1)
# anova() gives us sequential analysis of # variance of model fit, that is, how much
# does each sequential variable reduce the # residual sums of squares as more terms
# are added to the model.
anova(lm.a1)


# we can get rid of seasons as that has the least reduction in the sum of squares
#use function update to update the linear model
lm2.a1=update(lm.a1,.~.-season)



#some improvement in adjusted R squared , but not significant

# we should compare the fit of the nested models
anova(lm.a1,lm2.a1)


#R has backward elimination process method called step() function that 
#performs model search to fit the best model
final.lm=step(lm.a1)
#final.lm=step(lm.a1)
summary(final.lm)
# compare to the original model using anova
anova(lm.a1,final.lm)
# there is  major improvement in the sumsquares though not much improvement in the Adjusted R square



#2. Regression tree model*******************************

#add library(rpart)- recursive partitioning and regression trees
library(rpart)
?rpartXse
#set seed for the same result
set.seed(1234)
#fit the model using the same 12 variables
rt.a1=rpart(a1~.,data=algae[,1:12])
#rt.a1=rpart(a1~.,data=algae[,1:12])
rt.a1
# we use prettyTree to get a visual representation of the regression tree
par(mfrow=c(1,1))
prettyTree(rt.a1)

#trees are grown in two steps
#1. grow a large busy tree. 2. Prune the tree by deleting bottom nodes through a process of statistical elimination

printcp(rt.a1)

# rpart() simply grows the large tree, stopping  when certain criteria are met: 
#1) decrease in  deviance goes below a certain threshold (cp,# default is 0.01);
# 2) number of samples in a node is less than some threshold (minsplit, defaults is 20); 
# 3) tree depth exceeds some threshold (maxdepth, default is 30).

#we can grow and prune the tree in one step using rpartXse function where all of the pruning variables can be defined

rt.a1=rpartXse(a1~.,data=algae[,1:12])

rt.a1
prettyTree(rt.a1)

#*******Model Evaluation and Selection********************

#model selection is based on the predictive performance of the model

#comparison 1 using mean absolute error (MAE)

#first predict using the linear regression model
lm.predictions.a1=predict(final.lm,algae)

#get predictions for the regression tree model
rt.predictions.a1=predict(rt.a1,algae)
rt.predictions.a1

#calculate the mean absolute error for first
# first method of evaluation is the mean absolute error
mae.a1.lm=mean(abs(lm.predictions.a1-algae[,"a1"]))

mae.a1.rt=mean(abs(rt.predictions.a1-algae[,'a1']))

#MAE is better for Regression trees

# comparison 2 using mean squared error (mean squared error)
mse.a1.lm=mean((lm.predictions.a1-algae[,'a1'])^2)
#mse.a1.lm=mean((lm.predictions.a1-algae[,'a1'])^2)
mse.a1.rt=mean((rt.predictions.a1-algae[,'a1'])^2)
# mean squared error is also better for regression trees

#using Normalized mean squared error(nmse)

nmse.a1.lm=mean((lm.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)
nmse.a1.rt=mean((rt.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)

# NMSE is a unit-less error measure with values usually ranging from zero to one. If the model
# performs better than very simple baseline (mean), NMSE should be less than one, and the smaller,
# the better. Values greater than one means the  model predicts worse than the simple (mean)
# baseline measure.

# alternatively use the function regr.eval() which evaluates regression evaluation metrics

regr.eval(algae[,'a1'],rt.predictions.a1,
          train.y=algae[,'a1'])

regr.eval(algae[,'a1'],lm.predictions.a1,train.y=algae[,'a1'])
# the next step is to plot the two models to see which one gives beter results

par(mfrow=c(1,2))
plot(rt.predictions.a1,algae[,'a1'],main="Regression tree predicion")
?abline
abline(0,1,lty=2)
#call the second plot

plot(lm.predictions.a1,algae[,'a1'],main="Linear Model predicion")
abline(0,1,lty=2)
?abline
#some of the linear model predictions have negaiive values- which dosn't make sense

sensible.lm.predictions.a1=ifelse(lm.predictions.a1<0,0,lm.predictions.a1)
#find the regression evaluation parameters now

regr.eval(algae[,'a1'],lm.predictions.a1,train.y = algae[,'a1'])
regr.eval(algae[,'a1'],sensible.lm.predictions.a1,train.y = algae[,'a1'])

#K fold Cross Validation*****************************************************************
# to achieve a reliable estimate of a models performance on unseen data (test data)
# k-fold Cross Validation is often used to obtain reliable estimates using small datasets: Obtain
# k equally-sized and random subsets of the training data. For each of the k subsets, build a model
# using the remaining k-1 sets and evaluate this model on the kth set. Store the performance of the
# model and repeat this process for all remaining subsets.

# In the end, we have k performance measures, all # obtained by testing a model on data not used in
# the construction of the model.

# we will use the experimentalcomparison() function in the dmWR package
# experimentalComparison() has 3 parameters: (1) data  set to use; (2) alternative models; and (3) choice
# of experimental methodology for obtaining reliable  performance evaluation metrics estimates.

#1. to create variant functions to use for (2) alternative models for experimentalComparison function
?resp
cv.lm=function(form,train,test,...){
  #train
  m=lm(form,train,...)
  #test
  p=predict(m,test)
  # add the rule
  p=ifelse(p<0,0,p)
  #evaluate NMSE
  regr.eval(resp(form, test), p,
            stats=c('mae','nmse'), train.y=resp(form, train))
}

#create another function for regression trees
?resp
cv.rpart=function(form,train,test,...){
  #train
  m=rpartXse(form,train,...)
  #test
  p=predict(m,test)
  #Evaluate NMSE
  
  regr.eval(resp(form, test), p,
            stats=c('mae','nmse'), train.y=resp(form, train))
  
}

#Now we carry out the cross validation experiment using experimental comparison function

res=experimentalComparison(
  # first argument is vector of data sets in form
  # dataset(<formula>,<data frame>,<label>)
  c(dataset(a1~.,algae[,1:12],'a1')),
  # second argument is vector of learning system variants
  # with name of functions to carry out learn+test+evaluate cycle
  c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,1))),
  # third argument specifies 3 reps of k-fold cv process,
  # that k=10, and 1234 is random seed:
  cvSettings(3,10,1234))

# variants() function generates a set of alternative
# models resulting from all possible combinations of
# the parameter values. We use cv.lm() only with its
# default parameters and for cv.rpart() we specify
# different alternative values for parameter se. So
# we get three variants of regression trees. See info
# about third argument above.

summary(res)

plot(res)
#shows rpart.v1 i.e regression tree with se=0 is the best variant for prediction model
# with nmse=0.3069 (lower the better)
# to get the parameter setting for each label

getVariant('cv.rpart.v1',res)

# To check which is the best model, we use the function bestScores()

bestScores(res)

#let's also try to use a random forest approach

library(randomForest)

# create a cv function for random forest
cv.rf=function(form,train,test,...){
  #train
  m=randomForest(form,train,...)
  p=predict(m,test)
  regr.eval(resp(form, test), p,
            stats=c('mae','nmse'), train.y=resp(form, train))
  
}

res=experimentalComparison(
  c(dataset(a1~.,data = algae[,1:12],name = 'a1')),
  c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,0.7)),variants('cv.rf',ntree=c(200,500,700))),
  cvSettings(3,10,1234))

bestScores(res)
plot(res)
?randomForest
?sapply
#we now create a function to get the dataset function in experimentalcomparison() function - 1st arguement

DSs=sapply(names(algae)[12:18], 
           function(x,names.attr){
             f=as.formula(paste(x,"~."))
             dataset(f,algae[,c(names.attr,x)],x)
             }, names(algae)[1:11]  )

#Now we can apply the experimental comparison to the DSs dataset

res.all=experimentalComparison(
  DSs,c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,1)),variants('cv.rf',ntree=c(200,500,700))),
  cvSettings(5,10,1234))
bestScores(res.all)
plot(res.all)

#*****************Prediction for the best models on the test set*****************************
#********************************************************************************************

#We will use the model 
# that our cross validation indicated as "best" in our call to the bestScores() function, either
# "cv.rf.v3", "cv.rf.v2", "cv.rf.v1" or "cv.rpart.v3"




#*************Fraudulent transactions**********************************************************
#**********************************************************************************************
  
#load sales data
data(sales)
head(sales)
str(sales)

#**data explorations**

summary(sales)
sales=tbl_df(sales)
glimpse(sales)

#check in how many cases both quant and val are missing

length(which(is.na(sales$Quant) & is.na(sales$Val)))

#proportion of fraud in the insp field- only 0.32%

round(table(sales$Insp)/nrow(sales)*100,2)

# we look at no of transactions per sales person
names(sales)
totsd=sales%>%
  group_by(ID)%>%
  summarise(TotalCount=n())
  
 totsd 
 # find the number of transactions by product
 
 totpd=sales%>%
   group_by(Prod)%>%
   summarize(TotalCount=n())
 totpd
 #plot of transaction by sales person
 
library(ggplot2)
library(ggvis)
 #plotting using ggvis
 totsd%>%
   ggvis(x=~ID,y=~TotalCount)%>%
   layer_points()
 #plotting using ggplot
 
 ggplot(totsd,aes(ID,TotalCount))+geom_point(stat="identity")+theme_bw()
 
 #plotting using standard graphics
 
 plot(totsd$ID,totsd$TotalCount)
 
 #similarly plotting products by count
 ggplot(totpd,aes(Prod,TotalCount))+geom_point(stat="identity")+theme_bw()
 
 #compute the unit price of products
 
 sales$unitprice=round(sales$Val/sales$Quant,3)
 
 #grouping products by unit price
 unitsales=sales%>%
   group_by(Prod)%>%
   select(Prod,ID,unitprice)
 head(unitsales,20)
 
 #see the unit sales of first 50 observations by Prod and sales person
 
 ggplot(unitsales[1:50,],aes(ID,unitprice,fill=ID))+geom_bar(stat="identity")+
   facet_wrap(~Prod)+theme(legend.position="none")
 ggplot(unitsales[1:50,],aes(Prod,unitprice))+geom_boxplot()+theme_bw()
 
 #finding # products with less than 20 transactions- 982 transactions
 
 totpd%>%
   filter(TotalCount<20)
 
 #find the top most and least expensive products
 
 # find the mean of products using dplyr
 
 upp=sales%>%
   group_by(Prod)%>%
   summarise(MedianPrice=median(unitprice,na.rm=TRUE))
 head(upp,15)
 
 
 #find the five most expensive products
 
 highmed=upp%>%
   arrange(desc(MedianPrice))%>%
   head(5)
 
 # find the five least expensive items
 
 lowmed=upp%>%
   arrange(MedianPrice)%>%
   head(5)
 #finding the rows corresponding to highest median price
 
 topm=sales[sales$Prod %in% highmed$Prod,]
 
 # finding the rows corresponding to lowest median price
 
 sales[sales$Prod %in% lowmed$Prod,]
 
 #box plot of the most expensive data
topm$Prod=factor(topm$Prod)
str(topm)
 topm=data.frame(topm)
ggplot(topm,aes(Prod,unitprice,fill=Prod))+geom_boxplot(outlier.size = 0)+theme_bw()

# Analysis for which sales people bring more money to the company

#top 5 salesmen

# first find the total sales value by salesman
names(sales)
highsales=sales%>%
  group_by(ID)%>%
  summarise(TotalSales=sum(Val,na.rm=T))
#Top 10 salesman

highsales%>%
  arrange(desc(TotalSales))%>%
  head(10)
# top 10 lowest salesmen

highsales%>%
  arrange(TotalSales)%>%
  head(10)
   
# what is the sales income of top 100 sales people compared to total income

top100sales=highsales%>%
  arrange(desc(TotalSales))%>%
  head(100)%>%
  summarise(grandtotal=sum(TotalSales,na.rm=T))
round(top100sales$grandtotal/sum(sales$Val,na.rm = TRUE)*100,2)


#bottom 2000 sales people- what % to the total sales of the company

bottom2000sales=highsales%>%
  arrange(TotalSales)%>%
  head(2000)%>%
  summarise(grandtotal=sum(TotalSales,na.rm=T))
round(bottom2000sales$grandtotal/sum(sales$Val,na.rm = TRUE)*100,2)
 
# quantity sold for each product

quantsold=sales%>%
  group_by(Prod)%>%
  summarise(salesbyprod=sum(Quant,na.rm=T))

# find the total sales quantity of top 100 products

top100prod=quantsold%>%
  arrange(desc(salesbyprod))%>%
  head(100)%>%
  summarise(totalsales=sum(as.numeric(salesbyprod),na.rm=T))
round(top100prod$totalsales/sum(as.numeric(sales$Quant),na.rm = TRUE)*100,2)
 
#bottom 5000 products
bottom4000prod=quantsold%>%
  arrange(salesbyprod)%>%
  head(4000)%>%
  summarise(totalsales=sum(salesbyprod,na.rm=T))
round(bottom4000prod$totalsales/sum(as.numeric(sales$Quant),na.rm = TRUE)*100,2)

#find outliers in the model

# The Box-Plot Rule: Box-plots show outliers. The# rule is that an observation should be tagged as
# an anomaly, a high (low) value if it is above# (below) the high (low) whisker which is defined
# as Q3+(1.5 x IQR) for high and Q1-(1.5 x IQR)# for the low values, where Q1 is the first quartile,
# Q3 is the third quartile, and IQR=(Q3-Q1) and# is the inter-quartile range.

# This 'Box-Plot' Rule works well for normally-# distributed variables, and is robust to the
# presence of a few outliers since it is based# in robust statistics using quartiles.

# We determine the number of outliers (by above# definition) of each product:

# boxplot.stats() function obtains statistics used in the construction of box plots. It returns a
# list and the 'out' component contains observations considered to be outliers.

#using the tapply function to unitprice to find # of outliers by product


outliers=tapply(sales$unitprice,list(Prod=sales$Prod),function(x) length(boxplot.stats(x)$out))

#the top 10 products with most outliers are

outliers[order(outliers,decreasing = T)[1:10]]

#total number of outlier transaction- 29448 which is 7.3% of the total transactions

sum(outliers)
sum(outliers)/nrow(sales)

#*********Dealing with Data Problems****************

#sales people and products involved in unknown transactions (quant, value)- total 888 transactions

unknowntrans=sales%>%
  filter(is.na(Quant) & is.na(Val))%>%
  select(ID,Prod)
#proportion  of salespeople with large number of unknown transactions

salesunknown=unknowntrans%>%
  group_by(ID)%>%
  summarise(notrans=n())%>%
  mutate(percentsales=round(notrans/length(sales$ID)*100,2))%>%
  arrange(desc(notrans))
 salesunknown
 
 # so as a total % of sales people, the salesman wth unknown transaction is a very small number
 
 #Product with large number of unknown transactions
 
 unknownproducts=unknowntrans%>%
   group_by(Prod)%>%
   summarize(nopods=n())%>%
   arrange(desc(nopods))
 
 unknownproducts

 #proportion of products which are associated with unknown transactions to total products 
 
 productsproportion=unknownproducts%>%
   group_by(Prod)%>%
   summarise(notrans=n())%>%
   mutate(percentsales=round(notrans/length(sales$Prod)*100,2))%>%
   arrange(desc(notrans))
 productsproportion
 
 #again practically nil
 
 # However, after examining our alternatives, it  appears that the option of removing all transactions
 # with unknown values on both quantity and value is the best option.
 data(sales)
 sales=sales%>%
   filter(-which(is.na(Quant) & is.na(Val)))
 
 # We begin to analyze remaining reports with either Quant or Val missing.
 
 # Calculate proportion of transactions of each product that have quantity unknown:
 
 produnknownwquant=sales%>%
   filter(is.na(Quant))%>%
   group_by(Prod)%>%
     summarize(totalcount= n())%>%
     mutate(perunknown=round(totalcount/length(sales$Quant)*100,2))%>%
   arrange(desc(totalcount))
 
 produnknownwquant  

 
 library(shiny)
 library(knitr)
 runGist('https://gist.github.com/yihui/6091942')
 
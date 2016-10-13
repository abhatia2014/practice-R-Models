library(DMwR)
library(dplyr)
library(rattle)
data(algae)
head(algae)
algae=tbl_df(algae)
glimplse(algae)

# first step is to find the missing values
summary(algae)

#remove unknown values
#first find the number of incomplete rows
nrow(algae[!complete.cases(algae),])

#16 rows are incomplete with some information

#manyNAs from DMwR package flags rows with more than 20% incomplete cells

manyNAs(algae)
#thus 2 rows, 62 and 199 have more than 20% missing values

#remove these two rows

algae=algae[-manyNAs(algae),]

#the central imputation function function fills in all missing values using median for numerical values
# and mode for nominal values

summary(algae)
algae=centralImputation(algae)
summary(algae)

#the other and a better option is to find similaries between the 10 nearest neighbours to fill the missing values
#it can then be filled using mean , median , mode or weighted average

# reload the data set

data(algae)
#remove manyNA rows

algae=algae[-manyNAs(algae),]

algae=knnImputation(algae,k=10,meth = 'weighted average')

#check for any missing cases

summary(algae)
sum(!complete.cases(algae))
# gives a zero- which means all cases considered are complete

# we will use three different prediction models to predict quantity of algae a1 
#1. multiple linear regression

#create a basic linear model for predicting a1
lm.a1=lm(a1~.,data = algae[,1:12])
#check summary of the linear model
summary(lm.a1)
#notice adjusted r squared is only 32.15% which means only 32% of the model is explained by the chosen variables

anova(lm.a1)

#we should eliminate variables that are not contributing to the linear model
#step is a backward elimination funtion that removes variables iteratively and gets it to the best form
final.a1=step(lm.a1,direction="both")
summary(final.a1)
#adjusted Rsquare has only marginaly improved to 33.2%

#2. Lets try regression trees for predicting  variables now

library(rpart)
set.seed(1234)
rt.a1=rpart(a1~.,data=algae[,1:12])
rt.a1

#see visual representation of the tree
prettyTree(rt.a1)
printcp(rt.a1)

# we can grow, prune the tree in one instance

# we use rpartXSe function

rt.a1=rpart(a1~.,data=algae[1:12,])
summary(rt.a1)
prettyTree(rt.a1)
names(rt.a1)

# we have to minimize the xerror , find the value of cp which minimises the xerror

xerror=rt.a1$cptable[,'xerror']
minxerror=which.min(xerror)
mincp=rt.a1$cptable[minxerror,'CP']

#use this value of CP to prune the tree

final.rt.a1=prune(rt.a1,cp=mincp)
prettyTree(final.rt.a1)
#3. lets predict using the random forest package
rf.a1=randomForest(a1~.,data=algae[,1:12],ntree=500)

#predict using linear model
lm.predictions.a1=predict(final.a1,data=algae)
rt.predictions.a1=predict(final.rt.a1,data=algae)
table(rt.predictions.a1)
rf.predictions.a1=predict(rf.a1,data=algae)
table(rf.predictions.a1)

#some of the linear predictions have negative values that should be changed to zero

lm.predictions.a1=ifelse(lm.predictions.a1<0,0,lm.predictions.a1)

#model evaluation
# we can use normalized mean squared error for computation of model efficiency

nmse.a1.lm=mean((lm.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)
nmse.a1.rt=mean((rt.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)
nmse.a1.rf=mean((rf.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2)

# here decision trees (regression  trees) provide the best estimate 
#alternatively use the regr.eval function which calcualates all metrics for model evaluation

t1=regr.eval(trues = algae[,'a1'],preds = lm.predictions.a1,train.y = algae[,'a1'])
t2=regr.eval(trues = algae[,'a1'],preds = rt.predictions.a1,train.y = algae[,'a1'])
t3=regr.eval(trues = algae[,'a1'],preds = rf.predictions.a1,train.y = algae[,'a1'])
comb.result=data.frame(rbind(t1,t2,t3))
comb.result$model.name=c("Linear Model","Regression Tree","Random Forest")
comb.result=comb.result[,c(7,1:6)]

#lets plot all the three prediction models on a single plot

library(ggplot2)
all.predictions=data.frame(actual=algae[,'a1'],linear=lm.predictions.a1,regr.tree=rt.predictions.a1,rand.forest=rf.predictions.a1)

ggplot(all.predictions)+geom_point(aes(actual,linear),size=3,col='red',show.legend = TRUE)+
  geom_point(aes(actual,regr.tree),size=3,col="blue")+
  geom_point(aes(actual,rand.forest),size=3,col="green")+
  geom_abline(slope = 1,intercept = 0,col='black')

#perform K fold cross validation to check for model evaluation

#here we use the experimental comparison function

# first create a variant function for linear modeling

cv.lm=function(form,train,test,...){
  
  #first train the model
  m=lm(form,train,...)
  #next test the model
  p=predict(m,test)
  #make sure to get only the positive values
  p=ifelse(p<0,0,p)
  #put the evaluation criteria
  regr.eval(resp(form,test),p,stats = c('mae','nmse'),train.y = resp(form,train))
}

#create a CV function for Regression trees
cv.rt=function(form,train,test,...){
  
  #first train the model
  m=rpart(form,train,...)
  allerr=m$cptable[,'xerror']
  minerror=which.min(allerr)
  mincp=m$cptable[minerror,'CP']
  m=prune(m,cp=mincp)
  #next test the model
  p=predict(m,test)
  #put the evaluation criteria
  regr.eval(resp(form,test),p,stats = c('mae','nmse'),train.y = resp(form,train))
}

#similarly create a CV function for Random Forest

cv.rf=function(form,train,test,...){
  
  #first train the model
  m=randomForest(form,train,...)
  #next test the model
  p=predict(m,test)
  #put the evaluation criteria
  regr.eval(resp(form,test),p,stats = c('mae','nmse'),train.y = resp(form,train))
}

#now do the cross validation experiment using the experimental comparison function

excomp=experimentalComparison(
  #first arguement is a vector of the form dataset
  # dataset(<formula>,<data frame>,<label>)
  dataset(a1~.,algae[,1:12],'a1'),
  #second arguement is the vector of variant functions
  c(variants('cv.lm'),variants('cv.rt'),variants('cv.rf',ntree=c(200,500,700))),
  #third arguement is cross validation setting which is 3 repitition of 10 fold CV with a set seed of 1234
  cvSettings(r=3,f=10,s=1234)
  
)
summary(excomp)
plot(excomp)

#to check which model is best
bestScores(excomp)



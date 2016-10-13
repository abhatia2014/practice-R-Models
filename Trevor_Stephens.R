#Trevor Stephens method

train=read.csv("train.csv",header=TRUE,stringsAsFactors=FALSE)
test=read.csv("test.csv",header=TRUE,stringsAsFactors=FALSE)
str(train)
detach(train)
attach(train)
table(Survived)
# to view it as a proportion
prop.table(table(Survived))
# let's add a predition to the test data set that everyone dies
test$Survived=rep(0,418)
names(test)
# to submit the file - test with the passanger ID and Survived column
submit=data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
summary(submit)
write.csv(submit, file="theyallperish.csv",row.names=FALSE)
table(Sex)
prop.table(table(Sex))
prop.table(table(Sex,Survived))
#this is not clear, we want row wise proportions
prop.table(table(Sex,Survived),1)
#Column wise proportion
prop.table(table(Sex,Survived),2)
#74% of females survived while only 18.8% of males survived
# we again do the prediction
test$Survived=0
test$Survived[test$Sex=="female"]=1
table(test$Survived)
# we start looking at the age variable now
summary(Age)
str(train)
# create a new variable Child for all people under the age of 12
train$Child=0
train$Child[train$Age<18]=1
attach(train)
table(Child)
# we want to create a table with gender, age and survival proportions for different subsets
# we use a new function - aggregate to find how many people survived in each category
aggregate(Survived~Child+Sex,data=train,FUN=sum)
# to get the total # of people in each category
aggregate(Survived~Child+Sex,data=train,FUN=length)
# to find the proportion of people survived, we use a function
aggregate(Survived~Child+Sex,data=train,FUN=function(x){sum(x)/length(x)})
names(train)
summary(Fare)
#let us bin the fares into different categories, less than $10, between 10 and 20, 20 to 30, and more than 30
train$Fare2='30+'
train$Fare2[train$Fare<30 & train$Fare>=20]='20-30'
train$Fare2[train$Fare<20 & train$Fare>=10]='10-20'
train$Fare2[train$Fare<10]='<10'
attach(train)
table(Fare2)
# lets again run the aggregate function to see if there is anything interesting
aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN=function(x){sum(x)/length(x)})
# we make a new prediction
test$Survived=0
test$Survived[test$Sex=='female']=1
test$Survived[test$Sex=='female' & test$Pclass==3 & test$Fare>=20]=0
table(test$Survived)
prop.table(table(test$Survived))
# we will now make some decisions based on decision tree
# the package we need is called 'RPART' or Recursive Partitioning and Regression trees
# we have to import this package called rpart
library(rpart)
?rpart
# we do a regression analysis for Survived based on Pclass, Sex, Age, SibSp, Parch, Fare, Embarked
str(train)
library(party)
fit=rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Embarked,data=train,method="class")
summary(fit)
plot(fit)
text(fit)
#the plotting is not very insightful, we will need to install additional packages
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library('rattle')
library('rpart.plot')
library('RColorBrewer')
# we'll try to render the rpart with fancyRpartPlot function
?fancyRpartPlot
fancyRpartPlot(fit)
# let's put this prediction into submission
prediction=predict(fit,test,type="class")
submit=data.frame(PassengerId=test$PassengerId,Survived=prediction)
write.csv(submit,file="myfirstdtree.csv",row.names=FALSE)
summary(submit)
prediction
str(prediction)
#let's control the decision tree by uncapping the power of cp and minsplit 
fit=rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method="class",control=rpart.control(minsplit=2,cp=0))
fancyRpartPlot(fit)
# too many decision trees do not help
#let's try some other decision trees based on factors we see important
# we'll consider age, Pclass, Sex, Sibsp
fit2=rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked,data=train,method="class")
summary(fit2)
# complexity parameter of the model
printcp(fit2)
fancyRpartPlot(fit2)
Name[1]
# we use feature engineering
train=read.csv("train.csv",header=TRUE,stringsAsFactors=FALSE)
test=read.csv("test.csv",header=TRUE,stringsAsFactors=FALSE)
# we make the columns in both train and test data sets
# add a column 'survived' to test data set as well 
test$Survived=NA
# and bind the two data set together using rbind function
combi=rbind(train,test)
str(combi)
#explicitly convert the Name character variable to character
combi$Name=as.character(combi$Name)
combi$Name[1]
#let's split the name using the comma and full stop
#use the function strsplit()
strsplit(combi$Name[1],split = '[,.]')
# it gives a space before the title, we need to remove that as well
strsplit(combi$Name[1],split='[,.]')[[1]]
# to get to the second item of the list, i.e title
strsplit(combi$Name[1],split='[,.]')[[1]][2]
combi$Name[1:5]
# we use the sapply funtion to apply this strsplit function to all names
combi$title=sapply(combi$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})
table(combi$title)
plot(table(combi$title))
# now we strip off the spaces from the titles , we use sub() for this
combi$title=sub(' ','',combi$title)
table(combi$title)
#we'll combine some of the titles to reduce the total #
attach(combi)
title[title %in% c('Mme','Mlle')]='Mlle'
title[title %in% c('Capt','Don','Major','Sir')]='Sir'
title[title %in% c('Dona','Lady','the Countess','Jonkheer')]='Lady'
table(title)
# we convert the variable back to factor
title=factor(title)
table(title)
#let's combine the variables SibSp and Parch into one family size
combi$FamilySize=SibSp+Parch+1
attach(combi)
# we combine all the surnames together e.g Johnson
combi$Surname=sapply(combi$Name,FUN=function(x){strsplit(x, split='[,.]')[[1]][[1]]})
#we will now combine the family size variable to the name using the paste function
combi$FamilyID=paste(as.character(combi$FamilySize),combi$Surname,sep="")
table(combi$FamilyID)
attach(combi)
FamilyID[FamilySize<=2]='Small'
table(combi$FamilyID)
# analyzing with Random forests
#lets build a simple decision tree
attach(train)
classfit=rpart(Survived~Pclass+Sex+Embarked,data=train,method='class')
fancyRpartPlot(classfit)
# random forest means taking a sample of rows and variables in every iteration and performing a decision tree analysis
summary(Age)
summary(combi$Age)
# we will create a prediction on the age of the missing variables using a decision tree
#method used will be anova in the decision tree
agefit=rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+title+FamilySize,data=combi[!is.na(combi$Age),],method='anova')
fancyRpartPlot(agefit)
combi$Age[is.na(combi$Age)]=predict(agefit,combi[is.na(combi$Age),])
summary(combi$Age)
boxplot(combi$Age,range=0,horizontal=TRUE)
summary(combi$Embarked)
head(combi$Embarked)
str(combi$Embarked)
combi$Embarked=factor(combi$Embarked)
summary(combi$Embarked)
#we fill up the blank fields with S since that is the maximum in the summary
#to find which rows are blank
which(combi$Embarked=="")
# these two rows 62,830 will be filled by S
combi$Embarked[c(62,830)]="S"
table(combi$Embarked)
summary(combi$Fare)
# there is one NA in Fare which can be filled by the median of the other variables
which(is.na(combi$Fare))
#replace fare for row 1044 with median of others
combi$Fare[1044]=median(combi$Fare,na.rm=TRUE)
table(combi$Fare)
summary(combi$Fare)
#Random forests in R can only digest upto 32 levels
table(combi$FamilyID)
#our Family ID has more than 32 levels
#let's reduce the levels of FamilyID by converting to a new variable
combi$FamilyID2=combi$FamilyID
combi$FamilyID2=as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize<=3]="Small"
combi$FamilyID2=factor(combi$FamilyID2)
table(combi$FamilyID2)
#let's split the packages back to train and test
train=combi[1:891,]
test=combi[1:418,]
#let's examine them by the random forest package
install.packages("randomForest")
library(randomForest)
#set up a random seed to get the same results
set.seed(1234)
fit=randomForest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+FamilySize,data=train,importance=TRUE,ntree=2000)
varImpPlot(fit)
?part


train=read.csv("train.csv")
test=read.csv("test.csv")
str(train)
train$Survived=factor(train$Survived)
train$Pclass=factor(train$Pclass)
train$Name=as.character(train$Name)
train$Ticket=as.character(train$Ticket)
train$Cabin=as.character(train$Cabin)
attach(train)
str(train)
#lets first start with Data Munging
#we'll use Amelia package to get all the missing data map
install.packages("Amelia")
library(Amelia)
missmap(train,main="Titanic Training Data- Missing Map",col=c("green","black"),legend=TRUE)
plot(Survived~Pclass)
plot(Survived~Sex)
barplot(table(Survived),names.arg=c('Perished','Survived'),main="survived passenger fate",col='blue')
barplot(table(Pclass),names.arg=c('first','second','third'))
hist(Age,main="Age",xlab=NULL, col="brown")
barplot(table(SibSp),main="Siblings and spouse abroad",col="darkblue")
barplot(table(Parch),main='Parents and Kids aboard',col="darkblue")
hist(Fare,main="Fare-fee paid for the ticket",col="darkgreen")
boxplot(Fare,range=0,horizontal=TRUE)
barplot(table(Embarked))
#observing data using mosaic plots
mosaicplot(Pclass~Survived,main="Passenger Fate by Travellling Class",shade=FALSE,color=TRUE,xlab="Pclass",ylab="Survived")
mosaicplot(Embarked~Survived,main= " passenger travelling and embarking",color=TRUE)
boxplot(Age~Survived,range=0)
summary(Age)
names(train)
boxplot(Age~Pclass)
#appears that replacing NAs in  age by mean of the pclass is a better alternative than replacing by a group mean
head(Name,n=10L)
#extract the title out using strspilit function
?strsplit
title=strsplit(Name[1],split='[,.]')[[1]]
title
#get the second element of the name
title=strsplit(Name[1],split='[,.]')[[1]][[2]]
title
#now apply this to all elements of the Name vector- create a new field
train$title=sapply(train$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][[2]]})
table(train$title)
#remove the extra space from the title
train$title=sub(" ","",train$title)
#get all unique titles
unique(train$title)
#to find which records are NA in the age
which(is.na(Age))
#we use the title to substitute for age as we did earlier
#use grep() function to pull the title records in a vector
mr_vector=grep("Mr",x = Name,fixed=TRUE)
mr_vector
master_vector=grep("Master",x = Name,fixed=TRUE)
miss_vector=grep("Miss",x = Name,fixed=TRUE)
mrs_vector=grep("Mrs",x = Name,fixed=TRUE)
rev_vector=grep("Rev",x = Name,fixed=TRUE)
dr_vector=grep("Dr",x = Name,fixed=TRUE)
# find the mean of the ages in each of the vector
master_age=round(mean(Age[train$title=="Master"],na.rm=TRUE),digits = 2)
master_age
mr_age=round(mean(Age[train$title=="Mr"],na.rm=TRUE),digits = 2)
mr_age
miss_age=round(mean(Age[train$title=="Miss"],na.rm=TRUE),digits = 2)
mrs_age=round(mean(Age[train$title=="Mrs"],na.rm=TRUE),digits = 2)
rev_age=round(mean(Age[train$title=="Rev"],na.rm=TRUE),digits = 2)
dr_age=round(mean(Age[train$title=="Dr"],na.rm=TRUE),digits = 2)
# replace NA age by group means
attach(train)
for (i in 1:nrow(train)){
  if (is.na(train[i,6])){
    if (train[i,13]=="Master"){
      train[i,6]=master_age
    }
    else if (train[i,13]=="Mr"){
      train[i,6]=mr_age
    }
    else if (train[i,13]=="Miss"){
      train[i,6]=miss_age
    }
    else if (train[i,13]=="Mrs"){
      train[i,6]=mrs_age
    }
    else if (train[i,13]=="Rev"){
      train[i,6]=rev_age
    }
    else if (train[i,13]=="Dr"){
      train[i,6]=dr_age
    }
    else {
      print("uncaught title")
    }
  }
}
boxplot(Age~train$title)
summary(train$Age)
which(is.na(Age))
summary(train$title)
train$title
#the embarked also has 2 missing variables, let's assign it to the most common value
table(Embarked)
# we assign the missing to S
#replace blanks with NA in embarked
blank=sub("",'1',train$Embarked)
blank
table(Embarked)
table(train$Embarked)
sub("NAC","C",train$Embarked)
sub("NAS","S",train$Embarked)
sub("NAQ","Q",train$Embarked)
table(train$Embarked)
train$Embarked=sub("NAC","C",train$Embarked)
train$Embarked=sub("NAQ","Q",train$Embarked)
train$Embarked=sub("NAS","S",train$Embarked)
table(train$Embarked)
# we assign the NA to S
train$Embarked=sub("NA","S",train$Embarked)
table(train$Embarked)
summary(train$Fare)
boxplot(Fare,range=0)
min(Fare)
attach(train)
meanFcl1=round(mean(Age[Pclass==1]),2)
meanFcl2=round(mean(Age[Pclass==2]),2)
meanFcl3=round(mean(Age[Pclass==3]),2)
for (i in 1:nrow(train)){
  if (train$Fare[i]==0){
    if (train$Pclass[i]==1){
      train$Fare[i]=meanFcl1
    }
    else if (train$Pclass[i]==2){
      train$Fare[i]=meanFcl2
    }
    else {
      train$Fare[i]=meanFcl3
    }
  }
}
table(train$Fare)
summary(train$Fare)
zf=subset(train,Fare=0);zf
boxplot(train$Fare,range=0)
str(title)
str(train$title)
table(train$title)
# write a function to change the titles of many  to a few
functionchtitle=function(data,old.title, new.title){
  for (n in old.title){
    data$title[which(data$title==n)]=new.title
  }
  data$title
}
train$title=functionchtitle(train,c('Capt','Col','Don','Jonkheer','Lady','Major','Rev','Dr','Sir'),"Noble")
train$title=functionchtitle(train,c('the Countess','Ms'),"Mrs")
table(train$title)
train$title=functionchtitle(train,c('Mlle','Mme'),'Miss')
table(train$title)
mosaicplot(title~Survived,train,col=rainbow(2))
plot(train$Survived,train$title,col="darkgreen")

?colname

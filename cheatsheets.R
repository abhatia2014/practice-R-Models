library(dplyr)
data("iris")
tbl_df(iris)
glimpse(iris)
data_frame(a=1:3,b=4:6)
arrange(mtcars,mpg)
arrange(mtcars,desc(mpg))
rename(iris,S.L=Sepal.Length)
library(tidyr)
example(unite)
head(mtcars)
iris%>%
  summarize_each(funs(mean))
iris%>%
  count(Species,wt=NULL)
iris%>%
  group_by(Species)
datasets::"diamonds"

df=data_frame(grp=c("A","B"),fit=4:5,se=1:2)
df
library(ggplot2)
k=ggplot(df,aes(grp,fit,ymin=fit-se,ymax=fit+se))
k+geom_crossbar(fatten = 2)
k+geom_errorbar()
k+geom_linerange()
k+geom_pointrange()
display.brewer.all()
library(RColorBrewer)
display.brewer.all()
k=ggplot(iris,aes(Species, Petal.Length))
k+geom_bar(aes(stat="identity"))+scale_fill_brewer(palette="RdYlGn")
  
ggplot(iris,aes(Species,Petal.Length))+
  geom_violin(aes(fill=..x..))+scale_fill_gradient(low="green",high="red")+geom_rug()+
  geom_jitter()
rm(list=ls())

#predicting birth rate
library(MASS)
library(rpart)
head(birthwt)
?birthwt
summary(birthwt)
hist(birthwt$bwt)
boxplot(birthwt$bwt)

library(dplyr)
cols <- c('low', 'race', 'smoke', 'ht', 'ui')
birthwt[cols] <- lapply(birthwt[cols], as.factor)
glimpse(birthwt)

#let us sample the data into a training and test set
ind=sample(2,nrow(birthwt),replace=TRUE,prob = c(0.75,0.25))
train=birthwt[ind==1,]
test=birthwt[ind==2,]
head(train)
#let's build the model using regression trees
birthtree=rpart(low~.-bwt,data=train,method = 'class')
library(DMwR)
prettyTree(birthtree)
birthtree2=rpartXse(low~.-bwt,data=train,method='class')
plot(birthtree)
text(birthtree,pretty=0)
prettyTree(birthtree2)
preditbirth=predict(birthtree,data=train,type='class')
table(preditbirth,train$low)
summary(birthtree)
birthwtpred=predict(birthtree,newdata = test,type = 'class')
table(birthwtpred,test$low)
accuracy=(26+3)/(26+10+3)*100
accuracy
library(randomForest)
birthpred2=randomForest(low~.-bwt,data=train,method = 'class',ntree=500)
birthwtpred2=predict(birthpred2,newdata = test,type = 'class')
table(birthwtpred2,train$low)
table(train$low)

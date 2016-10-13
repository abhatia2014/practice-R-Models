#relative cPU performance data, described in terms of cycle time, memory size
url="https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data"
machine=read.csv(url,header = FALSE)
colnames(machine)=c('vendor_name','model_name','myct',
                    'mmin','mmax','cach','chmin','chmax','prp','erp')
head(machine)

#exploratory analysis

str(machine)

summary(machine)
sum(is.na(machine))
library(ggplot2)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
corr=machine[,3:9]
##awesome function
chart.Correlation(corr,histogram = TRUE,pch=19)

##Regression Analysis

#1. Linear Regression
model1=lm(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine)

#summary of the model
summary(model1)

#making predictions using linear regression

machine$pred_lm=predict(model1,newdata = machine)

library(caret)

#check the performance of the model using the Caret's postResample

postResample(pred=machine$pred_lm,obs=machine$prp)

#Plot the prediction

ggplot(machine,aes(x=pred_lm,y=prp))+geom_point(alpha=0.5,color="blue",aes(x=pred_lm))+
  geom_smooth(aes(x=pred_lm,y=prp),color='red')+geom_line(aes(x=prp,y=prp),color="black",linetype=2)+
  xlab("Predicted Performance")+ylab('Observed Performance')+
  ggtitle("Linear Regression Analysis of Computer Performance")

#2. Step wise linear Regression

model2=lm(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine)

summary(model2)

#step wise feature selection

final_lm=step(model2)

summary(final_lm)

#make predictions using the final_lm

machine$final_lm=predict(final_lm,newdata=machine)
postResample(pred=machine$final_lm,obs = machine$prp)

#3. Principal Component Regression (PCR)

library(pls)

model_pcr=pcr(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine,validation="CV")
model_pcr
summary(model_pcr)

#make predictions using pcr

machine$pcr=predict(model_pcr,newdata = machine,ncomp = 6)

postResample(machine$pcr,machine$prp)

#4. Using Partial Least Squares

model_plsr=plsr(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine,validation="CV")
summary(model_plsr)
plot(model_plsr)
#make predictions

machine$plsr=predict(model_plsr,newdata = machine,ncomp = 6)

#find accuracy

postResample(machine$plsr,machine$prp)

#5. Penalized Linear Regression
#5.1 Ridge regression

library(glmnet)
head(machine)
x=as.matrix(machine[,3:8])
y=as.matrix(machine[,9])

#create model

model_glmnet=glmnet(x,y,family='gaussian',alpha = 0,lambda = 0.001)
model_glmnet

summary(model_glmnet)
#making predictions

machine$glmnet=predict(model_glmnet,x,type = "link")
head(machine)
#find accuracy parameters
postResample(machine$glmnet,machine$prp)

#6. LASSO Regression (Least Absolute Shrinkage and Selection Operator)

library(lars)

#create model

model_lasso=lars(x,y, type='lasso')

summary(model_lasso)
#building the prediction model

best_lasso=model_lasso$df(which.min(model_lasso$RSS))

machine$lasso=predict(model_lasso,x,type = 'fit')$fit
machine$lasso=NULL


##Non Linear Regression

#7. Multivariate Adaptive Regresssion Splines (MARS)

library(earth)
#create model

model_MARS=earth(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine)
model_MARS
summary(model_MARS)
plot(model_MARS)

#summarize importance of variables
varImp(model_MARS)
#another way

evimp(model_MARS)

#predict using the model

machine$mars=predict(model_MARS,newdata = machine)
postResample(machine$mars,machine$prp)
#so far the best model

#8. Support vector machines

library(kernlab)

model_svm=ksvm(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine)
model_svm

summary(model_svm)
machine$svm=predict(model_svm,newdata=machine)
postResample(machine$svm,machine$prp)

#plotting the prediction

ggplot(machine,aes(x=svm,y=prp))+geom_point(alpha=0.5,color="red")+geom_smooth(aes(x=svm,y=prp),color="blue")+
  geom_line(aes(x=prp,y=prp),color="black")+ggtitle("Support Vector Machine For Regression Analysis")

#9. K Nearest Neighbour

library(caret)

model_knn=knnreg(x,y,k=2)

model_knn

summary(model_knn)

machine$knn=predict(model_knn,x)
postResample(machine$knn,machine$prp)

#plot knn

ggplot(machine,aes(knn,prp))+geom_point(alpha=0.5,color="red")+geom_smooth(aes(knn,prp))+
  geom_line(aes(prp,prp))+ggtitle("Knn regression of Computer Performance")

#10. Neural Network

library(nnet)

model_nnet=nnet(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine,size=12,maxit=500,lineout=T,decay=0.01)

model_nnet

summary(model_nnet)

#prediction

x=machine[,3:8]
machine$nnet=predict(model_nnet,x,type='raw')
postResample(machine$nnet,machine$prp)

##Decision trees for Regression

#11. Classification and Regression trees (CART)

library(rpart)

model_rpart=rpart(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine,control = rpart.control(minsplit = 5))
model_rpart
summary(model_rpart)
library(rpart.plot)
rpart.plot(model_rpart)

#predictions

machine$rpart=predict(model_rpart,x)
postResample(machine$rpart,machine$prp)

#plot the predcitions

ggplot(machine,aes(rpart,prp))+geom_point(alpha=0.5, color="blue")+geom_smooth(aes(rpart,prp),color="brown")+
  geom_line(aes(prp,prp),color="black",linetype=2)+ggtitle("CART Decision Tree for Regression Analysis")

#12. Conditional Regression trees (CTREE)

library(party)
model_ctree=ctree(prp~myct+mmin+mmax+cach+chmin+chmax,data=machine,controls = ctree_control(minsplit = 2,minbucket = 2,testtype = "Univariate"))
model_ctree
plot(model_ctree)

#make predictions

machine$ctree=predict(model_ctree,x)
postResample(machine$ctree,machine$prp)

#plot
ggplot(machine,aes(ctree,prp))+geom_point(alpha=0.5, color="blue")+geom_smooth(aes(ctree),color="brown")+
  geom_line(aes(prp,prp),color="black",linetype=2)+ggtitle("Conditional decision Tree for Regression Analysis")

# Model Trees

library(RWeka)

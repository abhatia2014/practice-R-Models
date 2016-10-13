#Xgboost is short for eXtreme Boosting package
#takes several input data
#1. Dense Matrix R - matrix
#2. Sparse Matrix R- Matrix::dgCMatrix
#3. Data File, local data file xgb.DMatrix

install.packages('xgboost')
library(xgboost)

#load the agaricus dataset embedded within the package
data("agaricus.train")
data("agaricus.test")
train=agaricus.train
test=agaricus.test

str(train)
dim(train$data)
head(train$data)
dim(test$data)

class(train$data)[1]
class(train$label)

#In sparse matrix, cells containing 0 are not stored in memory, memory size is reduced and its very efficient

#training the first model 

bstsparse=xgboost(data=train$data,label=train$label,max.depth=2,eta=1,nthread=2,nround=2,objective="binary:logistic")

#alternatively , we can put the data in a dense matrix, basic  R matrix

bstDense=xgboost(data=as.matrix(train$data),label=train$label,max.depth=2,eta=1,nthread=2,nround=2,objective="binary:logistic")

#using the xgb.DMatrix

dtrain=xgb.DMatrix(data=train$data,label=train$label)
bstDMatrix=xgboost(data=dtrain,max.depth=2,eta=1,nthread=2,nround=2,objective="binary:logistic")

#verbose option
#when verbose=0, there is no feedback

bst=xgboost(data=dtrain,max.depth=2,eta=1,nthread=2,nround=2,objective="binary:logistic",verbose = 0)

#verbose =1 to print evaluation matrix
bst=xgboost(data=dtrain,max.depth=2,eta=1,nthread=2,nround=2,objective="binary:logistic",verbose = 1)

#verbose=2 print information about the tree

bst=xgboost(data=dtrain,max.depth=2,eta=1,nthread=2,nround=2,objective="binary:logistic",verbose = 2)

#basic prediction using the xgboost

pred=predict(bst,test$data)
head(pred)

#to get binary classification
prediction=as.numeric(pred>0.5)
head(prediction)

#advanced features

#for the advanced features, the data need to be put in the xgb.DMatrix form

dtrain=xgb.DMatrix(data=train$data,label=train$label)
dtest=xgb.DMatrix(data=test$data,label=test$label)

#measure learning progress with xgb.train

#advanced version with xgb.train

watchlist=list(train=dtrain,test=dtest)

bst=xgb.train(data=dtrain,max.depth=2,eta=1,nthread=2,nround=2,watchlist = watchlist,objective="binary:logistic")

#use different evaluation metrics

bst=xgb.train(data=dtrain,max.depth=2,eta=1,nthread=2,nround=2,watchlist = watchlist,objective="binary:logistic",eval.metric="error",eval.metric="logloss")

#all of the above learning has been based on trees, we will now use the XG Boost on linear models
# remove the eta parameter and put booster="gblinear

bst=xgb.train(data=dtrain,booster="gblinear",max.depth=2,nthread=2,nround=2,watchlist = watchlist,objective="binary:logistic")

#to save the xgb.DMatrix, use xgb.DMatrix.save command

xgb.DMatrix.save(dtrain,"dtrain.buffer")

#to load the buffer

dtrain2=xgb.DMatrix("dtrain.buffer")

#understand feature importance 
importance_matrix=xgb.importance(model=bst)
#plot feature importance
xgb.plot.importance(importance_matrix = importance_matrix)
library(Ckmeans.1d.dp)
#plotting trees
library(DiagrammeR)
xgb.plot.tree(model = bst)
library(rattle)


#***********Practice xgBoost***********************************
#**************************************************************

require(xgboost)

require(methods)
require(data.table)
require(magrittr)

#variations in the loop 

#1. modifying an existing object
df=data.frame(
  a=rnorm(10),
  b=rnorm(10),
  c=rnorm(10),
  d=rnorm(10)
)
df
rescale01=function(x){
  rng=range(x,na.rm = TRUE)
  (x-rng[1])/(rng[2]-rng[1])
}

df$a=rescale01(df$a)
df$a

#applying the loop to all columns of df

for (i in seq_along(df)){
  df[[i]]=rescale01(df[[i]])
}

df

#ways to loop over a vector

#1. for i in 1:seq_along(xs), extracting the value with x[[i]]

#looping over names for (nm in names(xs))

apply(df,2,mean)
apply(df,1,mean)

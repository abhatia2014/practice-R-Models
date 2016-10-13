library(rjson)
pizza=fromJSON(file="train.json")
str(pizza)
##convert the list to a data frame

library(plyr)
pizza=rbind.fill(lapply(pizza,function(x)as.data.frame(t(x))))
head(pizza,2)
str(pizza)
pizza=as.data.frame(pizza,row.names = NULL,stringsAsFactors = TRUE)
head(pizza,2)
names(nummodpizza)

nummodpizza$requester_account_age_in_days_at_request=as.numeric(nummodpizza$requester_account_age_in_days_at_request)
nummodpizza$requester_days_since_first_post_on_raop_at_request=as.numeric(nummodpizza$requester_days_since_first_post_on_raop_at_request)
modpizza$requester_number_of_comments_at_request=as.numeric(modpizza$requester_number_of_comments_at_request)
modpizza$requester_number_of_comments_at_retrieval=as.numeric(modpizza$requester_number_of_comments_at_retrieval)
modpizza$requester_number_of_posts_at_request=as.numeric(modpizza$requester_number_of_posts_at_request)
modpizza$requester_number_of_posts_at_retrieval=as.numeric(modpizza$requester_number_of_posts_at_retrieval)
modpizza$requester_number_of_subreddits_at_request=as.numeric(modpizza$requester_number_of_subreddits_at_request)
modpizza$requester_upvotes_minus_downvotes_at_request=as.numeric(modpizza$requester_upvotes_minus_downvotes_at_request)
modpizza$requester_upvotes_minus_downvotes_at_retrieval=as.numeric(modpizza$requester_upvotes_minus_downvotes_at_retrieval)
str(modpizza)
names(modpizza)
str(modpizza$downvotes.retrieval)
modpizza$downvotes.retrieval=as.numeric(modpizza$downvotes.retrieval)
nummodpizza=modpizza[,c(1,2,4,7:15)]
str(nummodpizza)
names(nummodpizza)
nummodpizza=nummodpizza[,-10]
nummodpizza$requester_received_pizza=as.numeric(nummodpizza$requester_received_pizza)
str(nummodpizza)
table(nummodpizza$requester_received_pizza)

## now the data is clean, let's start analyzing based on numercial data first and then textual data

#let's put a logistic regression model first
names(nummodpizza)
pizzapred=glm(requester_received_pizza~.,data=nummodpizza,family=binomial("logit"))
summary(pizzapred)
step(pizzapred,direction="backward")
model2=glm(formula = requester_received_pizza ~ downvotes.retrieval + 
             upvotes.retrieval + no.comments.retrieval + requester_number_of_comments_at_retrieval + 
             requester_number_of_posts_at_retrieval + requester_upvotes_minus_downvotes_at_retrieval, 
           family = binomial("logit"), data = nummodpizza)
summary(model2)
model3=glm(formula = requester_received_pizza ~ downvotes.retrieval + 
             no.comments.retrieval + requester_number_of_comments_at_retrieval + 
             requester_number_of_posts_at_retrieval + requester_upvotes_minus_downvotes_at_retrieval, 
           family = binomial("logit"), data = nummodpizza)
summary(model3)
nummodpizza$pred=predict(model2)
head(nummodpizza$pred,10)

## prediction by decision tree
library(rpart)
fit=rpart(requester_received_pizza ~ downvotes.retrieval + 
            upvotes.retrieval + no.comments.retrieval + requester_number_of_comments_at_retrieval + 
            requester_number_of_posts_at_retrieval + requester_upvotes_minus_downvotes_at_retrieval,data=nummodpizza,method="class")
summary(fit)
plot(fit)
text(fit)

#we need to install additional packages
library('rattle')
library('rpart.plot')
library('RColorBrewer')
fancyRpartPlot(fit,main="Decision Tree plot")


## let's get the test data set, do the same functions as training data set and submit 

pizzatest=fromJSON(file="test.json")
str(pizzatest)
##convert the list to a data frame


pizzatest=rbind.fill(lapply(pizzatest,function(x)as.data.frame(t(x))))
head(pizzatest,2)
str(pizzatest)
pizzatest=as.data.frame(pizzatest,row.names = NULL,stringsAsFactors = TRUE)

#find the common column names in the test and training data sets
b=NULL
head(pizza,2)
names(pizza)
names(pizzatest)





modpizzatest=pizzatest[,-c(1,3,5,6,11,12,14,16)]
names(modpizzatest)
modpizza$requester_number_of_comments_at_retrieval=as.numeric(modpizza$requester_number_of_comments_at_retrieval)
str(modpizza$requester_number_of_comments_at_retrieval)
table(modpizza$requester_number_of_comments_at_retrieval)

#simplify the column names and convert the data type
names(modpizza)
colnames(modpizza)[1]="downvotes.retrieval"
names(modpizza)
colnames(modpizza)[2]="upvotes.retrieval"
colnames(modpizza)[4]="no.comments.retrieval"
colnames(modpizza)[1]="downvotes.retrieval"
modpizza[,c(1,2,4,7:15,17:21)]=as.numeric(modpizza[,c(1,2,4,7:15,17:21)])
modpizza=modpizza[,-c(9,10,13,14,20:22)]
names(modpizza)
modpizza$upvotes.retrieval=as.numeric(modpizza$upvotes.retrieval)
modpizza$no.comments.retrieval=as.numeric(modpizza$no.comments.retrieval)
modpizza$requester_number_of_comments_at_request=as.numeric(modpizza$requester_number_of_comments_at_request)
modpizza$requester_number_of_comments_at_retrieval=as.numeric(modpizza$requester_number_of_comments_at_retrieval)
modpizza$requester_number_of_posts_at_request=as.numeric(modpizza$requester_number_of_posts_at_request)
modpizza$requester_number_of_posts_at_retrieval=as.numeric(modpizza$requester_number_of_posts_at_retrieval)
modpizza$requester_number_of_subreddits_at_request=as.numeric(modpizza$requester_number_of_subreddits_at_request)
modpizza$requester_upvotes_minus_downvotes_at_request=as.numeric(modpizza$requester_upvotes_minus_downvotes_at_request)
modpizza$requester_upvotes_minus_downvotes_at_retrieval=as.numeric(modpizza$requester_upvotes_minus_downvotes_at_retrieval)
str(modpizza)
names(modpizza)
str(modpizza$downvotes.retrieval)
modpizza$downvotes.retrieval=as.numeric(modpizza$downvotes.retrieval)
nummodpizza=modpizza[,c(1,2,4,7:15)]
str(nummodpizza)
names(nummodpizza)
nummodpizza=nummodpizza[,-10]
nummodpizza$requester_received_pizza=as.numeric(nummodpizza$requester_received_pizza)
str(nummodpizza)
table(nummodpizza$requester_received_pizza)
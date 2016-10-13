library(MASS)
library(ISLR)

#1. simple linear regression
?Boston
?fix
names(Boston)
fix(Boston)
# we use a simple linear regression with medv as the response variable and lstat as the predictor
lm.fit=lm(medv~lstat,data=Boston)
lm.fit
summary(lm.fit)
names(lm.fit)
summary(lm.fit$residuals)
boxplot(lm.fit$residuals)
plot(lm.fit$fitted.values,lm.fit$residuals)
lm.fit$coefficients
coef(lm.fit)
?confint
confint(lm.fit)
#predict() function can be used to produce the confidence intervals and prediction intervals of medv  for a given value of lstat
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")
#we now plot medv and lstat along with the least square regression line using plot() and abline() functions
attach(Boston)
plot(lstat,medv)
abline(lm.fit)

#some experiments with abline() function
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

plot(lstat,medv,col="red")
plot(lstat,medv,pch=1)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=9)
par(mfrow=c(1,1))
plot(lm.fit)
# the function rstudent() will return the studentized residuals and we can use this function to plot th residuals against the fitted values
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
#leverage statistics can be computed using hatvalues() function
plot(hatvalues(lm.fit))
?hatvalues
which.max(hatvalues(lm.fit))

#Multiple linear regressions
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)

#regression using all of the data in Boston data set

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
names(lm.fit)
names(summary(lm.fit))
summary(lm.fit)$r.sq
summary(lm.fit)$adj.r.sq
library(car)
# The car package must be downloaded to find the VIF (variance Inflation factor) for this data
vif(lm.fit)
#since the vif is low for all variables, indicates no collinearity between the variables

# do the regression for all variables, except age since it has a high p value

lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
# also remove indus
lm.fit11=update(lm.fit1,~.-indus)
summary(lm.fit11)
# alternatively update function can be used to update lm.fit
lm.fit1=update(lm.fit,~.-age)
lm.fit2=update(lm.fit1,~.-indus)
summary(lm.fit2)

#to include the interaction terms
# the syntax lstat*age simultaneously includes lstat, age and the interaction term lstat*age as predictors
summary(lm(medv~lstat*age,data=Boston))

#non linear transformations of the predictors

# we can do non linear transformation using I(x^2) to do square predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

# we can use analysis of variance , anova function to quantify further the extent to which the quadratic fit is superior to the linear fit
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
plot(lm.fit1)

#now let's try to add a fifth order polynomial fit using the function poly()
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

# beyond 5th order does not have any significant improvement in the model

#let's look at the log transformation 

lm.fitlog=lm(medv~log(rm),data=Boston)
anova(lm.fit,lm.fitlog)

#using qualitative (factor) predictors

fix(Carseats)
?Carseats
names(Carseats)
str(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

# to understand the coding for dummy variables, contrasts() function returns the answer

detach(Boston)
attach(Carseats)
contrasts(ShelveLoc)

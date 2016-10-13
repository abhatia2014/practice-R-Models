##http://ww2.coastal.edu/kingw/statistics/R-tutorials/descriptive.html
getwd()
?rm
gender = rep(c("female","male"),c(1835,2691))
admitted = rep(c("yes","no","yes","no"),c(557,1278,1198,1493))
dept = rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
           c(89,17,202,131,94,24,19,8,391,244,299,317))
dept2 = rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
            c(512,353,120,138,53,22,313,207,205,279,138,351))
department = c(dept,dept2)
ucb = data.frame(gender,admitted,department)
rm(gender,admitted,dept,dept2,department)
str(ucb)
head(ucb)
colnames(ucb)
#look at every 400th row

ucb[seq(1,4526,400),]
summary(ucb)
table(ucb$gender)
#to calcualte proportions
table(ucb$department)
prop.table(table(ucb$department))

#calculate percentage 

round(prop.table(table(ucb$department))*100,2)

#prepare contigency tables or xtabs
with(ucb,table(gender,admitted))
#converting it as a prop table
prop.table(with(ucb,table(gender,admitted)))
#taking % by row
prop.table(with(ucb,table(gender,admitted)),1)

#using Xtabs

with(ucb,xtabs(~gender+admitted))
#alternatively,
a=xtabs(~gender+admitted,data=ucb)
a
prop.table(a,1)
#convert the prop.table to margin(totals)
addmargins(a)

#collapse table 

margin.table(a,1)
margin.table(a,2)#collapsing by admitted
table(ucb$admitted)

with(ucb,table(gender,department,admitted))

#using ftable

ftable(ucb)
ftable(ucb,col.vars="gender") # gender in columns

ftable(ucb,col.vars="admitted")# admitted in columns

#ftable can be used as a prop table as well

q=ftable(ucb,col.vars="admitted")
prop.table(q,1)

ftable(ucb,row.vars="department",col.vars=c("gender","admitted"))
prop.table(ftable(ucb,row.vars="department",col.vars=c("admitted","gender")),1)
my.table=with(ucb,table(admitted,department,gender))
my.table
my.dftable=as.data.frame(my.table)
my.dftable
names(my.dftable)
#using xtabs with the new dataframe

xtabs(Freq~gender+admitted+department,data=my.dftable)
ftable(my.dftable)
data(faithful)
str(faithful)
plot(faithful$eruptions,faithful$waiting,col=c("red","blue"))
attach(faithful)
sum(is.na(waiting))
# to find a particular quantile
quantile(waiting,0.36)
boxplot(waiting, eruptions,range=0,horizontal=TRUE)
library(ggplot2)
ggplot(faithful,aes(waiting))+geom_histogram(binwidth=1)+theme_bw()
table(waiting)
# let's try and cut the waiting data into 10 breaks
wait3=cut(waiting,breaks = 10)
table(wait3)
as.data.frame(table(wait3))
#defining the cut breaks yourself

wait4=cut(waiting,breaks=seq(40,100,5))
wait4
as.data.frame(table(wait4))
hist(waiting)
## better way of seeing a grouped frequency is by creating a stem and leaf display
stem(waiting)
detach(faithful)
rm(ucb)
rm(a,q,my.table)
rm(my.dftable)
states=as.data.frame(state.x77)
str(states)
states$region=state.region
head(states)
# to find mean of life expectancy by regions
by(data=states[4],INDICES=states[9],FUN=mean,)
?tapply
example(tapply)
# using tapply function
tapply(X = states[,4],INDEX = states[,9],FUN = mean)
names(states)[4]
names(states)[4]="life.exp"

## High level plotting functions
data(faithful)
attach(faithful)
plot(waiting,eruptions)
detach(faithful)
rm(faithful)

data(sunspots)
str(sunspots)
plot(sunspots)
rm(sunspots)
data(UCBAdmissions)
str(UCBAdmissions)
plot(UCBAdmissions)
class(UCBAdmissions)
rm(UCBAdmissions)
data(mtcars)
plot(mtcars)
pairs(mtcars)
rm(mtcars)
data(UCBAdmissions)
str(UCBAdmissions)
dept=margin.table(UCBAdmissions,3)
dept
head(UCBAdmissions)
pie(dept)
barplot(dept)
admit.by.dept=margin.table(UCBAdmissions,c(1,3))
admit.by.dept
barplot(admit.by.dept,beside=F)
data(faithful)
attach(faithful)
hist(waiting)
plot(density(waiting))
hist(waiting,prob=T)
lines(density(waiting))
detach(faithful)

rm(faithful)

##Probability distributions, quantiles

#chisq goodness of fit test

chisq.test(c(25,32,18,20))
no=c(25,32,18,20)
null.prob=c(1/3,1/3,1/6,1/6)
results=chisq.test(no,p = null.prob)
results
names(results)
results$p.value
results$expected
results$residuals
results=chisq.test(no,p=c(30,28,28,11)/97)
results

##Chisq test from a table object
d=data(HairEyeColor)
head(d)
dimnames(HairEyeColor)
eyes=margin.table(HairEyeColor,2)
eyes
table(HairEyeColor)
head(HairEyeColor)
table(HairEyeColor[2])
margin.table(HairEyeColor,2)
chisq.test(eyes,p=c(0.5,0.25,0.15,0.10))

# If the data contains categorical variables
data(survey,package="MASS")
str(survey)
?survey
table(survey$Smoke)
smokers=table(survey$Smoke)
smokers
## we can test the null hypothesis that 70% of the students are non-smokers and others (30% ) can be divided equally among the 3 categories
b=chisq.test(smokers,p=c(0.1,0.7,0.1,0.1))
b
b$residuals

## Chi sq test of independence
row1 = c(91,90,51)  
row2 = c(150,200,155)
row3 = c(109,198,172)
data.table=rbind(row1,row2,row3)
data.table
chisq.test(data.table)
row.names(data.table)=c("lt.45","45.to.59","ge.60")

data.table
colnames(data.table)=c("Monthly","Occassionally","Never")
## to add totals to the table
addmargins(data.table)
# add proportions to the table
prop.table(data.table,1)
barplot(data.table,beside=T)

#Data from a table object

UCBAdmissions
ftable(UCBAdmissions,row.vars=c("Admit"))
round(prop.table(ftable(UCBAdmissions,row.vars=c("Admit")),2),2)## admissions proportions by departments and column wise %tages
#we want to look only at data for department B

deptB=UCBAdmissions[,,2]# all rows and columns of layer 2
deptB
p=chisq.test(deptB)
p$expected
p
## data from a data frame

data(survey,package="MASS")
attach(survey)
table(Sex,Fold)
addmargins(table(Sex,Fold))
chisq.test(Sex,Fold)
detach(survey)
rm(survey)
freqs = c(2,2,3,8,7,2)
data.matrix=matrix(freqs,nrow=2)
dimnames(data.matrix)=list("treatment"=c("Celexa","placebo"),"outcome"=c("worse","same","better"))
data.matrix
p=chisq.test(data.matrix)
p
p$expected
fisher.test(data.matrix)
T=data(Titanic)
Titanic
str(Titanic)
dimnames(Titanic)
T=as.data.frame(Titanic)
T
margin.table(Titanic)


##Linear log analysis will allow us to look for the relationship among the variables
#in a multiway contingency table like this one
margin.table(Titanic,c(2,4))
addmargins(prop.table(margin.table(Titanic,c(2,4)),1))
summary(Titanic)
#let's try and fit some log linear models in R
library("MASS")
loglm(~ Class+Sex+Age+Survived,data=Titanic)
##log linear analysis done by a glm model

glm.model=glm(Freq~Class*Age*Sex*Survived,data=T,family=poisson)
summary(glm.model)
anova(glm.model,test="Chisq")
mosaicplot(Titanic,shade=TRUE)
normtemp=scan()
index=seq(1,388,3)
degreesF=normtemp[index]
head(degreesF)
qqnorm(degreesF)
qqline(degreesF)
plot(density(degreesF))
shapiro.test(degreesF)
t.test(degreesF,mu = 98.6,alternative = "two.sided")
t.test(degreesF,mu=98.6,conf.level = .99)

##Power function for testing the power of t test
power.t.test(n = 130,delta = 98.6-98.25,sd = 0.7332,sig.level = 0.05,type = "one.sample",alternative = "two.sided")


## Simple linear correlation and regression
library("MASS")
data(cats)
str(cats)
summary(cats)
with(cats,plot(Bwt,Hwt))
# correlation between body weight and height
with(cats,cor(Bwt,Hwt))
with(cats,cor.test(Bwt,Hwt))

##correlation and covariance matrix
rm(cats)
data(cement)
str(cement)
cor(cement)
pairs(cement)
rm(cement)
data(cats)
str(cats)
attach(cats)
a=lm(Hwt~Bwt)
a
summary(a)
anova(a)

plot(Hwt~Bwt,main="Kitty plot")
abline(a, col="red")
library(ggplot2)
g=ggplot(cats,aes(x = Bwt,y = Hwt))
g=g+geom_point()+geom_smooth(method="lm")+theme_bw()
g
par(mfrow=c(2,2))
plot(a)
## case 144 is perhaps worth taking a look
cats[144,]
names(a)
a$fitted[144]
a$residuals[144]
# let's remove the outlier and then take the regression

ahat=lm(Hwt~Bwt,subset=(Hwt<20.5))
ahat
summary(ahat)
#alternatively we can remove all of the outlier points
a2hat=rlm(Hwt~Bwt)
summary(a2hat)
anova(a2hat)

#Lowess stands for locally weighted scatterplot smoothening
par(mfrow=c(1,1))
plot(Hwt~Bwt)
lines(lowess(Hwt~Bwt),col="red")
scatter.smooth(Hwt~Bwt)
detach(cats)

##simple non linear correlation and regression
data(pressure)
str(pressure)
summary(pressure)
pressure$temperature=pressure$temperature+273.15
#temp is now in degrees kelvin
pressure$pressure=pressure$pressure*.1333
summary(pressure)
with(pressure, plot(pressure, temperature ))
pres=pressure$pressure
temp=pressure$temperature
rm(pressure)
plot(log(pres)~temp)
#alternatively we can also write as

plot(pres~temp,log="y")
#using both sides logs
plot(pres~temp,log="xy")
plot(pres~temp,log="x")
# let's fit a linear model to the log pres and temp

lm1=lm(log(pres)~temp)
lm1
summary(lm1)
plot(lm1$fitted,lm1$resid)
#lets look at the power model now

lm2=lm(log(pres)~log(temp))
summary(lm2)
plot(lm2$fitted,lm2$resid)

##polynomial regression

plot(pres~temp)

#lets' fit a 3rd order polynomial to the model
lm3=lm(pres~temp+ I(temp^2)+I(temp^3))
summary(lm3)
plot(lm3$fitted,lm3$resid)

##logistic regression and generalized linear models
#install package HSAUR
library(HSAUR)
head(plasma)
str(plasma)
# fitting a generalized linear model
fit=glm(ESR~fibrinogen, data=plasma,family=binomial())
summary(fit)
fit
confint(fit)
#let's exponentiate the output
exp(coef(fit)["fibrinogen"])
exp(confint(fit,parm="fibrinogen"))
anova(fit)
fit2=glm(ESR~fibrinogen+globulin,data=plasma,family="binomial")
summary(fit2)
## making a bubble plot
plot(globulin~fibrinogen,data=plasma,xlim=c(2,6),ylim=c(25,55))
symbols()
head(womensrole)
fm1=cbind(agree,disagree)~sex+education
# do another glm
gml1=glm(fm1,data=womensrole,family="binomial")
summary(gml1)

#Logit regression
library(ggplot2)
library(aod)
install.packages("aod")

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
summary(mydata)
sapply(mydata,FUN = sd)
str(mydata)
mydata$rank=factor(mydata$rank)
mylogit=glm(admit~gre+gpa+rank,data=mydata,family="binomial")
summary(mylogit)
#we use the confint for the coefficient estimates
confint(mylogit)
confint.default(mylogit)
exp(coef(mylogit))
plot(mylogit)

#multiple linear regresssion
state.x77
str(state.x77)
head(state.x77)
#coerce into a data frame
st=as.data.frame(state.x77)
str(st)
#let's add the population density variable here
colnames(st)[4]="life.exp"
colnames(st)[6]="hs.grad"
st$density=st$population*1000/st$area
names(st)=tolower(names(st))
str(st)
st$desity=NULL
str(st)
summary(st)
#find the correlation matrix
cor(st)
pairs(st)
#let us take life exp as the response variable
# lets put all the variables into a linear additive model
options(show.signif.start=F)
names(st)
model1=lm(life.exp~population+income+illiteracy+murder+hs.grad+frost+area+density,data=st)
summary(model1)
#another way of looking at the model
summary.aov(model1)
#the minimum adequate model- drop area
model2=update(model1,.~.-area)
summary(model2)
#compare the two models using anova
anova(model1,model2)
#let's take out illiteracy
model3=update(model2,.~.-illiteracy)
summary(model3)
#let us drop the income variable now
model4=update(model3,.~.-income)
summary(model4)
#all predictors are significant except density
model5=update(model4,.~.-density)
summary(model5)
anova(model5,model4)
#we now take the population also out
model6=update(model5,.~.-population)
summary(model6)
# what we did till now can be annotated by a step function
step(model1,direction="backward")
#confidence interval on the estimated coefficients
confint(model6)
#predictions can be made for the model equation using the predict functions
predict(model6,list(murder=10.5,hs.grad=48,frost=100))
#model diagnostic graphs
par(mfrow=c(2,2))
plot(model6)
names(model6)
sort(model6$resid)
#beta coefficients are the slopes if all the variables were on the same scale
#allows comparison of the predictors
model7=lm(scale(life.exp)~scale(murder)+scale(hs.grad)+scale(frost),data=st)
summary(model7)

#making predictions from the model
head(airquality)
airquality=na.omit(airquality)
model=lm(Ozone~.-Day,data=airquality)
summary(model)
names(model)

coef(model)
#one way to do the prediction is 
(predic=c(1,200,11,80,6)*coef(model))
sum(predic)
#alternatively prediction can also be done using
predict(model,list(Day=1,Solar.R=200,Wind=11,Temp=80,Month=6),interval="pred")

## chi sq test
ob=c(1920,347,19,84,130)
prob=c(0.8029,.1206,.0079,.0292,.0394)
p=chisq.test(ob,p = prob)
summary(p)
p
m=matrix(c(81,359,103,326,147,277),nrow = 2,ncol = 3)
m
row.names(m)=c("obese","not obese")
m
colnames(m)=c("dating","cohabiting","married")
m
margin.table(m)
m=addmargins(m)
m
#null hypothesis- there is no relation between status and obesity
#alternative hypothesis- there is relation between status and obesity
p=chisq.test(m)
names(p)
p$expected
p$residuals
p$statistic
md=as.data.frame(m)
md
chisq.test(md)

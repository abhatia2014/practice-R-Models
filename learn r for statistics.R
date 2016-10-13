getwd()
setwd()
getwd()
x= 1/0
x<- 1/Inf
x
x=1/0
x
x<- "hello"
x
x<- #Hello,x
x<- 1:20
x
x[6]
x<- c("a","b","C")
x
x[2]
# using a vector function
x= vector("numeric",length=10)
x
# explicit coercion
x=0:6
x
class(x)
as.numeric(x)
as.logical(x)
as.character(x)
as.complex(x)
#non sensical coercion
x
x= c("a","b","c")
as.numeric(x)
# Matrices
m= matrix(nrow=2,ncol=3)
m
dim(m)
attributes(m)
m=matrix(data=1:10,nrow=5,ncol=2)
m
m[3]
m[2,2]
m= 1:10
# transform a vector to a matrix by using the dim function
dim(m)= c(5,2)
m
# another way to bind the vector is my cbind and rbind functions
x= 1:3
y= 5:7
cbind(x,y)
rbind(x,y)
# List - each item of list can be an object of a different class
l= list(1, "a",TRUE, 1+4i)
x
l
# creating a factor with factor funtion- has 2 levels - yes and no
x= factor(c("yes", "yes", "no","yes","yes"))
x
table(x)
# describing levels
x= factor(c("yes", "yes", "no","yes","yes"), levels= c("yes","no"))
x
# missing values are denoted by either NaN or NA
is.na(x)
# NaN is a missing mathematical value
# data frame data type- more like a spreadsheet
# each row of a data frame has a name
# can be created by read.table or read.csv function
c= data.frame(abc= 1:4, def= c("a","b","c","d"))c
c
nrow(c)
ncol(c)
x= rnorm(100,20,10)/100
x
hist(x)  
# pnorm can be used to calculate the percentile function
pnorm(-1,mean = 0,sd = 1)
pnorm(1800,1500,300)
#function to find the z score, using the percentile, function is qnorm
qnorm(0.9,1500,300)
1-pnorm(50,45,3.2)
qnorm(0.2,77,5)
# function to choose # scenarios with 2 successes from 9 trials- how many scenarios
choose(9,2)
#calculate the probability of binomial distribution using dbinom
dbinom(x = 4,size = 10,prob = 0.13)
dbinom(8,10,.13)
dbinom(70,size=245, prob=0.25)
1-pnorm(70,61.25,6.77)
sum(dbinom(x = 70:245,size = 245,prob = 0.25))
dbinom(x =600,size = 1000,prob = 0.56 )
x=rnorm(100,56,4.96)
hist(x)
hist(x)
package "lattice"
package ?
package
library lattice
install.packages(pkgs = lattice)
sum(dbinom(60:100,100,0.56))
sum(dbinom(1:200,200,0.2))
dbinom(1,200,0.5)
dbinom(92,100,0.9)
1-pnorm(34,mean = 24,4)
1-pnorm(5,3.45,01.63)
setwd('~/Desktop')
data=read.table('lifeExptable.txt')
data
plot(data)
lifeexp=data[,2]
lifeexp
plot(lifeexp)
plot(sort(lifeexp), xlab='Country',ylab='Life Expectancy',ylim=c(0,86))
boxplot(lifeexp,ylab='Life Expectancy',ylim=c(0,86))
summary(lifeexp)
#next data set'
grades=c(79,68,69,88,90,74,87,76,93)
grades
sort(grades)
summary(grades)
boxplot(grades)
boxplot(grades, ylab='Grades',ylim=c(60,100))
six_grades=c(68,84,90,74,78,93)
sort(six_grades)
boxplot(six_grades,ylab='Six Grades',ylim=c(60,100))
plot(sort(six_grades),type='b',xlab='Student',ylab='Grade',ylim=c(60,100))
summary(six_grades)
skeleton=read.table('Skeletondata.txt',header=TRUE)
skeleton
summary(skeleton)
# to get access of all variable names- we use the command attach()

attach(skeleton)
boxplot(DGDifference,range=0,ylab='Estimated Age-Acutal Age (Years)')
summary(DGDifference)
# to manually calculate mean (average)
sum=sum(DGDifference)
sum
n=length(DGDifference)
sum/n
mean(DGDifference)
# if we wish to take mean and trim top and bottom 10% of the observations, 
mean(DGDifference,trim = 0.1)
nysalaries=c(3750
             44000
             138188
             45566.67
             44000
             141666.67
             292500
             5600000
             103500
             190000
             65000
             33750
             195000
             44000.04
             4600000
             194375
             33750
             112495.5
             95000
             301999
             181500
             33750
             185000
             205000
             44000
)
nysalaries=read.table('NYRedBullsSalaries.txt')
nysalaries
summary(nysalaries)
boxplot(nysalaries/1000,ylab="Salaries in 1000's of US Dollars")
# to get 8% trimmed mean of the salaries
mean(nysalaries)
nysalaries = c(33750, 44000, 138188, 45566.67, 44000, 141666.67,
               + 292500, 5600000, 103500, 190000, 65000, 33750, 195000, 44000.04,
               + 4600000, 194375, 33750, 112495.5, 95000, 301999, 181500, 33750,
               + 185000, 205000, 44000)
nysalaries=c(33750, 44000, 138188, 45566.67, 44000, 141666.67,
             + 292500, 5600000, 103500, 190000, 65000, 33750, 195000, 44000.04,
             + 4600000, 194375, 33750, 112495.5, 95000, 301999, 181500, 33750,
             185000, 205000, 44000)
data

lifeexp=data[,2]
lifeexp
summary(lifeexp)
boxplot(lifeexp,ylab='life Expectancy',ylim=c(40,90))
range(lifeexp)
max(lifeexp)-min(lifeexp)
# IQR is the inter quartile range i.e the difference in the range between the 3rd quartile to the first quartile

IQR(lifeexp)
grades
summary(grades)
boxplot(grades, ylab="Grades", ylim=c(60,100))
# to calculate variance of the grades
var(grades)
sd(grades)
IQR(grades)
sqrt(var(grades))
round(sd(grades),1)
nysalaries = c(33750, 44000, 138188, 45566.67, 44000, 141666.67,292500, 5600000, 103500, 190000, 65000, 33750, 195000, 44000.04,4600000, 194375, 33750, 112495.5, 95000, 301999, 181500, 33750,185000,205000, 44000)
nysalariesTrim=sort(nysalaries)
nysalariesTrim=nysalariesTrim[3:23]
length(nysalariesTrim)
median=median(nysalaries)
mean= mean(nysalaries)
range=max(nysalaries)-min(nysalaries)
iqr= IQR(nysalaries)
st.dev=sd(nysalaries)
origin=c(median, mean, range, iqr, st.dev)
origin
mediantrim= median(nysalariesTrim)
median=median(nysalaries)
meantrim= mean(nysalariesTrim)
rangetrim=max(nysalariesTrim)-min(nysalariesTrim)

iqrtrim= IQR(nysalariesTrim)
st.devtrim=sd(nysalariesTrim)
Trim=c(mediantrim, meantrim, rangetrim, iqrtrim, st.devtrim)
Trim
stats= cbind(origin, Trim)
stats
rownames(stats)=c('median','mean','range','IQR','std.dev')
stats
lifeexp
hist(lifeexp)
summary(lifeexp)
#to mention how many bins you need in a histogram, specify breaks=n
hist(lifeexp,breaks = 5, xlab="life expectancy (years)", main= 'Histogram of Life Expectancy')
# having a horizontal box plot
boxplot(lifeexp,horizontal=TRUE, range=0, xlab="life expectancy (years)")
skeleton
hist(DGDifference, breaks=10)
summary(DGDifference)
boxplot(DGDifference, horizontal=TRUE, range=0, xlab="Actual Age - expected age", main= "Box plot of DG Difference")
nysalaries
hist(nysalaries/1000, breaks=24, xlab="salaries in thousand of dollars(US)", main= "NY Salaries")
nysalaries_trimmed=sort(nysalaries)[1:23]
nysalaries_trimmed
hist(nysalaries_trimmed/1000, breaks=3, xlab="salaries in thousand of dollars(US)", main= "NY Salaries")
boxplot(nysalaries_trimmed/1000, horizontal= TRUE, range=0, xlab="Salaries in thousands of dollars", main= "NY Redbull Salaries BoxPlot")
# can specify how many digits are needed to be accurate
summary(nysalaries_trimmed, digits=7)
lifeexp
data= read.table("LifeExpRegion.txt")
data
plot(data$V1~data$V3)
region= data[,3]
region
table(region)
counts= table(region)
counts
relfreq=counts/sum(counts)
relfreq
region_names=c("Americas","E.Asia&Pc","Eur&C.As","M.E&N.Afr", "S.Asia","S-S.Africa")
region_names
barplot(relfreq,col=rainbow(6),names.arg=region_names, main="World Regions- Relative frequency")
pie(counts, col=rainbow(6), labels=region_names,main="World regions -Pie Chart")
skeleton
attach(skeleton)
sex_counts=table(Sex)
sex_counts
sex_relfreq= sex_counts/sum(sex_counts)
sex_relfreq
sex_names=c("males","females")
sex_names
barplot(sex_relfreq,col=rainbow(2),names.arg=sex_names, main="Sex by category")
pie(x = sex_counts,col = rainbow(2),labels = sex_names,main = "Skeleton Sex Pie Chart")
bmidata=table(BMI)
bmidata
bmi_rel_freq=bmidata/sum(bmidata)
bmi_rel_freq
barplot(bmi_rel_freq,col=rainbow(4),main="BMI Data-Bar Chart- Relative Frequency", ylim = c(0,0.7))
pie(bmidata,col=rainbow(4),main='BMI Pie Chart' )
lifedata=read.table('lifeExpRegion.txt')
colnames(lifedata)=c('Country', 'LifeExp','Region')
attach(lifedata)
table(Country)
table(Region)
lifedata[Region=='EAP',]
# to split a dataset by region
lifesplit=split(lifedata,Region)
lifesplit
lifesplit$SSA
lifeEAP=lifedata[Region=='EAP',]
lifeSSA=lifedata[Region=='SSA',]
lifeEAP
lifeSSA
summary(lifeEAP[,2])
summary(lifeSSA[,2])
boxplot(lifeEAP[,2],lifeSSA[,2],range=0,names= c('EAP','SSA'),border = rainbow(2),main='Boxplots of EAP, SSA')
#in case we want to create the box plot of all regions, there is no need to create the split
boxplot(LifeExp~Region, range=0, border= rainbow(6),main='Box plot by Regions', ylim=c(40,90))
LifeExp
skeleton
attach(skeleton)
boxplot(DGDifference~Sex, border=rainbow(2), range=0, main='Side by Side Box Plots for Age Difference',names = c("Males", "Females"))
boxplot(DGDifference~BMI, range=0, border=rainbow(4),main="Side by Side box plot by BMI index")
BMI
# to change the order of the variables in the data set, use levels command
BMI=factor(BMI, levels=c('underweight','normal','overweight','obese'))
Sex=factor(Sex, levels=c('1','2'),labels=c('Male','Female'))
Sex
BMI
freqBMI=table(BMI)
freqBMI
relfreqBMI= freqBMI/sum(freqBMI)
relfreqBMI
cbind(freqBMI, relfreqBMI)
freqSex=table(Sex)
freqSex
relfreqSex=freqSex/sum(freqSex)
cbind(freqSex, relfreqSex)
# install package gmodels
# load this package with the command library(gmodels)
library(gmodels)
# joining the two categorical variable tables with the command CrossTable
joint=CrossTable(x = BMI,y = Sex, prop.chisq=FALSE)
joint
joint_counts=joint$t
joint_counts
barplot(joint_counts,beside=TRUE,col = rainbow(4), ylab="Frequency", xlab="sex")
#specify the legend for the charts
legend('topright',c('underweight','normal','overweight','obese'),pch=15,col=rainbow(4))
BMI_givenSex=joint$prop.col
barplot(BMI_givenSex,beside=TRUE,col=rainbow(4),xlab='Sex', ylab='Frequency')
legend('topright', pch=15, c('underweight','normal','overweight','obese'), col=rainbow(6))
Patricia=matrix(c(23,6140,345,5673),byrow=TRUE,ncol = 2)
Patricia
# set the names of the rows and columns appropriately
colnames(Patricia)=c('Yes','No')
rownames(Patricia)=c('HPV Vaccine','Other Vaccine')
Patricia
Patricia_Joint=CrossTable(Patricia,prop.chisq=FALSE)
Patricia_Joint
#Install the package SMPracticals
#load the package with the command library(SMPracticals)
library(SMPracticals)
smoking
Countries=read.table('LifeGDPhiv.txt')
Countries
colnames(Countries)= c("countries","LifeExp","GDP","HIV")
Countries
attach(Countries)
# draw a GDP vs life Exp on a scatter plot
plot(GDP, LifeExp, xlab='GDP per Capita (2000 US $)',ylab='Life Expectancy (years)',main= 'Scatterplot')
# calculate correlation between GDP and Life expectancy
cor(GDP,LifeExp)
cor(LifeExp, GDP)
plot(HIV,LifeExp, xlab="HIV Rate",ylab='Life Expectancy', main="Scatterplot")
cor(LifeExp, HIV)
cor(DGDifference,Age)
#probability distribution
dbinom(1,size = 3, prob = 0.5)
x=seq(0,3,by=1)
prob_x=dbinom(x,3,0.5)
prob_x
p=cbind(x,prob_x)
rownames(p)=c('one head','two heads', '3 heads', '4 heads')
p
plot(x,prob_x,type='h',col='red',main="binomial distribution", ylim=c(0,1))
y=seq(0,50, 1)
y
prob_y=dbinom(y,50,0.5)
cbind(y,prob_y)
plot(y,prob_y,type='h',col='red')
#continuous distribution
x=seq(-1,2,length= 100)
x
# create a uniform distribution
uniform=dunif(x,min=0,max=1)
uniform
plot(x,uniform,type='l',col='red')
#plotting a normal distribution
x=seq(-3,3,length=100)
x
norm=dnorm(x,mean=0, sd=1)
norm
plot(x,norm,type='l', col='red')
#to get a sample  from binomial distribution, use rbinom function
rbinom(5,size = 10,prob = 0.5)
binom_sample=rbinom(100,10,0.5)
binom_sample
table(binom_sample)
plot(table(binom_sample),type='l',col='red')
freq=table(binom_sample)
plot(freq,type='l',col='red')
barplot(freq, xlab="no of heads")
summary(freq)
boxplot(freq)
risk=read.csv("IBMRiskAnalysis.csv",header = TRUE)
str(risk)
attach(risk)
plot(Impact~Likelihood,xlab="Likelihood",type = "s",ylab="Impact",col=rainbow(16),main ="Likelihood Vs Impact Risk Map*")
table(Impact)
table(Overall Risk )
## monte carlo simulations using R
n=1000
f=function(x) x^2
plot(runif(n),runif(n),col='blue',pch=20)
curve(f,0,1,n=100,col='White',add=TRUE)
# prepare a contour map
elevation=matrix(1,10,10)
elevation
elevation[4,6]=0
elevation
contour(elevation)
persp(elevation)
persp(elevation, expand=0.2)
volcano
contour(volcano)
persp(volcano, expand=0.2)
image(volcano)
#confidence intervals
#install the package binom using package installer in Tools > Install packages 
#load using the command library(binom)
library(binom)
#use the binom.confint function for confidene interval
binom.confint(x = 576,n = 1000,conf.level = 0.95, method="asymptotic")
seq(from = 1,to = 20,by = 2)

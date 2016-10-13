# R Tutor.com
#here is the link http://www.r-tutor.com/elementary-statistics/qualitative-data
#analyzing qualitative data
#load data set belonging to MASS package
library(MASS)
painters
str(painters)
painters$School
table(painters$School)
attach(painters)
cbind(table(School))
names(painters)
hist(Composition)
summary(Composition)
boxplot(Composition, range=0)
painters[1:3,]
#relative frequency
rfreq=table(School)/length(School)
round(rfreq,2)
cbind(round(rfreq,3)*100)
table(Composition)
rfreqC=table(Composition)/length(Composition)
rfreqC
cbind(paste(round(rfreqC*100,2),"%"))
barplot(table(School),col=rainbow(8))
pie(table(School),col=rainbow(8))
a=subset(Composition, School=="C")
mean(a)
median(a)
sd(a)
#tapply is for doing the mean of the composition by school
tapply(Composition, School,mean)
tapply(Composition, School, max)
# percentage of painters whose color score is equal or above 14

a=length(subset(Colour, Colour>14))
a/length(Colour)
#Quantitative data
head(faithful)
str(faithful)
attach(faithful)
range(eruptions)
breaks=seq(1.5,5.5,0.5)
breaks
# we try to classify the data in the sequence
duration=cut(eruptions,breaks, right=FALSE)
duration
#this gives the frequency of the erruptions
table(duration)
cbind(table(duration))
length(eruptions)
sum(table(duration))
str(duration)
duration=as.numeric(duration)
hist(duration)
duration
freq=table(duration)
freq
hist(eruptions,right=FALSE,col=rainbow(10),xlab='Duration minutes')
rfreq=round(table(duration)/length(eruptions),3)
rfreq
cbind(freq,round(rfreq,3))
#cumulative freq distribution
#use the function cumsum to table 
cfreq=cumsum(table(duration))
cfreq
cbind(freq,rfreq,cfreq)
#cumulative freq graph
cumfreq0=c(0,cfreq)
plot(breaks,cumfreq0,xlab='duration in minutes',type="b")
#relative cumulative frequency distribution
rcfreq=round(cfreq/length(eruptions),2)
cbind(freq,rfreq,cfreq,rcfreq)
rcfreq0=c(0,rcfreq)
plot(breaks,rcfreq0,type="b",main="cumulative relative freqency distribution")
#alternatively use the built in function for relative cumulative freq distr called ecdf
fun=ecdf(eruptions)
plot(fun)
stem(eruptions)
names(faithful)
plot(eruptions, waiting)
#linear regression model
m=lm(waiting~eruptions,data=faithful)
summary(m)
plot(waiting~eruptions)
abline(m)     
##Numerical measures
m=mean(eruptions)
median(eruptions)
quantile(eruptions)
boxplot(eruptions)
quantile(eruptions,c(0.32,0.50,0.98))
range(eruptions)
IQR(eruptions)
#covariance
cov(eruptions,waiting)
#correlation
cor(eruptions,waiting)
library(e1071)
skewness(eruptions,waiting)
##probability distributions
#Binomial distributions
dbinom(4,12,0.2)
sum(dbinom(0:4,12,0.2))
#or we can use cumulative probability function
pbinom(4,12,0.2)
1-ppois(16,lambda = 12)
# select 10 random numbers between 1 , 3
runif(10, 1,3)
#exponential function
pexp(2,rate = 1/3)
pnorm(84,mean = 72,sd = 15.2)
pnorm(84,72,15.2,lower.tail = FALSE)
#Chi squared distribution
qchisq(0.95,df = 7)
# Student T distribution
qt(c(0.25,0.975),df = 5)
#F distribution
qf(0.95,df1 = 5,df2 = 2)
library(MASS)
head(survey)
?survey
attach(survey)
names(survey)
plot(Height,Wr.Hnd)
boxplot(Height~Exer)
plot(Height~Exer)
plot(Clap~W.Hnd)
mosaicplot(Clap~W.Hnd)
summary(survey)
mean(Height,na.rm=TRUE)
# given std dev = 9.48, find confidence interval
#Interval Estimate using known variance or SD
length(Height)
#omit the NAs
n=na.omit(Height)
length(n)
# standard error
SE=9.48/sqrt(length(n))
SE
#critical value
E=qnorm(0.975);E
ConU=mean(Height,na.rm=TRUE)+E*SE
ConU
ConL=mean(Height,na.rm=TRUE)-E*SE
rbind(ConU,ConL)
cbind(ConU,ConL)
#Alternatively the Confidence Interval can be calculated loading TeachingDemos Package
library(TeachingDemos)
z.test()
z.test(x = n,sd = 9.48)
# Confidence Interval estimate using unknown variance/ Sd- uSe T distribution
#calculate the sd of the sample
s=sd(n)
s
SE=s/sqrt(n)
SE
#either use the built in formula or the t test formula from TeachingDemo packages
t=t.test(x = n)
names(t)
t$conf.int
# given SD, Margin of error, find sampling size at 95% conf Interval
# ME= Critical value *SE
#SE=9.48/sqrt(n) where n is the sample
# n= cv^2*SD^2/E^2
n=1.96^2*9.48^2/1.2^2
n
# point estimate of population proportion
pop=na.omit(Sex)
table(pop)
table(pop)/length(pop)
perfem=0.5
# finding the margin of error and estimate interval of female student proportion
sE=sqrt(0.5*0.5/length(pop))
#Alternatively we can use the prop.test function from the TeachingDemo package
#finding #females
fem=sum(pop=="Female")
fem
n=length(pop)
prop.test(fem,n)
# Hypothesis testing
#Lower tail test of population mean with known variance
#Suppose the manufacturer claims that the mean lifetime of a light
#bulb is more than 10,000 hours. 
#In a sample of 30 light bulbs, it was found that they only 
#last 9,900 hours on average. Assume the population standard 
#deviation is 120 hours. At .05 significance level, can we reject the claim by the manufacturer?
s=120
n=30
se=s/sqrt(n)
mu=10000
xbar=9900
z=(xbar-mu)/se
z
crit=qnorm(0.05)
crit
# we calculate the p value using the function pnorm

pvalue=pnorm(z)
pvalue
#since p value is < 0.05. we reject the null hypothesis saying that mean life is >10000 units
#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the population standard deviation is 0.25 grams. At .05 significance level, can we reject the claim on food label? 
mu0=2
n=35
xbar=2.1
a=0.05
s=0.25
#ho = at most 2 gms of saturated fat
#h1= more than 2 gms of saturated fat
se=s/sqrt(n)
z=(xbar-mu0)/se
z
#calculate p value
pval=pnorm(z,lower.tail=FALSE);pval
# as p value< significane level, we reject the null hypothesis
#Two tailed hypothesis testing with known variance
#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the population standard deviation is 2.5 kg. At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year? 
xbar=14.6
s=2.5
a=0.05
mu0=15.4
n=35
se=s/sqrt(n)
#h0= mean wt =15.4 kg
#h1= mean wt not equal to 15.4Kg
z=(xbar-mu0)/se;z
pval= 2*pnorm(z);pval
# as it is > 0.05 significance level, we fail to reject the null hypothesis
# lower tail test with unknown variance- use T distribution
# in all cases where the population SD is not given and we are assuming a sample standard deviation, we use the t test
#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. Assume the sample standard deviation is 125 hours. At .05 significance level, can we reject the claim by the manufacturer? 
mu0=10000
n=30
xbar=9900
s=125
se=s/sqrt(n);se
t=(xbar-mu0)/se;t
df=n-1;df
#ho= mean>10,000 hrs
#h1- mean less than 10,000 hrs
a=0.05
pval=pt(q = t,df = df)
pval
# as p value is very small and <a, we reject the null hypothesis in favor of alternative hypothesis
# upper tail test of population mean with unknow variance (use T distribution)
# Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the sample standard deviation is 0.3 gram. At .05 significance level, can we reject the claim on food label? 
xbar=2.1
mu0=2
s=0.3
a=0.05
n=35
se=s/sqrt(n)
se
z=(xbar-mu0)/se
z
df=n-1;df
# using t distribution and function, pt, upper tail test given by
1-pt(q = z,df)
#alternatively
pval=pt(z,df,lower.tail = FALSE)
#as pval <a, we reject the null hypothesis that the average fat content
#in the cookie is atmost 2 gms
# two tailed test of population mean with unknown variance
#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the sample standard deviation is 2.5 kg. At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year? 
mu0=15.4
n=35
xbar=14.6
s=2.5
a=0.5
#ho=mean penguin wt does not differ from last year
#h1= mean penguin wt differs from last year
se=s/sqrt(n)
df=n-1
z=(xbar-mu0)/se;z
#calculate pval
pval=2*pt(z,df)
pval
# since pval is >a, we fail to reject the null hypothesis
# lower tail test of population proportion
#Suppose 60% of citizens voted in last election. 85 out of 148 people in a telephone survey said that they voted in current election. At 0.5 significance level, can we reject the null hypothesis that the proportion of voters in the population is above 60% this year? 
mu0=0.6
xbar=85/148
xbar
a=0.05
#h0= population of citizens voting this year >=60%
#h1= % population voting this year <60%
# note that the population/sample standard deviation is not provided
n=148
se=sqrt(mu0*(1-mu0)/n);se
# critical value 
z=(xbar-mu0)/se;z
pval=pnorm(z)
pval
# as pval > a, we fail to reject the null hypothesis that population of voters is > 60% this year
#Alternative solution for p value
prop.test(85,148,p = 0.6,alternative = "less",correct=FALSE)
# upper tail for population proportion
#Suppose that 12% of apples harvested in an orchard last year was rotten. 30 out of 214 apples in a harvest sample this year turns out to be rotten. At .05 significance level, can we reject the null hypothesis that the proportion of rotten apples in harvest stays below 12% this year? 
mu0=0.12
xbar=30/214;xbar
a=0.05
n=214
se=sqrt(mu0*(1-mu0)/n);se
#h0- proportion of apples that turn rotton will remain below 12% this year
#h1- proportion of apples that turn rotten will be more that 12% this year
z=(xbar-mu0)/se
z
pval=pnorm(z,lower.tail = FALSE)
pval
# as pval >a, we fail to reject the null hypothesis
#alternative method, using prop.test function
prop.test(30,214,p=0.12,alt="greater",correct=FALSE)
# two tailed test of population proportion
#Suppose a coin toss turns up 12 heads out of 20 trials. At .05 significance level, can one reject the null hypothesis that the coin toss is fair? 
n=20
p0=0.5
#h0- coin toss is fair, the mean is 0.5
#h1- unfair coin, the mean is <> 0.5
se=sqrt(p0*(1-p0)/n);se
xbar=12/20;xbar
z=(xbar-p0)/se
z
pval=2*pnorm(z,lower.tail=FALSE)
pval
#pval > a, we fail to reject the null hypothesis that the coin is fair
#alternative solution using prop.test function
prop.test(12,20,p=0.5,correct=FALSE)
#Type II error in lower tail tet of population mean with known variance
#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. Assume actual mean light bulb lifetime is 9,950 hours and the population standard deviation is 120 hours. At .05 significance level, what is the probability of having type II error for a sample size of 30 light bulb? 
n=30
s=120
a=0.05
xbar=9950
mu0=  10000
se=s/sqrt(n);s
se
#lower bound of the sample mean for which null hypothesis mu>10000 would not be rejected
q=qnorm(p = 0.05,mean = mu0,sd=se)
q
# so if the sample mean is above 9964, the null hypothesis will not be rejected, the probability of getting a number 9964 is
pnorm(q,mean = 9950,sd=se,lower.tail=FALSE)
#if the actual mean is 9965
pnorm(9965,9950,se,lower.tail=FALSE)
#type 11 error in the uppertail test with known variance
#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. Assume the actual mean amount of saturated fat per cookie is 2.09 grams, and the population standard deviation is 0.25 grams. At .05 significance level, what is the probability of having type II error for a sample size of 35 cookies? 
n=35
s=0.25
mu=2
xbar=2.09
a=0.05
se=s/sqrt(n)
se
# upper bound of sample mean for which the null hypothesis at mu<=2 will not be rejected
q=qnorm(a,mean = mu,sd = se,lower.tail=FALSE)
q
# as long the mean is below 2.06, the null hypothesis will not be rejected
#probability of mean remaining below 2.09
pnorm(q,2.09,sd = se)
#probability of testng a type 2 error is 31.4% and power of hypothesis is 68.6%
#for 2.075 gms, what is probability of a type 11 error
pnorm(2.075,2.09,se)
# type II error in two tailed test of population mean with known variance
# ie. the pvalue is not significant enough to reject the null hypothesis even though the true population mean is significantly different from sample mean
#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. Assume the actual mean population weight is 15.1 kg, and the population standard deviation is 2.5 kg. At .05 significance level, what is the probability of having type II error for a sample size of 35 penguins?
mu0=15.4
xbar=15.1
s=2.5
a=0.05
n=35
#null hypothesis says that the sample mean = population mean
#type II error occurs if we fail to reject the null hypotheis even though the sample weight is <> population weight
se=s/sqrt(n)
se
# weight for which null hypothesis should be rejected
#we have to find the confidence interval in which null hypothesis of 15.4 kg will not be rejected
I=c(a/2,1-a/2)
q=qnorm(I,mean=mu0,se)
q
# as long the sample mean is between 14.5 and 16.2 , the null hypothesis will not be rejected
# since the actual mean is 15.1kg, we find the probability at both these end points
p=pnorm(q,xbar,se)
p
#the probability is the difference between these two points
diff(p)
#probabiity of having a typeII error is 89.1%  and the power
# of hypothesis testing is 10.9%
#Type II error in the lower tail test of population mean with unknown variance
# in lower tail test, the true population mean is greater than the hypothesized mean
#we'll use the student T distribution since variance is unknown
# same light bulb example
mu=10000
n=30
# sample standard deviation
s=125
xbar=9950
a=0.05
# standard estimate
se=s/sqrt(n)
se
# compute the lower bound of sample means
q=mu+qt(a,n-1)*se;q
# Estimate inference about two populations
#Population means between two samples from the same population- also called t paired test
library(MASS)
head(immer)
# to find the 95% confidence interval of the difference in the mean barley yields between years 1931 and 1932, we use the t.test function
attach(immer)
t.test(x = Y1,y = Y2,paired = TRUE)
#Population means between two samples from independent population- called unpaired t- test
mtcars
attach(mtcars)
length(mpg)
names(mtcars)
head(mtcars)
am[5]
am[1:5]
boxplot(mpg, range=0, horizontal=TRUE)
boxplot(mpg~am)
# find 95% confidence interval in the gas mileage of manual and automatic transmission
# gas mileage for auto transmission
a=subset(mtcars,am=="0")
head(a$mpg)
mean(a$mpg)
o=subset(mtcars, am=="1")
mean(o$mpg)
summary(mpg)
#now find the 95% confidence interval using t unpaired test
t.test(a$mpg,o$mpg,paired = FALSE)
#alternatively, 
t.test(mpg~am,data=mtcars)
#Comparison of two population proportions- distinct populations
head(quine)
attach(quine)
table(Eth, Sex)
# assuming that the data follows the normal distribution, find
#95% confidence interval between female proportion of Ab students and female proportion of non-ab students 
#we use prop.test function to compute the difference in female proportion


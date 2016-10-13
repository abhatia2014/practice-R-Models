source("http://www.openintro.org/stat/data/arbuthnot.R")
load("arbuthonot.R")
getwd()
dim(arbuthnot)
names(arbuthnot)
arbuthnot$boys
arbuthnot$girls
plot(arbuthnot$year, arbuthnot$girls)
summary(arbuthnot$year)
# to connect the plot by lines
plot(arbuthnot$year, arbuthnot$girls,type="b")
?plot
total= arbuthnot$girls+arbuthnot$boys
plot(arbuthnot$year,total)
arbuthnot$girls/total
arbuthnot$girls>arbuthnot$boys
source("http://www.openintro.org/stat/data/present.R")
names(present)
dim(present)
?count
ratio=present$boys/present$girls
ratio
total=present$boys+present$girls
present$year[max(total)]
source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)
dim(cdc)
str(cdc)
head(cdc)
tail(cdc)
summary(cdc$weight)
mean(cdc$weight)
median(cdc$weight)
sd(cdc$weight)
var(cdc$weight)
names(cdc)
table(cdc$smoke100)
table(cdc$smoke100)/20000
barplot(table(cdc$smoke100))
names(cdc$smoke100)=c("female","male")
table(cdc$smoke100)
smoke=table(cdc$smoke100)
barplot(smoke)
summary(cdc$height)
boxplot(cdc$height)
?boxplot
names(cdc)
head(cdc)
table(cdc$genhlth)
table(cdc$genhlth)/20000
table(cdc$gender,cdc$smoke100)
#create a mosiac plot of the table
?mosaicplot
mosaicplot(table(cdc$gender,cdc$smoke100))
cdc[1:10,6]
cdc[1:10,]
cdc[1:10,weight]
cdc$weight
cdc$gender=="m"
table(cdc$gender)
mdata=subset(cdc,cdc$gender=="m")
table(mdata$smoke100)
summary(mdata)
boxplot(mdata$age)
mdata1=subset(cdc,cdc$gender == "m" & cdc$age> 30)
m_and_over30 <- subset(cdc, cdc$gender == "m" & cdc$age > 30)
mdata1
m_or_over30= subset(cdc,cdc$gender=="m" |cdc$age >30)
table(m_or_over30$smoke100)
under23andsmoke=subset(cdc,cdc$age<23 & cdc$smoke100==1)
table(under23andsmoke$gender)
boxplot(cdc$height)
summary(cdc$height)
boxplot(cdc$height~cdc$gender)
bmi=cdc$weight/((cdc$height)^2)*703
boxplot(bmi~cdc$genhlth)
boxplot(bmi~cdc$gender)
hist(cdc$age)
hist(bmi)
hist(bmi,breaks=50)
boxplot(cdc$smoke100~cdc$genhlth)
boxplot(cdc$height,cdc$genhlth)
head(cdc)
attach(cdc)
plot(weight,wtdesire)
boxplot(weight, wtdesire, range=0,col = rainbow(3))
summary(weight)
summary(wtdesire)
boxplot(weight)
wdiff=wtdesire-weight
head(wdiff)
sd(wdiff)
summary(wdiff)
boxplot(wdiff,horizontal = TRUE,range=0)
wmen=subset(wtdesire,gender=="m")
wwmen=subset(wtdesire,gender=="f")
boxplot(wmen,wwmen, range=0, horizontal=TRUE)
?boxplot
example(boxplot)
summary(wmen~ wwmen)
boxplot(wtdesire~gender,range=0)
m=mean(weight)
s=sd(weight)
wtup=m+s
wtdw=m-s
wtpar=subset(weight,weight>wtdw & weight <wtup)
wtpar
?count
?counts
dim(wtpar)
summary(wtpar)
boxplot(wtpar)
table(wtpar)
str(wtpar)

# probability
download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")
load("kobe.Rdata")
head(kobe)
names(kobe)
dim(kobe)
str(kobe)
attach(kobe)
table(basket)
pattern=kobe[game="1']
pattern=subset(kobe,kobe$game=="1")
pattern
pattern$basket[1:9]
kobe_streat= calc_streak(kobe$basket)
kobe_streat
barplot(table(kobe_streat))
boxplot(kobe_streat)
summary(kobe_streat)
table(kobe_streat)
# simulationsin R
outcomes=c("head","Tail")
sample(outcomes, size=10,replace=TRUE)
simFairCoin=sample(outcomes,size=100, replace=TRUE)
simFairCoin
table(simFairCoin)
simUnfairCoin=sample(outcomes, size=100, replace=TRUE, prob = c(0.2,0.8))
table(simUnfairCoin)
outcomes=c('H','M')
simIndep=sample(outcomes,size=100,replace=TRUE)
table(simIndep)
simIndep=sample(outcomes,size=133,replace=TRUE,prob = c(0.45,0.55))
table(simIndep)
table(kobe$basket)
results=rbind(table(kobe$basket),table(simIndep))
?rownames
rownames(results)=c("Kobe","Indep")
results
results/133
simstreak=calc_streak(simIndep)
simstreak
getwd()
download.file("http://www.openintro.org/stat/data/bdims.RData", destfile = "bdims.RData")
load("bdims.RData")
dim(bdims)
str(bdims)
head(bdims)
attach(bdims)
boxplot(wgt~sex)
mdims=subset(bdims,sex==1)
fdims=subset(bdims,sex==0)
m=summary(mdims$hgt)
f=summary(fdims$hgt)
total=c(m,f)
total
cbind(total)
rbind(total)
boxplot(hgt~sex,col=rainbow(2),range=0,names = c("females","males"),ylab="Height",horizontal=TRUE)
hist(mdims$hgt,ylab="height of men",breaks=50)
hist(fdims$hgt,ylab="height of women",breaks=50)
fmean=mean(fdims$hgt)
fsd=sd(fdims$hgt)
fsd
# constructing a density histogram
hist(fdims$hgt,probability=TRUE,ylim=c(0,0.06))

summary(fdims$hgt)
x=140:190
y=dnorm(x,fmean,fsd)
lines(x,y,col="blue")
qqnorm(fdims$hgt)
qqline(fdims$hgt)
simnorm=rnorm(n=length(fdims$hgt),mean=fmean,sd=fsd)
qqnorm(simnorm)
qqline(simnorm)
qqnormsim(fdims$hgt)
hist(fdims$wgt)
fdims
fdims$wgt
hist(fdims$wgt)
boxplot(fdims$wgt)
barplot(fdims$wgt)
hist(fdims$wgt)
summary(fdims$wgt)
x=40:110
fmwt=mean(fdims$wgt)
fswt=sd(fdims$wgt)
y=dnorm(x,fmwt,fswt)
lines(x,y,col="blue")
qqnorm(fdims$wgt)
qqline(fdims$wgt)
#what is the probabibility that a random chosen female is taller than 6 ft
1-pnorm(q = 182,mean=fmean,sd = fsd)
sum(fdims$hgt>182)/length(fdims$hgt)
qqnorm(fdims$kne.di)
qqline(fdims$kne.di)
hist(fdims$kne.di, breaks = 30)
summary(fdims$kne.di)
x=12:30
y=dnorm(x,mean(fdims$kne.di),sd(fdims$kne.di))
lines(x,y,col="blue")
#foundations for statistical inference
download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
head(ames)
area=ames$Gr.Liv.Area
price=ames$SalePrice
summary(area)
boxplot(area,range=0,horizontal=TRUE)
hist(area, breaks=30)
length(area)
#getting a sample to understand the mean living area
samp1=sample(area,100)
mean(samp1)
summary(samp1)
a=cbind(summary(area),summary(samp1))
colnames(a)=c("area","sample")
a
boxplot(area,range=0)
samp2=sample(area,100)
mean(samp2)
a=cbind(summary(area),summary(samp1),summary(samp2))
colnames(a)=c("area","sample1","sample2")
a
boxplot(area,samp1,samp2,horizontal=TRUE,range=0,names=c("area","sample1","sample2"))
#generating a sampling distribution of sample means
distr=rep(0,5000)
for (i in 1:5000){
  samp= sample(area,50)
  distr[i]<-mean(samp)
}
hist(distr,breaks=50)
x=100:2000
y=dnorm(x,mean(distr),sd(distr))
lines(x,y,col="blue")
summary(distr)
summary(area)
samplesmall100=rep(0,5000)
samplesmall10=rep(0,5000)
for (i in 1:5000) {
sample1=sample(area,10)
samplesmall10[i]=mean(sample1)
sample1=sample(area,100)
samplesmall100[i]=mean(sample1)
}
par(mfrow = c(3,1))
#xlimits=range(samplesmall10)
hist(samplesmall10,breaks=30)
hist(samplesmall100,breaks=30)
hist(distr,breaks=30)
summary(price)
samprice=sample(price,50)
mean(samprice)
samprice=rep(0,5000)
for (i in 1:5000){
sam1=sample(price,50)
samprice[i]=mean(sam1)
}
hist(samprice,breaks=50)
summary(samprice)
samprice1=rep(0,5000)
samprice2=rep(0,5000)
for (i in 1:5000){
sam1=sample(price,50)
samprice1[i]=mean(sam1)
sam1=sample(price,150)
samprice2[i]=mean(sam1)
}
par(mfrow=c(3,1))
hist(samprice,breaks=50)
hist(samprice1,breaks=50)
hist(samprice2,breaks=50)
par(mfrow=c(1,1))
boxplot(samprice,samprice1,samprice2,range=0,horizontal=TRUE)
a=max(area)
mean(area)
max(area)
var(area)
sd(area)
samp=sample(area,60)
summary(samp)
boxplot(samp,range=0,names="sample parameters")
#95% confidence interval is obtained by adding and subtracting 1 sigma (1.96)to the mean
se=sd(samp)/sqrt(length(samp))
se
sd(samp)
lower=mean(samp)-1.96*se
upper=mean(samp)+1.96*se
c(lower,upper)
sampmean=rep(0,50)
sampsd=rep(0,50)
for (i in 1:50){
samp1=sample(area,60)
sampmean[i]=mean(samp1)
sampsd[i]=sd(samp1)
}

upper=sampmean+1.96*sampsd/(sqrt(60))
lower=sampmean-1.96*sampsd/(sqrt(60))
c(upper[1],lower[1])
c(upper,lower)
cbind(upper, lower)
plot_ci(lower,upper, mean(area))
#inference for numerical data
download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")
str(nc)
head(nc)
tail(nc)
length(nc$marital)
summary(nc)
boxplot(nc$gained)
attach(nc)
plot(weight~whitemom, range=0)
plot(gained~mature, range=0)
plot(weight~habit,range=0)
plot(weight~lowbirthweight,range=0)
plot(lowbirthweight~habit)
plot(premie~mature)
plot(habit~whitemom)
plot(fage~lowbirthweight)
plot(habit~weight)
by(weight, habit, mean)
by(nc$weight,nc$habit,mean)
## this is an observed difference, but is it statistically significant, we will do hypothesis testing to check that
inference(y = weight,x = habit,est = "mean",type = "ht",null=0,alternative = "twosided",method = "simulation")
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0,
alternative = "twosided", method = "theoretical",
order = c("smoker","nonsmoker"))
inference(y = nc$weight,x = nc$habit,est = "mean",order = c("smoker","nonsmoker"),null = 0,type = "ci",alternative = "twosided",method="theoretical")
boxplot(weeks,range=0)
summary(weeks)
meanweek=mean(weeks,na.rm = TRUE)
sdweek=sd(weeks,na.rm = TRUE)
seweek=sdweek/sqrt(length(weeks))
cbind(meanweek,sdweek,seweek)
#confidence interval at 95% confidence interval
CIupper= meanweek+1.96*seweek
CIlower= meanweek-1.96*seweek
cbind(CIupper, CIlower)
inference(y = weeks,est = "mean",type="ci",null=0,alternative="twosided",method="theoretical")
# for 90% confidence interval
inference(y = weeks,est = "mean",type="ci",null=0,alternative="twosided",conflevel = 0.90,method="theoretical")
nc[1:3]
head(nc)
boxplot(gained~mature)
summary(gained)
mw=subset(nc$gained,nc$mature=='younger mom')
yw=subset(nc$gained,nc$mature=='mature mom')
summary(mw,yw)
summary(mw)
summary(yw)
inference(y = gained,x = mature,est = "mean",null = 0,alternative = "twosided",type = "ht",method="theoretical")
head(nc)
boxplot(fage~mature,range = 0)
summary(fage,mature=="mature mom")
head(nc)
boxplot(mage~mature,range=0)
boxplot(gained~gender,range=0)
inference(y = gained,x = gender,est = "mean",null = 0,alternative = "twosided",type = "ht",method = "theoretical")
# data for linear regression
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
# data for multiple regression
download.file("http://www.openintro.org/stat/data/evals.RData", destfile = "evals.RData")
# Inference for categorical data
download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")
us=subset(atheism, atheism$nationality=="United States" & atheism$year=="2012")
length(us)
length(us$nationality)
boxplot(us$nationality)
head(us)
boxplot(us$response)
plot(us$response)
table(us$response)
freq=table(us$response)/length(us$response)
freq
inference(y = us$response,est = "proportion",success = "atheist",type = "ci",method = "theoretical")
In=subset(atheism,atheism$nationality=="India" & atheism$year=="2012")
table(In$response)
plot(In$response)
freqIn=table(In$response)/length(In$response)
freqIn
## to find the confidence interval of atheist in India
inference(y = In$response, est = "proportion",success = "atheist",type = "ci",method = "theoretical")
#relationship between proportion p and margin of error ME
n <- 1000
p <- seq(0, 1, 0.01)
me <- 2 * sqrt(p * (1 - p)/n)
plot(me ~ p)
## Introduction to linear regression
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")
data=mlb11
head(data)
attach(data)
plot(runs~hits)
plot(runs~at_bats)
boxplot(runs)
boxplot(runs,bat_avg*1000 )
summary(bat_avg)
plot(runs,bat_avg)
plot(at_bats,runs)
# quantify the strength of the relationship using correlation coefficient
cor(runs, at_bats)
plot_ss(x = at_bats,y = runs,showSquares = TRUE)
#we use the linear model command lm to find the line for linear regression
m1=lm(runs~at_bats,data=data)
summary(m1)
m2=lm(runs~homeruns,data=data)
summary(m2)
str(data)
m3=lm(runs~hits, data=data)
summary(m3)
m4=lm(wins~bat_avg, data=data)
summary(m4)
m5=lm(wins~runs,data=data)
summary(m5)
plot(runs~at_bats)
abline(m1)
y=-2789.24+0.6305*5578
y
plot(m1$residuals~at_bats)
#adds a horizontal dashed line at y=0
abline(h=0,lty=3)
# to check if the residuals are nearly normal
hist(m1$residuals)
# or a normal probability plot of the residuals
qqnorm(m1$residuals)
qqline(m1$residuals) #adds a diagonal line to the normal prob plot
str(data)
plot(runs,bat_avg)
m=lm(runs~at_bats,data=data)
summary(m)
names(data)
m1=lm(runs~hits,data=data)
summary(m1)
names(data)
m2=lm(runs~homeruns,data=data)
summary(m2)
m2$residuals
mean(m2$residuals)
#multiple linear regression
download.file("http://www.openintro.org/stat/data/evals.RData", destfile = "evals.RData")
load("evals.Rdata")
str(evals)
summary(evals$score)
summary(evals$cls_students)
hist(evals$score)
attach(evals)
names(evals)
plot(score~age)
plot(score~gender, range=0)
str(evals)
plot(score~pic_outfit,range=0)
plot(score~pic_color,range=0)
plot(score~language,range=0)
plot(score~age)
plot(gender~score)
plot(score~bty_avg)
?mosaicplot
mosaicplot(gender~score)
example(mosaicplot)
names(evals)
mosaicplot(~gender+ethnicity,data=evals)
mosaicplot(~gender+pic_outfit,data=evals)
mosaicplot(score~pic_outfit, data=evals)
plot(score~pic_outfit)
plot(score~bty_avg)
m1=lm(score~bty_avg,data=evals)
abline(m1)
summary(m1)
length(score)
length(bty_avg)
?jitter
#Jitter adds a small amount of noise to the numbers
example(jitter)
plot(jitter(score)~bty_avg)
plot(score~jitter(bty_avg))
plot(m1$residuals)
hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals)
plot(bty_avg~bty_f1lower)
cor(x = bty_avg,bty_f1lower)
#relationships between all beauty variables
plot(evals[,13:19])
# to see if beauty is still a predictor after adding gender of the professor
btyG=lm(score~bty_avg+gender, data=evals)
summary(btyG)
multiLines(btyG)
plot(score~gender,range=0)
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval+ cls_students + cls_level + cls_profs + cls_credits + bty_avg+ pic_outfit + pic_color, data = evals)
summary(m_full)

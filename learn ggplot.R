getwd()
library(ggplot2)
#create factors with value labels
attach(mtcars)
gearf=factor(gear,levels=c(3,4,5),labels=c("3gears","4gears","5gears"))
table(gearf)
summary(mpg)
boxplot(mpg)
amf=factor(am,levels=c(0,1),labels=c("Automatic","Manual"))
cylf=factor(cyl,levels=c(4,6,8),labels=c("4cyl","6cyl","8cyl"))
#Kernel density plots for mpg
#grouped by # of gears
qplot(x=mpg, data= mtcars,geom ="density",fill=gearf,size=I(0.5),main="distribution of gas mileage",xlab="miles per gallon")
# scatterplot of mpg vs hp for each combination of gears and cyl
attach(mtcars)
qplot(hp,mpg,data=mtcars,shape=amf,size=I(3),xlab="Horsepower",ylab="Miles per gallon")
qplot(mpg,hp,data=mtcars,facets = gear~cyl,size=I(3),xlab="horsepower",ylab="miles per gallon")
#regression plot
qplot(wt,mpg,data=mtcars,geom=c("point","smooth"),method="lm",formula=y~x,color=cylf)
qplot(wt,mpg,data=mtcars,fill=gearf,geom="smooth",facets=cyl~gear)
p=qplot(hp,mpg,data=mtcars,shape=amf,color=amf)
p


## learn ggplot2 using R Cookbook
##http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_%28ggplot2%29/

#drawing line graphs
# add a data frame
df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
                 total_bill = c(14.89, 17.23))
df
#we'll put time on x axis, total bill on y-axis
ggplot(df,aes(time,total_bill,group=1))+geom_line()
ggplot(df,aes(time, total_bill))+geom_line(aes(group=1))

# add points
ggplot(df,aes(time,total_bill,group=1))+geom_line()+geom_point()

#change color and make it a dotted line

ggplot(df,aes(time,total_bill,group=1))+
  geom_line(color="red",linetype="dotted",size=1.5)+
    geom_point(color="red",fill="white",size=4)

#graph with more variable
df1 <- data.frame(sex       = factor(c("Female","Female","Male","Male")),
                  time       = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
                  total_bill = c(13.53, 16.81, 16.24, 17.42))
df1
#time - x axis, sex= color fill, total_bill=yaxis
#stacked bar graph
ggplot(df1,aes(time,total_bill,fill=sex))+geom_bar(stat="identity")
ggplot(df1,aes(time,total_bill,fill=sex))+geom_bar(stat="identity",position=position_dodge(),color="black")

#change colors
ggplot(data=df1, aes(x=sex, y=total_bill, group=time, shape=time, color=time)) + geom_line() + geom_point()

##plotting means and error bars graphs
df=ToothGrowth
head(df)
table(df$dose)
str(df)

##run the summarize data function
## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
library(plyr)
dfc <- summarySE(df, measurevar="len", groupvars=c("supp","dose"))
install.packages("plyr")
#basic line graphs
ggplot(dfc,aes(x=dose,y=len,color=supp))+ geom_errorbar(aes(ymin=len-se,ymax=len+se),width=.1)+ geom_line()+geom_point()

##plotting distributions
set.seed(1234)
df <- data.frame(cond = factor( rep(c("A","B"), each=200) ), 
                 rating = c(rnorm(200),rnorm(200, mean=.8)))
df
## adding means to the histograms
library(plyr)
cdf=ddply(df,"cond",summarise,rating.mean=mean(rating))
cdf

#overlaid histogram with means
ggplot(df,aes(rating,fill=cond))+geom_histogram(binwidth=.5,  position="identity")+
  geom_vline(data=cdf,aes(xintercept=rating.mean,color=cond),linetype="dashed",size=1)

#density plot with means
ggplot(df,aes(x=rating,color=cond))+geom_density()+
  geom_vline(data=cdf,aes(xintercept=rating.mean,color=cond),linetype="dashed",size=1)+theme_bw()

##using facets with mean
ggplot(df,aes(rating))+geom_histogram(binwidth=.5,fill="white",color="black")+facet_grid(cond~.)+theme_bw()+geom_vline(data=cdf,aes(xintercept=rating.mean),linetype="dashed",color="blue",size=1)

#basic box plot

ggplot(df,aes(x=cond,y=rating))+geom_boxplot()

#with conditions colored
ggplot(df,aes(x=cond,rating,fill=cond))+geom_boxplot()+coord_flip()+theme_bw()
# add mean to the chart using stat_summary( ) function
ggplot(df,aes(x=cond,rating,fill=cond))+geom_boxplot()+coord_flip()+theme_bw()+
  stat_summary(fun,y=mean,geom="point",shape=5,size=4)


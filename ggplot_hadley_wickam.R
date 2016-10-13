library(ggplot2)
mpg

data(mpg)
head(mpg)
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
ggplot(mpg,aes(displ,hwy,color=class))+geom_point()
ggplot(mpg,aes(displ,hwy,shape=drv))+geom_point()
ggplot(mpg,aes(displ,hwy,size=cyl,color=class))+geom_point()

#to set the aesthetic to a fixed value, do it outside of aes

ggplot(mpg,aes(displ,hwy))+geom_point(aes(color="blue"))
ggplot(mpg,aes(displ,hwy))+geom_point(color="blue")

#faceting
ggplot(mpg,aes(displ,hwy))+geom_point()+facet_wrap(~class)+geom_smooth()
#with confidence intervals
ggplot(mpg,aes(displ,hwy))+geom_point()+geom_smooth()
#with no confidence intervals
ggplot(mpg,aes(displ,hwy))+geom_point()+geom_smooth(se=FALSE)

#the wiggliness of the smooth line is controlled by the span parameter

ggplot(mpg,aes(displ,hwy))+geom_point()+geom_smooth(span=0.2) #very wiggly
ggplot(mpg,aes(displ,hwy))+geom_point()+geom_smooth(span=0.8) #less wiggly

#default method for smoothening is loess

#when n>1000, use method ='gam' and load mgcv package prior to that

library(mgcv)

ggplot(mpg,aes(displ,hwy))+geom_point()+geom_smooth(method='gam',formula = y~s(x))

#method='lm' fits a linear model
ggplot(mpg,aes(displ,hwy))+geom_point()+geom_smooth(method='lm')

#boxplots

#when x=categorical and y=continous

#example fuel economy with car class

ggplot(mpg,aes(drv,hwy))+geom_point()
ggplot(mpg,aes(drv,hwy,color=drv))+geom_boxplot()

#adding a little jitter to the plot

ggplot(mpg,aes(drv, hwy))+geom_jitter()

#adding a violin plot
ggplot(mpg,aes(drv, hwy))+geom_violin()

#histogram and frequency polygon

#show distribution of single numeric variable

ggplot(mpg,aes(hwy))+geom_histogram()
ggplot(mpg,aes(hwy))+geom_freqpoly()

#using different binwidths
ggplot(mpg,aes(hwy))+geom_histogram(binwidth = 2.5)
ggplot(mpg,aes(hwy))+geom_histogram(binwidth = 1)
ggplot(mpg,aes(hwy))+geom_histogram(binwidth = 0.1)
#different colors in the freqpoly can represent diferent groups
ggplot(mpg,aes(displ,color=drv))+geom_freqpoly(binwidth=0.5)
ggplot(mpg,aes(displ,fill=drv))+geom_histogram(binwidth = 0.5)+facet_wrap(~drv,ncol=1)

#bar charts

ggplot(mpg, aes(manufacturer))+geom_bar()

#time series with line and path plots

#line follows a sorting by time variable on the x axis while path plots joins the points as they appear

data("economics")
head(economics)
?economics

ggplot(economics,aes(date, unemploy/pop))+geom_line()
ggplot(economics, aes(date, uempmed))+geom_line()

#to draw both time series on the same plot

ggplot(economics,aes(unemploy/pop,uempmed))+geom_path()+geom_point()
year=function(x) as.POSIXlt(x)$year +1900

ggplot(economics,aes(unemploy/pop,uempmed))+geom_path(color="grey50")+geom_point(aes(color=year(date)))
cor(unemploy/pop,unempmed,data=economics)
attach(economics)
detach(economics)
with(economics,cor(unemploy/pop,uempmed))
cor(unemploy/pop,uempmed)

#modifying the axes

ggplot(mpg,aes(cty,hwy))+geom_point(alpha=1/3)+xlab("city driving")+ylab("highway driving")

#xlab(NULL) will remove the axis labels

#xlim() and ylim() modify the limits of the axes

#for continuous scales, use NA to set only one limit

ggplot(mpg,aes(drv,hwy))+
  geom_jitter(width=0.25,na.rm = TRUE)+
  xlim('f','r')+
  ylim(NA,30)

#Chapter 3 - Toolbox

ggplot(mpg,aes(displ,hwy))+geom_text(aes(label=model))+xlim(1,8)

#remove overlapping

ggplot(mpg,aes(displ,hwy))+geom_text(aes(label=model),check_overlap = TRUE)+xlim(1,8)

#geom_label draws rounded label around the text

label=data.frame(
  waiting=c(55,80),
  eruptions=c(2,4.8),
  label=c("peak one","peak two")
)
head(faithfuld)
ggplot(faithfuld,aes(waiting,eruptions))+geom_tile(aes(fill=density))+
  geom_label(data=label,aes(label=label))
data("presidential")
head(presidential)
library(dplyr)
head(economics$date)
presidential=presidential%>%
  filter(start>economics$date[1])
ggplot(economics)+
  geom_rect(aes(xmin=start,xmax=end,fill=party),ymin=-Inf,ymax=Inf,alpha=0.2,data=presidential)+
  geom_vline(aes(xintercept=as.numeric(start)),
             data=presidential,color="grey50",alpha=0.5)+
  geom_text(aes(x=start,y=2500,label=name),data=presidential,size=3,vjust=0,hjust=0,nudge_x = 100)+
  geom_line(aes(date, unemploy))+
  scale_fill_manual(values=c("blue","red"))
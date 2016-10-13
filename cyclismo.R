#Intermediate Plotting
#plotting continous data
#plotting correlation
x=rnorm(10,mean=20,sd=5)
y=2.5*x-1.0+rnorm(10,mean=10,sd=9)
cor(x,y)
#create multiple data sets on one plot
plot(x,y,xlab="Independent",ylab="dependent")
x1=runif(8,15,25)
y1=2.5*x1-1+runif(8,-6,-6)
points(x1,y1,col="blue")
#adding error bars to a set of data points
#added by arrows command
xhigh=x
yhigh=y+abs(rnorm(10,sd=3.5))
xlow=x
ylow=y-abs(rnorm(10,sd=3.1))
arrows(xhigh,yhigh,xlow,ylow,col=2,angle=90,length=0.1,code=3)
#adding some jitter to the plots
white=rhyper(400,4,5,3)
chipped=rhyper(400,2,7,3)
par(mfrow=c(1,2))
plot(white,chipped,xlab="#white marbles drawn",ylab="#chipped marbles drawn")
#adding jitter to it
plot(jitter(white),jitter(chipped),xlab="#white marbles drawn",ylab="#chipped marbles drawn")
#multiple charts on one place
par(mfrow=c(2,3))
boxplot(white)
boxplot(chipped)
plot(jitter(white),jitter(chipped))
hist(white)
hist(chipped)
mosaicplot(table(white,chipped))
#Density plot
par(mfrow=c(1,1))
smoothScatter(white,chipped)
grid(4,3)
#pairwise relationship
udata=rnorm(20)
vdata=rnorm(20,mean=5)
wdata=udata+vdata+rnorm(20,sd=0.5)
xdata=2*udata+rnorm(20,sd=0.1)
ydata=3*vdata+rnorm(20,sd=2.4)
d=data.frame(udata,vdata,wdata,xdata,ydata)
pairs(d)
x = c(-1,1,1,-1,-1)
y = c(-1,-1,1,1,-1)
plot(x,y)
polygon(x,y,col='blue')
# how to plot a surface with the persp command
x=seq(0,2*pi,by=pi/100)
y=x
xg=(x*0+1)%*%t(y)
yg=x%*% t(y*0+1)
f=sin(xg+yg)
persp(x,y,f,theta=-10,phi=40)
#Discrete Data
white=as.factor(white)
table(white)
barplot(table(white))
santa <- data.frame(belief=c('no belief','no belief','no belief','no belief',
                             'belief','belief','belief','belief',
                             'belief','belief','no belief','no belief',
                             'belief','belief','no belief','no belief'),
                    sibling=c('older brother','older brother','older brother','older sister',
                              'no older sibling','no older sibling','no older sibling','older sister',
                              'older brother','older sister','older brother','older sister',
                              'no older sibling','older sister','older brother','no older sibling')
)
santa
summary(santa)
plot(santa$belief)
plot(santa$belief,santa$sibling)

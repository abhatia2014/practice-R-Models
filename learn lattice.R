getwd()
#y~x|A*B means display relationship between numeric variables x&y separately for every combination of factors A,B
library(lattice)
attach(mtcars)
# create factors with value labels
str(mtcars)
head(mtcars,3)
gearf=factor(gear,levels=c(3,4,5),labels=c("3gears","4gears","5gears"))
table(gearf)
summary(cyl)
table(cyl)
cylf=factor(cyl,levels=c(4,6,8),labels=c("4cyl","6cyl","8cyl"))
table(cylf)
#kernel density plot
densityplot(~mpg,main="Density Plot",xlab="Miles per Gallon")
#Kernel density plot by factor levels
densityplot(~mpg|cylf,main="density plot by number of cyls",xlab="miles per gallon")
#kernel density plot by factor level(alternate method
densityplot(~mpg|cylf,layout=c(1,3))
#boxplot for each combination of two factors
bwplot(cylf~mpg|gearf,ylab="cylinders",xlab="miles per gallon",main="mileage by cylinders and gears",layout=c(1,3))
#scatterplots for each combination of two factors
xyplot(mpg~wt|cylf,layout=c(1,3),ylab="miles per gallon", xlab="Car Weight")
#3D scatter plot by factor level
cloud(mpg~wt*qsec|cylf,main="3D scatter plot by cylinders")
#dotplot forcombination of two factors
dotplot(cylf~mpg|gearf)
#scatterplot matrix
names(mtcars)
splom(mtcars[c(1,3,4,5,6)],)
#smooothen the graph
smooth=function(x,y){
  panel.xyplot(x,y)
  panel.loess(x,y)
}
hpc=cut(hp,3)
xyplot(mpg~wt|hpc,scales=list(cex=0.8,col="red"),panel=smooth,xlab="car wt",ylab="miles per gallon")

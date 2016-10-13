library(data.table)
DF=data.frame(x=c("b","b","b","a","a"),v=rnorm(5))
DF
# a data table is created in exactly the same manner
DT=data.table(x=c("b","b","b","a","a"),v=rnorm(5))
DT
CARS=data.table(cars)
CARS
head(CARS)

#to list all data.tables in memory
tables()
#to see all column types
sapply(DT,class)
sapply(CARS,class)
DT
DT[2,]
DT[x=='b']

#we set a key so that all values of the DT can be accessed easily
setkey(DT,x)
DT
#now running tables command
tables()
head(CARS)
setkey(CARS,speed)
CARS
tables()
#since DT has a key ie. x
#we can directly access elements of the datatable
DT['b',]
#mult argument allows first or the last row of the matched rows to be returned
DT['b',mult='first']
DT['b',mult='last']
#comma is optional too
DT['b']

grpsize = ceiling(1e7/26^2) 
LETTERS
letters

#create a large data frame
system.time( DF <- data.frame(
     x=rep(LETTERS,each=26*grpsize),
    y=rep(letters,each=grpsize),
    v=runif(grpsize*26^2),
    stringsAsFactors=FALSE)
   )
tables()
head(DF)
tail(DF)

#operation using normal dataframe

system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",])
head(ans1,3)

#now converting it to a data table and extracting the same information out
DT=as.data.table(DF)
tables()
system.time(setkey(DT,x,y))
system.time(ans2<-DT[list("R","h")])
head(ans2)
identical(ans1$v,ans2$v)
mapply(identical,ans1,ans2)
# there is a alias for list - .()
system.time(ans3<-DT[.('P','q')])
head(DT)

#fast grouping
system.time(abc2<-sum(DF$v))
system.time(abc<-DT[,sum(v)])
# we can also perform sum by which column
DT[,sum(v),by=x]
library(dplyr)
DF%>%
  group_by(x)%>%
  summarise(v1=sum(v))
#lets take the sum by two columns
system.time(ans4<-DT[,sum(v),by='x,y'])
system.time(tapply(DT$v,list(DT$x,DT$y),sum))

# Ref http://www.r-bloggers.com/solve-common-r-problems-efficiently-with-data-table-3/
#1.how to sort datatable by column
#Sort dataset dat by variables z and b. Use descending order for z and ascending for b.

#setorder(dat, -z, b)  

head(DT)
#in this example we will order the data table by decending v

setorder(DT,-v)
head(DT)
system.time(setorder(DT,v))

#Having dataset dt with variables x and grp calculate sum of x and length of x by groups specified by grp variable.

#dt[, .(sum(x), .N), grp]

system.time(a<-DT[,.(sum(v),.N),x])
system.time(b<- DF%>%
              group_by(x)%>%
              summarise(v1=sum(v),count=n()))
object.size(DT)
gc()
object.size(NULL)

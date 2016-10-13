## Transform and manipulate data
library(rattle)
rattle()
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
ds=weatherAUS
str(ds)
# let us consider the locations
cities=c("Adelaide","Brisbane","Canberra","Darwin")
names(ds)
levels(ds$Location)
summary(ds$Location)
# to take a subset of the data based on cities

dss=subset(ds,Location %in% cities)
summary(dss$Location)
# we have to refactor using factor()
dss$Location=factor(dss$Location)
levels(dss$Location)
summary(dss$Location)

#add a column to the dataframe

ds$TempRange=ds$MaxTemp-ds$MinTemp
summary(ds$TempRange)
#make a few plots
p=ggplot(ds,aes(x=TempRange))
q=p+geom_bar(binwidth=1)
q
?plyr
example(mutate)
?dplyr
ds%>%
group_by(Location)
summarise(total=sum(Rainfall))%>%
arrange
ds%>%
  group_by(Location)%>%
  summarise(total=sum(Rainfall))%>%
  arrange(desc(total))%>%
  head(5)
names(ds)
tail(ds$excess)
names(ds)
ds$Cloud3pm=NULL
names(ds)
dss=subset(ds,Date==max(Date))
dim(dss)
head(dss)
## reshape the data using melt package
library(reshape2)
dssm=melt(dss,c("Date","Location"))

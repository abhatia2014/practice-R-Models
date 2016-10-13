install.packages("rlist")
library(rlist)
library(dplyr)
dev=list(
  p1=list(name="Ken",age=22,
          interest=c("reading","music","movies"),
          lang=list(r=2,csharp=4,python=3)),
  p2=list(name="James",age=25,
          interest=c("sports","music"),
          lang=list(r=3,java=2,cpp=5)),
  p3=list(name="Penny",age=24,
          interest=c("movies","reading"),
          lang=list(r=1,cpp=4,python=2))
)

dev[[2]][1]
#filtering members with age no less than 24 by calling list.filter

str(list.filter(dev,age<=24))
list.map(dev,name)
list.map(dev,age)
list.map(dev,interest)

#get the programming language each person has been using for the longest time

list.map(dev,sort(unlist(lang),decreasing = TRUE)[1])

#selecting the name and age of each person by calling list. select
str(list.select(dev,name,age))

#select the name and evaluate range of the number of years using the programming language

str(list.select(dev,name,score.range=range(unlist(lang))))

#build a list that contain sublists representing age groups (building a group of age )
str(list.group(dev,age))

#sort the developers by number of interests in desending order and then by number of years they have been using R in descending order

str(list.sort(dev,desc(length(interest)),desc(lang$r)))

#use list.update to update the list by removing age and lang columns and introducing number of lang each member uses as nlang

str(list.update(dev,age=NULL,lang=NULL,nlang=length(lang)))
#pipeR package tremendously improves th readability of the code

install.packages("pipeR")
library(pipeR)

#following code returns age of developers whose age is less than 25, creates a dataframe
#where age is sorted by number of years using r in decending order, each row tells us name,# years using r
#and the longest time using a lang they know

dev%>>%
  list.filter(age<=25)%>>%
  list.sort(desc(lang$r))%>>%
  list.map(data.frame(name=name,r=lang$r,longest=max(unlist(lang))))%>>%
  list.rbind

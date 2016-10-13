x=data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
x
x=x[sample(1:5),]
x$var2[c(1,3)]=NA
x
x[(x$var1<=3 |x$var2>5),]
#if you don't want to return the NAs, use which command
x[which(x$var2>0),]
#sort variables by sort command
sort(x$var3)
#decreasing order
sort(x$var1,decreasing=TRUE)
#sort put NAs in the end
sort(x$var2,na.last=TRUE)
#order a data frame by a variable
x[order(x$var1),]
#order by multiple variables
x[order(x$var1,x$var3),]
#do an automatic sorting using plyr package
library(plyr)
arrange(x,var1)
arrange(x, desc(var1))
#summarizing data
rest=read.csv("Restaurants.csv")
head(rest,3)
tail(rest,3)
str(rest)
names(rest)
summary(rest)
attach(rest)
str(rest)
quantile(councilDistrict)
quantile(councilDistrict,probs=c(0.5,0.75,0.9))
pie(table(policeDistrict))
barplot(table(policeDistrict))
table(zipCode)
table(zipCode,useNA="ifany")
#making a two dimensional table
table(councilDistrict,zipCode)
#check for missing values in a variable
sum(is.na(councilDistrict))
summary(rest)
any(is.na(councilDistrict))
#checks if all values satisfy the condition
all(zipCode>0)
#check NA for all columns in the data frame
colSums(is.na(rest))
all(colSums(is.na(rest))==0)
# to find a specific element in a column
table(zipCode %in% c("21212"))
table(zipCode %in% c("21212","21213"))
table(name %in% c("mcdonald's"))
summary(name)
# to get the list of items who match the %in% enteries
rest[table(name %in% c("mcdonald's")),]
#cross tabs
data(UCBAdmissions)
DF=as.data.frame(UCBAdmissions)
summary(DF)
table(DF$Freq)
xt=xtabs(Freq~Gender+Admit,data=DF)
xt
#to see the size of a data set
a=rnorm(1e5)
object.size(a)
print(object.size(a),units="Mb")
#creating new variables
#will use the Baltimore restaurant data
rest=read.csv("Restaurants.csv")
print(object.size(rest),units="Mb")
rest$nearMe=rest$neighborhood %in% c("Roland Park","Homeland")
table(rest$nearMe)
#we can break a quantitative variable like zip code by groups using the cut command
rest$zipgroups=cut(rest$zipCode,breaks=quantile(rest$zipCode))
table(rest$zipgroups)
table(rest$zipgroups,rest$zipCode)
# reshaping data
#we use the reshape2 package for reshaping data to be more meaningful
library(reshape2)
head(mtcars)
#we use melt function to define which of the variables are just ID variables and which are measure variables
mtcars$carname=row.names(mtcars)
carmelt=melt(data = mtcars,id.vars = c("carname","gear","cyl"),measure.vars = c("mpg","hp"))
names(mtcars)
head(carmelt)
tail(carmelt,3)
# we use a function dcast to look only at specific variables rather than whole set
#let's say we want to look at cyl data for mpg and hp variables
  cyldata=dcast(data = carmelt,cyl ~ variable,mean)
cyldata
namedata=dcast(carmelt, carname~variable,mean)
namedata
geardata=dcast(carmelt,gear~variable,mean)
geardata
plot(gear~mpg,data=mtcars)
#averaging values
tail(InsectSprays,4)
# to see the sum of the count of sprays for each spray type
tapply(InsectSprays$count,InsectSprays$spray,sum)
table(InsectSprays$spray)
## if we want to split the sprays as separate lists
insplit=split(InsectSprays$count,InsectSprays$spray)
insplit
# we can sum the elements by lapply
sumcount=lapply(insplit,sum)
sumcount
#alternatively use the plyr package to do a function ddply
ddply(.data = InsectSprays,.(spray),summarize,sum=sum(count))
library(plyr)
# merging data- merge two or more data sets together
#download the data from Jeffs drop box account
file1="http://dl.dropoxusercontent.com/u/7710864/data/reviews-apr29.csv"
file2= "http://dl.dropoxusercontent.com/u/7710864/datasolutions-apr29.csv"
download.file(file1,destfile='./datareviews.csv',method="curl")
download.file(file2,destfile='./datasolutions.csv',method="curl")
review=read.csv("reviews.csv")
solutions=read.csv("solutions.csv")
#merge() command can merge the two data sets
head(review)
head(solutions)
merged=merge(x = review,y = solutions,by.x = "solution_id",by.y = "id",all=TRUE)
#all= true inserts NAs for all rows which do not have a merge
head(merged)
#to get all common names, we use intersect function
intersect(names(review),names(solutions))
# we can also use join from plyr package to join datasets together
#however Join can only merge/join data sets based on common names 
#if you have multiple data frames, very difficult to do with merge command, but if there is a common ID, it can be done using  join command
df1=data.frame(id=sample(1:10),x=rnorm(10))
df2=data.frame(id=sample(1:10),y=rnorm(10))
df3=data.frame(id=sample(1:10),z=rnorm(10))
df=list(df1,df2,df3)
df
join_all(df)
# Editing text variables
rest=read.csv("Restaurants.csv")
names(rest)
head(rest)

camera=read.csv("camera.csv")
head(camera)
names(camera)
# convert all names to lower case using the function tolower()
tolower(names(camera))
# we now separate out the variables(names) separated by .using strsplit() command
splitname=strsplit(x = names(camera),"\\.")
splitname
splitname[[6]][1]
# write a function called first element which will just take the first element from the list
firstelement=function(x){ x[1]}
sapply(splitname,firstelement)
# we again use the review and soutions data frame
names(review)
# we substitute _ with "" for the names using sub function
sub("_","",names(review))
names(review)
#searching for specific data points in names or values using grep() function
grep("Alameda",camera$intersection)
# if use value=TRUE, it will give the names of the data points and not just rows
grep("Alameda",camera$intersection,value=TRUE)
# string functions
#use stringr package
library(stringr)
nchar("aankur bhatia")
substr("aankur bhatia",1,8)
paste("aankur", "bhatia")
# if you want to paste with no spaces
paste0("aankur","bhatia")
str_trim("aankur    ")
# regular expressions
#^I think will match all lines starting with 'I think' - the character ^ matches to the start of the line
#$ represents the end of the line so 'morning$' will match all entries at the end of the line
#[Bb][Uu][Ss][Hh] will match all versions of Bush
# combining them ^[Ii] am will match the begining of line with lower or upper case I
#^[0-9][a-zA-Z]will match all entries starting with number followed by upper/lower text
#[^?.]$ means end of all lines which do not have a questionmark or period at the end
# . represents any possible character so 9.11 will match all entries which have 9-11 or 9:11 or anything else
# | means OR so flood|fire will match all lines having either of these characters
# we can also combine the expressions as ^[Ggood]|[Bbad] which means begining of the line with upper or lower case Good and Bad anywhere else in the ine
# if we have something optional we can use [Gg]eorge([Ww]\.)? [Bb]ush this means W. is optional
# use * to signify repeat any character any no of times, e.g (.*) repeat any character any number of times in parentesis
# [0-9]+ (.*)[0-9]+ means atleast one number followed by parenthesis followed by atleast one more number
# if we want to match the repetition of a phrase, use +([a-zA-Z]+)+\1+ means will match a space (at the begining) followed by atleast one character followed by repetion of the same characters

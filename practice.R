getwd()
#writing functions in R
add=function(a,b){
  return (a+b)
}

add(2,5)

sqadd=function(a,b){
  
  return (a^2+b^2)
}

sqadd(2,9)

y=10
g=function(x){
  x*y
}

f=function(x){
  
  y=2
  y^2+g(x)
}

f(3)

make_power_function=function(pow){
  
  return(function(a)return (a^pow))
}
  
square=make_power_function(2)
cube=make_power_function(3)
square(5)
cube(10)


#error debugging

get('pow',environment(square))
get('pow',environment(cube))

#using the If statement
number=2
if(number>5){
  print("great")
} else {
  print("that's sad")
}

#another function is any

v=1:10
any(v>5)

all(v>5)
#all returns true only if all of the conditions are satisfied

# %in% checks if a particular element is present and returns true if there is

v=1:10

15 %in% v

data(mtcars)
names(mtcars)

if ('wt' %in% colnames(mtcars)) {
  print("yes")
} else {
  print("no")
}

# repetition or iteration

v=1:10

for (element in v) {
  print(element)
}

# or alternatively using indices

v=1:10

for (i in seq_along(v)) {
  
  print(v[i])
}

#using while loop- while loop is ended when a condition is no longer true

i=1
while (i<=10){
  
  print(i)
  i=i+1
}

#repeat loop repeats the code until a condition is met and break is called

i=1
repeat{
  print(i)
  if (i>10) break
  i=i+1
}

v=c(1,2,3,4)

for (x in v) {
  print(x^2)
}

#using repeat loops
i=1
repeat{
  print(v[i]^2)
  if (i>4)
    break
  i=i+1
}

# using while loop
i=1
while (i<5) {
  print(v[i]^2)
  i=i+1
}

# apply loops

#we have a list of 100 csv files, we want to read them and combine in one dataset
#lapply is for lists
#sapply is for vectors
#tapply is for dataframe

#e.g using tapply on warpbreaks dataset

data(warpbreaks)
str(warpbreaks)

#using tapply

tapply(warpbreaks[,'breaks'],warpbreaks[,c('wool','tension')],sum)

#an apply loop on matrices is called plain apply

m=matrix(1:100,10,10)
apply(m, 2, mean)

#data manipulation using dplyr

library(dplyr)
filter(mtcars,cyl==8,qsec>16)
# select is to select columns or rename columns based on criteria

select(mtcars,mpg,wt,new_cyl=cyl)
select(mtcars,starts_with('c'))
select(mtcars,ends_with('yl'))
select(mtcars,contains('ar'))
select(mtcars,one_of('mpg','cyl'))

#rename nenames columns, only keeps those
rename(mtcars,new_mpg=mpg)

#distinct selects unique rows based on the content of one or more columns
distinct(mtcars,cyl)

#mutate edits or add columns but keeps all columns mentioned in the function

dat=select(mtcars,mpg,wt)
mutate(dat,new_column=mpg+wt,new_column2=mpg*wt)

#transmute also edits and add columns but only keeps the new columns
transmute(mtcars,new_column=mpg+wt)

#summarize summarises columns

summarize(mtcars,meanwt=mean(wt),max_milespergallon=max(mpg),
          min_cyl=min(cyl))
#summarize_each summarizes multiple columns at once

summarize_each(mtcars,funs(mean,sd),mpg,wt)

#sample_n sample n rows from the data.frame

sample_n(mtcars,10)

#sample_frac samples fraction of rows

sample_frac(mtcars,0.5,replace=FALSE)

#glimpse gets a quick overview of the data

glimpse(mtcars)

#do, executes R expression

result=do(mtcars,model=lm(mpg~wt,data=.))
summary(result)
result
result$model

#some more expressions from tidyr

#two functions - gather, spread

dat=data.frame(temp=runif(3,15,25),
               rain_station=runif(3,1,3),
               rain_station2=runif(3,1,3),
               rainstation3=runif(3,1,3))
dat
#we use the gather function to wrap the precipitation values into one column, add an additional identification column that speciies the station where the precipitation was observerd

library(tidyr)
gather(dat,station_id,rainfall,-temp)

#group_by function in dplyr

mtcars_grouped=group_by(mtcars,am)
mtcars_grouped
summarize(mtcars_grouped,mean_weight=mean(wt),min_cylinder=min(cyl))

#using piping
mtcars%>%
  filter(wt>3.5)%>%
  group_by(cyl,am)%>%
  summarize(mn=mean(mpg))
names(mtcars)
mtcars%>%
  filter(wt>3)%>%
  summarize(mean(hp))
  
#how many cars of unique no of gears are present

mtcars%>%
  group_by(gear)%>%
  summarize(count=n())

library(reshape2)
dat=melt(EuStockMarkets)
glimpse(dat)
glimpse(EuStockMarkets)

names(dat)
dat=dat[,-1]
glimpse(dat)
colnames(dat)=c('market_id','closing_price')
dat%>%
  group_by(market_id)%>%
  summarize(mean_closing=mean(closing_price))%>%
  arrange(mean_closing)

#which stock exchange varies most over time

dat%>%
  group_by(market_id)%>%
  summarise(most_variance=sd(closing_price))%>%
  arrange(desc(most_variance))

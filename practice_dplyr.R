install.packages('readr')
library(dplyr)
library(ggplot2)
gd_url="http://tiny.cc/gapminder"
gtbl=gd_url %>%read.delim %>% tbl_df
head(gtbl)
gtbl %>% glimpse
# use mutate() to add new variables
gtbl=gtbl %>% 
  mutate(gdp=pop*gdpPercap)
gtbl
gtbl %>% glimpse

#get the numbers for US only
just_US= gtbl %>% 
  filter(country =="United States")
just_US
#alternatively

head(gtbl[gtbl$country=="United States",])

# match the year to just US year and compare the gdp per cap of each country to us

gtbl=gtbl %>% 
  mutate(US=just_US$gdpPercap[match(year,just_US$year)],gdpcomp=gdpPercap/US)
gtbl
gtbl %>%
  select(country,year,gdpPercap,US,gdpcomp)

#find the summary of the gdpcomp

gtbl %>%
  select(gdpcomp) %>%
  summary
#filter by records where gdpcomp>1

gtbl %>%
  filter(gdpcomp>1) %>%
  select(country,year,gdpPercap,US,gdpcomp)

gtbl %>%
  filter(country=="Canada") %>%
  select(country,year,gdpPercap,US,gdpcomp)

gtbl %>%
  select(gdpcomp) %>%
  summary

## if you want to order the data by year , country rather than country , year
gtbl %>%
  arrange(desc(year),desc(country))

#let's say you want the 2007 year only sorted by life expectancy

y2007=gtbl %>%
  filter(year==2007) %>%
  arrange(lifeExp) %>%
  select(country,year,lifeExp,gdpPercap)
y2007 %>%
  arrange(lifeExp)
cor(gtbl$gdpPercap,gtbl$lifeExp)
ggplot(y2007,aes(gdpPercap,lifeExp))+geom_point()+
  geom_smooth()

#sorting out lifeExp in desending order

gtbl %>%
  filter(year==2007) %>%
  arrange(desc(lifeExp))

#get lifeexpectancy for India
gtbl %>%
  filter(country=="India") %>%
  select(country,year, lifeExp, gdpPercap, gdpcomp)

#use rename() to rename variables
?rename
?rename_vars()

gtbl %>%
  rename(life.exp=lifeExp, gdp.per.cap=gdpPercap)

#counting things up

#how many observations per continent

gtbl %>%
  group_by(continent) %>%
  summarise(n.obs=n())

# a convenience function to count the number of observations is tally()

gtbl %>%
  group_by(continent) %>%
  tally

#add the number of unique countries to each continent

gtbl %>%
  group_by(continent) %>%
  summarize(n.obs=n(),n.countries=n_distinct(country))

# see the details of Oceania continent

gtbl
gtbl %>%
  filter(continent=="Oceania", year==2007) %>%
  select(country, lifeExp,gdpPercap,US, gdpcomp)

#lets compute the average life expectancy by continent

gtbl %>%
  group_by(continent) %>%
  summarize(avg.exp=round(mean(lifeExp),2))

#summarize_each applies the same function to multiple variables
# mean, median of life expectancy, gdpPercap, gdpcomp

gtbl %>%
  group_by(continent) %>%
  summarise_each(funs(mean, median),lifeExp,gdpPercap,gdpcomp)

# which all countries come in Americas

gdp.asia=gtbl %>%
  filter(continent=="Asia", year==2007) %>%
  group_by(country,year) %>%
  summarise_each(funs(mean, median),lifeExp, gdpPercap) %>%
  arrange(desc(gdpPercap_mean))
gdp.asia
ggplot(gdp.asia, aes(country,lifeExp_mean))+geom_bar(stat="identity")

#what's the min and max life expectancy seen for Asia

gtbl %>%
  filter(continent =='Asia') %>%
  group_by(year) %>%
  summarize(min.life.exp=min(lifeExp),max.life.exp=max(lifeExp))

# which countries contribute to the min and max life expectancy 
gtbl %>%
  filter(continent=="Asia") %>%
  select(year, country, lifeExp) %>%
  arrange(year) %>%
  group_by(year) %>%
  filter(min_rank(desc(lifeExp))<2 |min_rank(lifeExp)<2)

# http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# the NYC flight data
install.packages("nycflights13")
library(nycflights13)
dim(flights)
head(flights)

#better to convert the data to a table using tbl_df
# as it would make a wrapper around a data frame and 
#wont accidentally print a lot of data to the screen

# verbs used by dplyr
# filter() and slice()
# arrange()
#distinct()
#mutate() and transmute()
#summarise()
#sample_n()and sample_frac()

filter(flights, month==1,day==1)
#alternatively
filter(flights,month==1 | day==1)

# to select rows by position, use slice()

slice(flights,1:10)

#use of arrange

arrange(flights,desc(arr_delay))

#use select to find a subset of columns that you want to work with

select(flights,year,month,day)

#alternatively, to select all flights between carrier, origin

select(flights,carrier:dest)

# can remove some of the columns out as

select(flights,-(year:day),-(air_time:minute))

# there are other functions with select using matches() or contains()

#you can also rename variable using select

select(flights,tail_num=tailnum)

#alternatively, use rename() to rename variables

#get distinct rows (unique rows)
distinct(select(flights,tailnum))
distinct(select(flights,carrier))
distinct(select(flights,origin,dest))

#add a new column using mutate() function
mutate(flights,
       gain=arr_delay-dep_delay,
       speed=distance/air_time *60)%>%
  select(flights,minute:speed)

#transmute() will only contain the new variables

#summarise value with summarise function

summarise(flights,delay=mean(dep_delay,na.rm=TRUE))
flights%>%
  group_by(carrier)%>%
  summarise(delay=mean(dep_delay,na.rm=TRUE))

#randomly sample rows with sample_n() and sample_frac()
sample_n(flights,10)

# or a fraction of the total sample

sample_frac(flights,0.05)

# group by tailnum
by.tailnum=group_by(flights,tailnum)
delay=summarise(by.tailnum,count=n(),
                dis=mean(distance,na.rm=TRUE),
                delay=mean(arr_delay,na.rm=TRUE))
delay=filter(delay,count> 20,dist< 2000)

ggplot(delay,aes(dist,delay))+
  geom_point(aes(size=count),alpha=1/2)+
  geom_smooth()+ scale_size_area()

# find the number of flights and planes that go to each destination
destination=group_by(flights,dest)
summarise(destinations,planes=n)
install.packages("installr")
library(installr)
updateR()

###################################################################
#https://github.com/abhatia2014/dplyr-tutorial/blob/master/dplyr-tutorial-2.Rmd
#learn from this site

library(dplyr)
library(ggplot2)
library(nycflights13)

flights

# deselect columns which you donot want to display

flights%>% select(-air_time,-hour)

# or hide a range of columns
flights%>% select(-(dest:minute))

# or hide any colums with matching name

flights%>% select(-contains("time"))

#pick columns using character vector of column names

cols=c("origin","dest","distance","carrier")
flights%>%select(one_of(cols))

# chosing rows- use filter, between, slice, sample_n,top_n,distinct

#filter allows use of multiple conditions

flights%>% filter(dep_time>=600,dep_time<=601)

#if the numeric values fall in a range then its better to use between

flights%>% filter(between(dep_time,600,602))

# using is.na
flights%>% filter(is.na(dep_time))

#slice filters rows by position

flights%>% slice(1000:1010)


# keep the first three rows within each group 
flights%>%group_by(month,day)%>% slice(1:3)

#sample any three rows from each group
flights%>%group_by(month,day)%>%sample_n(3)

#find the top three departure delay
flights%>% group_by(month,day)%>% top_n(3,dep_delay)

# also sort by departure delay for each group

flights%>% group_by(month, day)%>%top_n(3,dep_delay)%>% arrange(desc(dep_delay))

# use distinct function to find unique rows only

flights%>%select(origin,dest)%>%distinct()


#adding new variables- use mutate, transmutate, add_rownames()

# use mutate- keeps all existing variables

flights%>%mutate(speed=distance/air_time*60)

#transmute() only keeps the new variables

flights%>%transmute(speed=distance/air_time*60)


# get the other data frame mtcars()

mtcars%>%head

#add_rownames () turns rownames into explicit variables

mtcars%>% add_rownames("model")%>% head

mtcars%>% add_rownames("model")

mtcars%>%add_rownames("model")%>%tbl_df

#grouping and counting- summarize, tally, count, group_size,n_groups, ungroup

#summarise can be used to count the no of rows in earch group

flights%>% group_by(month)%>% summarise(cnt=n())

#tally and count() can do this even better

flights%>% group_by(month)%>%tally()

flights%>% count(month)

# sorting the count

flights%>% group_by(month)%>%summarize(cnt=n())%>%arrange(desc(cnt))

#tally and count have a sort parameter for this purpose

flights%>%group_by(month)%>%tally(sort=TRUE)
flights%>% count(month,sort=TRUE)

#sum over a specific variable instead of counting rows
flights%>% group_by(month)%>% summarize(dist=sum(distance))

#tally and count also have a parameter called wt for this purport

flights%>% group_by(month)%>%tally(wt=distance)
flights%>%count(month,wt=distance) 

#group_size() returns the counts as vectors

flights%>% group_by(month)%>% group_size()

#n_groups reports it as # of groups
flights%>%group_by(month)%>% n_groups()

# group by two variables may be confusing

flights%>% group_by(month,day)%>%summarize(cnt=n())%>%arrange(desc(cnt))%>%print(n=40)

flights%>%group_by(month,day)%>%summarize(cnt=n())%>%ungroup()%>%arrange(desc(cnt))

flights%>%print(width=Inf)
flights%>% print(n=30,width=Inf)



# practice dplyr

data(mtcars)
data=mtcars
data$cyl=factor(data$cyl)
glimpse(data)

data%>%
  slice(5:20)

vars=c("mpg","wt","qsec")
data%>%
  group_by(cyl)%>%
  select(one_of(vars))%>%
  summarise_each(funs(median(.)))

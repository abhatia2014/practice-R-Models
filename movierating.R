library(dplyr)
library(stringr)
library(DMwR)
library(ggplot2)
library(grid)
t=read.csv("./Movie Ratings/10Mratings/ratings.dat",sep = ":",header = FALSE)
t=t[,-c(2,4,6)]
t1=read.csv("./Movie Ratings/10Mratings/movies.dat",header = FALSE,sep=":",stringsAsFactors = FALSE)
mov=t1[,-c(2)]
str(mov)
colnames(mov)=c("Movie","Name","Extra","Genre")
mov=mov[complete.cases(mov),]
mov$Extra=NULL
mov[mov==""]=NA
mov=na.omit(mov)

str(t)
colnames(t)=c("ID","Movie","Rating","Timestamp")
t$name=mov$Name[match(t$Movie,mov$Movie)]
head(t)
t$Genre=mov$Genre[match(t$Movie,mov$Movie)]
head(t)
t=na.omit(t)
head(t)

#find the top 10 movies with the highest mean rating

highrat=t%>%
  group_by(name)%>%
  summarise(meanrating=round(mean(Rating),2))%>%
  arrange(desc(meanrating))%>%
  head(20)
str(highrat)
ggplot(highrat)+geom_bar(aes(x=name,y=meanrating,fill=name,sort(order(meanrating))),stat="identity")+coord_flip()+theme_bw()+
  theme(legend.position="none")+geom_text(aes(name,meanrating,label=meanrating))
# which Genre got the highest average rating
t%>%
  group_by(Genre)%>%
  summarise(meanrating=mean(Rating))%>%
  arrange(desc(meanrating))
  
#Movie names (top 10) by highest of rating (how many people rated)

highrating=t%>%
  group_by(name)%>%
  summarise(count=n(),meanrating=round(mean(Rating),2))%>%
  arrange(desc(count))%>%
  head(20)
  
ggplot(highrating)+geom_bar(aes(x=reorder(name,count),y=count,fill=count),stat="identity")+scale_fill_gradient2(low="red",mid="orange",high="green")+theme_bw()+
  theme(legend.position="none")+coord_flip()+geom_text(aes(name,count,label=count))+
  labs(x="Movie Names",y="Number of Users Rated",title="Top 20 Rated Movies with Mean Rating")+
  geom_text(aes(name,count/2,label=meanrating))

highcount=t%>%
  group_by(name)%>%
  summarise(count=n(),meanrating=round(mean(Rating),2))%>%
  filter(count>1000)%>%
  arrange(desc(meanrating))%>%
  head(20)

ggplot(highcount)+geom_bar(aes(x=reorder(name,meanrating),y=meanrating,fill=meanrating),stat="identity")+scale_fill_gradient2(low="red",mid="orange",high="green")+theme_bw()+
  theme(legend.position="none")+coord_flip()+geom_text(aes(name,meanrating,label=meanrating))+
  labs(x="Mean Rating with Count",y="Movie Names",title="Top 20 Rated Movies with Count of ratings")+
  geom_text(aes(name,meanrating/2,label=count))

# Genre (top10) by highest of rating (how many people rated)

t%>%
  group_by(Genre)%>%
  summarise(count=n(),meanrating=mean(Rating))%>%
  arrange(desc(count))%>%
  head(20)
  
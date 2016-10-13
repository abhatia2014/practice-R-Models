# This is a tutorial to explain how to clean the data
#data() command gives all the data sets available to us
# Ways to manipulate data
#1. Missing Values
#2. Data Summarization
#3. Group by factors
#4. Aggregate
#5. Subset/Exclude
#6. Bucketing Values
#7. Rearrange (shape)
#8. Merge Datasets
#9. and how to commit

glimpse(movies)
movies=tbl_df(movies)
movies
movies%>%
  filter(Comedy==1 | Documentary==1,year>1990,votes>100,rating>5)%>%
  select(title,year,length,rating,votes,Comedy,Documentary)%>%
  arrange(desc(rating),desc(year))%>%
  sample_frac(.02)

avg_rating=movies%>%
  group_by(year)%>%
  summarize(avg_rating=mean(rating))
avg_rating

ggplot(avg_rating,aes(year,avg_rating))+geom_point()+
  theme_bw()+geom_smooth()

#average rating for comedy movies
avg_com=movies%>%
  filter(Comedy==1)%>%
  group_by(year)%>%
  summarize(avg_rating=mean(rating))
avg_com

ggplot(avg_com,aes(year,avg_rating))+geom_point()+
  theme_bw()+geom_smooth()

glimpse(movies)

avg_rom=movies%>%
  filter(Romance==1)%>%
  group_by(year)%>%
  summarize(avg_rating=mean(rating))
avg_rom

ggplot(avg_rom,aes(year,avg_rating))+geom_point()+
  theme_bw()+geom_smooth()

#barplots of average ratings by year
avg_rating

ggplot(avg_rating,aes(year,avg_rating,fill=year))+geom_bar(stat="identity")+theme_bw()+
  stat_density2d(aes(fill=..level..))
  
data()
pressure
plot(pressure)
pressure=tbl_df(pressure)
pressure=pressure%>%
  mutate(temperature=temperature+273.15,pressure=pressure*.1333)
plot(pressure)

ggplot(pressure,aes(temperature,pressure))+geom_point()+
  scale_y_log10()+scale_x_log10()

# let's fit some models to the distribution

lm.out1=lm(log(pressure)~temperature,data=pressure)
lm.out1
summary(lm.out1)

#let's evaluate the fitted vs residual values

plot(lm.out1$fitted,lm.out1$residual)

#let's fit the model for log of pressure and temperature
# this is also called the power model


lm2=lm(log(pressure)~log(temperature),data=pressure)
plot(lm2$fitted,lm2$residual)


#polynomial regression- lets try to fit the fitted values and residuals

ls()
lm3=lm(pressure~temperature+I(temperature^2)+I(temperature^3),data=pressure)
summary(lm3)
plot(lm3$fitted,lm3$residual)

#Box-Cox Transformations

#taking the reciprocal of temperature 

plot(pressure~I(1/temperature),data=pressure)

#Box-Cox transformations allow us to find the optimum transformation
#of the response variable using maximum-likelihood methods

library("MASS")

boxcox(pressure~I(1/temperature),data=pressure)
boxcox(pressure~I(1/temperature),lambda=seq(-0.2,0.2,.01),data=pressure)

#let's use a log transform of the response variable

plot(log(pressure)~I(1/temperature),data=pressure)
lm4=lm(log(pressure)~I(1/temperature),data=pressure)
summary(lm4)

plot(lm4$fitted,lm4$residuals)
  
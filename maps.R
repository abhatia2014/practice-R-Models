setwd('~/Datasets')
install.packages("rworldmap")
library(rworldmap)
vignette('rworldmap')
#let's start putting the map of the entire world
nmap=getMap(resolution="low")
plot(nmap,main="World")
# crop to the area desired (outside US)

plot(nmap,xlim=c(-139.3,-58.8),
     ylim=c(13.5,55.7),
     asp=1,
     main="US from Worldmap")

library(ggplot2)
library(ggmap)
mworld=map_data(map="world")

str(mworld)

#to find how many unique regions

length(unique(mworld$region))

#how many group polygons since some regions have multiple subregions

length(unique(mworld$group))

ggplot(mworld,aes(long,lat,group=group,fill=region))+
  geom_polygon()+
  labs(title="World Map")+theme_bw()+theme(legend.position="none")

ggplot(mworld,aes(long,lat,group=group,fill=region))+
  geom_path()+ #country outline instead
  labs(title="World Map")+theme_bw()+theme(legend.position="none")

#zooming on a specific region

library(ggmap)

library(mapproj)
map=get_map(location = "Albuquerque, New Mexico",zoom = 10,maptype = "hybrid")
ggmap(map)+labs(title="New Mexico Map")
map2=get_map(location="Frankfurt, Germany",zoom=6)
ggmap(map2)
#adding points and paths to the map

dat <- read.table(text = "
location lat long
MathStat 35.08396 -106.62410
Ducks 35.08507 -106.62238
SC1Class 35.08614 -106.62349
Biology 35.08243 -106.62296
CSEL 35.08317 -106.62414
", header = TRUE)
# getting a different watercolor

map2=get_map (location ="New Mexico",
          zoom=16,maptype="watercolor")

map2=get_map(location = "University of New Mexico",zoom = 10,maptype = "hybrid")

ggmap(map2)+geom_point(data=dat,aes(long,lat, shape=location,color=location),size=7)+
  geom_text(data=dat,aes(long,lat,label=location),hjust=-.2)+
  theme(legend.position="none",panel.grid.major=element_blank())

#alternatively

dat.pts=data.frame(x=dat$long,y=dat$lat)

map3=get_googlemap("University of New Mexico",zoom=16,
                   maptype="satellite",
                   markers=dat.pts,
                   path=dat.pts,
                   scale=2)
ggmap(map3,extent='device',darken=0.2)+
  geom_text(data=dat,aes(long,lat,label=location,hjust=-0.2,color="white",size=6))+
  theme(legend.position="none")

#plot locations of crimes, ranked by their severity

str(crime)
library(dplyr)
crime=tbl_df(crime)
glimpse(crime)

#extract location of crimes in Housten

violent.c=crime%>%
  filter(!offense=="auto theft")%>%
  filter(!offense=="theft")%>%
  filter(!offense=="burglary")

#rank voilent crimes

violent.c=violent.c%>%
  mutate(offense=factor(offense,levels=c("robbery","aggravated assault","rape","murder")))
glimpse(violent.c)

#restrict to downtown only

violent.c=violent.c%>%
  filter(lon>= -95.39681 & lon <= -95.34188 & lat >= 29.73631 & lat <= 29.784)
# first get B&W map of Houston TX

map=get_map(location="Houston TX", zoom=14,
            maptype="roadmap", color="bw")
ggmap(map)+geom_point(data=violent.c,aes(lon,lat,size=offense,color=offense))+
  theme(legend.position=c(0.0,0.7), legend.justification = c(0, 0)
        , legend.background = element_rect(colour = F, fill = "white")
        , legend.key = element_rect(fill = F, colour = F)
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text = element_blank()
        , axis.title = element_blank()
        , axis.ticks = element_blank()
  )
# draw a 2D density plot

ggmap(map)+stat_density2d(data=violent.c,aes(lon,lat,fill=..level..,alpha=..level..,size=2,bins=4,geom="polygon"))+
  scale_fill_gradient("Violent\nCrime\nDensity")+facet_wrap(~day,nrow=2)+theme_bw()+theme(legend.position="none")
  
mapworld=get_map(location="World")
ggmap(mapworld)

mapeu=get_map(location="Europe",zoom=4)
ggmap(mapeu)

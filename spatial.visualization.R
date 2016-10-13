library(ggmap)
## to get the longitude and latitude 
a=geocode("the white house")
geocode(" 7 Center Ct, Bethpage, NY")
home="7 Center Ct, Bethpage, NY, 11714"
qmap(home,zoom=14)
qmap(home,zoom=14,source="osm")
obama="the white house"
qmap(obama,zoom=14)
# Using Stamen Maps
qmap(obama,zoom=14,source="stamen",maptype="toner")
qmap(obama,zoom=14,source="stamen",maptype="watercolor")
set.seed(500)
df=round(data.frame(
  x=jitter(rep(-95.36,50),amount=0.3),y=jitter(rep(29.76,50),amount=0.3)),digits=2)
map=get_googlemap('houston',markers=df,path=df,scale=2)
ggmap(map,extent="device")
#using google map with marker
map=get_googlemap('Bethpage, NY',marker=home,path=home,scale=4)
ggmap(map,extent="device")

# examples using get_map function
paris=get_map(location="paris")
str(paris)
#extent = normal puts the Lon and Lat also to the maps
ggmap(paris,extent="normal")

## working with ggmaps crime data set

str(crime)
qmap('houston',zoom=13)
gglocator(2)
## get only violent crimes
head(crime)
library(ggmap)
head(crime)
length(crime$offense)
voilent.crime=subset(crime,offense!="auto theft" & offense != "theft" & offense != "burglary")
# order violent crimes
str(voilent.crime$offense)
levels(voilent.crime$offense)
voilent.crime$offense=factor(voilent.crime$offense,levels=c("robbery","aggravated assault","rape","murder"))

#restrict to downtown

voilent.crime=subset(voilent.crime, +
    -95.39681 <= lon & lon <= -95.34188 & 29.73631 <= lat & lat<= 29.78400)
theme_set(theme_bw(16))
Houstonmap=qmap('houston',zoom=14,color="bw",legend="topleft")
Houstonmap+geom_point(aes(x=lon,y=lat,color=offense,size=offense),data=voilent.crime)
Houstonmap+stat_bin2d(aes(x=lon,y=lat,color=offense, fill=offense),size=.2,bins=30,alpha=1/2,data=voilent.crime)


##  plotting voilent crimes using ggplot

houston=get_map('houston',zoom=14)
houstonmap=ggmap(houston,extent="device",legend="topleft")
houstonmap+stat_density2d(aes(x=lon,y=lat,fill=..level..,alpha=..level..),size=2,bins=4,data=voilent.crime,geom="polygon")
## add a inset to the plot
houstonmap+overlay+inset(
  grob=ggplotGrob(ggplot()+overlay+theme_inset()),xmin=-95.35836,xmax=Inf,ymin=-Inf,ymax=25.75062)

houston=get_map('houston',zoom=14,color="bw")
houstonmap=ggmap(houston,base_layer=ggplot(aes(x=lon,y=lat),data=voilent.crime,extent="device"))
houstonmap+stat_density2d(aes(x=lon,y=lat,fill=..level..,alpha=..level..),bins=6,geom="polygon",data=voilent.crime)+
  scale_fill_gradient(low="black",high="red")+facet_wrap(~day,ncol = 4)

## geocode function of ggmaps

geocode('baylor university',output="more")
# reverse geocode function

gc=geocode("baylor university")
gc=as.numeric(gc)
gc
revgeocode(gc)
home=geocode("7 Center Ct, Bethpage, NY")
home=as.numeric(home)
revgeocode(home)
revgeocode(home,output="more")
revgeocode(home,ouput="all")

## mapdist function

from=c("houston","dallas")
to=c("waco, texas", "san antonio")
a=mapdist(from,to)
names(a)
a
distQueryCheck()
.GoogleDistQueryCount
legs_df=route("7 Center Ct, Bethpage, NY","2 Jericho Plaza, Jericho, NY",alternatives=TRUE)
legs_df
## plot it onto a graph

qmap('John F Kennedy Middle School, Bethpage, NY',zoom=15,base_layer=ggplot(aes(x=startLon,y=startLat),data=legs_df))+
  geom_leg(aes(x=startLon,y=startLat,xend=endLon,yend=endLat,color=route), alpha=3/4,size=2,data=legs_df)+
  labs(x="Longitudinal",y="Latitude",color="Route")+facet_wrap(~route,ncol=3)+theme(legend.position='top')

# get an example shape file

download.file('http://www.census.gov/geo/cod/bdy/tr/tr00sp/tr48_d00_shp.zip',destfile='census.zip')



ver=read.csv("vcdb.csv",header = TRUE)
ver=tbl_df(ver)
ver=ver[,-(1458:1621)]
ver=ver[,-(566:816)]
ver=ver[,-(1250:1279)]
options(dplyr.width=Inf)

#get all incidents post 2008
post2008=ver%>%
  filter(timeline.incident.year> 2007)
post2008%>%
  group_by(timeline.incident.year)%>%
  summarize(count=n())
  
# find the attacks by patterns

pattern.year=post2008%>%
  group_by(timeline.incident.year,pattern)%>%
  summarize(count=n())
ggplot(pattern.year,aes(timeline.incident.year,count,fill=pattern))+theme_bw()+geom_bar(stat="identity")+
  facet_wrap(~pattern,ncol = 4)+geom_smooth(method="lm")

# write the file back to the database
write.csv(post2008,"breachespost2008.csv",row.names=FALSE )
sector=read.csv("sector.csv")
names(post2008)
table(post2008$victim.industry3)
table(sector$Sector)
  
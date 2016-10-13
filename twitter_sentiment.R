library(twitteR)
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
library(tm)
library(SnowballC)
library(qdap)
library(wordcloud)

api_key=  "pmdVLMK7TiXtOZRSgNJeOHkxl"
api_secret= "VkCXokPgp4sJ9Dxpgs8xLq2jwA26Ns2aw0odgMYT995apNoOYy"
access_token= "2381215371-Jt8jLfmiPvMqekXrRDbgbK0z1aTJlDMDgxvpgKP"
access_token_secret= "ZV6AulHPO2WbAV0lJDigBjdmj69rMXFA6IBZgjb7ZKh6M"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

positive=scan('positive-words.txt',what='character',comment.char=';')
negative=scan('negative-words.txt',what='character',comment.char=';')

positive=c(positive,"cloud")
negative=negative[negative!="cloud"]
positive=positive[positive!="trump"]

#********************************************************************************
findfd= "Cruz"
number= 5000
#********************************************************************************
start.time=Sys.time()
tweet=searchTwitter(findfd,number)
end.time=Sys.time()
time.taken=end.time-start.time
time.taken

#use laply to get text from all texts
tweetT=lapply(tweet,function(t)t$getText())


# Perform Sentiment analysis


# we define a function that will handle the sentence cleaning

# define an error catching function

tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

clean=function(t){
 t=gsub('[[:punct:]]','',t)# remove punctuations
 t=gsub('[[:cntrl:]]','',t)# remove 
 t=gsub('\\d+','',t)# remove punctuations
 t=gsub('[[:digit:]]','',t)# remove digits
 t=gsub('@\\w+','',t)# remove people
 t=gsub('http\\w+','',t)# remove http links
 t=gsub("^\\s+|\\s+$", "", t)# remove extra space
 t=sapply(t,function(x) tryTolower(x))
 t=str_split(t," ")
 t=unlist(t)
 return(t)
}



#lapply the clean function to tweet

tweetclean=lapply(tweetT,function(x) clean(x))

tail(tweetclean,5)

#remove tweetT
rm(tweetT)
  
#get the positive and negative score and words
#Create a function to calculate the positive score for all tweets

returnpscore=function(tweet) {
    pos.match=match(tweet,positive)
    pos.match=!is.na(pos.match)
    pos.score=sum(pos.match)
    return(pos.score)
}



# find the positive score for tweet clean file
positive.score=lapply(tweetclean,function(x) returnpscore(x))

#define a loop to add all items in a list
pcount=0

for (i in 1:length(positive.score)) {
  pcount=pcount+positive.score[[i]]
}
pcount

# find the positive match words- define a function


poswords=function(tweets){
    pmatch=match(tweets,positive)
    posw=positive[pmatch]
    posw=posw[!is.na(posw)]
    return(posw)
  }


# apply to the tweet set 
words=NULL
pdatamart=data.frame(words)

for (t in tweetclean) {
  pdatamart=c(poswords(t),pdatamart)
}


# Calculate the count of negative words


negscore=function(tw) {
    neg.match=match(tw,negative)
    neg.match=!is.na(neg.match)
    neg.score=sum(neg.match)
    return(neg.score)
}

#apply the function to the tweet clean list
negative.score=lapply(tweetclean,function(x) negscore(x))

#define a loop to add all items in a list
ncount=0

for (i in 1:length(negative.score)) {
  ncount=ncount+negative.score[[i]]
}
ncount

# find the negative match words- define a function

negwords=function(tweets){
    nmatch=match(tweets,negative)
    negw=negative[nmatch]
    negw=negw[!is.na(negw)]
    return(negw)
}


# apply to the tweet set 
nwords=NULL
ndatamart=data.frame(nwords)

for (t in tweetclean) {
  ndatamart=c(negwords(t),ndatamart)
}



# Decision for sentiment analysis
result=function(n,p) {
  if (n>p){
    paste("The sentiment for",findfd,"is negative in the ratio of ", round(n/p,1)," : 1" )
  } else {
    paste("The sentiment for",findfd,"is positive in the ratio of ", round(p/n,1)," :1")
  }
  
}

result(ncount,pcount)

pwords=unlist(pdatamart)
nwords=unlist(ndatamart)
head(nwords,50)
#remove all unused variables

rm(positive.score,words,negative.score)

# first convert the positive words and negative words into dataframe

dpwords=data.frame(table(pwords))
dnwords=data.frame(table(nwords))

#using dplyr plot them as unique on the bar charts with counts

dpwords=tbl_df(dpwords)
dnwords=tbl_df(dnwords)

#find top positive and negative words

dpwords=dpwords%>%
  mutate(pwords=as.character(pwords))%>%
  filter(Freq>10)

dnwords=dnwords%>%
  mutate(nwords=as.character(nwords))%>%
  filter(Freq>10)

graph1=ggplot(dpwords,aes(pwords,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(pwords,Freq,label=Freq),size=4)+
  labs(x="Major Positive Words", y="Frequency of Occurance",title=paste("Major Positive Words and Occurance in \n '",findfd,"' twitter feeds, n =",number))+
  geom_text(aes(1,200,label=paste("Total Positive Words :",pcount,"\n Sentiment ratio",result(ncount,pcount))),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))

graph2=ggplot(dnwords,aes(nwords,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(nwords,Freq,label=Freq),size=4)+
  labs(x="Major Negative Words", y="Frequency of Occurance",title=paste("Major Negative Words and Occurance in \n '",findfd,"' twitter feeds, n =",number))+
  geom_text(aes(1,25,label=paste("Total Negative Words :",ncount,"\n Sentiment ratio",result(ncount,pcount))),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))

# compare to other candidates
#********************************************************************************
findfd2= "Trump"
number= 5000
#********************************************************************************
start.time=Sys.time()
tweet=searchTwitter(findfd2,number)
end.time=Sys.time()
time.taken=end.time-start.time
time.taken

#use laply to get text from all texts
tweetT2=lapply(tweet,function(t)t$getText())


# Perform Sentiment analysis


# we define a function that will handle the sentence cleaning

# define an error catching function

tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

clean=function(t){
  t=gsub('[[:punct:]]','',t)# remove punctuations
  t=gsub('[[:cntrl:]]','',t)# remove 
  t=gsub('\\d+','',t)# remove punctuations
  t=gsub('[[:digit:]]','',t)# remove digits
  t=gsub('@\\w+','',t)# remove people
  t=gsub('http\\w+','',t)# remove http links
  t=gsub("^\\s+|\\s+$", "", t)# remove extra space
  t=sapply(t,function(x) tryTolower(x))
  t=str_split(t," ")
  t=unlist(t)
  return(t)
}



#lapply the clean function to tweet

tweetclean2=lapply(tweetT2,function(x) clean(x))

tail(tweetclean2,5)

#remove tweetT
rm(tweetT2)

#get the positive and negative score and words
#Create a function to calculate the positive score for all tweets

returnpscore=function(tweet) {
  pos.match=match(tweet,positive)
  pos.match=!is.na(pos.match)
  pos.score=sum(pos.match)
  return(pos.score)
}



# find the positive score for tweet clean file
positive.score2=lapply(tweetclean2,function(x) returnpscore(x))

#define a loop to add all items in a list
pcount2=0

for (i in 1:length(positive.score2)) {
  pcount2=pcount2+positive.score2[[i]]
}


# find the positive match words- define a function


poswords2=function(tweets){
  pmatch=match(tweets,positive)
  posw=positive[pmatch]
  posw=posw[!is.na(posw)]
  return(posw)
}


# apply to the tweet set 
words2=NULL
pdatamart2=data.frame(words2)

for (t in tweetclean2) {
  pdatamart2=c(poswords2(t),pdatamart2)
}


# Calculate the count of negative words


negscore=function(tw) {
  neg.match=match(tw,negative)
  neg.match=!is.na(neg.match)
  neg.score=sum(neg.match)
  return(neg.score)
}

#apply the function to the tweet clean list
negative.score2=lapply(tweetclean2,function(x) negscore(x))

#define a loop to add all items in a list
ncount2=0

for (i in 1:length(negative.score2)) {
  ncount2=ncount2+negative.score2[[i]]
}


# find the negative match words- define a function

negwords=function(tweets){
  nmatch=match(tweets,negative)
  negw=negative[nmatch]
  negw=negw[!is.na(negw)]
  return(negw)
}


# apply to the tweet set 
nwords2=NULL
ndatamart2=data.frame(nwords2)

for (t in tweetclean2) {
  ndatamart2=c(negwords(t),ndatamart2)
}



# Decision for sentiment analysis
result2=function(n,p) {
  if (n>p){
    paste("The sentiment for",findfd2,"is negative in the ratio of ", round(n/p,1)," : 1" )
  } else {
    paste("The sentiment for",findfd2,"is positive in the ratio of ", round(p/n,1)," :1")
  }
  
}

result2(ncount2,pcount2)

pwords2=unlist(pdatamart2)
nwords2=unlist(ndatamart2)

#remove all unused variables

rm(positive.score2,words2,negative.score2)

# first convert the positive words and negative words into dataframe

dpwords2=data.frame(table(pwords2))
dnwords2=data.frame(table(nwords2))

#using dplyr plot them as unique on the bar charts with counts

dpwords2=tbl_df(dpwords2)
dnwords2=tbl_df(dnwords2)

#find top positive and negative words

dpwords2=dpwords2%>%
  mutate(pwords2=as.character(pwords2))%>%
  filter(Freq>10)

dnwords2=dnwords2%>%
  mutate(nwords2=as.character(nwords2))%>%
  filter(Freq>10)

graph3=ggplot(dpwords2,aes(pwords2,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(pwords2,Freq,label=Freq),size=4)+
  labs(x="Major Positive Words", y="Frequency of Occurance",title=paste("Major Positive Words and Occurance in \n '",findfd2,"' twitter feeds, n =",number))+
  geom_text(aes(1,100,label=paste("Total Positive Words :",pcount2,"\n Sentiment ratio",result2(ncount2,pcount2))),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))

graph4=ggplot(dnwords2,aes(nwords2,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(nwords2,Freq,label=Freq),size=4)+
  labs(x="Major Negative Words", y="Frequency of Occurance",title=paste("Major Negative Words and Occurance in \n '",findfd2,"' twitter feeds, n =",number))+
  geom_text(aes(1,25,label=paste("Total Negative Words :",ncount2,"\n Sentiment ratio",result2(ncount2,pcount2))),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))

library(gridExtra)

grid.arrange(graph1,graph2,graph3,graph4 ,ncol=2,nrow=2)
#*******************************************************************************************


#create a corpus for the cleaned tweets

tweetscorpus=Corpus(VectorSource(tweetclean))

# removing the most common stop words

#what are stop words

#length(stopwords("english"))

#stopwords("english")[30:50]


tweetscorpus=tm_map(tweetscorpus,removeWords,stopwords("english"))

# creating a wordcloud of the tweets

wordcloud(tweetscorpus,scale=c(5,0.5),random.order = TRUE,rot.per = 0.20,use.r.layout = FALSE,colors = brewer.pal(6,"Dark2"),max.words = 300)


#create document matrix for the words 


dtm=DocumentTermMatrix(tweetscorpus)




class(dtm)
dim(dtm)


#transpose is called Term document matrix

#tdm=TermDocumentMatrix(docs)

# exploring the Document Term Matrix

# freq=colSums(as.matrix(dtm))
# length(freq)
# 
# ord=order(freq)
# 
# #least frequent items
# freq[head(ord)]
# 
# #most frequent terms
# 
# freq[tail(ord)]
# 
# head(table(freq),20)
# 
# #convert to matrix before writing to csv
# 
# mat=as.matrix(dtm)
# dim(mat)
# 
# #write.csv(mat,"dtm.csv")
# rm(mat)
# #removing sparse terms


dtms=removeSparseTerms(dtm,.99)
dim(dtms)
freq=colSums(as.matrix(dtms))
freq
table(freq)

#find most frequent terms in the corpus

findFreqTerms(dtm,lowfreq=100)

#get some more frequent terms

findFreqTerms(dtm,lowfreq=50)

#find associations with a word, specifying the correlation limit



findAssocs(dtm, "security",corlimit=0.6)

#draw correlation plots

#plot(dtm,terms=findFrequentTerms(dtm,lowfreq=1000),corThreshold=0.4)

#plotting word frequencies

freq=sort(colSums(as.matrix(dtm)),decreasing=TRUE)
head(freq,20)
str(freq)

#convert the matrix to a dataframe

wf=data.frame(word=names(freq),freq=freq)

#plot using ggplot

wfh=wf%>%
  filter(freq>=75,!word==tolower(findfd))

ggplot(wfh,aes(word,freq))+geom_bar(stat="identity",fill='lightblue')+theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_text(aes(word,freq,label=freq),size=4)+labs(x="High Frequency Words ",y="Number of Occurances", title=paste("High Frequency Words and Occurance in \n '",findfd,"' twitter feeds, n =",number))+
  geom_text(aes(1,max(freq)-100,label=paste("# Positive Words:",pcount,"\n","# Negative Words:",ncount,"\n",result(ncount,pcount))),size=5, hjust=0)

#making a word cloud

#wordcloud(names(freq),freq,min.freq=40)

#to reduce the clutter

#wordcloud(names(freq),freq,min.freq=40,max.words=100)

#adding some color

#wordcloud(names(freq),freq,min.freq=20,colors=brewer.pal(6,"Dark2"))

#remove all variables not in use

rm(dnwords,dpwords,wfh,ndatamart,negative,nwords,pcount,pdatamart,positive,pwords,start.time,t,negscore,negwords,poswords,returnpscore)


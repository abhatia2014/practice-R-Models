setwd('~/Datasets')
install.packages("tm")
install.packages("wordcloud")
library(tm)
library(wordcloud)
library(SnowballC)

#put the document into a corpus

lords=Corpus(DirSource("WordCloud/"))

#to see what's in corpus

inspect(lords)

# let's clean up the text file

lords=tm_map(lords,stripWhitespace)# strip white spaces

lords=tm_map(lords,PlainTextDocument)# convert to lower case

lords=tm_map(lords,removeWords,stopwords("english")) # remove common english words called stop words

#lords=tm_map(lords,stemDocument)# remove stem words which are derevatitve of normal words

wordcloud(lords,scale=c(5,0.5),random.order = FALSE,rot.per = 0.50,use.r.layout = TRUE,colors = brewer.pal(8,"Dark2"))

# can remove specific words from it

lords= tm_map(lords,removeWords,"noble")
lords= tm_map(lords,removeWords,"lord")
# or make a list of words and remove in one go
listw=c('noble','lord','knight')
lords= tm_map(lords,removeWords,listw)
wordcloud(lords,scale=c(5,0.5),random.order = TRUE,rot.per = 0.50,use.r.layout = TRUE,colors = brewer.pal(8,"Dark2"))
lords
head(lords)
str(lords)

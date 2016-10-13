?runif
runif(100,3,65)
?hist
hist(runif(100,3,65))
hist(rnorm(100,3,1.5)*rnorm(100,10,3))
install.packages("twitteR")
library(twitteR)
#get the 1500 most recent tweets using the words cyber attacks
cyber.tweets=searchTwitter('@CyberAttack',n=1500)
install.packages(c("twitteR","ROAuth","plyr","sringr"),dependencies=T)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)

#Authenticate with twitter API
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

# Accessing the twitter API
requestURL="https://api.twitter.com/oauth/request_token"
accessURL="https://api.twitter.com/oauth/access_token"
authURL="https://api.twitter.com/oauth/authorize"
consumerKey="pmdVLMK7TiXtOZRSgNJeOHkxl"
consumerSecret="VkCXokPgp4sJ9Dxpgs8xLq2jwA26Ns2aw0odgMYT995apNoOYy"
cred=OAuthFactory$new(consumerKey=consumerKey,
                      consumerSecret=consumerSecret,
                      requestURL=requestURL,
                      accessURL=accessURL,
                      authURL=authURL)
cred$handshake(cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))
accessToken=4707597
#save the credentials and register
save(cred,file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
registerTwitterOAuth(cred)
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
#lets get tweets on #cyberattack
#setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token=accessToken, access_secret=accessSecret)
cyber.list=searchTwitter('#cyberattack',n=1000)

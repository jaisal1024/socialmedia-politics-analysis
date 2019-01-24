################################################################
## In Class Assigment Day 5
## Jaisal Friedman
## Social Media and Political Participation
## Lab 5, January 15th 2019
################################################################

## SET PATHS & LOAD LIBRARIES
setwd("~/cs/SocMedClass/finalassignment")
library(streamR)
library(devtools)
library(NYU160J)
library(wordcloud)
load("oauth_token.Rdata")
data = read.csv("data.csv", header = TRUE,sep=",")

## GATHER SENATOR TWITTER DATA
getTimeline(filename="tweets_mike_lee.json", screen_name="SenMikeLee", 
            n=3200, oauth=my_oauth, trim_user="false", sleep = 0.5)
getTimeline(filename="tweets_mike_lee_campaign.json", screen_name="MikeLeeforUtah", 
            n=3200, oauth=my_oauth, trim_user="false", sleep = 0.5)
tweets = parseTweets("tweets_mike_lee.json")
tweets_campaign = parseTweets("tweets_mike_lee_campaign.json")
saveRDS(tweets,file="mikeleetweets2019a.rds")
saveRDS(tweets_campaign,file="mikeleetweetscampaign2019a.rds")
tweets<-readRDS("mikeleetweets2019a.rds")
tweets_campaign<-readRDS("mikeleetweetscampaign2019a.rds")

## VIEW THE TWEETS
View(tweets)
View(tweets_campaign)

## PLOT BY MONTH 
# PLOT 2018 MONTHS
months = countMonthsTweets(tweets$created_at)
months_campaign = countMonthsTweets(tweets_campaign$created_at)
plot(x=months$month, y=months$tweets, xlab="Date", ylab="Tweets per month", xlim=c(as.Date("2018-01-01"),as.Date("2018-12-31")), main="Tweet Volume for Mike Lee in 2018")
lines(x=months$month, y=months$tweets)
plot(x=months_campaign$month, y=months_campaign$tweets, xlab="Date", ylab="Tweets per month", xlim=c(as.Date("2018-01-01"),as.Date("2018-12-31")), main="Tweet Volume for Mike Lee Campaign Account in 2018")
lines(x=months_campaign$month, y=months_campaign$tweets)
# PLOT ALL-TIME MONTHS
plot(x=months$month, y=months$tweets, cex=0.6, xlab="Date", ylab="Tweets per month", main="Tweet Volume for Mike Lee by Month")
lines(x=months$month, y=months$tweets)
plot(x=months_campaign$month, y=months_campaign$tweets, cex=0.6, xlab="Date", ylab="Tweets per month", main="Tweet Volume for Mike Lee Campaign Account by Month")
lines(x=months_campaign$month, y=months_campaign$tweets)

## UNDERSTAND TWEETING PATTERNS 
# ADD IS A RETWEET & CONTENT BREAKDOWN
tweets$isretweet<-grepl("RT", substr(tweets$text,1,3), ignore.case=FALSE)
tweets_campaign$isretweet<-grepl("RT", substr(tweets_campaign$text,1,3), ignore.case=FALSE)
tweets$hasurl <- !(is.na(tweets$url))
tweets_campaign$hasurl <- !(is.na(tweets_campaign$url))
tweets$hasbody <- grepl(" ", tweets$text)
tweets_campaign$hasbody <- grepl(" ", tweets_campaign$text)
tweets$id_str <- paste("https://twitter.com/SenMikeLee/status/",tweets$id_str,sep="") 
tweets_campaign$id_str <- paste("https://twitter.com/MikeLeeforUtah/status/",tweets_campaign$id_str,sep="")
# FILTER NO RETWEETS
tweets_noRT <- tweets[which(!tweets$isretweet),]
tweets_RT <- tweets[which(tweets$isretweet),]
tweets_campaign_noRT <- tweets_campaign[which(!tweets_campaign$isretweet),]
tweets_campaign_RT <- tweets_campaign[which(tweets_campaign$isretweet),]
# RETWEET & FAVORITE ANALYSIS 
avg_retweets <- mean(tweets_noRT$retweet_count) #112.0
avg_retweets_campaign <- mean(tweets_campaign_noRT$retweet_count) #19.6
avg_likes <- mean(tweets_noRT$favorite_count) # 216.9
avg_likes_campaign <- mean(tweets_campaign_noRT$favorite_count) #34.8

# GET MOST COMMON HASHTAGH
ht = getCommonHashtags(tweets$text, n=15)
wordcloud(words=names(ht), freq=ht, max.words=250, 
          random.order=F, colors="black", scale=c(5,.5), rot.per=0)
ht_campaign = getCommonHashtags(tweets_campaign$text, n=15)
wordcloud(words=names(ht_campaign), freq=ht_campaign, max.words=250, 
          random.order=F, colors="black", scale=c(5,.5), rot.per=0)

# TWEET BREAKDOWN
breakdown_nort <- c(length(which(tweets_noRT$photo)),length(which(tweets_noRT$hasurl)), length(which(tweets_noRT$hasbody)))
breakdown_rt <- c(length(which(tweets_RT$photo)),length(which(tweets_RT$hasurl)), length(which(tweets_RT$hasbody)))
breakdown_nort <- breakdown_nort/length(tweets_noRT$text)*100
breakdown_rt <- breakdown_rt/length(tweets_RT$text)*100
View(breakdown_nort)
View(breakdown_rt)
mydata <- data.frame("RT"=c("Has Photo"=4.98, "Contains URL"=37.11, "Has Text"=100), "NO RT" =c(5.95,79.98,99.85))
barplot(t(as.matrix(mydata)),  main="Tweets Breakdown", beside=TRUE,legend = c("Retweets", "Not Retweets") ,col=c("darkblue", "red"), ylab = "% of Retweet/Not Retweet", xlab = "Type of Tweet")

## FIND 5 MOST-LIKED & RETWEETED
most_liked <- head(tweets_noRT[order(-tweets_noRT$favorite_count),], 5)
most_liked_campaign <- head(tweets_campaign_noRT[order(-tweets_campaign_noRT$favorite_count),], 5)
View(most_liked)
View(most_liked_campaign)
most_retweeted <- head(tweets_noRT[order(-tweets_noRT$retweet_count),], 5)
most_retweeted_campaign <- head(tweets_campaign_noRT[order(-tweets_campaign_noRT$retweet_count),], 5)
View(most_retweeted)
View(most_retweeted_campaign)


## REGRESSION
tweets_noRT$hash<-grepl("#", tweets_noRT$text, ignore.case = TRUE)
tweets_noRT$shoutout<-grepl("@", tweets_noRT$text, ignore.case = TRUE)
tweets_noRT$trump<-grepl("trump", tweets_noRT$text, ignore.case = TRUE) 
tweets_noRT$dem <-grepl("dem", tweets_noRT$text, ignore.case = TRUE) | grepl("democrats", tweets_noRT$text, ignore.case = TRUE) 
tweets_noRT$border<-grepl("border", tweets_noRT$text, ignore.case = TRUE) | grepl("secure", tweets_noRT$text, ignore.case = TRUE) | grepl("wall", tweets_noRT$text, ignore.case = TRUE)
tweets_noRT$shutdown<-grepl("shutdown", tweets_noRT$text, ignore.case = TRUE) 
tweets_noRT$utah<-grepl("utah", tweets_noRT$text, ignore.case = TRUE) 
tweets_noRT$pres<-grepl("president", tweets_noRT$text, ignore.case = TRUE) | grepl("pres", tweets_noRT$text, ignore.case = TRUE)| grepl("potus", tweets_noRT$text, ignore.case = TRUE)
tweets_noRT$bill<-grepl("bill", tweets_noRT$text, ignore.case = TRUE) | grepl("government", tweets_noRT$text, ignore.case = TRUE) | grepl("senate", tweets_noRT$text, ignore.case = TRUE)
tweets_noRT$firststepact <- grepl("FirstStepAct", tweets_noRT$text, ignore.case = TRUE) | grepl("First Step Act", tweets_noRT$text, ignore.case = TRUE) | grepl("Criminal Justice", tweets_noRT$text, ignore.case = TRUE)
tweets_noRT$townhall<-grepl("teletownhall", tweets_noRT$text, ignore.case = TRUE) | grepl("townhall", tweets_noRT$text, ignore.case = TRUE)
tweets_noRT$kavanaugh<-grepl("kavanaugh", tweets_noRT$text, ignore.case = TRUE)

# RETWEET REG
leeregrt<-lm(retweet_count~hash+shoutout+trump+kavanaugh+dem+border+shutdown+utah+bill+firststepact+pres+townhall+hasbody+hasurl+photo, data=tweets_noRT)
summary(leeregrt)
# LIKE REG
leereglike<-lm(favorite_count~hash+shoutout+trump+kavanaugh+dem+border+shutdown+utah+bill+firststepact+pres+townhall+hasbody+hasurl+photo, data=tweets_noRT)
summary(leereglike)

## GEN A WORD CLOUD 
wordFreq = word.frequencies(tweets_noRT$text, stopwords=c("lee", "mike","@SenMikeLee", "@senmikelee"))
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
          random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)
wordFreq_campaign = word.frequencies(tweets_campaign_noRT$text, stopwords=c("lee", "mike","@SenMikeLee", "@senmikelee"))
wordcloud(words=names(wordFreq_campaign), freq=wordFreq_campaign, max.words=50, 
          random.order=F, colors="black", scale=c(5,.5), rot.per=0)

## ELECTION ANALYSIS 
# MIDTERMS
format.str <- "%a %b %d %H:%M:%S %z %Y"
tweets$time<-as.POSIXct(strptime(tweets[,"created_at"], format.str, tz = "GMT"), tz = "GMT")
midterms_tweets = tweets[tweets$time>as.POSIXct('2018-09-01')&tweets$time<as.POSIXct('2018-11-10'),]
View(midterms_tweets)
tweets_campaign$time<-as.POSIXct(strptime(tweets_campaign[,"created_at"], format.str, tz = "GMT"), tz = "GMT")
midterms_tweets_campaign = tweets_campaign[tweets_campaign$time>as.POSIXct('2018-09-01')&tweets_campaign$time<as.POSIXct('2018-11-10'),]
View(midterms_tweets_campaign)

# 2016 ELECTIONS
election_tweets = tweets[tweets$time>as.POSIXct('2016-09-01')&tweets$time<as.POSIXct('2016-11-10'),]
View(election_tweets)
election_tweets_campaign = tweets_campaign[tweets_campaign$time>as.POSIXct('2016-09-01')&tweets_campaign$time<as.POSIXct('2016-11-10'),]
View(election_tweets_campaign)
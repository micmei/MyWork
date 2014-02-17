require(twitteR)
require(streamR)
require(ROAuth)
require(RCurl)
require(rjson)
require(qdap)
require(data.table)
require(stringr)
require(scales)
load("twitter authentication.Rdata")

filterStream(file = "20140216_nbaallstar.json", locations = c(42.5,-88.5,43.5,-87.5), 
             track=c("NBA","AllStar","nba","allstar"),timeout = 14400, oauth = cred)
dataPath <- "20140216_nbaallstar.json"
temp <- data.table(parseTweets(dataPath, simplify = FALSE, verbose = TRUE))
tweets   <- temp[,c(2:8,10:42) := NULL]

## Modify time into R's time format
tweets[,created_at := gsub("0000","",created_at)]
tweets[,created_at := gsub("\\+","",created_at)]
tweets[,time := as.POSIXct(strptime(created_at, format="%a %b %d %T %Y"), format="%Y %T")] 
tweets[,created_at := NULL]

## remove all digits and punctuations
tweets[,text := gsub("[^[:alnum:][:space:]']", "", text)]

## remove all http addresses
tweets[,text := gsub("http\\w+", "", text)]

## remove all control characters
tweets[,text := gsub("[[:cntrl:]]", "",text)]

## remove all new line chars and replace them with single white space
tweets[,text := gsub("\n", " ",text)]

## change two or more consecutive white chars into one
tweets[,text := gsub("\t{2,}"," ",text)]

## lowercase all letters
tweets[,text := tolower(text)]

## calculate how much time has elapsed from the start
tweets[,timestamp_second := as.numeric(time-time[[1]])]
tweets[,timestamp_minute := floor(timestamp_second/60)]
tweets[,timestamp_5minute := floor(timestamp_minute/5)]
tweets[,timestamp_10minute := floor(timestamp_minute/10)]
tweets[,timestamp_20minute := floor(timestamp_minute/20)]
## 
pol <- polarity(tweets$text)$all
tweets[, words := pol$wc]
tweets[, pol := pol$polarity]

lw1 <- with(tweets,loess(pol ~ timestamp_second))
lw_1m <- with(tweets,loess(pol ~ timestamp_minute))
lw_5m <- with(tweets,loess(pol ~ timestamp_5minute))
lw_5m_span1 <- with(tweets,loess(pol ~ timestamp_5minute, span=0.1))
lw_5m_span25 <- with(tweets,loess(pol ~ timestamp_5minute, span=0.25))
lw_10m <- with(tweets,loess(pol ~ timestamp_10minute))
lw_20m <- with(tweets,loess(pol ~ timestamp_20minute))

with(tweets,plot(timestamp_5minute,pol,ylim=c(-0.05,0.05),main="Sentimental Analysis of NBA AllStar 2014",
                 ylab="Sentiment (tweet-level polarity)",col="lightgrey"))
mtext("Shooting Stars, Skills Challenge, Three-Point Contest, Slam Dunk")
with(tweets,lines(timestamp_5minute,lw_5m$fitted,col="royalblue",lwd=2))
with(tweets,lines(timestamp_5minute,lw_5m_span1$fitted,col="gold",lwd=2))
with(tweets,lines(timestamp_5minute,lw_5m_span25$fitted,col="firebrick1",lwd=2))
abline(0,0,col="black",lwd=1)
legend(35,0.045, c("span=0.75","span=0.1","span=0.25"), lty=c(1,1,1), 
       lwd=c(2.5,2.5,2.5),col=c("royalblue","gold","firebrick1")) 


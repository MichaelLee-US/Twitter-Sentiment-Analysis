library(dplyr)
library(purrr)
library(twitteR)
library(tidyr)
library(tidyverse)
library(base)

#O'Auth Authentication
setup_twitter_oauth(consumer_key = "",
                    consumer_secret = "" ,
                    access_token = "",
                    access_secret = ""
                    )

retrieve.tweet <- function(account.name){
  tweets <- userTimeline(account.name, n = 3200, includeRts = TRUE, excludeReplies = TRUE)
  tweets_df <- tbl_df(map_df(tweets,as.data.frame))
}

analyze.tweet <- function(tweet.data){
  #:: DATA INITIALIZATION & CLEANING ::
  cleaned.tweets <- tweet.data %>% 
    select(id, text, created)
  
  #Transform tweets into a list of words
  library(stringr)
  cleaned.tweets$words <- str_split(cleaned.tweets$text,"-|\\s")
  cleaned.tweets$nid <- lapply(cleaned.tweets$words,length)
  cleaned.tweets$mid <- mapply(rep.int,cleaned.tweets$id, cleaned.tweets$nid)
  cleaned.tweets$date <- mapply(rep, cleaned.tweets$created, cleaned.tweets$nid)
  
  #Producing a dataframe of every words from tweets tagged wtih tweet ID and created date
  texts.id <- map_df(cleaned.tweets$mid,as.data.frame)
  texts.words <- map_df(cleaned.tweets$words,as.data.frame)
  texts.date <- map_df(cleaned.tweets$date,as.data.frame)
  new.text <- cbind(texts.id,texts.date,texts.words)
  colnames(new.text) <- c("id","date","texts")
  
  #Filtering out the retweet & links
  filtered.texts <- new.text %>%
    filter(!texts %in% c("RT"), str_detect(texts,"https://t.co/[A-Za-z\\d]+|&amp;", negate = TRUE) )
  
  #Identifying the mentioned Twitter accounts
  mentioned.acct <- new.text %>%
    filter(str_detect(texts,"@[A-Za-z]\\w+")) %>% 
    (function(x) as.data.frame(lapply(x,function(y) gsub("[^[:alnum:][:blank:]@_$]","",y))))
  
  #A Top 10 list of most mentioned account names
  accounts <- as.data.frame(table(mentioned.acct$texts)) %>%
    (function (y) y[order(y[,2], decreasing = TRUE),]) %>%
    (function (x) x[1:10,])
    
  #Word only Analysis: used (only alphanumerical chars)
  split2.words <-map_df(str_split(cleaned.tweets$text, "[[:punct:]]|[[:blank:]]"), as.data.frame)
  only.alphnum <- new.text %>%
    filter(!texts %in% c("RT"), str_detect(texts,"https://t.co/[A-Za-z\\d]+|&amp;", negate = TRUE) ) %>% 
    (function(x) as.data.frame(lapply(x,function(y) gsub("[[:punct:]]","",y))))
  
  library(tm)
  word.stats <- only.alphnum$texts %>%
    (function(z) as.data.frame(table(z))) %>%
    (function(x) x[order(x[,2], decreasing = TRUE),])
  
  word.stats.list <- as.data.frame(word.stats[,1])
  stop.words <- as.data.frame(stopwords(kind = "en"))
  word.freq <- word.stats[!(word.stats[,1]%in%stopwords(kind = "en")),]
  
  library(tidytext)
  library(textdata)
  words.sentiment <- word.freq %>%
    inner_join(get_sentiments("afinn"), by =c("z"="word")) %>%
    ((function(x) cbind((x[,1]), (x[,2]*x[,3]))))
  words.sentiment <- as.data.frame(words.sentiment)
  
  
  #:: MAIN ANALYSIS 1 :: Overall Negative VS. Positive Sentiment by words only
  #Convert (word vs freq) data.frame columns into proper type
  library(ggplot2)
  library(taRifx)
  small.sent <- remove.factors(words.sentiment)
  small.sent$V2 = as.numeric(small.sent$V2)
  small.sent$abs <- abs(small.sent$V2)
  new.sent <- small.sent[order(small.sent$abs, decreasing = TRUE),]
  new.sent$V1 <- factor(new.sent$V1, levels = rev(new.sent$V1))
  sent.graph <- new.sent[1:30,]
  sent.graph$color <- ifelse(sent.graph$V2 > 0, "red", "green")
  #Produce Sentiment vs Words Chart
  plot1 <- ggplot(sent.graph, aes(V1,V2)) + 
    geom_bar(stat = "identity", aes(fill = color)) + 
    coord_flip() + 
    ylab("Sentiment Multiplier (Frequency x Multiplier)") +
    xlab("Words") +
    labs(title = "Sentiment Analysis of Frequently Used Words", fill = "Sentiment", caption = "Words (y-axis) are sorted from the most frequent from top to bottom") +
    scale_fill_discrete(labels = c("Negative","Positive"))
  
  
  #:: MAIN ANALYSIS 2 :: Normal Distribution by negativity level
  #Categorize words by sentiment values for Normal Curve Distribution Analysis
  word.freq.grouped <- word.freq %>%
    inner_join(get_sentiments("afinn"), by =c("z"="word")) %>%
    group_by(value)
  distribution <- word.freq.grouped %>% 
    summarise(freq.grouped = sum(Freq))
  
  plot2 <- dist.graph <- ggplot(distribution,aes(value,freq.grouped)) + geom_point() + geom_line(color = "black") +
    labs(title = "Distribution of Textual Sentiment", caption = "-5 is Most Negative, +5 is Most Positive" ) +
    geom_line(aes(0,), data = NULL, color = "red") +
    xlim(-5,5) +
    xlab("Sentiment Level") +
    ylab("Frequency")
  
  #Calculating skewness & kurtosis
  library(e1071)
  val.skewness <- skewness(distribution$freq.grouped, type = 3) #Shows how negative or positive tweets are in general
  val.kurtosis <- kurtosis(distribution$freq.grouped, type = 3) #Shows how extreme sentiments are in the tweets
  
  
  #:: MAIN ANALYSIS 3 :: Analyzing Sentiment over time (Does negativity differ greatly by publishing stations?)
  #Excluding the retweeted Twitter account name (which itself is irrelevant to tone)
  text.time <- filtered.texts %>% 
    filter(!str_detect(texts,"@[A-Za-z]\\w+")) %>%
    (function(y) as.data.frame(lapply(y, (function(x) gsub("[[:punct:]]", "",x))))) %>%
    remove.factors()
  
  text.time$new.date <- text.time$date %>% 
    substr(1,8)
  #The block below is repeated to generate plot of different Twitter accounts
  #text.date = CNN, 2 = MSNBC, 3 = VOX, 4 = NYT, 5 = WIRED
  text.date <- text.time %>% inner_join(get_sentiments("afinn"), by =c("texts"="word")) %>%
    group_by(new.date) %>%
    summarize(mean = mean(value))
  #return(dist.graph)
  
  list.analysis <- list(words.sentiment,plot1,distribution,plot2,val.skewness,val.kurtosis,text.date)
  return(list.analysis)
  }

tweet.cnn <- retrieve.tweet("CNN")
tweet.msnbc <- retrieve.tweet("MSNBC")
tweet.vox <- retrieve.tweet("voxdotcom")
tweet.nyt <- retrieve.tweet("nytimes")
tweet.wired <- retrieve.tweet("WIRED")

data.cnn <- analyze.tweet(tweet.cnn)
data.msnbc <- analyze.tweet(tweet.msnbc)
data.vox <- analyze.tweet(tweet.vox)
data.nyt <- analyze.tweet(tweet.nyt)
data.wired <- analyze.tweet(tweet.wired)

#TEMPORARY BLOCK
num.text.date.cnn <- as.numeric(text.date.cnn$new.date)
num.text.dat.msnbc <- as.numeric(text.date.msnbc$new.date)
num.text.date.vox <- as.numeric(text.date.vox$new.date)
num.text.dat.nyt <- as.numeric(text.date.nyt$new.date)
num.text.date.wired <- as.numeric(text.date.wired$new.date)


ggplot() +
geom_line(data = data.cnn[[7]], aes(new.date, mean, group = 1),color = "red") +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
geom_line(data = data.msnbc[[7]], aes(new.date, mean, group =1), color = "green") +
geom_line(data = data.vox[[7]], aes(new.date, mean, group =1), color = "purple") 
geom_line(data = data.nyt[[7]], aes(new.date, mean, group =1), color = "blue") 
geom_line(data = data.wired[[7]], aes(new.date, mean, group =1), color = "black") 
  xlim(20191230, 20200121)

##### R Task

## load rtweet package
library(rtweet)
library(dplyr)
library(quanteda)

## Pick your favorite celebrity who has a Twitter account. 

# Ryan Reynolds

## find the most recent tweet the celebrited liked

vancity_favorites <- get_favorites("jk_rowling", n = 1)

##Download their 500 most recent tweets. 
vancity_timeline <- get_timelines("VancityReynolds", n = 500)
#Calculate which one got the most ``likes"

# Remove retweets
vancity_tweets <- vancity_timeline[vancity_timeline$is_retweet==FALSE, ] 
# Remove replies
vancity_tweets_nr <- subset(vancity_tweets, is.na(vancity_tweets$reply_to_status_id))
# find most popular tweets
vancity_tweets_ranked <- vancity_tweets_nr %>% arrange(-favorite_count)

### Create a DFM from the text of these tweets
vancity_corpus <- corpus(vancity_tweets_ranked)
vancity_dfm <- dfm(vancity_corpus, stopwords(language="en"), verbose=TRUE)

### After removing stopwords, what word did the celebrity tweet most often?
vancity_wordcloud <- textplot_wordcloud(vancity_dfm, rotation=0, min_size=.75, max_size=3, max_words=50)

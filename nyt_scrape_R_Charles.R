
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(quanteda)


##code adapted from Heather Geiger

setwd("C:/Users/16108/Downloads/TAD_2021-master(1).zip/TAD_2021-master/R lessons")


NYTIMES_KEY <- ("0TnkamHarXlWApDJFdAiLhi0dqwDEL8v")

term <- "phillies"
begin_date <- "20200101"
end_date <- "20200401"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)

pages_2020 <- vector("list",length=5)

for(i in 0:4){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2020[[i+1]] <- nytSearch 
  Sys.sleep(10) #I was getting errors more often when I waited only 1 second between calls. 5 seconds seems to work better.
}
phillies_2020_articles <- rbind_pages(pages_2020)



term <- "mets"
begin_date <- "20210101"
end_date <- "20210401"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)

pages_2021 <- vector("list",length=5)

for(i in 0:5){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
  pages_2021[[i+1]] <- nytSearch
  Sys.sleep(10)
}
mets_2021_articles <- rbind_pages(pages_2021)





#####in-class practice: 


### save the results of two different queries from the date range jan 1 2021 - APril 1 2021


### calculate the proportion of the headlines from each search term assigned to a given section name

table(phillies_2020_articles$response.docs.section_name)

table(mets_2021_articles$response.docs.section_name)

# phillies = 1
50/50

# mets = 1.2
60/50

## create a combined dfm with the text of all of the lead paragraphs

philly_corpus <- corpus(phillies_2020_articles$response.docs.lead_paragraph)

mets_corpus <- corpus(mets_2021_articles$response.docs.lead_paragraph)

dfm_phils <- dfm(philly_corpus)
dfm_mets <- dfm(mets_corpus)

rbind(dfm_phils,dfm_mets)

## calculate the average Flesch Reading Ease score (hint: use code form descriptive_2.R) for the lead paragraphs from each search term. Which is higher?

phils_readability <- textstat_readability(phillies_2020_articles$response.docs.lead_paragraph)
mean(phils_readability$Flesch)
# phils = 49.09906
mets_readability <-textstat_readability(mets_2021_articles$response.docs.lead_paragraph)
mean(mets_readability$Flesch)
# mets = 48.08171

####  00. Setup  ####

library(tidyverse) # Data Science
library(dplyr) #data manipulation, summarize
library(data.table) #join & modify tables

rm(list = ls())
setwd("/Users/ann/Documents/University/SS23/MA_Media-Literacy-by-Design/Repository")

#### 01.Load data ####

#Results
df <-read.csv("./data/temp/survey_part1_res.csv")
# Particpants that were identified as cheaters manually, e.g. not annotating at all, just clicking on the first word
cheat <-read.csv("./data/temp/anno_cheated.csv")
df <- df[!df$participantId %in% cheat[,1], ]

#Allsides Outlet Ratings
allsides_data <- readr::read_csv("https://raw.githubusercontent.com/favstats/AllSideR/master/data/allsides_data.csv")
MBFC_data <-read.csv("./data/raw/unmatched_outlets_MBFC.csv", sep=";")

#### 02. Transform outlet answers to outlet list####

# Remove special characters from newOutlet column
df$newsOutlets <- gsub("\\{", "", df$newsOutlets)
df$newsOutlets <- gsub("\\}", "", df$newsOutlets)
df$newsOutlets <- gsub("\"", "", df$newsOutlets)

newsOutlets <- df$newsOutlets %>% 
  strsplit(., ",") %>% 
  setNames(df$participantId) %>% 
  reshape2::melt(value.name = 'variable') %>% 
  #dummy variable
  mutate(hit = 1) %>% 
  #spread list, display hits per outlet
  spread(key = variable, value = hit) %>% 
  list(data.frame(L1 = df$participantId[!df$participantId %in% .[['L1']]]), .) %>% 
  rbindlist(., use.names = T, fill = T) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

# Avg. amount per participant
#1 to 16 outlet per person, 3.031 mean, 2.0 median, sd = 2.8

newsOutlets$num_outlets<- rowSums(newsOutlets[,-1] == 1)
summary(newsOutlets$num_outlets) 

# Export avg. outlets
news_num <- data.frame(participantId = newsOutlets$L1,
                       num_outlets = newsOutlets$num_outlets)

avg_news_p <- aggregate(participantId ~ num_outlets, data = news_num, FUN = function(x) length(unique(x)))

# Transform back, outlets per participant
newsOutlets <- gather(newsOutlets, outlet, hit, c(2:47)) %>% filter(hit>0) 
#Rename L1 to participant ID
names(newsOutlets)[names(newsOutlets) == 'L1'] <- 'participantId'
view(newsOutlets)



#### 03. Recode outlets ####

#Rename according to AllSides ID
newsOutlets$outlet <- gsub("^AP$", "Associated Press", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^AP News$", "Associated Press", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^bbc$", "BBC News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^BBC$", "BBC News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Breitbart$", "Breitbart News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^BuzzFeed$", "BuzzFeed News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^CNN$", "CNN (Web News)", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^DailyWire.com$", "The Daily Wire", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Fox News$", "Fox Online News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Google news$", "Google News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Huffington Post$", "HuffPost", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^NBC News$", "NBCNews.com", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^New York Times$", "New York Times - News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^NPR$", "NPR Online News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^the hill$", "The Hill", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^TheGateWayPundit.com$", "The Gateway Pundit", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^The Wall Street Journal$", "Wall Street Journal - News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^The Washington Post$", "Washington Post", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Time magazine$", "Time Magazine", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^USA Today$", "USA TODAY", newsOutlets$outlet)

# Free Speech TV, Mint Press corriere della sera, Counter Punch, Deutsche Welle, Independent news presenters like Tim Pool
# KTLA, NHK, Sky News, TheLibertyDaily.com, TYT 
# -> matched using MBFC manually, as they were not in the latest Allsides or MBFC CSV from 2020

#Rename according to MBFC ids
newsOutlets$outlet <- gsub("^ Free Speech TV$", "FStv", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^ Mint Press$", "Mint Press News", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^corriere della sera$", "Corriere Della Sera", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Counter Punch$", "CounterPunch", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Deutsche Welle$", "Deutsche Welle", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^Independent news presenters like Tim Pool$", "TimCast", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^TheLibertyDaily.com$", "The Liberty Daily", newsOutlets$outlet)
newsOutlets$outlet <- gsub("^TYT$", "The Young Turks", newsOutlets$outlet)

#### 04. News Outlet Frequency ####
view(table(newsOutlets$outlet))
#CNN (Web News)               83
#New York Times - News        97
#Washington Post              60
#MSNBC                        51
#ABC News                     41
#CBS News                     41
#Reuters                      36
#Fox Online News              33
#Wall Street Journal - News   33
#The Guardian                 27
#Vice                         26



#### 05. Match surveyed outlets with Allsides / MBFC ####
#coding: from left to right (1-5)

# Match survey outlets with Allsides Scores
newsOutlets_rating <- merge(newsOutlets,allsides_data[, c("news_source", "rating_num")], by.x = "outlet", by.y = "news_source", all.x = TRUE)

# Match remaining outlets with handpicked MBFC score
newsOutlets_rating  <- merge(newsOutlets_rating, MBFC_data[, c("news_source", "rating_num")], by.x = "outlet", by.y = "news_source", all.x = TRUE)

# Join the scores 
newsOutlets_rating$rating <-coalesce(newsOutlets_rating$rating_num.x, newsOutlets_rating$rating_num.y)
newsOutlets_rating <- select(newsOutlets_rating, outlet, rating, participantId)

# Remaining unmatched outlets
view(newsOutlets_rating )
unmatched <- unique(newsOutlets_rating$outlet[is.na(newsOutlets_rating$rating)])
# local news -> no match possible, excluded as participant listed other sources
# telemundo -> not enough data points available, excluded as participant listed other sources

#### 06. Calculate average outlet bias####
mean_outlet <- newsOutlets_rating %>% 
  group_by(participantId) %>% 
  summarise(mean_outlet = mean(rating, na.rm=T)) 


df <- merge(df,mean_outlet, by="participantId", all.x = TRUE)
df <- merge(df,news_num, by="participantId")

#### 07. Output####
write.csv(df,file="./data/temp/survey_part1_pol.csv", row.names=FALSE)

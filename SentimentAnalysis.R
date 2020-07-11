#load all neccesary packages
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('tidytext')) install.packages('tidytext'); library(tidytext)
if (!require('readxl')) install.packages('readxl'); library(readxl)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('tokenizers')) install.packages('tokenizers'); library(tokenizers)
if (!require('glue')) install.packages('glue'); library(glue)
if (!require('stringr')) install.packages('stringr'); library(stringr)
if (!require('wordcloud')) install.packages('wordcloud'); library(wordcloud)
if (!require('devtools')) install.packages('devtools')
if (!require('wordcloud2')) install.packages('wordcloud2'); library(wordcloud2)
if (!require('reshape2')) install.packages('reshape2'); library(reshape2)
if (!require('textdata')) install.packages('textdata'); library(textdata)


#before running this code, go to the menu bar: Session >> Set working directory >> Source file location

###################################################################################################
#####################################    SECTION 1  ###############################################
###################################################################################################
#This section calculate companies' actual apologies and Twitter apology score and write the results in a 
#new csv file, together with the initial input data. After running this section, check for a file 
#named 'SentimentApologyData.csv' in the working directory.

#import data
homeDir <- getwd() #set working directory to read file
dataDir <- paste(homeDir, "Data_Final.csv", sep = "/") #create link to file
data <- read.csv(dataDir, stringsAsFactors = FALSE) #read csv file


#create an ID for each incident, from 1 -> the last incident in our subset
data$ID<-seq(from=1,to=nrow(data))

#Subset data to include incidents with full text apoplogies only
apology<-data[which(data$Ready_for_R==1),]


#display afinn dictionary
#this is the dictionary we use for the majority of the analysis
get_sentiments("afinn")

######################### SENTIMENTS FOR OFFICIAL APOLOGY ####################################

#this sentiment analysis works by breaking down each words in tweets and calculate their score 
#after that we sum the scores up based on their ID we created above
#First, we tokenize apology (break down the words)
tidy_apology <- apology %>%
  group_by(ID) %>%
  ungroup() %>%
  unnest_tokens(word, Actual.Apology)

#get sentiment score for each word in apologies using AFINN dictionary, then sum them up based on apology ID
#because the dictionary has been updated, if show error object 'value' not found, change sum(value) on the third 
#line to sum(score)
afinn <- tidy_apology %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(ID) %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

#map the sentiment scores to the orginal table, based on apology ID
mastertable <- merge(apology, afinn, by = "ID", all.x = TRUE)


################# SENTIMENTS FOR COMPANY TWITTER APOLOGY/ ANNOUNCEMENT ######################

#repeat the process for tweets
tweets<-data[which(is.na(data$Apology.Address.Tweet)==FALSE &  data$Apology.Address.Tweet != ''),]
#Apologies.Tweets$ID<-seq(from=1,to=nrow(Apologies.Tweets))

#this sentiment analysis works by breaking down each words in tweets and calculate their score 
#after that we sum the scores up based on their ID we created above

#Tokenize tweets (break down the words)
tidy_tweets <- tweets %>%
  group_by(ID) %>%
  ungroup() %>%
  unnest_tokens(word, Apology.Address.Tweet)

#get sentiment scores using AFINN
#because the dictionary has been updated, if show error object 'value' not found, change sum(value) on the third 
#line to sum(score)
afinn_tweets <- tidy_tweets %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(ID) %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

#map the sentiment scores to the orginal table
mastertable <- merge(mastertable, afinn_tweets, by = "ID", all.x = TRUE)

########################### CLEAN UP TABLE AND WRITE THE RESULTS IN CSV FILE ######################################
selectCol <- c("ID", "Year", "Company", "Ticker", "Type.of.Crisis", "Type.of.Firms", "Description.of.Event",
               "Deaths", "Injuries.Sick", "Casualty", "Date.first.announced", "Date.of.apology",
               "Actual.Apology", "sentiment.x", "NYT..WSJ..Twitter..etc..Headline", "Relevant.Link", "Country.of.Firm",
               "Country.of.Incident", "Apology.Address.Tweet", "sentiment.y", "Total.RTs", "Total.Faves" )

table <- mastertable[, selectCol]
colnames(table) <-  c("ID", "Year", "Company", "Ticker", "Crisis_Type", "Firm_Type", "Event_Description",
                            "Deaths", "Injuries_Sick", "Casualty", "Date_First_Announce", "Date_Apology",
                            "Actual_Apology", "Apology_Sentiment", "Headline", "Link", "Country_Firm",
                            "Country_Incident", "Twitter_Apology", "Tweets_Sentiment", "Total_RTs", "Total_Faves" )
table$ID <- seq(1, nrow(table))
write.csv(table, "SentimentApologyData.csv") #file will appear in working directory






###################################################################################################
#####################################    SECTION 2  ###############################################
###################################################################################################
#This section creates multiple wordclouds with BING dictionary

### read and clean data
homeDir <- getwd() #set working directory to read file
dataDir <- paste(homeDir, "SentimentApologyData.csv", sep = "/") #create link to file
apology <- read.csv(dataDir, stringsAsFactors = FALSE) #read csv file

tidy_apology <- apology %>%
  group_by(ID) %>%
  ungroup() %>%
  unnest_tokens(word, Actual_Apology)

tidy_tweets <- apology %>%
  group_by(ID) %>%
  ungroup() %>%
  unnest_tokens(word, Twitter_Apology)


### Create wordcloud for all actual apologies in the dataset
#Positive-negative words wordcloud
#saved as pos_neg_apology
tidy_apology %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(scale=c(4,1),colors = c("darkred", "forestgreen"),
                   max.words = 100, tittle.size = 0.5)

#Wordcloud for negative words in all apologies
#break up all words and associate positive/negative sentiment to each
apology_word <- tidy_apology %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 


#clean up out put
apology_word <- as.data.frame(apology_word)
colnames(apology_word) <- c("word", "sentiment", "freq")
rownames(apology_word) <- apology_word$word

#select negative words only
neg_apology_word <- apology_word[which(apology_word$sentiment == "negative"),c(1,3)]
pos_apology_word <- apology_word[which(apology_word$sentiment == "positive"),c(1,3)]

#make word cloud of negative words
wordcloud2(neg_apology_word, col = "darkred") #saved as neg_word_apology in the folder
wordcloud2(pos_apology_word, col = "darkgreen") #saved as neg_word_apology in the folder


### Create wordcloud for Twitter apologies
#create stop words so http, https, a, the, and, etc. do not show up
custom_stop_words <- bind_rows(data_frame(word = c(as.character(1:100000),"http","https","t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)

#Positive-negative word cloud
#saved as pos_neg_twitter in folder
tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(scale=c(5,1),colors = c("darkred", "forestgreen"),
                   max.words = 100, tittle.size = 1)


#Wordcloud twitter icon
twitter_word <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 
neg_twitter_word <- twitter_word[which(twitter_word$sentiment == "negative"),c(1,3)]

twitter_word <- select(twitter_word, "word", "n")
twitter_word <- as.data.frame(twitter_word)
rownames(twitter_word) <- twitter_word$word
neg_twitter_word <- as.data.frame(neg_twitter_word)

figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(twitter_word, figPath = figPath, size = 1,color = "skyblue") #saved as twitter_apology_icon
wordcloud2(neg_twitter_word, figPath = figPath, size = 1, color = "skyblue") #saved as neg_twitter_apology_icon
#this can be funky sometimes, if plot does not appear on first try, keep clicking on the refresh icon on the 
#right of the viewer window, next to publish.



###################################################################################################
#####################################    SECTION 3  ###############################################
###################################################################################################
### This section analyzes sentiments of tweets made from company handles during the interval of incidents
### if change Handles to Replies --> tweets people reply to companies


#read tweets file
homeDir <- getwd() #set working directory to read file
#change file name based on incidents. For example, with Airbnb incident in 2015
#if want to analyze tweets people reply to companies, change "Handles/Airbnb15.xlsx" to ""Replies/repAirbnb.xlsx"
fileDir <- paste(homeDir, "Handles/Airbnb15.xlsx", sep = "/") 
file <- read_excel(fileDir) #import file here
head(file) #look at the top of the table

#prepare data for analysis
file$Groups<-seq(from=1,to=nrow(file)) #create id for 
file <- file[,c(7,1,2,3,4,5,6)]

#Tokenize tweets
tidy_tweets <- file %>%
  group_by(Groups) %>%
    ungroup() %>%
  unnest_tokens(word, Tweet)

#get sentiment scores
#because the dictionary has been updated, if show error object 'value' not found, change sum(value) on the third 
#line to sum(score)
afinn <- tidy_tweets %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(Groups) %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidy_tweets %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_tweets %>% 
                            inner_join(get_sentiments("nrc")) %>% 
                                  
                            mutate(method = "NRC"),
                            tidy_tweets %>%
                            inner_join(get_sentiments("loughran")) %>% 
                            mutate(method = "loughran"))
  

POS<-file %>% 
  merge(afinn, by = "Groups",all.x = TRUE)

write.csv(POS, "Airbnb15score.csv") #calculate sentiment analysis score
write.csv(bing_and_nrc, "Airbnbposneg.csv") #list out all the positive/ negative words in tweets



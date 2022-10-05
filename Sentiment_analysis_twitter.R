
install.packages('ggplot2')
install.packages('dplyr')
install.packages("sentimentr")
install.packages("tidyverse")
install.packages('httr')
install.packages('jsonlite')

require(httr)
require(jsonlite)
require(dplyr)
library(sentimentr)
library(tidyverse)

# plotting and pipes - tidyverse!
library(ggplot2)
# text mining library
library(tidytext)


# Name you assigned to your created app
appname <- "MMP_Sentiment"

## api key (example below is not a real key)
key <- "4ZYcwCBUfSa4mmonEU9VQMGLR"

## api secret (example below is not a real key)
secret <- "aLaY1KulVTQLjMKeQg1gQwXWTxjiPU0Nf5CLTHK2EEiCicZ2zG"

access_token <- "947525047492558849-KHuToWxEu89lafOIwSE9vkoqCvvBENS"
access_secret <- "ir3gWIBev5NYyNFsUJsLFWaw4EbAHgZlvIT5o2ZRu6YlT"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


######## api v2


bearer_token <- "AAAAAAAAAAAAAAAAAAAAAINUWAEAAAAAwZh3k55EOne0qixyGaTqpHDFRoA%3DPy5IrmAjifIC5wSTjUNFRsOSLDoDazFCVWIg4XJPpTYMl17Hwa"

headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params <- list(
  `query` = 'covid-19 vaccine',
  `max_results` = '100000',
  `tweet.fields` = 'text,geo,created_at,lang,conversation_id,public_metrics',
  `expansions`= 'author_id,geo.place_id',
  `user.fields` = 'username,location,description,created_at,user_metrics'
)


response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', 
                      httr::add_headers(.headers=headers), query = params)

obj <- httr::content(response, as = "text")
print(obj)

# flatten the JSON data 

tweets_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame
View(tweets_data)

## Sentiment Analysis

# Tweets data set extracted from twitter

Tweets <- head(vaccination_all_tweets,114103) # Sample size of 100 for testing 2021-06-17

Tweets # checking data

Tweets %>% 
  get_sentences(text) %>% view()  # Splitting text data into sentences (a process called sentence boundary disambiguation)

sentiment_by(Tweets %>% 
               get_sentences(text) ) %>% view() # To aggregate sentiment level by element (column cell or vector element)

Testdata <- (out <- with(
  Tweets, 
  sentiment_by(
    get_sentences(text), 
    list(user_name, as.character(as.Date(date)), text)
  )
)) %>% view()  # Setting the sentiment numbers

colnames(Testdata)[2] <- "date" # changing date format name
Testdata %>% view()

FinalData<- Testdata %>% mutate(polarity_level = ifelse(ave_sentiment < 0, "Negative",
                                                        ifelse(ave_sentiment > 0.1, "Positive","Neutral"))) %>% view() # Readable polarity level


FinalData %>% filter(polarity_level == "Positive") %>% View()  # Only positive results
FinalData %>% filter(polarity_level == "Negative") %>% View()  # Only negative results
FinalData %>% filter(polarity_level == "Neutral") %>% View()  # Only neutral results
count(FinalData %>% filter(polarity_level == "Neutral")) # Count polarity

FinalData %>% 
  ggplot() + geom_density(aes(ave_sentiment)) +labs(x = "Average Sentiment", 
                                                    y = "Density",
                                                    title = "Density Plot",
  ) # Density plot

FinalData %>% 
  ggplot() + geom_boxplot(aes(x = date, y = ave_sentiment)) +
  labs(x = "Date", 
       y = "Average Sentiment",
       title = "Box Plot",
  )# Box plot



# base plot
ggplot(data = FinalData) +
  geom_line(aes(x = date, y = ave_sentiment), 
            color = "#09557f",
            alpha = 0.6,
            size = 0.6) +
  labs(x = "Date", 
       y = "Average Sentiment",
       title = "Base Plot") +
  theme_minimal()


#Highlight 
FinalData$text %>% 
  get_sentences() %>% 
  sentiment_by() %>% #View()
  highlight()

# Bar plot

ggplot(data=FinalData, aes(x=date, y=sum(ave_sentiment), fill= polarity_level ))+
  labs(x = "Date", 
       y = "Average Sentiment",
       title = "Bar Plot",
  )+
  geom_bar(stat="identity")   # Bar plot




# Number of positive, negative and neutral polarity for specific date
Date_filtered_data<- FinalData %>%
  group_by(date) %>%
  summarise(
    Positive = sum(polarity_level== "Positive"),
    Negative = sum(polarity_level== "Negative"),
    Neutral = sum(polarity_level== "Neutral")
  )
Date_filtered_data %>% view()

# Basic line plot
ggplot(Date_filtered_data) +
  geom_line(aes(x = date, y = Positive,group = 1),color = 'darkred', size = 1)+
  geom_line(aes(x = date, y = Negative,group = 1),color = 'darkgreen', size = 1)+
  geom_line(aes(x = date, y = Neutral,group = 1),color = 'darkblue', size = 1) +
  labs(x = "Date", 
       y = "Total polarity count",
       title = "Line Plot",
  )+
  theme_minimal()



# Area plot
ggplot(Date_filtered_data, aes(x=date)) + 
  geom_area(aes(y = Positive,group = 1), fill = "red", 
            color = "red", alpha=0.5) + 
  geom_area(aes(y = Negative,group = 1), fill = "green",
            color = "green",  alpha=0.5) +
  geom_area(aes(y = Neutral,group = 1), fill = "blue",
            color = "blue",  alpha=0.5) +
  labs(x = "Date", 
       y = "Total polarity count",
       title = "Area Plot",
  ) +
  theme_minimal()

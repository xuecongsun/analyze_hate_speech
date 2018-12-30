# Libraries needed
library(rtweet)                # General-purpose Twitter API package
library(tidytext)              # General-purpose data wrangling
library(rvest)                 # Parsing of HTML/XML files  
library(stringr)               # String manipulation
library(rebus)                 # Verbose regular expressions
library(lubridate)             # Eases DateTime manipulation
library(dplyr)                 # grep/text cleaning
library(topicmodels)           # General Purpose Topic Modeling
library(stm)                   # Special Topic Modeling
library(ggplot2)               # Visualization Package
library(reshape2)              # Visualization of special
library(tm)                    # Corpus Manipulation
library(splitstackshape)       # Another data manipulation/cleaning package
library(SnowballC)             # Stemming and corpus manipulation
library(wordcloud)             # Simple creation of word clouds
library(textnets)              # Used for identifying relationships between words

###############################################################################
#                                                                             #
#               Required Twitter credentialing for API use                    #
#                                                                             #
###############################################################################

# Twitter Token API Information
app_name          <- "app_name"
consumer_key      <- "consumer_key"
consumer_secret   <- "consumer_secret"
access_token      <- "access_token"
access_secret     <- "access_secret"

#creating the token from our specific keys and secrets
create_token(
  app             <- app_name,
  consumer_key    <- consumer_key,
  consumer_secret <- consumer_secret,
  access_token    <- access_token,
  access_secret   <- access_secret,
  set_renv        <- TRUE
) -> twitter_token

###############################################################################
#                                                                             #
#               SCREEN SCRAPING                                               #
#                                                                             #
#               The Following in creating the needed information              #
#               used in the end analysis. This will utilize API               #
#               and screen scraping using the rvest and tidytext              #
#               libraries to build our data sets.                             #
#                                                                             #
###############################################################################

# In order to initially flag tweets for hate speech we will need to build criteria to 
# look at the words and context in order to compare against.

# The first, and arguably most straight forward method is to build a database of racial slurs
# Luckily a number of webages have this information for free.

# The Racial slur Database (RSDB) at http://www.rsdb.org/full
# is the most robust with 2655 slurs. Not all of these are explicit, but it is the most complete.
# We will use the "rvest" package to pull the table into a dataframe

SlurDB <- read_html('http://www.rsdb.org/full')

SlurDB %>%
  html_node("slur_2 td") %>%        # Pull information from the HTML node titled "slur_2 td"
  html_text()                       # convert the node into text

# After we pull the data we convert it to and HTML table for manipulation using rvest

SlurDB %>%
  html_nodes("table") %>%           # convert the node/text to a table
  .[[1]] %>%
  html_table() %>%                  # convert the table to a data frame
  select(Slur) -> Slurs             # only select the column Slur and save it as "Slurs" 

# Then we select the slur column and unnest the tokens 
# so that we can more to text preprocessing

# Finally we will write this to a CSV file so that we have it for later in case the site 
# changes drastically enough that our screen scraping is rendered useless

write.csv(Slurs, file = "RSDB.csv")

# Initially we were planning on using a dataset of random tweets read from the stream.
# This became an issue with identifying racist tweets due to the limited amount of tweets 
# which were being pulled every 15 seconds over the course of an hour

# In our attempt to utilize a dictionary based search, we realized that a number of words would
# not be useful as they are generally words used in normal conversation (I.E Apple and Banana)
# to adjust for this we then culled the dictionary to only pick out the most explicit slurs
# first (Ni****, Chi**, Sp**).

###############################################################################
#                                                                             #
#               EXPLICIT RACIST TWEETS CURATION                               #
#                                                                             #
#               We first determined the top 3 common slurs for each group,    #
#               then pulled tweets containing these slurs from 8 November     #
#               2018 through 18 November 2018 for 18000 tweets per group.     #
#               Lastly, we filtered these tweets using Google's Perspective   #
#               API and only kept tweets which had a toxicity rating above    # 
#               0.6.                                                          #
#                                                                             #
###############################################################################


###############################################################################
#                                                                             #
#               Determining the most common slurs against whites              #
#                                                                             #
###############################################################################

#read in selected slur words.  
slurwords         <- read.csv("RSDB white.csv")

#loop to concatenate and search posts
#concatenate first 20 keywords into a giant string
str1              <- '"'
space             <- " "
ORword            <- "OR"
totalstringloop   <- ''

for(word in slurwords$Slur[]){
  slurwordstring  <- paste(str1, word, str1, space, ORword, space,sep="")
  totalstringloop <- paste(totalstringloop, slurwordstring)
}
# remove the last OR to get final totalstring
totalstringloop   <- substring(totalstringloop, 1, nchar(totalstringloop)-3)
# check number of characters in the string, 
# maximum character length is 500 for search_tweet function
nchar(totalstringloop)

# search_tweet with stringed keyword
whiteracisttweet  <- search_tweets(q = totalstringloop,
                                   "lang:en",
                                   geocode=lookup_coords("usa"),
                                   n = 10000, 
                                   include_rts = FALSE,
                                   type = "recent", 
                                   retryonratelimit = TRUE)

# before this step, i already pulled down tweets containing racial slurs, 
# I call the tweets dataset trulyracisttweet
trulyracisttweet   <- whiteracisttweet

# tokenize and see top frequency words, sentiment and topic
tidy_trulyracisttweet <- trulyracisttweet%>%
  dplyr::select(created_at, text) %>%
  unnest_tokens("word", text)

# remove stop words
data("stop_words")
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  anti_join(stop_words)

# remove numbers
tidy_trulyracisttweet <- tidy_trulyracisttweet[-grep("\\b\\d+\\b", 
                                                     tidy_trulyracisttweet$word),]

# remove whitespaces
tidy_trulyracisttweet$word <- gsub("\\s+", "", tidy_trulyracisttweet$word)

# stemming to the root words
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  mutate_at("word", funs(wordStem((.), language="en")))

# change slurword list to lower letters for easier comparison
# this is important, because our slur database use capital letters, 
# and grep won't recognize unless you change it to lower
slurwords$Slur <- tolower(slurwords$Slur)

# count frequency of each slur word
# create empty dataframe which contains slur word 
# and its count in the 18000 tweets of 10 keywords
# make stringasfactors=false or there will be some problem
slurvocab <- data.frame(vocab = character(nrow(slurwords)), 
                        count=numeric(nrow(slurwords)), 
                        stringsAsFactors = FALSE)
i <- 1

for(each in slurwords$Slur){
  slurvocab$vocab[i] = each
  # grep returns the indices of rows containing the slur
  rownumber = grep(each,tidy_trulyracisttweet$word)
  # number of rows will be the frequencies of the slur
  slurvocab$count[i] = length(rownumber)
  i <- i+1
}

# Barplot
selectedslurvocab=slurvocab%>%
  arrange(desc(count)) %>%
  top_n(10)

ggplot(selectedslurvocab, 
       aes(reorder(vocab,-count), 
           count, 
           fill = factor(vocab))) + 
  geom_bar(stat = "identity")+
  labs(x = "slurvocab", y = "count",
       title = "Top 10 slurwords for Whites in 18000 Tweets",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet")

###############################################################################
#                                                                             #
#               Determining the most common slurs against blacks              #
#                                                                             #
###############################################################################

# read in selected slur words.  
slurwords         <- read.csv("RSDB black.csv")

# loop to concatenate and search posts
# concatenate first 20 keywords into a giant string
str1              <- '"'
space             <- " "
ORword            <- "OR"
totalstringloop   <- ''

for(word in slurwords$Slur[]){
  slurwordstring  <- paste(str1, word, str1, space, ORword, space, sep="")
  totalstringloop <- paste(totalstringloop, slurwordstring)
}
# remove the last OR to get final totalstring
totalstringloop   <- substring(totalstringloop, 1, nchar(totalstringloop)-3)
# check number of characters in the string, 
# maximum character length is 500 for search_tweet function
nchar(totalstringloop)

# search_tweet with stringed keyword
blackracisttweet  <- search_tweets(q=totalstringloop,
                                   "lang:en",
                                   geocode=lookup_coords("usa"),
                                   n = 10000, 
                                   include_rts = FALSE, 
                                   type = "recent", 
                                   retryonratelimit = TRUE)

# before this step, i already pulled down tweets containing racial slurs, 
# I call the tweets dataset trulyracisttweet
trulyracisttweet   <- blackracisttweet

# tokenize and see top frequency words, sentiment and topic
tidy_trulyracisttweet <- trulyracisttweet%>%
  dplyr::select(created_at, text) %>%
  unnest_tokens("word", text)

# remove stop words
data("stop_words")
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  anti_join(stop_words)

# remove numbers
tidy_trulyracisttweet <- tidy_trulyracisttweet[-grep("\\b\\d+\\b", 
                                                     tidy_trulyracisttweet$word),]

# remove whitespaces
tidy_trulyracisttweet$word <- gsub("\\s+", "", tidy_trulyracisttweet$word)

# stemming to the root words
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  mutate_at("word", funs(wordStem((.), language="en")))

# change slurword list to lower letters for easier comparison
# this is important, because our slur database use capital letters, 
# and grep won't recognize unless you change it to lower
slurwords$Slur <- tolower(slurwords$Slur)

# count frequency of each slur word
# create empty dataframe which contains slur word 
# and its count in the 18000 tweets of 10 keywords
# make stringasfactors=false or there will be some problem
slurvocab <- data.frame(vocab = character(nrow(slurwords)), 
                        count = numeric(nrow(slurwords)), 
                        stringsAsFactors = FALSE)
i <- 1

for(each in slurwords$Slur){
  slurvocab$vocab[i] = each
  # grep returns the indices of rows containing the slur
  rownumber = grep(each,tidy_trulyracisttweet$word)
  # number of rows will be the frequencies of the slur
  slurvocab$count[i] = length(rownumber)
  i <- i+1
}

# Barplot
selectedslurvocab=slurvocab%>%
  arrange(desc(count)) %>%
  top_n(10)

ggplot(selectedslurvocab, 
       aes(reorder(vocab,-count), 
           count, 
           fill = factor(vocab))) + 
  geom_bar(stat = "identity")+
  labs(x = "slurvocab", y = "count",
       title = "Top 10 slurwords for Blacks in 18000 Tweets",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet")

###############################################################################
#                                                                             #
#               Determining the most common slurs against Hispanic            #
#                                                                             #
###############################################################################

# read in selected slur words.
slurwords         <- read.csv("RSDB hisp.csv")

# loop to concatenate and search posts
# concatenate first 20 keywords into a giant string
str1              <- '"'
space             <- " "
ORword            <- "OR"
totalstringloop   <- ''

for(word in slurwords$Slur[]){
  slurwordstring  <- paste(str1, word, str1, space, ORword, space, sep="")
  totalstringloop <- paste(totalstringloop, slurwordstring)
}
#remove the last OR to get final totalstring
totalstringloop   <- substring(totalstringloop, 1, nchar(totalstringloop)-3)
# check number of characters in the string, 
# maximum character length is 500 for search_tweet function
nchar(totalstringloop)

# search_tweet with stringed keyword
hispracisttweet   <- search_tweets(q = totalstringloop,
                                   "lang:en",
                                   geocode = lookup_coords("usa"),
                                   n = 10000,
                                   include_rts = FALSE, 
                                   type = "recent", 
                                   retryonratelimit = TRUE)

# before this step, i already pulled down tweets containing racial slurs, I call the tweets dataset trulyracisttweet
trulyracisttweet  <- hispracisttweet

# tokenize and see top frequency words, sentiment and topic
tidy_trulyracisttweet <- trulyracisttweet%>%
  dplyr::select(created_at, text) %>%
  unnest_tokens("word", text)

# remove stop words
data("stop_words")
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  anti_join(stop_words)

# remove numbers
tidy_trulyracisttweet <- tidy_trulyracisttweet[-grep("\\b\\d+\\b", 
                                                     tidy_trulyracisttweet$word),]

# remove whitespaces
tidy_trulyracisttweet$word <- gsub("\\s+", "", tidy_trulyracisttweet$word)

# stemming to the root words
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  mutate_at("word", funs(wordStem((.), language ="en")))

# change slurword list to lower letters for easier comparison
# this is important, because our slur database use capital letters, 
# and grep won't recognize unless you change it to lower
slurwords$Slur <- tolower(slurwords$Slur)

# count frequency of each slur word
# create empty dataframe which contains slur word and its count in the 18000 tweets
# of 10 keywords
# make stringasfactors=false or there will be some problem
slurvocab <- data.frame(vocab = character(nrow(slurwords)), 
                        count = numeric(nrow(slurwords)), 
                        stringsAsFactors = FALSE)
i <- 1

for(each in slurwords$Slur){
  slurvocab$vocab[i] = each
  # grep returns the indices of rows containing the slur
  rownumber = grep(each,tidy_trulyracisttweet$word)
  # number of rows will be the frequencies of the slur
  slurvocab$count[i] = length(rownumber)
  i <- i+1
}

# Barplot
selectedslurvocab=slurvocab%>%
  arrange(desc(count)) %>%
  top_n(10)

ggplot(selectedslurvocab, 
       aes(reorder(vocab,-count), 
           count, 
           fill = factor(vocab))) + 
  geom_bar(stat = "identity")+
  labs(x = "slurvocab", y="count",
       title = "Top 10 slurwords for Hispanic in 18000 Tweets",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet")

###############################################################################
#                                                                             #
#               Determining the most common slurs against Asian               #
#                                                                             #
###############################################################################

#read in selected slur words. 
slurwords         <- read.csv("RSDB asian.csv")

#loop to concatenate and search posts
#concatenate first 20 keywords into a giant string
str1              <- '"'
space             <- " "
ORword            <- "OR"
totalstringloop   <- ''

for(word in slurwords$Slur[]){
  slurwordstring  <- paste(str1, word, str1, space, ORword, space,sep="")
  totalstringloop <- paste(totalstringloop, slurwordstring)
}
#remove the last OR to get final totalstring
totalstringloop   <- substring(totalstringloop, 1, nchar(totalstringloop)-3)
#check number of characters in the string, maximum character length is 500 for search_tweet function
nchar(totalstringloop)

#search_tweet with stringed keyword
asiasnracisttweet <- search_tweets(q=totalstringloop,
                                   "lang:en",
                                   geocode = lookup_coords("usa"),
                                   n = 10000, 
                                   include_rts=FALSE, 
                                   type = "recent", 
                                   retryonratelimit = TRUE)

# before this step, i already pulled down tweets containing racial slurs, I call the tweets dataset trulyracisttweet
trulyracisttweet  <- asianracisttweet

# tokenize and see top frequency words, sentiment and topic
tidy_trulyracisttweet <- trulyracisttweet%>%
  dplyr::select(created_at,text) %>%
  unnest_tokens("word",text)

# remove stop words
data("stop_words")
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  anti_join(stop_words)

# remove numbers
tidy_trulyracisttweet <- tidy_trulyracisttweet[-grep("\\b\\d+\\b", 
                                                     tidy_trulyracisttweet$word),]

# remove whitespaces
tidy_trulyracisttweet$word <- gsub("\\s+", "", tidy_trulyracisttweet$word)

# stemming to the root words
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  mutate_at("word", funs(wordStem((.), language = "en")))

# change slurword list to lower letters for easier comparison
# this is important, because our slur database use capital letters, 
# and grep won't recognize unless you change it to lower
slurwords$Slur <- tolower(slurwords$Slur)

# count frequency of each slur word
# create empty dataframe which contains slur word and its 
# count in the 18000 tweets of 10 keywords
# make stringasfactors=false or there will be some problem
slurvocab <- data.frame(vocab = character(nrow(slurwords)), 
                        count = numeric(nrow(slurwords)), 
                        stringsAsFactors = FALSE)
i <- 1

for(each in slurwords$Slur){
  slurvocab$vocab[i] = each
  # grep returns the indices of rows containing the slur
  rownumber = grep(each,tidy_trulyracisttweet$word)
  # number of rows will be the frequencies of the slur
  slurvocab$count[i] = length(rownumber)
  i <- i+1
}

# Barplot
selectedslurvocab=slurvocab %>%
  arrange(desc(count)) %>%
  top_n(10)

ggplot(selectedslurvocab, 
       aes(reorder(vocab, -count), 
           count, 
           fill = factor(vocab))) + 
  geom_bar(stat = "identity")+
  labs(x = "slurvocab", y="count",
       title = "Top 10 slurwords for Asian in 18000 Tweets",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet")

###############################################################################
#                                                                             #
#               Determining the most common slurs against Jewish              #
#                                                                             #
###############################################################################

# read in selected slur words. 
slurwords <- read.csv("RSDB asian.csv")

# loop to concatenate and search posts
# concatenate first 20 keywords into a giant string
str1              <- '"'
space             <- " "
ORword            <- "OR"
totalstringloop   <- ''

for(word in slurwords$Slur[]){
  slurwordstring  <- paste(str1, word, str1, space, ORword, space, sep="")
  totalstringloop <- paste(totalstringloop, slurwordstring)
}
# Remove the last OR to get final totalstring
totalstringloop   <- substring(totalstringloop, 1, nchar(totalstringloop)-3)
# check number of characters in the string, 
# maximum character length is 500 for search_tweet function
nchar(totalstringloop)

# search_tweet with stringed keyword
jewracisttweet       <- search_tweets(q=totalstringloop,
                                      "lang:en",
                                      geocode = lookup_coords("usa"),
                                      n = 10000, 
                                      include_rts = FALSE, 
                                      type = "recent", 
                                      retryonratelimit=TRUE)

# Before this step, I already pulled down tweets containing racial slurs, 
# I call the tweets dataset trulyracisttweet
trulyracisttweet   <- jewracisttweet

# Tokenize and see top frequency words, sentiment and topic
tidy_trulyracisttweet <- trulyracisttweet%>%
  dplyr::select(created_at,text) %>%
  unnest_tokens("word",text)

# Remove stop words
data("stop_words")
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  anti_join(stop_words)

# Remove numbers
tidy_trulyracisttweet <- tidy_trulyracisttweet[-grep("\\b\\d+\\b", 
                                                     tidy_trulyracisttweet$word),]

# Remove whitespace
tidy_trulyracisttweet$word <- gsub("\\s+", "", tidy_trulyracisttweet$word)

# stemming to the root words
tidy_trulyracisttweet <- tidy_trulyracisttweet %>%
  mutate_at("word", funs(wordStem((.), language = "en")))

# change slurword list to lower letters for easier comparison
# this is important, because our slur database use capital letters, 
# and grep won't recognize unless you change it to lower
slurwords$Slur <- tolower(slurwords$Slur)

# count frequency of each slur word
# create empty dataframe which contains slur word and its count 
# in the 18000 tweets of 10 keywords
# make stringasfactors=false or there will be some problem
slurvocab <- data.frame(vocab = character(nrow(slurwords)), 
                        count = numeric(nrow(slurwords)), 
                        stringsAsFactors = FALSE)
i <- 1

for(each in slurwords$Slur){
  slurvocab$vocab[i] = each
  # grep returns the indices of rows containing the slur
  rownumber = grep(each,tidy_trulyracisttweet$word)
  # number of rows will be the frequencies of the slur
  slurvocab$count[i] = length(rownumber)
  i <- i+1
}

# Barplot
selectedslurvocab=slurvocab%>%
  arrange(desc(count)) %>%
  top_n(10)

ggplot(selectedslurvocab, 
       aes(reorder(vocab,-count), 
           count, 
           fill = factor(vocab))) + 
  geom_bar(stat = "identity")+
  labs(x = "slurvocab", y="count",
       title = "Top 10 slurwords for Jewish in 18000 Tweets",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet")

###############################################################################
#                                                                             #
#               Pulling the multiple days worth of tweets for analysis        #
#               These tweets combine the three most frequently used words     #
#               for each group to determine the most common among all groups  #
#                                                                             #
###############################################################################

# read in selected top 3 slur words 

slurwords <- read.csv("RSDB top3s.csv")

# loop to concatenate and search posts
# concatenate first 20 keywords into a giant string
str1              <- '"'
space             <- " "
ORword            <- "OR"
totalstringloop   <- ''

for(word in slurwords$Slur[]){
  slurwordstring  <- paste(str1,word,str1,space, ORword,space,sep="")
  totalstringloop <- paste(totalstringloop,slurwordstring)
}
# remove the last OR to get final totalstring
totalstringloop   <- substring(totalstringloop, 1, nchar(totalstringloop)-3)
# check number of characters in the string, maximum character 
# length is 500 for search_tweet function
nchar(totalstringloop)

# search_tweet with stringed keyword
racisttweet       <- search_tweets(q = totalstringloop,
                                   "lang:en",
                                   geocode = lookup_coords("usa"),
                                   n = 10000, 
                                   include_rts = FALSE, 
                                   type = "recent", 
                                   retryonratelimit = TRUE)

###############################################################################
#                                                                             #
#               Combining the multiple days worth of tweets for analysis      #
#               (8 November 2018 through 18 November 2018)                    #
#                                                                             #
###############################################################################

# loaded and combined 4 rda files of tweets
#alltweets            <- rbind(tweet1,tweet2,tweet3,tweet6)

# after combining the tweets ensure all are unique
#alltweetsde          <- unique(alltweets)

# remove any duplicates that may have been pulled due to Twitter's search algorithm
#alltweetsde         <- alltweets[!duplicated(alltweets),]

# eventually combined tweets is saved in 1_combinedtweets.rda
load("1_combinedtweets.rda")


###############################################################################
#                                                                             #
#               TEXT CLEANING                                                 #
#                                                                             #
#               In order to properly anaylize the tweets for their content    #
#               we had to remove superfluous information (http, #, numbers)   #
#               as well as determine if a tweet actually contained a slur,    #
#               as Twitter's search function searches both content of tweets  #
#               as well as user names. Once the text was cleaned and the      #
#               content was confirmed, each tweet was searched and            #
#               and categorized by race.                                      #
#                                                                             #
###############################################################################

# Step 1 of cleaning Tweets: further processing of tweets' text. 
# Add a column called cleantext

# Get rid of references to other screennames
clean_alltweetsdegotslur <- str_replace_all(alltweetsde$text,
                                            "@[a-z,A-Z]*","")
# Remove URLs
clean_alltweetsdegotslur <- str_replace_all(clean_alltweetsdegotslur, 
                                            "https://t.co/[a-z,A-Z,0-9]*","")
# Remove hashtags
clean_alltweetsdegotslur <- str_replace_all(clean_alltweetsdegotslur,
                                            "#[a-z,A-Z]*","")
# Remove any words starting with_
clean_alltweetsdegotslur <- str_replace_all(clean_alltweetsdegotslur,
                                            "_[a-z,A-Z]*","")
# Remove numbers
clean_alltweetsdegotslur <- str_replace_all(clean_alltweetsdegotslur,
                                            "[1-9]*","")
# Remove hypostrophy
clean_alltweetsdegotslur <- str_replace_all(clean_alltweetsdegotslur,
                                            "'*","")
# Remove words of less than 3 alphabets/2 words
clean_alltweetsdegotslur <- str_replace_all(clean_alltweetsdegotslur,
                                            " *\\b[[:alpha:]]{1,3}\\b *","")
# Remove words starting with numbers
clean_alltweetsdegotslur <- str_replace_all(clean_alltweetsdegotslur,
                                            "[0-9]*","")

alltweetsde$cleantext    <- clean_alltweetsdegotslur

# Step 2 of tweets cleaning: loop through tweets' cleantext to check 
# whether the tweet text contains any of the top 3 slur words. 
# If it doesn't contain, we discard that tweet entry. 
# Create a new dataframe called alltweetsdegotslur only containing tweets that have slur words

newdataframe          <- alltweetsde
newdataframe$isRacist <- rep(0,nrow(newdataframe))

#capital letter
top3slurs=slurwords
for(i in top3slurs$Slur){
  rownumber = grep(i,newdataframe$cleantext)
  if(length(rownumber) > 0){
    newdataframe$isRacist[rownumber] = 1 
  }
}

#non-capital letter
for(a in top3slurs$Slur){
  lowerslur = tolower(a)
  rownumber = grep(lowerslur,newdataframe$cleantext)
  if(length(rownumber) > 0){
    newdataframe$isRacist[rownumber] = 1 
  }
}

alltweetsdegotslur    <- newdataframe %>% 
  filter(isRacist == 1)

# step 3 of tweet cleaning - label each tweets its race - 
# whether it's black, white, jewish,asian,

# create a data frame of the three most common white slurs
slurwhite                 <- c("gringo","hillbilly","redneck","Gringo","Hillbilly","Redneck")
# Create a column "white" in alltweetsdegotslur and set all values to zero
alltweetsdegotslur$white  <- rep(0,nrow(alltweetsdegotslur))

# step through the alltweetsdegotslur and label any tweet containing "slurwhite" as a 1 in the 
# "alltweetsdegotslur$white" column
for(i in slurwhite){
  rownumber = grep(i,alltweetsdegotslur$cleantext)
  if(length(rownumber) > 0){
    alltweetsdegotslur$white[rownumber] = 1 
  }
}

# create a data frame of the three most common Jewish slurs
slurjewish                <- c("zionist","kike","zog","Zionist","Kike","Zog")
# Create a column "jewish" in alltweetsdegotslur and set all values to zero
alltweetsdegotslur$jewish <- rep(0,nrow(alltweetsdegotslur))

# step through the alltweetsdegotslur and label any tweet containing "slurjewish" as a 1 in the 
# "alltweetsdegotslur$jewish" column
for(i in slurjewish){
  rownumber = grep(i,alltweetsdegotslur$cleantext)
  if(length(rownumber) > 0){
    alltweetsdegotslur$jewish[rownumber] = 1 
  }
}

# create a data frame of the three most common hispanic slurs
slurhispan                <- c("beaner","wetback","spic","Beaner","Wetback","Spic")
# Create a column "hispan" in alltweetsdegotslur and set all values to zero
alltweetsdegotslur$hispan <- rep(0,nrow(alltweetsdegotslur))

# step through the alltweetsdegotslur and label any tweet containing "slurhispan" as a 1 in the 
# "alltweetsdegotslur$hispan" column
for(i in slurhispan){
  rownumber = grep(i,alltweetsdegotslur$cleantext)
  if(length(rownumber) > 0){
    alltweetsdegotslur$hispan[rownumber] = 1 
  }
}

# create a data frame of the three most common asian slurs
slurasian                 <- c("chink","gook","chinaman","Chink","Gook","Chinaman")
# Create a column "asian" in alltweetsdegotslur and set all values to zero
alltweetsdegotslur$asian  <- rep(0,nrow(alltweetsdegotslur))

# step through the alltweetsdegotslur and label any tweet containing "slurasian" as a 1 in the 
# "alltweetsdegotslur$asian" column
for(i in slurasian){
  rownumber = grep(i,alltweetsdegotslur$cleantext)
  if(length(rownumber) > 0){
    alltweetsdegotslur$asian[rownumber] = 1 
  }
}

# create a data frame of the three most common black slurs
slurblack=c("nigger","coon","mandingo","Nigger","Coon","Mandingo")
# Create a column "black" in alltweetsdegotslur and set all values to zero
alltweetsdegotslur$black=rep(0,nrow(alltweetsdegotslur))

# step through the alltweetsdegotslur and label any tweet containing "slurblack" as a 1 in the 
# "alltweetsdegotslur$black" column
for(i in slurblack){
  rownumber = grep(i,alltweetsdegotslur$cleantext)
  if(length(rownumber) > 0){
    alltweetsdegotslur$black[rownumber] = 1 
  }
}

# determine the number of tweets in each group
countblack  <- sum(alltweetsdegotslur$black)
countjewish <- sum(alltweetsdegotslur$jewish)
counthispan <- sum(alltweetsdegotslur$hispan)
countasian  <- sum(alltweetsdegotslur$asian)
countblack  <- sum(alltweetsdegotslur$black)

# determine the overall count of derogitory tweets
sum(countblack,countjewish,counthispan,countasian,countblack)

#create a combined racelabel column
alltweetsdegotslur$racelabel <- rep(0,nrow(alltweetsdegotslur))
alltweetsdegotslur$racelabel[alltweetsdegotslur$white==1]  <- "white"
alltweetsdegotslur$racelabel[alltweetsdegotslur$jewish==1] <- "jewish"
alltweetsdegotslur$racelabel[alltweetsdegotslur$black==1]  <- "black"
alltweetsdegotslur$racelabel[alltweetsdegotslur$asian==1]  <- "asian"
alltweetsdegotslur$racelabel[alltweetsdegotslur$hispan==1] <- "hispanic"

# eventually cleaned and race-labeled tweets are saved in finalcleanedtweets.rda
load("2_finalcleanedtweets.rda")

###############################################################################
#                                                                             #
#               TOXICITY - from Google's Perspective's API                    #
#                                                                             #
#               https://www.perspectiveapi.com/#/                             #
#               The tweets pulled previously are sent through the API         #
#               in order to determine if the tweets are truly racist.         #
#               This is determined by a toxicity score based on sentiment     #
#               analysis and machine learning. A probability for being a      #
#               toxic statement is given to each Tweet (between 0 and 1).     #
#               From there we chose only those Tweets which had a .60 or      #
#               higher for our analysis                                       #
#                                                                             #
###############################################################################

# python script is used to run perspective API. Tweets with calculated toxicity score is stored in 3_labelledalltweets.rda
load("3_labelledalltweets.rda")

# Till here we finished curation of explicit racist tweets

###############################################################################
#                                                                             #
#               VISUALIZATIONS                                                #
#                                                                             #
#               In order to conduct a thorough analysis we used multiple      #
#               techniques and methods to better understand the data          #
#               through graphical means.                                      #
#                                                                             #
###############################################################################

# have a dataframe with toxic tweets only for comparison
alltweetsdegotslurtoxic=alltweetsdegotslur %>%
  filter(toxicity>0.6)

############################################################################### 
#                                                                             #
#               What’s the most frequently used racial slur?                  #
#                                                                             #
###############################################################################

# code below is for all racist tweets not filtered for toxicity score. Repeat the same code for toxic tweets only for comparison
# count frequency of each slur word for each race and rank them
# create empty dataframe which contains slur word and its count in the 18000 tweets of 3 keywords
slurvocabday1<-data.frame(vocab = character(nrow(top3slurs)), count=numeric(nrow(top3slurs)), stringsAsFactors = FALSE)
i=1
for(each in tolower(top3slurs$Slur)){
  slurvocabday1$vocab[i]=each
  rownumber=grep(each,tidy_alltweetsdegotslur$word)
  slurvocabday1$count[i]=length(rownumber)
  i=i+1
}

#Barplot
slurvocabday1$group=c("white","white","white","jewish","jewish","jewish","hispanic","hispanic","hispanic","asian","asian","asian","black","black","black")

selectedslurvocabday1=slurvocabday1%>%
  arrange(desc(count))


ggplot(selectedslurvocabday1, aes(reorder(vocab,-count), count, fill=factor(group))) + geom_bar(stat="identity")+
  labs(
    x="slurvocab", y="count",
    title="Frequencies of top 3 slurs for each race for 4 days of 18000 tweets",
    caption="\nSource: Data collected from Twitter's REST API via rtweet")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  #scale_fill_brewer(palette = 'Spectral')
  scale_fill_manual("legend", values = c("white" = "royalblue4", "black" = "#404040", "asian" = "#ca0020","hispanic"="#bababa", "jewish"="pink3"))


############################################################################### 
#                                                                             #
#               Which race is mostly discriminated against?                   #
#                                                                             #
###############################################################################

# violin plot of tweets' toxicity score for 5 races
ggplot(alltweetsdegotslur, aes(racelabel,toxicity,fill=factor(racelabel)))+
  geom_violin()+
  scale_fill_manual("legend", values = c("white" = "royalblue4", "black" = "#404040", "asian" = "#ca0020","hispanic"="#bababa", "jewish"="pink3"))+
  labs(
    x="races", y="tweet toxicity score",
    title="Violin Plot of Tweets' Toxicity Score for 5 Races",
    caption="\nSource: Data collected from Twitter's REST API via rtweet")

# Proportion of Toxic Tweets vs Original Proportion of Tweets by Race
toxic=alltweetsdegotslur[alltweetsdegotslur$toxicity>0.6,]
toxicwhite=toxic[toxic$white==1,]
toxicwhiteprob=nrow(toxicwhite)/nrow(toxic)

toxicblack=toxic[toxic$black==1,]
toxicblackprob=nrow(toxicblack)/nrow(toxic)

toxicasian=toxic[toxic$asian==1,]
toxicasianprob=nrow(toxicasian)/nrow(toxic)

toxicjewish=toxic[toxic$jewish==1,]
toxicjewishprob=nrow(toxicjewish)/nrow(toxic)

toxichispan=toxic[toxic$hispan==1,]
toxichispanprob=nrow(toxichispan)/nrow(toxic)

originalprobwhite=nrow(alltweetsdegotslurwhite)/nrow(alltweetsdegotslur)*100
originalprobblack=nrow(alltweetsdegotslurblack)/nrow(alltweetsdegotslur)*100
originalprobjewish=nrow(alltweetsdegotslurjewish)/nrow(alltweetsdegotslur)*100
originalprobhispan=nrow(alltweetsdegotslurhispan)/nrow(alltweetsdegotslur)*100
originalprobasian=nrow(alltweetsdegotslurasian)/nrow(alltweetsdegotslur)*100
originaldata=c('white'=originalprobwhite,'black'=originalprobblack,'asian'=originalprobasian,'jewish'=originalprobjewish,'hispanic'=originalprobhispan)

race=c("white","black","jewish","hispanic","asian")
prob=data.frame(toxicprob=toxicdata,originalprob=originaldata,race=race)
probnew=melt(prob)
ggplot(probnew, aes(x = race, y= value, fill = variable), xlab="Race") +
  geom_bar(stat="identity", width=.5, position = "dodge")+
  labs(y="proportion in percentage", title="Proportion of Toxic Tweets vs Original Proportion of Tweets by Race")+
  scale_fill_manual(values = c("#ca0020","#404040"))


############################################################################### 
#                                                                             #
#               Does location affect who is discriminated against?            #
#                                                                             #
###############################################################################


# create lat/lng variables using all available tweet and profile geo-location data
#lat_lng gives 2 columns of lat and lng for each tweet

# using Rtweet create a column for lattitude and logitude from twitters locational data
rtusaloc <- lat_lng(alltweetsdegotslur)

# Create a dataframe specifically for slurs against blacks
alltweetsdegotslurblack  <- alltweetsdegotslur %>%
  filter(alltweetsdegotslur$black == 1)

# Create a column in the above dataframe for location data
rtusalocblack            <- lat_lng(alltweetsdegotslurblack)

# Create a dataframe specifically for slurs against jewish
alltweetsdegotslurjewish <- alltweetsdegotslur %>%
  filter(alltweetsdegotslur$jewish == 1)

# Create a column in the above dataframe for location data
rtusalocjewish           <- lat_lng(alltweetsdegotslurjewish)

# Create a dataframe specifically for slurs against hispanics
alltweetsdegotslurhispan <- alltweetsdegotslur %>%
  filter(alltweetsdegotslur$hispan == 1)

# Create a column in the above dataframe for location data
rtusalochispan           <- lat_lng(alltweetsdegotslurhispan)

# Create a dataframe specifically for slurs against asian
alltweetsdegotslurasian  <- alltweetsdegotslur %>%
  filter(alltweetsdegotslur$asian == 1)

# Create a column in the above dataframe for location data
rtusalocasian            <- lat_lng(alltweetsdegotslurasian)

# Create a dataframe specifically for slurs against whites
alltweetsdegotslurwhite  <- alltweetsdegotslur %>%
  filter(alltweetsdegotslur$white == 1)

# Create a column in the above dataframe for location data
rtusalocwhite            <- lat_lng(alltweetsdegotslurwhite)

# plot state boundaries
par(mar = c(0, 0, 0, 0))
map("state", lwd = .25)

##plot lat and lng points onto state map
with(rtusalocwhite, points(lng, lat, pch = 20, cex = .75, col = "blue"))
with(rtusalocblack, points(lng, lat, pch = 20, cex = .75, col = "black"))
with(rtusalocjewish, points(lng, lat, pch = 20, cex = .75, col = "orange"))
with(rtusalocasian, points(lng, lat, pch = 20, cex = .75, col = "red"))
with(rtusalochispan, points(lng, lat, pch = 20, cex = .75, col = "purple"))
legend("bottomright",legend=c("White", "Jewish", "Asian", "Black", "Hispanic"),
       col=c("blue", "orange","red","black", "purple"), pch=20, cex=0.75)
title(main="Location Distribution of Tweets of Top 3 Racial Slurs for 5 Races")


############################################################################### 
#                                                                             #
#               How do events affect hate speech?                             #
#                                                                             #
###############################################################################

### Time Series of the frequency of the tweets over the 10 days of pulling data
ts_plot(alltweetsdegotslurtoxic,by="hours")+geom_line(stat="identity")+
  labs(
    x="time", y="count",
    title="Frequencies of Tweets of Top 3 Racial Slur Words for 5 Races across 10 Days",
    caption="\nSource: Data collected from Twitter's REST API via rtweet")

############################################################################### 
#                                                                             #
#              What are the key topics of racist tweets?                      #
#                                                                             #
###############################################################################

# topics of white racist tweets
# convert to DTM first
# tokenize
tidy_alltweetsdegotslurwhite<- alltweetsdegotslurwhite %>%
  dplyr::select(created_at,text) %>%
  unnest_tokens("word",text)
head(tidy_alltweetsdegotslurwhite)

# other processings
whitewords=c("white","racist","hillbilli","ass","fuck")
otherstopwords=c("https","t.co","amp","rt","white","racist","hillbilli","ass","fuck","redneck","i'm","don't","peopl","it'","yee")
otherstopwords=data.frame(word=otherstopwords,stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurwhite<-tidy_alltweetsdegotslurwhite %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)


tidy_alltweetsdegotslurwhite$word <- gsub("\\s+","",tidy_alltweetsdegotslurwhite$word)
tidy_alltweetsdegotslurwhite<-tidy_alltweetsdegotslurwhite %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#Change to DTM
tidy_DTM_white<-
  tidy_alltweetsdegotslurwhite %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

#run LDA
tweet_topic_model_white = LDA(tidy_DTM_white, k=3, control = list(seed = 321))

#visualization
tweet_topics_white <- tidy(tweet_topic_model_white, matrix = "beta")  
#beta is the probability of each word associating with each topic

tweet_top_terms <- 
  tweet_topics_white %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


tweet_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# black
# convert to DTM first
# tokenize
tidy_alltweetsdegotslurblack<- alltweetsdegotslurblack %>%
  dplyr::select(created_at,text) %>%
  unnest_tokens("word",text)


# other processings
otherstopwords=c("https","t.co","amp","rt","nigger","coon","ass","don't","it'","call","black","nigga","1")
otherstopwords=data.frame(word=otherstopwords,stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurblack<-tidy_alltweetsdegotslurblack %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)


# tidy_alltweetsdegotslur<-tidy_alltweetsdegotslur[-grep("\\b\\d+\\b", tidy_alltweetsdegotslur$word),]
tidy_alltweetsdegotslurblack$word <- gsub("\\s+","",tidy_alltweetsdegotslurblack$word)
tidy_alltweetsdegotslurblack<-tidy_alltweetsdegotslurblack %>%
  mutate_at("word", funs(wordStem((.), language="en")))


# Change to DTM
tidy_DTM_black<-
  tidy_alltweetsdegotslurblack %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

# run LDA
tweet_topic_model_black = LDA(tidy_DTM_black, k=3, control = list(seed = 321))

# visualization
tweet_topics_black <- tidy(tweet_topic_model_black, matrix = "beta")  
# beta is the probability of each word associating with each topic

tweet_top_terms <- 
  tweet_topics_black %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


tweet_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# jewish
# tokenize
tidy_alltweetsdegotslurjewish<- alltweetsdegotslurjewish %>%
  dplyr::select(created_at,text) %>%
  unnest_tokens("word",text)

#other processings
otherstopwords=c("https","t.co","amp","rt")
otherstopwords=data.frame(word=otherstopwords,stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurjewish<-tidy_alltweetsdegotslurjewish %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)

tidy_alltweetsdegotslurjewish$word <- gsub("\\s+","",tidy_alltweetsdegotslurjewish$word)
tidy_alltweetsdegotslurjewish<-tidy_alltweetsdegotslurjewish %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#Change to DTM
tidy_DTM_jewish<-
  tidy_alltweetsdegotslurjewish %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

#run LDA
tweet_topic_model_jewish = LDA(tidy_DTM_jewish, k=3, control = list(seed = 321))

#visualization
tweet_topics_jewish <- tidy(tweet_topic_model_jewish, matrix = "beta")  
#beta is the probability of each word associating with each topic

tweet_top_terms <- 
  tweet_topics_jewish %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


tweet_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


############################################################################### 
#                                                                             #
#       What are words that have the highest association with racial slurs?   #                                                                           #
#                                                                             #
###############################################################################

###############################################################################
#                                                                             #
#               Step 1: Tokenize by race                                      #
#                                                                             #
###############################################################################

### tokenize for black
tidy_alltweetsdegotslurblack  <- alltweetsdegotslur %>%
  filter(black == 1) %>%
  dplyr::select(created_at, cleantext) %>%
  unnest_tokens("word", cleantext)

# other processings
otherstopwords                <- c("https", "t.co", "amp", "rt")
otherstopwords                <- data.frame(word = otherstopwords, stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurblack  <-tidy_alltweetsdegotslurblack %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)

tidy_alltweetsdegotslurblack$word <- gsub("\\s+", "", tidy_alltweetsdegotslurblack$word)

tidy_alltweetsdegotslur       <- tidy_alltweetsdegotslur %>%
  mutate_at("word", funs(wordStem((.), language = "en")))

tidy_alltweetsdegotslurblack %>%
  count(word) %>%
  arrange(desc(n))

### tokenize for jewish
tidy_alltweetsdegotslurjewish <- alltweetsdegotslur %>%
  filter(jewish == 1) %>%
  dplyr::select(created_at, cleantext) %>%
  unnest_tokens("word", cleantext)

# other processings
otherstopwords                 <- c("https", "t.co", "amp", "rt")
otherstopwords                 <- data.frame(word = otherstopwords, stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurjewish  <- tidy_alltweetsdegotslurjewish %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)

tidy_alltweetsdegotslurjewish$word <- gsub("\\s+", "", tidy_alltweetsdegotslurjewish$word)

tidy_alltweetsdegotslur        <- tidy_alltweetsdegotslur %>%
  mutate_at("word", funs(wordStem((.), language="en")))

tidy_alltweetsdegotslurjewish %>%
  count(word) %>%
  arrange(desc(n))

### tokenize for hispan
tidy_alltweetsdegotslurhispan  <- alltweetsdegotslur %>%
  filter(hispan ==1) %>%
  dplyr::select(created_at,cleantext) %>%
  unnest_tokens("word",cleantext)

# other processings
otherstopwords                 <- c("https", "t.co", "amp", "rt")
otherstopwords                 <- data.frame(word = otherstopwords, stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurhispan  <-tidy_alltweetsdegotslurhispan  %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)

tidy_alltweetsdegotslurhispan $word <- gsub("\\s+", "", tidy_alltweetsdegotslurhispan $word)

tidy_alltweetsdegotslur        <- tidy_alltweetsdegotslur %>%
  mutate_at("word", funs(wordStem((.), language = "en")))

tidy_alltweetsdegotslurhispan  %>%
  count(word) %>%
  arrange(desc(n))

### tokenize for asian
tidy_alltweetsdegotslurasian   <- alltweetsdegotslur %>%
  filter(asian == 1) %>%
  dplyr::select(created_at, cleantext) %>%
  unnest_tokens("word", cleantext)

# other processings
otherstopwords                 <- c("https", "t.co", "amp", "rt")
otherstopwords                 <- data.frame(word = otherstopwords, stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurasian   <- tidy_alltweetsdegotslurasian  %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)

tidy_alltweetsdegotslurasian $word <- gsub("\\s+", "", tidy_alltweetsdegotslurasian $word)

tidy_alltweetsdegotslur        <- tidy_alltweetsdegotslur %>%
  mutate_at("word", funs(wordStem((.), language = "en")))

tidy_alltweetsdegotslurasian  %>%
  count(word) %>%
  arrange(desc(n))

###tokenize for white
tidy_alltweetsdegotslurblack    <- alltweetsdegotslur %>%
  filter(white ==1) %>%
  dplyr::select(created_at,cleantext) %>%
  unnest_tokens("word",cleantext)

#other processings
otherstopwords                  <- c("https","t.co","amp","rt")
otherstopwords                  <- data.frame(word=otherstopwords,stringsAsFactors = FALSE)
data("stop_words")
tidy_alltweetsdegotslurwhite    <- tidy_alltweetsdegotslurwhite  %>%
  anti_join(stop_words) %>%
  anti_join(otherstopwords)


tidy_alltweetsdegotslurblack $word <- gsub("\\s+","",tidy_alltweetsdegotslurwhite $word)

tidy_alltweetsdegotslur         <- tidy_alltweetsdegotslur %>%
  mutate_at("word", funs(wordStem((.), language="en")))

tidy_alltweetsdegotslurwhite  %>%
  count(word) %>%
  arrange(desc(n))

###############################################################################
#                                                                             #
#               Step 2: Word Embedding - Slurs against Blacks                 #
#                                                                             #
###############################################################################


#word embedding for black tweets only
alltweetsdenolist        <- alltweetsdegotslur[,1:5] 
alltweetsdenolist$text   <- alltweetsdegotslur$cleantext
alltweetsdenolist$white  <- alltweetsdegotslur$white
alltweetsdenolist$jewish <- alltweetsdegotslur$jewish
alltweetsdenolist$hispan <- alltweetsdegotslur$hispan
alltweetsdenolist$asian  <- alltweetsdegotslur$asian
alltweetsdenolist$black  <- alltweetsdegotslur$black

alltweetsdenolistblack   <- alltweetsdenolist %>%
  filter(black == 1)

#create tweet id
alltweetsdenolistblack$postID <- row.names(alltweetsdenolistblack)

#unigram probabilities
unigram_probs <- alltweetsdenolistblack %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#skipgram probabilities

tidy_skipgrams <- alltweetsdenolistblack %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, postID, ngramID) %>%
  unnest_tokens(word, ngram)

tidy_skipgrams=tidy_skipgrams[,10:11]

skipgram_probs <- tidy_skipgrams %>%
  widyr::pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalizedtableblack=normalized_prob %>% 
  filter(word1 == "nigger") %>%
  arrange(-p_together) %>%
  top_n(60)


ggplot(normalizedtableblack, aes(reorder(word2,p_together), p_together)) + geom_bar(stat="identity",width = 0.8, position = position_dodge(width = 1.2)) + 
  labs(
    x="Top 30 Words", y="count",
    title="Top 30 Words Most Closely Associated with Racist Tweets on Black",
    caption="\nSource: Data collected from Twitter's REST API via rtweet")+
  coord_flip()

#word cloud
set.seed(1234)
wordcloud(words = normalizedtableblack$word2, freq = normalizedtableblack$p_together, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "BuPu"))

###############################################################################
#                                                                             #
#              Step 2: Word Embedding - Slurs against Whites                  #
#                                                                             #
###############################################################################

#word embedding for white tweets only
alltweetsdenolistwhite <- alltweetsdenolist %>%
  filter(white == 1)

#create tweet id
alltweetsdenolistwhite$postID <- row.names(alltweetsdenolistwhite)

#unigram probabilities
unigram_probs <- alltweetsdenolistwhite %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#skipgram probabilities
tidy_skipgrams <- alltweetsdenolistwhite %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, postID, ngramID) %>%
  unnest_tokens(word, ngram)

tidy_skipgrams <- tidy_skipgrams[,10:11]

skipgram_probs <- tidy_skipgrams %>%
  widyr::pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "redneck") %>%
  arrange(-p_together)

normalizedtablewhite <- normalized_prob %>% 
  filter(word1 == "redneck") %>%
  arrange(-p_together) %>%
  top_n(30)

# Bar Plot for words most associated wtih racist tweets against Whites
ggplot(normalizedtablewhite, 
       aes(reorder(word2,p_together), p_together)) + 
  geom_bar(stat="identity",
           width = 0.8, 
           position = position_dodge(width = 1.2),
           fill="royalblue4") + 
  labs(x="Top 30 Words",
       y="count",
       title="Top 30 Words Most Closely Associated with Racist Tweets on White",
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  coord_flip()

set.seed(1234)

# Word Cloud for most common slurs and words associated with Jewish People
wordcloud(words = normalizedtablewhite$word2, 
          freq = normalizedtablewhite$p_together, 
          min.freq = 1,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Blues"))

###############################################################################
#                                                                             #
#               Step 2: Word Embedding - Slurs against Jews                   #
#                                                                             #
###############################################################################

alltweetsdenolistjewish <- alltweetsdenolist %>%
  filter(jewish == 1)

#create tweet id
alltweetsdenolistjewish$postID <- row.names(alltweetsdenolistjewish)

#unigram probabilities
unigram_probs <- alltweetsdenolistjewish %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#skipgram probabilities

tidy_skipgrams <- alltweetsdenolistjewish %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, postID, ngramID) %>%
  unnest_tokens(word, ngram)

tidy_skipgrams <- tidy_skipgrams[,10:11]

skipgram_probs <- tidy_skipgrams %>%
  widyr::pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "zionist") %>%
  arrange(-p_together)

normalizedtablejewish=normalized_prob %>% 
  filter(word1 == "zionist") %>%
  arrange(-p_together) %>%
  top_n(30)

# Bar Plot for words most associated wtih racist tweets against Jews
ggplot(normalizedtablejewish, 
       aes(reorder(word2,p_together), p_together)) + 
  geom_bar(stat="identity",
           width = 0.8, 
           position = position_dodge(width = 1.2),
           fill="pink3") + 
  labs(x="Top 30 Words",
       y="count",
       title="Top 30 Words Most Closely Associated with Racist Tweets on Jews",
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  coord_flip()

set.seed(1234)

# Word Cloud for most common slurs and words associated with Jewish People
wordcloud(words = normalizedtablejewish$word2,
          freq = normalizedtablejewish$p_together,
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Oranges"))

###############################################################################
#                                                                             #
#             Step 2: Word Embedding - Slurs against Asians                   #
#                                                                             #
###############################################################################

alltweetsdenolistasian <- alltweetsdenolist %>%
  filter(asian == 1)

#create tweet id
alltweetsdenolistasian$postID <- row.names(alltweetsdenolistasian)

#unigram probabilities
unigram_probs <- alltweetsdenolistasian %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#skipgram probabilities

tidy_skipgrams <- alltweetsdenolistasian %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, postID, ngramID) %>%
  unnest_tokens(word, ngram)

tidy_skipgrams <- tidy_skipgrams[,10:11]

skipgram_probs <- tidy_skipgrams %>%
  widyr::pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "chink") %>%
  arrange(-p_together)

normalizedtableasian=normalized_prob %>% 
  filter(word1 == "chink") %>%
  arrange(-p_together) %>%
  top_n(30)

# Bar Plot for words most associated wtih racist tweets against asians
ggplot(normalizedtableasian, 
       aes(reorder(word2,p_together), p_together)) + 
  geom_bar(stat="identity",
           width = 0.8, 
           position = position_dodge(width = 1.2)
           ,fill="#ca0020") + 
  labs(x="Top 30 Words", 
       y="count",
       title="Top 30 Words Most Closely Associated with Racist Tweets on Asian",
       caption="\nSource: Data collected from Twitter's REST API via rtweet") +
  coord_flip()

set.seed(1234)

# Word Cloud for most common slurs and words associated with Asian People
wordcloud(words = normalizedtableasian$word2,
          freq = normalizedtableasian$p_together, 
          min.freq = 1,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

###############################################################################
#                                                                             #
#            Step 2: Word Embedding - Slurs against Hispanics                 #
#                                                                             #
###############################################################################

alltweetsdenolisthispan <- alltweetsdenolist %>%
  filter(hispan == 1)

#create tweet id
alltweetsdenolisthispan$postID <- row.names(alltweetsdenolisthispan)

#unigram probabilities
unigram_probs <- alltweetsdenolisthispan %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#skipgram probabilities
tidy_skipgrams <- alltweetsdenolisthispan %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, postID, ngramID) %>%
  unnest_tokens(word, ngram)

tidy_skipgrams <- tidy_skipgrams[,10:11]

skipgram_probs <- tidy_skipgrams %>%
  widyr::pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "spic") %>%
  arrange(-p_together)

normalizedtablehispan=normalized_prob %>% 
  filter(word1 == "spic") %>%
  arrange(-p_together) %>%
  top_n(30)

# Bar Plot for words most associated wtih racist tweets against hispanics
ggplot(normalizedtablehispan, 
       aes(reorder(word2,p_together), p_together)) + 
  geom_bar(stat="identity",
           width = 0.8, 
           position = position_dodge(width = 1.2),
           fill="#bababa") + 
  labs(x = "Top 30 Words",
    y = "count",
    title = "Top 30 Words Most Closely Associated with Racist Tweets on Hispanic",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  coord_flip()

set.seed(1234)

# Word Cloud for most common slurs and words associated with Hispanic People
wordcloud(words = normalizedtablehispan$word2, 
          freq = normalizedtablehispan$p_together,
          min.freq = 0,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))


###############################################################################
#                                                                             #
#               TEXT NETWORKS                                                 #
#               Are there associations among words used in Hate speech        #
#               against different races?                                      #
#                                                                             #                                                                             #                                                                           #
###############################################################################

toxic                  <-alltweetsdegotslurtoxic[,c(2,90,97)]

# tokenize by word
tidy_toxic             <- toxic %>%
  select(cleantext, status_id) %>%
  unnest_tokens("word", cleantext)
toxic_id               <- alltweetsdegotslurtoxic[,c(2,97)]

# merge by user_id
word_race              <- merge(tidy_toxic, toxic_id, by="status_id")

black_frame            <- word_race[word_race$racelabel=="black",]
white_frame            <- word_race[word_race$racelabel=="white",]
jewish_frame           <- word_race[word_race$racelabel=="jewish",]
asian_frame            <- word_race[word_race$racelabel=="asian",]
hispanic_frame         <- word_race[word_race$racelabel=="hispanic",]

# black
top_black              <- black_frame %>%
  count(word) %>%
  arrange(desc(n))
top_black              <- top_black[1:100,]
top_black$racelabel    <- rep("black",100)

dim(top_black)

# white
top_white              <- white_frame %>%
  count(word) %>%
  arrange(desc(n))
top_white              <- top_white[1:100,]
top_white$racelabel    <- rep("white",100)

# jewish
top_jewish             <- jewish_frame %>%
  count(word) %>%
  arrange(desc(n))
top_jewish             <- top_jewish[1:100,]
top_jewish$racelabel   <- rep("jewish",100)

# hispanic
top_hispanic           <- hispanic_frame %>%
  count(word) %>%
  arrange(desc(n))
top_hispanic           <- top_hispanic[1:100,]
top_hispanic$racelabel <- rep("hispanic",100)

# asian
top_asian              <- asian_frame %>%
  count(word) %>%
  arrange(desc(n))

top_asian              <-top_asian[1:100,]
top_asian$racelabel    <- rep("asian",100)

# total
total_top              <- rbind(top_black, 
                                top_white, 
                                top_jewish,
                                top_hispanic, 
                                top_asian)

total_top2             <- total_top

text                   <- word_race %>% filter (word_race$word%in%total_top2$word)

prepped                <- PrepText(text, groupvar = "racelabel", 
                                   textvar = "word", 
                                   node_type = "words", 
                                   tokenizer = "words", 
                                   pos = "nouns", 
                                   remove_stop_words = TRUE, 
                                   compound_nouns = FALSE)

text_network           <- CreateTextnet(prepped)

# visualize the Text network in 3D

VisTextNet(text_network, label_degree_cut = 0, alpha = 1)
vis                   <- VisTextNetD3(text_network)

top_words_modularity_classes <- InterpretText(text_network, prepped)
head(top_words_modularity_classes)

textnet               <- read.csv("~/Desktop/textnet.csv", comment.char="#")
textnet_grouped       <- textnet %>% 
  group_by(screen_name)

prepped_people        <- PrepText(textnet_grouped, 
                                  groupvar = "screen_name", 
                                  textvar = "text", 
                                  node_type = "groups", 
                                  tokenizer = "words", 
                                  pos = "nouns", 
                                  remove_stop_words = TRUE, 
                                  compound_nouns = TRUE)

text_network_people   <- CreateTextnet(prepped_people)
VisTextNet(text_network_people, label_degree_cut = 0, alpha = 1)

top_words_modularity_classes <- InterpretText(text_network_people, prepped_people)
head(top_words_modularity_classes)

text_centrality       <- TextCentrality(text_network_people)


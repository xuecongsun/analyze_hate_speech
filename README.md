# analyze_hate_speech
This is a repository for my project on understanding racial hate speech in 2018

Duke University - Masters of Interdisciplinary Data Science             
Project Name: Understanding Racial Hate Speech on Twitter
Instructor: Dr. Chris Bail                                              
Team Members: Emma Sun, Joe Littell, Chang Shu, Julia Oblasova          

This project aims to understand the landscape of explicit hate speech against 5 key races on Twitter through various text mining techniques.                                                           
                                                                           
To curate explict racist tweets, we first utilized screen scraping to pull in a dictionary of slur words for 5 key racial/ethic groups - White, Black, Hispanic, Asian, and Jewish. With this list we conducted a dictionary based search of tweets and identified top 3 most common slurs per group. We then pulled tweets containing top 3 slurs from 8 November 2018 through 18 November 2018 for 18000 tweets per group. To ensure the tweets to be explicitly racist, these tweets were analyzed 
for a toxicity rating through Google's Racial Toxicity API and filtered for the most toxic tweets (those above a threshold of 60% likeliness).

Based on these explicit racist tweets curated, we tried to answer the questions below via conducted analytical methods such as time series, topic modeling, text networks, frequency modeling, word clouds, and social network analysis:

-What’s the most frequently used racial slur?
-What's the most discriminated race?
-Does location affect who is being discriminated against? 
-How do events affect hate speech?
-What are words that have the highest association with racial slurs?
-Are there associations among words used in hate speech against different races?
-What are associations among people who tweeted hate speech?
  

   

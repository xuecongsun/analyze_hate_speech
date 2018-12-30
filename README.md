# Analyze_hate_speech
This is a repository for my project on understanding racial hate speech in 2018

Duke University - Masters of Interdisciplinary Data Science             
Project Name: Understanding Racial Hate Speech on Twitter
Instructor: Dr. Chris Bail                                              
Team Members: Emma Sun, Joe Littell, Chang Shu, Julia Oblasova          

## Research Question
This project aims to understand the landscape of explicit hate speech against 5 key races on Twitter through various text mining techniques.                                                           
                                                                           
To curate explict racist tweets, we first utilized screen scraping to pull in a dictionary of slur words for 5 key racial/ethic groups - White, Black, Hispanic, Asian, and Jewish. With this list we conducted a dictionary based search of tweets and identified top 3 most common slurs per group. We then pulled tweets containing top 3 slurs from 8 November 2018 through 18 November 2018 for 18000 tweets per group. To ensure the tweets to be explicitly racist, these tweets were analyzed 
for a toxicity rating through Google's Racial Toxicity API and filtered for the most toxic tweets (those above a threshold of 60% likeliness).

Based on these explicit racist tweets curated, we tried to answer the questions below via conducted analytical methods such as time series, topic modeling, text networks, frequency modeling, word clouds, and social network analysis:

1. What’s the most frequently used racial slur?
2. What's the most discriminated race?
3. Does location affect who is being discriminated against? 
4. How do events affect hate speech?
5. What are words that have the highest association with racial slurs?
6. Are there associations among words used in hate speech against different races?
7. What are associations among people who tweeted hate speech?
  
## Key Findings
1. The use of the word “nigger” is by far the most commonly used for racially charged speech
2. Blacks, far and away, has the most racism (and the most vitriolic at that) leveled at them on twitter
3. Usage frequencies of explicit racial slurs may not reflect hostility against that race. Usage frequency and negativity of slurs not always go hand in hand
4. Events do affect how people use twitter, and racially charged events can lead to hate speech online
5. Generally if hate speech is being used, it is coupled with other insults and heavy use of expletive language

## Detailed Findings
1. Top 3 racial slurs for whites are Redneck, Gringo and Hillbilly; for blacks are N*, Coon and Mandigo; for hispanics are Beaner, Spic and Wetback; for Asians are Chink, Gook and Chinaman and for Jewish are Zionist, Kike and Zog.Racial slurs most frequently used may not be the ones most associated with hate speech, as among tweets containing racial slurs, Redneck, Zionist and Gringo are top 3 frequent slurs, but among tweets containing racial slurs and confirmed to be hate speech, Nigger, Redneck, Zionist are top 3 frequent slurs.
2. Both Black and Hispanics see a higher volume of toxicity in Tweets against them
3. The vast majority of Tweets were against whites. Interestingly the number of anti Asian tweets in Southern Florida given limited population.Blacks saw the second most frequent amount of vitriol. Most tweets didn’t align with any particular topic that was identifiable.Jewish saw the third most frequent amount of vitriol. Topics are mostly about the recent happened shooting case, or Israel-Palestine conflict.Unsurprisingly, most anti- Hispanic speech correlates to anti immigrant sentiment.
This sentiment is highly visible in large Hispanic areas, particularly along the border.
4. There are 3 volume peak of racist tweets and they correpsond to 3 events. One is on November 7th, that is one day after American Midterm election. Two on Nov 12th and 13th, those were the days when conflicts happened between Israeli and Palestanian. 
5. Words that have the highest association with racial slurs are mostly just other insults and heavy use of expletive language.
6. Common words shared among racist tweets of different groups are trump, cause, attention, today which don't make much sense.
7. We didn't identify strong connection among typical users. However among key white supremacist tweeter account, we found out Richard B. Spencer and Westland Will, FreeSpeechJim and TomBurchett have strongest connection among their tweets - meaning utilization of similar texts and words.






   

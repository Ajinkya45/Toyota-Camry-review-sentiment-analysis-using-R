# Toyota-Camry-review-sentiment-analysis-using-R

This shiny app is created to perform sentiment analysis on toyota camry car reviews from cars.com
Methodology used - 
1) Done web scrapping to scrape reviews from cars.com
2) Normalized each review - removed special characters, converted words to lower case and assighned tags to review based on presence of word 'service', 'price', interior'.
3) Sentiment score calculated using 'AFIN' lexicon.
4) Scaled that score to 1 to 5 level to comapre it with star rating given by users
5) created model to predict star rating based on sentiment score.
6) Visualized top words by tf-idf score in each tag.

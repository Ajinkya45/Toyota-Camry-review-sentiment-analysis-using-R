shinyServer(function(input, output) {
# web scrapping  
  # to create data frame with year, review count and sublink
  yearReviewCount <- function(url, year){
    webpage <- read_html(url)
    
    # extracting sub links
    sublinks <- html_nodes(webpage, '.star-rating-wrapper')
    year_url <- html_attr(sublinks, 'href')
    
    #etracting year
    review_year <- html_nodes(webpage, '.cui-accordion-section__header h3')
    review_year <- html_text(review_year)
    
    # extracting review count
    review_count <- html_nodes(webpage, '.star-rating__consumer-review span')
    review_count <- str_sub(html_text(review_count), 1, 3)
    
    # data frame with year, review count and sublink is craeted
    review_count_df <- data.frame(review_year, review_count, year_url,stringsAsFactors = FALSE)
    review_count_df <- subset(review_count_df, review_year %in% year)
  }
  
  # to scrape reviews from cars.com
  reviews <- function(url, year, updateProgress = NULL){
    review_df <- data.frame()
    yearReviewCount_df <- yearReviewCount(url, year)
    
    for (ind in 1:length(yearReviewCount_df[,1])){
      # sub URL is created
      sub_url <- paste0('https://www.cars.com', yearReviewCount_df[ind,'year_url'],
                        '?nr=', yearReviewCount_df[ind,'review_count'])
      page <- read_html(sub_url)
      
      # exctracting ratings 
      ratings <- html_nodes(page, '.cr-star-rating')
      ratings <- html_attr(ratings, 'rating')
      
      # extracting reviews
      review <- html_nodes(page, '.mmy-reviews__blurb span')
      review <- html_text(review)
      
      # data frame with year, ratings and reviews is created
      review_data <- data.frame(year = yearReviewCount_df[ind,'review_year'], ratings, review,stringsAsFactors = FALSE)
      review_df <- data.frame(rbind(review_df, review_data),stringsAsFactors = FALSE)
      
      # updating progress in progress bar
      if (is.function(updateProgress)) {
        updateProgress()
      }
    }
    return(review_df)
  }
  
  # to create tags and calculate sentiment of review
  reviews_df <- function(url, task){
    # creating progress object
    progress <- shiny::Progress$new()
    progress$set(message = task, value = 0)
    on.exit(progress$close())
    
    # updating progress object
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value <- value + (progress$getMax() / 6)
      }
      progress$set(value = value, detail = detail)
    }
    
    review_df <- reviews(url, 2012:2017, updateProgress)
    
    # creating tidy review data frame
    tidy_review <- review_df %>%
      group_by(year) %>%
      mutate(review_no = row_number()) %>%                                               
      ungroup() %>%
      unnest_tokens('word', 'review')
    
    # normalised review data frame
    tidy_r <- tidy_review %>% 
      group_by(year, review_no) %>% 
      mutate(normalized_review = paste0(word, collapse = " ")) %>%
      ungroup() %>%
      subset(select = -c(word)) %>%
      distinct()
    
    review_df <- data.frame(cbind(review_df, tidy_r[c('review_no','normalized_review')]))
    
    # creating different tags
    review_df <- review_df %>%
      mutate(service = str_detect(review_df$normalized_review,'service'),
             price = str_detect(review_df$normalized_review,'price'),
             handling = str_detect(review_df$normalized_review,'handling'),
             interior = str_detect(review_df$normalized_review,'interior'))
    
    for(i in 1:nrow(review_df)){
      tags <- ""
      if(review_df$service[i] == TRUE)
        tags <- paste(tags, 'Service', sep = ' ')
      if(review_df$price[i] == TRUE)
        tags <- paste(tags, 'Price', sep = ' ')
      if(review_df$handling[i] == TRUE)
        tags <- paste(tags, 'Handling', sep = ' ')
      if(review_df$interior[i] == TRUE)
        tags <- paste(tags, 'Interior', sep = ' ')
      review_df[i,'tag'] = tags
    }
    
    # calculating sentiment score using 'AFINN'
    review_score <- tidy_review %>%
      inner_join(get_sentiments('afin'), by='word') %>%
      group_by(year, review_no) %>%
      summarise(Sentiment_Score = sum(score))
    
    review_df <- review_df %>%
      inner_join(review_score, by=c("year", 'review_no')) %>%
      subset(select=c(year, ratings, review, normalized_review, tag, Sentiment_Score))
  }
  
  # to show training data set
  output$Train <- DT::renderDataTable({
    if (input$Enter == 0)
      return()
    # new progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Scraping Training Data", value = 0)
    on.exit(progress$close())
    
    # updating progress bar
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() / 5)
      }
      progress$set(value = value, detail = detail)
    }
    
    input$Enter
    url <- isolate({input$DOwnloadLink})
    review_df <- reviews(url, 2012:2016, updateProgress)
    
  })
  
  # to show test data set
  output$Test <- DT::renderDataTable({
    if (input$Enter == 0)
      return()
    # new progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Scraping Test Data", value = 0)
    on.exit(progress$close())
    
    # to update progress object
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value <- value + (progress$getMax() / 2)
      }
      progress$set(value = value, detail = detail)
    }
    
    input$Enter
    url <- isolate({input$DOwnloadLink})
    review_df <- reviews(url, 2017, updateProgress)
  })
  
  # to show data frame with normalized review, tags and sentiment score
  output$Score <- DT::renderDataTable({
    if (input$Enter == 0)
      return()
    input$Enter
    url <- isolate({input$DOwnloadLink})
    review_df <- reviews_df(url,'Calculating Sentiment Score')
  })
  
  # comparing average rating and average sentiment score
  output$Avg_Score <- DT::renderDataTable({
    if (input$Enter == 0)
      return()
    input$Enter
    url <- isolate({input$DOwnloadLink})
    review_df <- reviews_df(url, 'Comparing Rating and Score')
    review_df <- subset(review_df, year %in% 2012:2016)
    
    # rescaling review on scale of to 5 
    review_df$normalized_score <- round(rescale(review_df$Sentiment_Score, to=c(1,5)),2)
    review_df$ratings <- as.integer(review_df$ratings)
    avg_rating <- round(mean(review_df$ratings),2)
    avg_score <- round(mean(review_df$normalized_score),2)
    
    average <- data.frame('Title' = 'Train_Reviews', 'Average Rating' = avg_rating, 
                          'Average Score' = avg_score, stringsAsFactors = FALSE)
    review_df$tag <- trimws(review_df$tag, "both")
    review_df <- review_df %>%
      separate(tag, c('tag1','tag2','tag3'), sep=" ", fill = 'left')
    
    # calculating average ratings and sentiment score for each tag
    tags <- c('Service', 'Price', 'Handling', 'Interior')
    for (tag in tags)
    {
      one <- subset(review_df, tag1 == tag, select=c(year, ratings, normalized_score))
      two <- subset(review_df, tag2 == tag, select=c(year, ratings, normalized_score))
      three <- subset(review_df, tag3 == tag, select=c(year, ratings, normalized_score))
      
      df <- rbind(one, two, three, stringsAsFactors = FALSE)
      avg_rating <- round(mean(df$ratings),2)
      avg_score <- round(mean(df$normalized_score),2)
      
      avg <- data.frame('Title' = tag, 'Average Rating' = avg_rating, 
                            'Average Score' = avg_score, stringsAsFactors = FALSE)
      
      average = rbind(average, avg)
    }
    average
  })
  
  # to show results of prediction model
  output$pred <- renderPrint({
    if (input$Enter == 0)
      return()
    input$Enter
    url <- isolate({input$DOwnloadLink})
    review_df <- reviews_df(url, 'Running Model')
    
    train <- subset(review_df, year %in% 2012:2016)
    test <- subset(review_df, year == 2017, select=c('ratings','Sentiment_Score'))
    train$ratings <- as.factor(train$ratings)
    test$ratings <- as.factor(test$ratings)
    
    # used decison tree to model rating against sentiment score
    fit <- rpart(ratings ~ Sentiment_Score,
                 method="class", data=train)
    predicted= predict(fit,test['Sentiment_Score'], type='class')
    confusionMatrix(predicted, test$ratings)
  })
  
  # to show top ten words with highest tf-idf in each tag
  output$vis <- renderPlot({
    if (input$Enter == 0 )
      return()
    input$Enter
    tag_review <- data.frame()
    url <- isolate({input$DOwnloadLink})
    review_df <- reviews_df(url, 'Generating Plot')
    review_df <- subset(review_df, year %in% 2012:2016)
    
    review_df$tag <- trimws(review_df$tag, "both")
    review_df <- review_df %>%
      separate(tag, c('tag1','tag2','tag3'), sep=" ", fill = 'left')
    
    # creating datframe separted by each tag 
    tags <- c('Service', 'Price', 'Handling', 'Interior')
    for (tag in tags)
    {
      one <- subset(review_df, tag1 == tag, select=c(year, ratings, review))
      two <- subset(review_df, tag2 == tag, select=c(year, ratings, review))
      three <- subset(review_df, tag3 == tag, select=c(year, ratings, review))
      
      df <- rbind(one, two, three, stringsAsFactors = FALSE)
      df <- mutate(df, tag_name = tag)
      tag_review <- data.frame(rbind(tag_review, df), stringsAsFactors = FALSE)
    }  
    # adding few stop words to existing list in tidytext
    sw <- data.frame(word=c('car','cars','toyota','camry','se', 'bought', 'model', 'le', 
                            '2012','2007','2009','makes', 'david', 'gonna'))
    stopWords <- data.frame(rbind(stop_words[1], sw), stringsAsFactors = FALSE)
    word_count <- tag_review %>%
      unnest_tokens('word', 'review') %>%
      anti_join(stopWords, by='word') %>%
      group_by(tag_name) %>%
      count(word, sort=TRUE) %>%
      ungroup()
    
    # calculating tf-idf 
    word_tf_idf <- word_count %>%
      bind_tf_idf(word, tag_name, n)
      
    # generating plot for top 10 words in each tag 
    word_tf_idf %>%
      arrange(desc(tf_idf)) %>%
      group_by(tag_name) %>%
      top_n(10, tf_idf) %>%
      ungroup() %>%
      ggplot(aes(x=reorder(word,tf_idf), tf_idf, fill = tag_name)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~tag_name, ncol = 2, scales = "free") +
        coord_flip()
  })
  
})

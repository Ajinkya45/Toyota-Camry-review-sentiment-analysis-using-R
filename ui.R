# loading required packages
packages <- c("shiny","DT","stringr","magrittr","rvest","dplyr","ggplot2",
              "tidytext","scales","tidyr","rpart","caret")
for(package in packages)
{
  if(require(package,character.only = TRUE)==FALSE)
  {
    install.packages(package)
    library(package)
  }
}

shinyUI(fluidPage(

  # Application title
  titlePanel("Ajinkya_Rane_NLP"),

  # Sidebar with a text input for URL and action button
  sidebarLayout(
    sidebarPanel(
      textInput('DOwnloadLink','Please Provide a link to download data:'),
#      tags$hr(),
      actionButton("Enter", "Download")
#      tags$hr()
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Training Data",DT::dataTableOutput("Train")),      # to display training data
        tabPanel("Test Data",DT::dataTableOutput("Test")),           # to display test data
        tabPanel("Sentiment Score",DT::dataTableOutput("Score")),    # to display sentiment score
        tabPanel("Average Score",DT::dataTableOutput("Avg_Score")),  # to compare sentiment score and rating
        tabPanel("Prediction", verbatimTextOutput("pred")),          # to show prediction results
        tabPanel("Vis", plotOutput("vis")),                          # to show top 10 words by tags
        type = "pills"          
      )
    )
  )
))

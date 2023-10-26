# Load required packages
library(shiny)
library(tm)
library(wordcloud)
library(dplyr)

# data set from https://www.kaggle.com/datasets/subhajeetdas/hate-comment

# Filter data-set to only include 
data <- read.csv("hate.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
negative_comments <- data %>%
  filter(label == "P") %>%
  select(comment)

# Define UI
ui <- fluidPage(
  titlePanel("Top words found in comments that are considered a Hate comment"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_words", "Number of words to include in word cloud:",
                  min = 50, max = 500, value = 100)
    ),
    mainPanel(
      plotOutput("wordcloud", width = "600px", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to process the pre-loaded CSV data
  process_text <- reactive({
    text <- data$comment
    text <- tolower(text)
    text <- gsub("[[:punct:]]", "", text)
    text <- unlist(strsplit(text, "\\s+"))
    
    # Remove specific words, e.g., "people"
    text <- text[!text %in% c("people", "can", "just", "will", "get", "like", "dont")]
    
    return(text)
  })
  
  # Generate the word cloud using the processed text
  output$wordcloud <- renderPlot({
    wordcloud(process_text(), max.words = input$num_words)
  })
}

# Create the Shiny app by combining UI and server
shinyApp(ui = ui, server = server)


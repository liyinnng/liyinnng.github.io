library(shiny)
library(ggplot2)

social_media_health <- read.csv("social-media-and-mental-health.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  titlePanel("Social Media and Mental Health"),
  sidebarLayout(
    sidebarPanel(
      # selector for age group
      selectInput(
        inputId = "select_gender",
        label = "Select Gender",
        choices = c("All", "Female", "Male"),
        selected = "All"
      )
    ),
    mainPanel(
      plotOutput("bar_plot1"),
      plotOutput("bar_plot2"),
      height = "1000px"
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Load your dataset
  social_media_health <- read.csv("social-media-and-mental-health.csv")
  
  # Create a reactive expression for filtered data
  filtered_data <- reactive({
    if (input$select_gender == "All") {
      return(social_media_health)
    } else {
      return(social_media_health[social_media_health$X2..Gender == input$select_gender, ])
    }
  })
  
  # Render the filtered data in a graph
  output$filtered_data_table <- renderDataTable({
    filtered_data()
  })
  # Create two bar plots
  output$bar_plot1 <- renderPlot({
    filtered <- filtered_data()
    counts <- table(filtered$X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media.)
    bar_data <- data.frame(Comparison = names(counts), Count = as.numeric(counts))
    ggplot(bar_data, aes(x = Comparison, y = Count)) +
      geom_bar(stat = "identity") +
      labs(title = "Comparison Frequency",
           x = "How often do you compare yourself to other successful people through SM?",
           y = "Count")
  })
  
  output$bar_plot2 <- renderPlot({
    filtered <- filtered_data()
    counts <- table(filtered$X16..Following.the.previous.question..how.do.you.feel.about.these.comparisons..generally.speaking.)
    bar_data <- data.frame(Feelings = names(counts), Count = as.numeric(counts))
    ggplot(bar_data, aes(x = Feelings, y = Count)) +
      geom_bar(stat = "identity") +
      labs(title = "Feelings",
           x = "What are your feelings following the previous question?",
           y = "Count")
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

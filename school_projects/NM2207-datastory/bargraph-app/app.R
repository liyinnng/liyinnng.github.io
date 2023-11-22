library(shiny)
library(echarts4r)
library(dplyr)

# Load your dataset
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
      echarts4rOutput("bar_plot1"),
      echarts4rOutput("bar_plot2"),
      height = "1000px"
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Create a reactive expression for filtered data
  filtered_data <- reactive({
    if (input$select_gender == "All") {
      return(social_media_health)
    } else {
      return(social_media_health[social_media_health$X2..Gender == input$select_gender, ])
    }
  })
  
  # Create two echarts4r bar plots
  output$bar_plot1 <- renderEcharts4r({
    filtered <- filtered_data()
    counts <- table(filtered$X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media.)
    bar_data <- data.frame(Comparison = names(counts), Count = as.numeric(counts))
    
    echarts4r() %>%
      e_bar(Comparison, Count) %>%
      e_title("Comparison Frequency") %>%
      e_x_axis(name = "How often do you compare yourself to other successful people through SM?") %>%
      e_y_axis(name = "Count")
  })
  
  output$bar_plot2 <- renderEcharts4r({
    filtered <- filtered_data()
    counts <- table(filtered$X16..Following.the.previous.question..how.do.you.feel.about.these.comparisons..generally.speaking.)
    bar_data <- data.frame(Feelings = names(counts), Count = as.numeric(counts))
    
    echarts4r() %>%
      e_bar(Feelings, Count) %>%
      e_title("Feelings") %>%
      e_x_axis(name = "What are your feelings following the previous question?") %>%
      e_y_axis(name = "Count")
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


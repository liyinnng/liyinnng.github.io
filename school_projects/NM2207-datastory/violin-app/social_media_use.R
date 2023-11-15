library(shiny)
library(plotly)
library(dplyr)

# Read data
social_media_health <- read.csv("social-media-and-mental-health.csv")

# Filter age outliers
social_media_health <- social_media_health %>%
  filter(X1..What.is.your.age. != 91)

# Define UI
ui <- fluidPage(
  titlePanel("Social Media Health Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Choose a variable to analyze:",
        choices = c(
          "Frequency of facing issues regarding sleep",
          "Difficulty in concentrating on things",
          "Being bothered by worries"
        ),
        selected = "Frequency of facing issues regarding sleep"
      )
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  create_heatmap <- function(variable) {
    data <- social_media_health
    title <- ""
    y_label <- ""
    
    if (variable == "Frequency of facing issues regarding sleep") {
      title <- "Comparison of Sleep Quality by Social Media Use and Sleep Issues"
      y_label <- "X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep."
    } else if (variable == "Difficulty in concentrating on things") {
      title <- "Comparison of Concentration Ability"
      y_label <- "X14..Do.you.find.it.difficult.to.concentrate.on.things."
    } else if (variable == "Being bothered by worries") {
      title <- "Comparison of Worries Botheration"
      y_label <- "X13..On.a.scale.of.1.to.5..how.much.are.you.bothered.by.worries."
    }
    
    if (title == "" || y_label == "") {
      return(NULL)
    }
    
    heatmap_data <- table(data$X6..Do.you.use.social.media., data[[y_label]])
    
    if (sum(heatmap_data) == 0) {
      # Handle case where the heatmap data is empty
      return(NULL)
    }
    
    # Transpose the data matrix
    heatmap_data_transposed <- t(heatmap_data)
    
    # Create custom hover text
    hover_text <- matrix(
      paste("Frequency: ", heatmap_data_transposed),
      nrow = nrow(heatmap_data_transposed),
      ncol = ncol(heatmap_data_transposed),
      byrow = TRUE
    )
    
    heatmap <- plot_ly(
      x = colnames(heatmap_data_transposed),
      y = rownames(heatmap_data_transposed),
      z = as.matrix(heatmap_data_transposed),
      type = "heatmap",
      colorscale = list(c(0,1), c("#FFFFFF", "#FACA5E")),
      colorbar = list(title = "Frequency")
    ) %>% 
      layout(
        title = title,
        xaxis = list(title = "Do you use social media?"),
        yaxis = list(title = y_label)
      )
    
    return(heatmap)
  }
  
  output$plot <- renderPlotly({
    variable <- input$variable
    heatmap <- create_heatmap(variable)
    if (!is.null(heatmap)) {
      heatmap
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
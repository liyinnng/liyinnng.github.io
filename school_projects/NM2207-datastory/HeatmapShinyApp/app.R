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
          "Frequency of issues with sleep",
          "Concentration Ability",
          "Being bothered by worries"
        ),
        selected = "Frequency of issues with sleep"
      )
    ),
    mainPanel(
      plotlyOutput("plot"), 
      height = "800px"
    )
  )
)

# Define server
server <- function(input, output) {
  create_heatmap <- function(variable) {
    data <- social_media_health
    title <- ""
    y_label <- ""
    
    if (variable == "Frequency of issues with sleep") {
      title <- "Frequency of issues with sleep"
      y_label <- "X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep."
    } else if (variable == "Concentration Ability") {
      title <- "Concentration Ability"
      y_label <- "X14..Do.you.find.it.difficult.to.concentrate.on.things."
    } else if (variable == "Being bothered by worries") {
      title <- "Being bothered by worries"
      y_label <- "X13..On.a.scale.of.1.to.5..how.much.are.you.bothered.by.worries."
    }
    
    if (title == "" || y_label == "") {
      return(NULL)
    }
    
    heatmap_data <- table(data$X6..Do.you.use.social.media., data[[y_label]])
    
    if (sum(heatmap_data) == 0) {
      return(NULL)
    }
    
    # Transpose the data matrix
    heatmap_data_transposed <- t(heatmap_data)
    
    # Custom y-axis labels
    custom_y_names <- c("Frequency of facing issues regarding sleep",
                        "Difficulty in concentrating on things",
                        "Comparison of being bothered by Worries")
    
    # Create custom hover texts
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
        yaxis = list(title = input$variable)
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
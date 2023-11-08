library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)

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
          "Difficulty in concentrating on things"
        ),
        selected = "Frequency of facing issues regarding sleep"
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  label_mapping <- list(
    "X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep." = "Frequency of Sleep Issues",
    "X14..Do.you.find.it.difficult.to.concentrate.on.things." = "Difficulty in Concentrating on Things"
  )
  
  create_plot <- function(variable) {
    data <- social_media_health
    title <- ""
    y_label <- ""
    
    if (variable == "Frequency of facing issues regarding sleep") {
      title <- "Comparison of Sleep Quality by Social Media Use and Sleep Issues"
      y_label <- "X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep."
    } else if (variable == "Difficulty in concentrating on things") {
      title <- "Comparison of Concentration Ability"
      y_label <- "X14..Do.you.find.it.difficult.to.concentrate.on.things."
    }
    
    if (title == "" || y_label == "") {
      return(NULL)
    }
    
    gg <- ggplot(data, aes(x = X6..Do.you.use.social.media., y = !!sym(y_label))) +
      geom_violin(aes(fill = X6..Do.you.use.social.media.), scale = "width") +
      labs(
        title = title,
        caption = "Note: 1 being no difficulty faced, and 5 being very difficult",
        x = "Do you use social media?",
        y = label_mapping[[y_label]], 
        fill = "Do you use social media"
      ) +
      scale_fill_manual(values = c("No" = "#D64045", "Yes" = "#9FC0DE"))
    
    return(gg)
  }
  
  output$plot <- renderPlot({
    variable <- input$variable
    plot <- create_plot(variable)
    if (!is.null(plot)) {
      print(plot)
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)

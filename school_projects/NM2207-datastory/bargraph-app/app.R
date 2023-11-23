library(shiny)
library(echarts4r)
library(dplyr)
library(tidyr)
library(magrittr)

# Load your dataset
social_media_health <- read.csv("social-media-and-mental-health.csv")

social_media_health <- mutate(social_media_health,
                              age_group = case_when(
                                X1..What.is.your.age. <= 25 ~ "Child",
                                between(X1..What.is.your.age., 25, 30) ~ "Youth",
                                between(X1..What.is.your.age., 31, 45) ~ "Adults",
                                X1..What.is.your.age. > 45 ~ "Old-aged adults",
                                TRUE ~ NA_character_
                              ))

# Function to filter data based on selected gender
filter_data <- function(data, agegroup) {
  if (agegroup == "All ages") {
    return(data)
  } else {
    return(filter(data, age_group == agegroup))
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Social Media and Mental Health"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "select_agegroup", 
        label = "Select Age Group", 
        choices = c("All ages", "Child", "Youth", "Adults", "Old-aged adults"),
        selected = "All ages"
      )
    ),
    mainPanel(
      echarts4rOutput("myChart")
    )
  )
)

# Define server
server <- function(input, output) {
  # Reactive function to filter data based on selected gender
  filtered_data <- reactive({
    filter_data(social_media_health, input$select_agegroup)
  })
  
  observe({
    df_X15 <- filtered_data() %>%
      group_by(X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media.) %>%
      summarise(count_X15 = n())
    
    df_X16 <- filtered_data() %>%
      group_by(X16..Following.the.previous.question..how.do.you.feel.about.these.comparisons..generally.speaking.) %>%
      summarise(count_X16 = n())
    
    # Merge the two data frames by their respective columns
    df <- full_join(df_X15, df_X16, by = c(
      "X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media." = "X16..Following.the.previous.question..how.do.you.feel.about.these.comparisons..generally.speaking."
    ))
    
    # Calculate the minimum non-missing value for X15
    min_X15 <- ifelse(all(is.na(df$X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media.)),
                      0, min(df$X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media., na.rm = TRUE))
    
    output$myChart <- renderEcharts4r({
      df %>%
        e_charts(
          `X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media.`,
          axisPointer = list(
            link = list(
              xAxisIndex = "all"
            )
          )
        ) %>%
        e_x_axis(min = min_X15) %>%
        e_line(count_X15, name = "comparison frequency") %>%
        e_area(count_X16, name = "comparison feelings") %>%
        e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.seriesName + ': ' + params.value[1];}")) %>%
        e_theme("macarons2")
    })
  })
}

# Create Shiny app
shinyApp(ui, server)


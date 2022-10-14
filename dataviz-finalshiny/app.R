#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Import packages
library(shiny)
library(tidyverse)
library(ggplot2)

# Load the spotify data
spotify <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# Modify some data
spotify <- spotify %>% 
  na.omit() %>% 
  mutate(mode = as.factor(mode))
  

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Spotify Data EDA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bandwidth",
                  "Choice of bandwidth:",
                  min = 10,
                  max = 80,
                  value = 30),
      
      selectizeInput("artist",
                     "Artist:",
                     unique(spotify$track_artist),
                     selected = "Maroon 5"),
      
      checkboxGroupInput("genre",
                         "Playlist Genre:",
                         unique(spotify$playlist_genre),
                         selected = "pop")
      
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("characterPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$characterPlot <- renderPlot({
    spotify %>% 
      na.omit() %>% 
      filter(track_artist == input$artist) %>%
      filter(playlist_genre == input$genre) %>% 
      ggplot(aes(x = tempo, y = track_popularity)) +
      geom_density2d(h = c(input$bandwidth, input$bandwidth)) +
      geom_point(aes(color = mode), alpha = .5, size = 2) + 
      xlim(0, 200) +
      ylim(0, 100) +
      coord_fixed() +
      labs(x = "Tempo", y = "Popularity", colour = "Mode")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

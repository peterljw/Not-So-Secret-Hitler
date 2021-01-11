library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Not-So-Secret Hitler"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("player1", h4("President"), 
                value = "Enter player name..."),
      selectInput("player1_claim","", 
                  choices = list("3L", 
                                 "2L+1F",
                                 "1L+2F",
                                 "3F"), selected = "3L"),
      textInput("player2", h4("Chancellor"), 
                value = "Enter player name..."),
      selectInput("player2_claim","", 
                  choices = list("2L", 
                                 "1L+1F",
                                 "2F"), selected = "2L"),
      radioButtons("outcome", h4("Outcome"),
                   choices = list("Liberal" = "L", "Fascist" = "F"),selected = "L"),
      actionButton("update", "Update",icon('feather-alt'),
                   style="color: #fff; background-color: #337ab7"),
      br(),
      br(),
      actionButton("undo", "Undo",icon('undo'),
                   style="color: #fff; background-color: #FFC300"),
      actionButton("reset", "Reset",icon('trash'),
                   style="color: #fff; background-color: #FF4833")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Next Round's Projected Probability"),
      DT::dataTableOutput('probability_table'),
      h4("Match History"),
      DT::dataTableOutput("history_table")
      # tabsetPanel(type = "tabs",
      #             tabPanel("Projected Probability", DT::dataTableOutput('probability_table')),
      #             tabPanel("Match History", DT::dataTableOutput("history_table"))
      #             )
    )
  )
))

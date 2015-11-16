library(shiny)
library(ggvis)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel('Farts'),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        selectInput("xaxis", 
                    label = "Choose a variable to display on x axis",
                    choices =names(bigData),
                    selected = names(bigData)[[12]]),
        selectInput("yaxis", 
                    label = "Choose a variable to display on y axis",
                    choices =names(bigData),
                    selected = names(bigData)[[2]])
        )
    
        
    ),
   
    # Show a plot of the generated distribution
    mainPanel( ggvisOutput("distPlot")
      )
    )#,
#     fluidRow(
#           column(width = 3,
#              verbatimTextOutput("hover_info")
#     
#       )
    )
    
    )
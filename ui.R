library(shiny)
library(ggvis)
# Define UI for application that draws a histogram

add_input_selector <- function(x_label,y_label){
  fluidRow(
    selectInput(x_label, 
                label = "Choose a variable to display on x axis",
                choices =names(bigData),
                selected = names(bigData)[[12]]
    ),
    selectInput(y_label, 
                label = "Choose a variable to display on y axis",
                choices =names(bigData),
                selected = names(bigData)[[2]]
    )
  )
  
}
shinyUI(fluidPage(
  
  # Application title
  titlePanel('Farts'),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(h5('filter menu here')
                
         
       
    ),
   
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(div(style = 'height:"50%";'),
        column(6, 
                actionButton('toggle1','Toggle Show/Edit', icon = icon("cog", lib = "glyphicon")),
                      conditionalPanel(condition = 'input.toggle1 % 2 == 0',
                                add_input_selector('x1','y1')
                                ),
               conditionalPanel('input.toggle1 % 2 == 1', ggvisOutput("plot1"))
               ),
               column(3,
                      actionButton('toggle2','Toggle Show/Edit', icon = icon("cog", lib = "glyphicon")),
                      conditionalPanel(condition = 'input.toggle2 % 2 == 0',
                                       add_input_selector('x2','y2')
                      ),
                      conditionalPanel('input.toggle2 % 2 == 1', ggvisOutput("plot2"))
               )
        ),
      fluidRow(
        column(6,
               actionButton('toggle3','Toggle Show/Edit', icon = icon("cog", lib = "glyphicon")),
               conditionalPanel(condition = 'input.toggle3 % 2 == 0',
                                add_input_selector('x3','y3')
               ),
               conditionalPanel('input.toggle3 % 2 == 1', ggvisOutput("plot3"))
        ),
        column(3,
               actionButton('toggle4','Toggle Show/Edit', icon = icon("cog", lib = "glyphicon")),
               conditionalPanel(condition = 'input.toggle4 % 2 == 0',
                                add_input_selector('x4','y4')
               ),
               conditionalPanel('input.toggle4 % 2 == 1', ggvisOutput("plot4"))
        )
      )
      )
      
      )))
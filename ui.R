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
  titlePanel('NeuroElectro Visuals'),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(h5('filter menu here'), width = 2
                
         
       
    ),
   
    # Show a plot of the generated distribution
    mainPanel(
      actionButton('toggle1','Toggle Show/Edit', icon = icon("cog", lib = "glyphicon")),
       fluidRow(column(12,
               conditionalPanel(condition = 'input.toggle1 % 2 == 0',
                                fluidRow(style="padding: 0px 0px 0px 40px;",h2('Select variables to plot')),
                                fluidRow(column(6,fluidRow(style="padding: 50px 0px 150px 40px;",add_input_selector('x1','y1')), 
                                fluidRow(style="padding: 0px 0px 0px 40px;",add_input_selector('x2','y2'))),
                                column(6, fluidRow(style="padding: 50px 0px 150px 40px;",add_input_selector('x3','y3')),
                                fluidRow(style="padding: 0px 0px 0px 40px;",add_input_selector('x4','y4')))
                                )),
               conditionalPanel('input.toggle1 % 2 == 1',
                                fluidRow(
                                  column(6, style="padding: 0px 100px 0px 0px;",ggvisOutput("plot1"), ggvisOutput('plot2')),
                                  column(6, ggvisOutput("plot3"), ggvisOutput('plot4'))
                                )
               )
      )
      ), fluid =TRUE
      
      ))))
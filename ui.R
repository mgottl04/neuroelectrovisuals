# Define UI for application that draws a histogram

# Control panel

nt_panel_contents = shinyTree("nt_tree", checkbox = TRUE, search = TRUE, dragAndDrop = FALSE)
ephys_panel_contents = sliderInput("integer", "Integer:", 
                          min=0, max=1000, value=500)
metadata_panel_contents = list(sliderInput("yoyo", "yoyo:", 
                           min=0, max=1000, value=500),shinyTree("random_tree", checkbox = TRUE, search = TRUE, dragAndDrop = FALSE))

nt_panel = bsCollapsePanel(title = "Neuron Type Filters", nt_panel_contents, style = "info")
ephys_panel = bsCollapsePanel(title = "Ephys Property Filters", ephys_panel_contents, style = "success")
metadata_panel = bsCollapsePanel(title = "Metadata Filters", metadata_panel_contents, style = "warning")

# End control panel

add_input_selector <- function(x_label,y_label){
  fluidRow(
    selectInput(x_label, 
                label = "Choose a variable to display on x axis",
                choices =names(bigData),
                selected = names(bigData)[[8]]
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
    sidebarPanel(width = 3, bsCollapse(nt_panel, ephys_panel, metadata_panel, id = "filterMenu", multiple = TRUE, open = NULL)),
   
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(style='width:1200',column(2,actionButton('toggle1', width = 150,'Toggle Show/Edit', icon = icon("cog", lib = "glyphicon"))),
              
               column(2,actionButton('clearance','Clear Highlighting', width = 150, icon = icon("undo", lib = "font-awesome")),actionButton('restoreRemoved','Restore Removed', width = 150,icon = icon("undo", lib = "font-awesome"))),
               column(2, radioButtons('mode', 'Highlight on:', c('Hover' = 'hover', 'Click' = 'click'),width = 150) ),
               column(2,checkboxInput('remove','Remove on Click', value = FALSE),'*Overrides Highlight on Click')),
       fluidRow(style = 'height: 800px; width: 1200px',column(12,
               conditionalPanel(condition = 'input.toggle1 % 2 == 0',
                                fluidRow(style="padding: 0px 0px 0px 40px;",h2('Select variables to plot')),
                                fluidRow(column(6,style = "width: 50%; height: 50%",fluidRow(style="padding: 50px 0px 150px 40px;",add_input_selector('x1','y1')), 
                                fluidRow(style="padding: 0px 0px 0px 40px;",add_input_selector('x2','y2'))),
                                column(6,style = "width: 50%; height: 50%", fluidRow(style="padding: 50px 0px 150px 40px;",add_input_selector('x3','y3')),
                                fluidRow(style="padding: 0px 0px 0px 40px;",add_input_selector('x4','y4')))
                                )),
               conditionalPanel('input.toggle1 % 2 == 1', #style = 'height: 800px; width: 1200px',
                                fluidRow(style = "padding: 30px 0px 0px 0px",
                                  column(6, style="width: 50%;",ggvisOutput("plot1"), ggvisOutput('plot2')),
                                  column(6, style="width: 50%;",ggvisOutput("plot3"), ggvisOutput('plot4'))
                                )
               )
      )
      ), fluid =TRUE
      
      ))))
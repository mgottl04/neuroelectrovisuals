# Define UI for application that draws a histogram

# Control panel
library(shinyjs)
library(V8)
addSlider <- function(name, units, min, max, step) {
  sliderInput(name,paste(name, " (", units, ")"),min,max,value = c(min,max))
}

nt_panel_contents = shinyTree("nt_tree", checkbox = TRUE, search = TRUE, dragAndDrop = FALSE)
ephys_panel_contents = stuff <- lapply(seq(1,length(prop_names)), function(x) {addSlider(prop_names[x], props[[x,c("usual.units")]],
                                                                           props[[x,c("Min.Range")]],props[[x,c("Max.Range")]])})
organism_panel_contents = list(h3("Species"),shinyTree("species_tree", checkbox = TRUE, search = FALSE, dragAndDrop = FALSE),
                               addSlider("Age","days",min(age),max(age)))

nt_panel = bsCollapsePanel(title = "Neuron Type", nt_panel_contents, style = "info")
organism_panel = bsCollapsePanel(title = "Organism", organism_panel_contents, style = "success")
ephys_panel = bsCollapsePanel(title = "Ephys Properties", ephys_panel_contents, style = "warning")

# End control panel

add_input_selector <- function(x_label,y_label){
  
  fluidRow(
    selectInput(x_label, 
                label = "Choose a variable to display on x axis",
                choices =names(bigData)[axis_names],
                selected = names(bigData)[axis_names][[1]]
    ),
    selectInput(y_label, 
                label = "Choose a variable to display on y axis",
                choices =names(bigData)[axis_names],
                selected = names(bigData)[axis_names][[2]]
    )
  )
  
}

shinyUI(fluidPage(
  
  useShinyjs(),
  extendShinyjs(text = "shinyjs.collapseNodesOnLoad = function(){$.jstree.defaults.core.expand_selected_onload = false;}"),
#   extendShinyjs(text = 'shinyjs.removeStuckToolTip = function(){$("#ggvis-tooltip").remove();}'),
  
  # Application title
  titlePanel('NeuroElectro Visuals'),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(width = 3, bsCollapse(nt_panel, organism_panel,ephys_panel, id = "filterMenu", multiple = TRUE, open = NULL)),
   
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(style='width:1200',column(3,actionButton('toggle1','Toggle Show/Edit', icon = icon("cog", lib = "glyphicon"))),
              
               column(3,actionButton('clearance','Clear Highlighting',  icon = icon("undo", lib = "font-awesome")),actionButton('restoreRemoved','Restore Removed', width = 150,icon = icon("undo", lib = "font-awesome"))),
               column(3, radioButtons('mode', 'Highlight on:', c('Hover' = 'hover', 'Click' = 'click')) ),
               column(3,checkboxInput('remove','Remove on Click', value = FALSE),'*Overrides Highlight on Click')),
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
# toggleState
#runjs(code = "$.jstree.defaults.core.expand_selected_onload = false;")


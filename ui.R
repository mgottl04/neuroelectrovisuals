# *** Control panel logic ***

log2Slider <-
  "shinyjs.log2Slider = function(params) {
    var vals = [0];
    var powStart = 0;
    var powStop = params.max_power;
    for (i = powStart; i <= powStop; i++) {
      var val = Math.pow(2, i);
      val = parseFloat(val.toFixed(8));
      vals.push(val);
    }
    $('#' + params.id).data('ionRangeSlider').update({'values':vals})}"

addSlider <- function(name, units, min, max, step) {
  sliderInput(name,paste(name, " (", units, ")"),min,max,value = c(min,max))
}

nested_ephys_panel_AM <- bsCollapsePanel(lapply(seq(g1_start,g1_end), 
                                      function(x) {addSlider(rownames(props)[x], props[[x,c("usual.units")]],
                                      props[[x,c("Min.Range")]],props[[x,c("Max.Range")]])}), title = "A-M", style = "warning")

nested_ephys_panel_NZ <- bsCollapsePanel(lapply(seq(g2_start,g2_end), 
                                      function(x) {addSlider(rownames(props)[x], props[[x,c("usual.units")]],
                                      props[[x,c("Min.Range")]],props[[x,c("Max.Range")]])}), title = "N-Z", style = "warning")

nt_panel_contents <- shinyTree("nt_tree", checkbox = TRUE, search = TRUE, dragAndDrop = FALSE)
organism_panel_contents <- list(h5("Species",style='font-weight: bold'),
                               fluidRow(column(12,shinyTree("species_tree", checkbox = TRUE, search = FALSE, dragAndDrop = FALSE),style = 'padding-bottom: 15px')),
                               addSlider("Age","days",0,log2(age_max) + 1), addSlider("Weight","grams",floor(min(weight)),ceiling(max(weight))),addSlider("Temperature","C",floor(min(temp)),ceiling(max(temp))))
ephys_panel_contents <- bsCollapse(nested_ephys_panel_AM, nested_ephys_panel_NZ, id = "ephys_panel", multiple = TRUE, open = NULL)

nt_panel <- bsCollapsePanel(title = "Neuron Type Filters", nt_panel_contents, style = "info")
organism_panel <- bsCollapsePanel(title = "Organism Filters", organism_panel_contents, style = "info")
ephys_panel <- bsCollapsePanel(title = "Ephys Property Filters", ephys_panel_contents, style = "info")

# *** Main panel logic ***

add_input_selector <- function(x_label,y_label, border_widths, x_initial, y_initial){
  
  fluidRow(style=paste('background-color: #d9edf7;border-style: solid; border-color: #bce8f1; border-width:',border_widths),
           column(6,style='padding: 5px', selectInput(x_label,
                label = "Choose a variable to display on x axis",
                choices =sort(names(bigData)[axis_names]),
                selected = x_initial
    )),
          column(6,style='padding: 5px', selectInput(y_label, 
                label = "Choose a variable to display on y axis",
                choices =c("---",sort(names(bigData)[axis_names])),
                selected = y_initial
    ))
  )
}

# *** Define UI ***

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "neuroelectrovisuals.css")
  ),
  
  useShinyjs(),
  extendShinyjs(text = "shinyjs.collapseNodesOnLoad = function(){$.jstree.defaults.core.expand_selected_onload = false;}"),
  extendShinyjs(text = 'shinyjs.removeStuckToolTip = function(){$("#ggvis-tooltip").remove();}'),
  extendShinyjs(text = log2Slider),
  
  # Application title
  titlePanel('NeuroElectro Visuals'),
  
  # Control panel sidebar
  sidebarLayout(
    sidebarPanel(width = 3, bsCollapse(nt_panel,organism_panel,ephys_panel,id = "filterMenu", multiple = TRUE, open = NULL)),
   
    mainPanel(
      tabsetPanel(
      tabPanel("Explore",
        fluidRow(style = 'padding: 10px; width: 500px',column(4,  actionButton('clearance','Clear Highlighting',  icon = icon("undo", lib = "font-awesome"))),
           column(4,actionButton('restoreRemoved','Restore Removed',icon = icon("undo", lib = "font-awesome"))),
           column(4,actionButton('remove','Remove Highlighted', icon = icon('remove',lib='font-awesome') ))),
        
        fluidRow(style = 'height: 400px; width: 1320px;padding: 0px 0px 10px 0px',
          column(6, style="width: 48%;border-color: #bce8f1;border-radius: 4px;border-style: solid; border-width: 2px",add_input_selector('x1','y1', '0px 0px 0px 0px',"resting.membrane.potential","spike.threshold"),ggvisOutput("plot1")),
          column(1,style = 'height: 100%;width: 2%'),
          column(6, style="width: 48%;border-color: #bce8f1;border-radius: 4px;border-style: solid; border-width: 2px",add_input_selector('x3','y3','0px 0px 0px 0px',"Species","BrainRegion"),ggvisOutput("plot3"))
          ),
        
        fluidRow(style = 'height: 400px; width: 1320px;padding: 0px 0px 10px 0px',
          column(6, style="margin-top: 20px;width: 48%;border-color: #bce8f1;border-radius: 4px;border-style: solid; border-width: 2px",add_input_selector('x2','y2','0px 0px 0px 0px',"AnimalAge","input.resistance"),ggvisOutput('plot2')),
          column(1,style = 'height: 100%;width: 2%'),
          column(6, style="margin-top: 20px;width: 48%;border-color: #bce8f1;border-radius: 4px;border-style: solid; border-width: 2px",add_input_selector('x4','y4','0px 0px 0px 0px',"Species","---"),ggvisOutput('plot4'))
          )
      ),
      tabPanel("Overview",
               fluidRow(style = 'width: 1400px', 
                        fluidRow(style= 'padding: 0px 0px 15px 0px',column(5,plotOutput("hivePlot", 
                                                            height = 600,
                                                            width = 600,
                                                            hover = hoverOpts(id = "image_hover",
                                                                              delay = 500,
                                                                              delayType = "throttle")
                        )),
                          column(7,plotOutput('freqMat',
                                                   height = 600,
                                                   width = 800))),
                        
                        bsCollapse(bsCollapsePanel(div(dataTableOutput('table'),
                                            style='font-size:75%;'),
                                        title='Table',
                                        style='info'), open = 'Table')
                        )
               )
      ), fluid =TRUE
      )

    )
  )
  )
  

# toggleState
#runjs(code = "$.jstree.defaults.core.expand_selected_onload = false;")


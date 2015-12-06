# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
 
  output$nt_tree <- renderTree({
    region_groups
  })
  
  output$species_tree <- renderTree({
    list(Mice = structure("Mice",stselected=TRUE),
    Rats = structure("Rats",stselected=TRUE),
    Other = structure(as.list(setNames(misc_species,misc_species)),stselected = TRUE))
  })
 
  values <- reactiveValues(selected = rep(1, nrow(bigData)))
  removed <- reactiveValues(selected = rep(FALSE,nrow(bigData)))
  
  observeEvent(input$clearance, {
    values$selected <- rep(1,nrow(bigData))
  })
  observeEvent(input$restoreRemoved, {
    removed$selected <- rep(FALSE,nrow(bigData))
  })
  do_remove <- reactive({input$remove})
  mode <- reactive({input$mode})
  
  make_main_plot <- function(df, x_axis, y_axis){
    
    data_frame <- df()
    data_frame$col <- reactive({values$selected[data_frame$key]})() 
    data_frame$remove <- reactive({removed$selected[data_frame$key]})()
    
    #filter out NA values
    
    data_frame <- data_frame%>%filter((!is.na(data_frame[,as.character(x_axis())]))&
               (!is.na(data_frame[,as.character(y_axis())])))
    
    data_frame[!data_frame$remove,] %>%
      ggvis(x =x_axis(),  y= y_axis(), key := ~key, fill = ~col, size = ~col ) %>% 
      hide_legend(scales = c('fill','size')) %>%
      add_axis('y', properties = axis_props(labels=list(angle = -40,fontSize=10),title=list(fontSize=16,dy = -55)))%>%
      add_axis('x', properties = axis_props(labels=list(angle = -40,fontSize=10, dx = -30,dy=5),title=list(fontSize=16,dy = 50)))%>%
      layer_points() %>%
      set_options(height = 400, width = 600) %>%
      add_tooltip(function(data){
        paste0(       
                 "ID: ", as.character(data$key),"<br>",
                      x_axis(),": ", as.character(data[[1]]), "<br>", y_axis(), ": ", as.character(data[[2]]),"<br>")
      }, "hover")%>%set_options(renderer = "canvas") %>% handle_click(on_click = function(data,...){
             
              if (do_remove()){
                isolate(removed$selected[data$key] <- TRUE)
              } else if (mode() == 'click') {
              
             isolate(values$selected[data$key] <- 2)
              } 
        }  
      
      )%>% handle_hover(on_mouse_over = function(data,...){
        if (mode() == 'hover'){
        isolate(values$selected[values$selected == 2] <- 1)
        isolate(values$selected[data$key] <- 2)
        }
<<<<<<< HEAD
      }) 
=======
      }) #ggvis-tooltip 
    
>>>>>>> bcc13095d439875f03777d14fac2bcfe243bb6de
  }
 
  mtc <- reactive({
    #global filtering will occur in this reactive
    selected_nts <- get_selected(input$species_tree)
    data = bigData 
    data
  })
  
  selected_nts <- reactive({get_selected(input$species_tree)})
  
  x1 <- reactive({as.symbol(input$x1)})
  x2 <- reactive({as.symbol(input$x2)})
  x3 <- reactive({as.symbol(input$x3)})
  x4 <- reactive({as.symbol(input$x4)})
  
  
  y1 <- reactive({as.symbol(input$y1)})
  y2 <- reactive({as.symbol(input$y2)})
  y3 <- reactive({as.symbol(input$y3)})
  y4 <- reactive({as.symbol(input$y4)})
  
 
  reactive({make_main_plot(mtc,x1,y1)})%>%
    bind_shiny('plot1')
  reactive({make_main_plot(mtc,x2,y2)})%>%
    bind_shiny('plot2')
  reactive({make_main_plot(mtc,x3,y3)})%>%
    bind_shiny('plot3')
  reactive({make_main_plot(mtc,x4,y4)})%>%
    bind_shiny('plot4')
 
 js$collapseNodesOnLoad()
})
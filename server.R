
shinyServer(function(input, output, session) {
  
  js$collapseNodesOnLoad()
  js$log2Slider(id = "Age", max_power = log2(age_max))
 
  output$nt_tree <- renderTree({
    list(All = structure(region_groups,stselected = TRUE,stopened= TRUE))
  })
  
  output$species_tree <- renderTree({
    list(All = structure(list(Mice = structure("Mice",stselected=TRUE),
    Rats = structure("Rats",stselected=TRUE),
    Others = structure(as.list(setNames(misc_species,misc_species)),stselected = TRUE)),stopened=TRUE))
  })
 
  values <- reactiveValues(selected = rep(1, nrow(bigData)))
  removed <- reactiveValues(selected = rep(FALSE,nrow(bigData)))
 
   observeEvent(input$toggle1,{
    js$removeStuckToolTip()
  })
  
  observeEvent(input$clearance, {
    values$selected <- rep(1,nrow(bigData))
    js$removeStuckToolTip()
  })
  
  observeEvent(input$restoreRemoved, {
    removed$selected <- rep(FALSE,nrow(bigData))
  })
  
  do_remove <- reactive({input$remove})
  
  make_main_plot <- function(df, x_axis, y_axis){
    
    data_frame <- df()
    data_frame$col <- reactive({values$selected[data_frame$key]})() 
    data_frame$remove <- reactive({removed$selected[data_frame$key]})()
    
    x_axis_col <- as.character(x_axis())
    y_axis_col <- as.character(y_axis())
    x_axis_lab <- x_axis_col
    y_axis_lab <- y_axis_col
    
    #filter out NA values
    if (y_axis_lab == "---") { 
      data_frame <- data_frame %>% filter(!is.na(data_frame[,x_axis_lab])) 
    }
    else { 
      data_frame <- data_frame %>% filter(!is.na(data_frame[,x_axis_lab]) & !is.na(data_frame[,y_axis_lab])) 
    }
    
    # Add units for ephys props (TODO - get units for metadata)
    if (x_axis_lab %in% rownames(props)) {
      x_axis_lab <- paste(x_axis_lab, " (", props[[x_axis_lab,c("usual.units")]], ")")
    }
    
    # Add units for ephys props
    if (y_axis_lab %in% rownames(props)) {
      y_axis_lab <- paste(y_axis_lab, " (", props[[y_axis_lab,c("usual.units")]], ")")
    }
    
    # Just one variable selected - show frequency bars
    if (y_axis_lab == "---") {
      # No data - make an empty plot
      if (nrow(data_frame[!data_frame$remove,]) == 0) {
        data_frame[!data_frame$remove,] %>% ggvis(x=x_axis(),y=x_axis()) %>% layer_points() %>%
        add_axis('y', title = "Count", properties = axis_props(labels=list(angle = -40,fontSize=10),title=list(fontSize=16,dy = -55)))%>%
        add_axis('x', title = x_axis_lab, properties = axis_props(labels=list(angle = -40,fontSize=10, dx = -30,dy=5),title=list(fontSize=16,dy = 50)))
      }
      # Categorical - make a frequency bar chart
      else if (!is.numeric(data_frame[,x_axis_col])) {
        data_frame[!data_frame$remove,] %>% ggvis(x=x_axis()) %>% layer_bars() %>%
        add_axis('y', title = "Count", properties = axis_props(labels=list(angle = -40,fontSize=10),title=list(fontSize=16,dy = -55)))%>%
        add_axis('x', title = x_axis_lab, properties = axis_props(labels=list(angle = -40,fontSize=10, dx = -30,dy=5),title=list(fontSize=16,dy = 50)))
      }
      # Continous - make a histogram
      else {
        data_frame[!data_frame$remove,] %>% ggvis(x=x_axis()) %>% layer_histograms() %>%
        add_axis('y', title = "Count", properties = axis_props(labels=list(angle = -40,fontSize=10),title=list(fontSize=16,dy = -55)))%>%
        add_axis('x', title = x_axis_lab, properties = axis_props(labels=list(angle = -40,fontSize=10, dx = -30,dy=5),title=list(fontSize=16,dy = 50)))
      }
    } 
  
    # Both variables categorical - make a frequency matrix
    else if (!is.numeric(data_frame[,x_axis_col]) && !is.numeric(data_frame[,y_axis_col])) {
      # No data - make an empty plot
      if (nrow(data_frame[!data_frame$remove,]) == 0) {
        data_frame[!data_frame$remove,] %>% ggvis(x=x_axis(),y=x_axis()) %>% layer_points() %>%
        add_axis('y', title = y_axis_lab, properties = axis_props(labels=list(angle = -40,fontSize=10),title=list(fontSize=16,dy = -70)))%>%
        add_axis('x', title = x_axis_lab, properties = axis_props(labels=list(angle = -40,fontSize=10, dx = -30,dy=5),title=list(fontSize=16,dy = 70)))
      }
      else {
        form <- as.formula(paste("~",x_axis_col,"+",y_axis_col))
        freqs <- melt(xtabs(form, data_frame))
        freqs%>%
          ggvis(x=x_axis(), y=y_axis(), fill=~value)%>%
          layer_rects(width = band(), height = band()) %>%
          layer_text(
            x = prop("x", x_axis(), scale = "xcenter"),
            y = prop("y", y_axis(), scale = "ycenter"),
            text:=~value, fontSize := 14, fill:="white", baseline:="middle", align:="center") %>%
          scale_nominal("x", padding = 0, points = FALSE) %>%
          scale_nominal("y", padding = 0, points = FALSE) %>% 
          scale_nominal("x", name = "xcenter", padding = 1, points = TRUE) %>%
          scale_nominal("y", name = "ycenter", padding = 1, points = TRUE) %>%
          add_axis('y', title = y_axis_lab, properties = axis_props(labels=list(angle = -40,fontSize=10),title=list(fontSize=16,dy = -70)))%>%
          add_axis('x', title = x_axis_lab, properties = axis_props(labels=list(angle = -40,fontSize=10, dx = -30,dy=5),title=list(fontSize=16,dy = 70)))
      }
    }
    
    # Both variables continous - make a scatterplot
    else {
      format_x <- if (x_axis_col == "PubYear") "####" else ""
      format_y <- if (y_axis_col == "PubYear") "####" else ""
      data_frame[!data_frame$remove,] %>%
        ggvis(x =x_axis(),  y= y_axis(), key := ~key, fill = ~col, size = ~col ) %>% 
        hide_legend(scales = c('fill','size')) %>%
        add_axis('y',title = y_axis_lab, format = format_y, properties = axis_props(labels=list(angle = -40,fontSize=10),title=list(fontSize=16,dy = -55)))%>%
        add_axis('x', title = x_axis_lab, format = format_x, properties = axis_props(labels=list(angle = -40,fontSize=10, dx = -30,dy=5),title=list(fontSize=16,dy = 50)))%>%
        layer_points() %>%
        set_options(height = 400, width = 600) -> stuff
        if (x_axis_col %in% log_transform) {
          stuff %>% scale_numeric("x", trans="log", expand=0) -> stuff
        }
        if (y_axis_col %in% log_transform) {
          stuff %>% scale_numeric("y", trans="log", expand=0) -> stuff
        }
        stuff %>% 
        add_tooltip(function(data){
          paste0(       
            "ID: ", as.character(data$key),"<br>",
            x_axis(),": ", as.character(data[[1]]), "<br>", y_axis(), ": ", as.character(data[[2]]),"<br>")
        }, "hover")%>%set_options(renderer = "canvas") %>% handle_click(on_click = function(data,...){
          if (do_remove()){
            isolate(removed$selected[data$key] <- TRUE)
          } 
        }  
      ) #ggvis-tooltip
    }
  }
 
  mtc <- reactive({
    
    data = bigData
    
    # Apply filters (only on attributes where not everything is selected)
    
    if (!is.null(input$nt_tree)) {
      selected_nts <- get_selected(input$nt_tree)
      selected_nts <- selected_nts[!(selected_nts %in% c("All",regions))]
      if (length(selected_nts) < nrow(neuron_types)){
        data <- data[data$NeuronName %in% selected_nts,]
      }
    }
    
    if (!is.null(input$species_tree)) {
      selected_species <- get_selected(input$species_tree)
      selected_species <- selected_species[!(selected_species %in% c("All","Others"))]
      if (length(selected_species) < length(species)){
        data <- data[data$Species %in% selected_species,]
      }
    }
    
    age_slider_min <- input$Age[1]
    age_slider_max <- input$Age[2]
    age_low <- if (age_slider_min == 0) 0 else (2^(age_slider_min - 1))
    age_high <- if (age_slider_max == 0) 0 else (2^(age_slider_max - 1))
    if (age_low > 0 || age_high < age_max) {
      data <- data[which(!is.na(data$AnimalAge) & data$AnimalAge >= age_low & data$AnimalAge <= age_high),]
    }
    
    temp_low <- input$Temperature[1]
    temp_high <- input$Temperature[2]
    if (temp_low > floor(min(temp)) || temp_high < ceiling(max(temp))) {
      data <- data[which(!is.na(data$RecTemp) & data$RecTemp >= temp_low & data$RecTemp <= temp_high),]
    }
    
    weight_low <- input$Weight[1]
    weight_high <- input$Weight[2]
    if (weight_low > floor(min(weight)) || weight_high < ceiling(max(weight))) {
      data <- data[which(!is.na(data$AnimalWeight) & data$AnimalWeight >= weight_low & data$AnimalWeight <= weight_high),]
    }
    
    for (x in prop_names) {
      if (!is.null(input[[x]])) {
        slider_val = input[[x]] 
        if (slider_val[1] > props[[x,c("Min.Range")]] || slider_val[2] < props[[x,c("Max.Range")]]) {
          data <- data[which(!is.na(data[[x]]) & data[[x]] >= slider_val[1] & data[[x]] <= slider_val[2]),]
        }
      }
    }
    
    data
  })
  
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
  
  output$hivePlot <- renderPlot({makeHivePlot(mtc())})
#   output$hivePlot <- renderPlot({makeHivePlot_mike(mtc()$key)})
  output$table <- renderDataTable(mtc()[,sapply(mtc(),is.character)])
})
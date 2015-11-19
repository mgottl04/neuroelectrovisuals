library(shiny)
library(ggvis)
library(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # Expression that generates a histogram. The expression is
  
  
  mtc <- reactive({
    #global filtering will occur in this reactive
    data = bigData #%>% filter(!is.na(JxnOffset))
    data
  })
  
  plot1 <- reactive({
    plotThing <- reactive({
      data = mtc()
      data %>%filter((!is.na(data %>% select(match(input$x1, names(data)))) & (!is.na(data %>% select(match(input$y1, names(data))))))) 
    })
    plot <- plotThing %>% 
      ggvis(x=as.name(input$x1), y =as.name(input$y1), key:=~Pmid) %>% 
      layer_points() %>%
      add_tooltip(function(data){
        paste0("Pmid: ", as.character(data$Pmid),"<br>",
               input$x1,": ", as.character(data[[1]]), "<br>", input$y1, ": ", as.character(data[[2]]),"<br>")
      }, "hover")%>%set_options(renderer = "canvas")
    
  })
  
  plot2 <- reactive({
    plotThing <- reactive({
      data = mtc()
      data %>%filter((!is.na(data %>% select(match(input$x2, names(data)))) & (!is.na(data %>% select(match(input$y2, names(data))))))) 
    })
    plotThing %>% 
      ggvis(x=as.name(input$x2), y =as.name(input$y2), key:=~Pmid) %>% 
      layer_points() %>%
      add_tooltip(function(data){
        paste0("Pmid: ", as.character(data$Pmid),"<br>",
               input$x2,": ", as.character(data[[1]]), "<br>", input$y2, ": ", as.character(data[[2]]),"<br>")
      }, "hover")%>%set_options(renderer = "canvas")
    
  })
  
  
  plot3 <- reactive({
    plotThing <- reactive({
      data = mtc()
      data %>%filter((!is.na(data %>% select(match(input$x3, names(data)))) & (!is.na(data %>% select(match(input$y3, names(data))))))) 
    })
    plotThing %>% 
      ggvis(x=as.name(input$x3), y =as.name(input$y3), key:=~Pmid) %>% 
      layer_points() %>%
      add_tooltip(function(data){
        paste0("Pmid: ", as.character(data$Pmid),"<br>",
               input$x3,": ", as.character(data[[1]]), "<br>", input$y3, ": ", as.character(data[[2]]),"<br>")
      }, "hover")%>%set_options(renderer = "canvas")
    
  })
  
  
  plot4 <- reactive({
    plotThing <- reactive({
      data = mtc()
      data %>%filter((!is.na(data %>% select(match(input$x4, names(data)))) & (!is.na(data %>% select(match(input$y4, names(data))))))) 
    })
    plotThing %>% 
      ggvis(x=as.name(input$x4), y =as.name(input$y4), key:=~Pmid) %>% 
      layer_points() %>%
      add_tooltip(function(data){
        paste0("Pmid: ", as.character(data$Pmid),"<br>",
               input$x4,": ", as.character(data[[1]]), "<br>", input$y4, ": ", as.character(data[[2]]),"<br>")
      }, "hover")%>%set_options(renderer = "canvas")
    
  })
  
  
  plot1 %>%bind_shiny("plot1")
  plot2 %>%bind_shiny('plot2')
  plot3 %>%bind_shiny('plot3')
  plot4 %>%bind_shiny('plot4')
})
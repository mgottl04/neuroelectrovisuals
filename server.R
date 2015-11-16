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
    data %>%filter((!is.na(data %>% select(match(input$xaxis, names(data)))) & (!is.na(data %>% select(match(input$yaxis, names(data))))))) 
  })
  plotThing %>% 
      ggvis(x=as.name(input$xaxis), y =as.name(input$yaxis), key:=~Pmid) %>% 
    layer_points() %>%
    add_tooltip(function(data){
      paste0("Pmid: ", as.character(data$Pmid),"<br>",
             input$xaxis,": ", as.character(data[[1]]), "<br>", input$yaxis, ": ", as.character(data[[2]]),"<br>")
    }, "hover")%>%set_options(renderer = "canvas")

})
plot1  %>%bind_shiny("distPlot")
})
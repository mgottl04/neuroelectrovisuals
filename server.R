library(shiny)
library(ggvis)
library(dplyr)

make_main_plot <- function(df, x_axis, y_axis){
  data_frame <- df()
  #filt <-
  data_frame %>%  
    filter((!is.na(data_frame[,as.character(x_axis())]))&
             (!is.na(data_frame[,as.character(y_axis())]))) %>%
    ggvis(x =x_axis(), y= y_axis() ) %>% 
    layer_points() %>%
    add_tooltip(function(data){
      paste0("Pmid: ", as.character(data$Pmid),"<br>",
             x_axis(),": ", as.character(data[[1]]), "<br>", y_axis(), ": ", as.character(data[[2]]),"<br>")
    }, "hover")%>%set_options(renderer = "canvas")
}

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # Expression that generates a histogram. The expression is
  

  mtc <- reactive({
    #global filtering will occur in this reactive
    data = bigData #%>% filter(!is.na(JxnOffset))
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
  reactive({make_main_plot(mtc,x2,y2)})%>%
    bind_shiny('plot3')
  reactive({make_main_plot(mtc,x4,y4)})%>%
    bind_shiny('plot4')

})
library(shiny)
library(ggplot2)
shinyServer(function(input,output)({
  
  ## Get the value of the dataset that is selected by user from the list of datasets
  data <- reactive({
    get(input$dataset)
  })
  
  ## to output the dataset
  output$dat <- renderPrint({
    data()
  })
  
  # Pulling the list of variable for choice of variable x
  output$varx <- renderUI({
    selectInput("variablex", "select the X variable", choices=names(data()))
  })
  
  # Pulling the list of variable for choice of variable y
  output$vary <- renderUI({
    selectInput("variabley", "select the Y variable", choices=names(data()))
    
  })
  
  # to output the structure of the dataset
  output$struct <- renderPrint({
    str(get(input$dataset))
  })
  
  # for summary
  output$summary <- renderPrint({
    summary(get(input$dataset))
  })
  
  # For plot
  output$plot <- renderPlot({
    ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
      geom_point() 
  }) 
  
}))
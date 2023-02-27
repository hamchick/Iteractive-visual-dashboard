# makecondition -----------

library(econR)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
sample <- read.csv(. %//% "support/SampleData.csv")
sample$TARGET <- as.character(sample$TARGET)
input <- "AMT_ANNUITY"
input2 <- "CONTRACT_TYPE"


# server --------

server <- function(input, output) {
  select1 <- reactive({
    select(sample, input$varible1)
  })
  select2 <- reactive({
    select(sample, input$varible2)
  })
  output$distPlot <- plotly::renderPlotly({
    if (is.character(select1()[1, 1])) {
      if (is.character(select2()[1, 1])) {
        plotOutcome <- ggplot(data = sample) +
          geom_count(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
      else {
        plotOutcome <- ggplot(data = sample) +
          geom_col(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
    }
    else {
      if (is.character(select2()[1, 1])) {
        plotOutcome <- ggplot(data = sample) +
          geom_col(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
      else {
        plotOutcome <- ggplot(data = sample) +
          geom_smooth(aes(
            x = get(input$varible1),
            y = get(input$varible2)
          ))
      }
    }
    plotly::ggplotly(plotOutcome)
  })
  output$mytable <- renderDT({
    sample
  })
}

shinyApp(
  ui = shiny::htmlTemplate("www/index.html"),
  server
)

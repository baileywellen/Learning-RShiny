#Dashboard of a Basic K-Means Clustering Algorithm for Iris Dataset

#import the library
library(shiny)

#plotting only works with numerical data
vars <- setdiff(names(iris), "Species")

#Sets up a UI Object
ui <- fluidPage(
  selectInput(inputId = "X", label = "X Value", choices = vars),
  selectInput(inputId = "Y", label = "Y Value", choices = vars),
  numericInput(inputId = "clusters", label = "Number of Clusters", min = 1, max = 10, value =1),
  plotOutput(outputId = "plot")
  )


#Sets up a Server Object
server <- function(input, output) {
  
  #make a new dataframe with just the data the user requested
  selectedData <- reactive({
    iris[, c(input$X, input$Y)]
  })
  result <- reactive({kmeans(selectedData(), input$clusters)})
 
  output$plot <- renderPlot({plot(selectedData(), col = result()$cluster )})
  
  
}
#Knit the two objects together into a Shiny App
shinyApp(ui = ui, server = server)

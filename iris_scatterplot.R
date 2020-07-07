#Plot a Scatterplot of any combination of Iris Sepal.Width, Sepal.Length, Petal.Width, and Petal.Length

#import the library
library(shiny)

#plotting only works with numerical data
vars <- setdiff(names(iris), "Species")

#Sets up a UI Object
ui <- fluidPage(
  selectInput(inputId = "X", label = "X Value", choices = vars),
  selectInput(inputId = "Y", label = "Y Value", choices = vars),
  plotOutput(outputId = "plot")
  )


#Sets up a Server Object
server <- function(input, output) {
  
  #make a new dataframe with just the data the user requested
  selectedData <- reactive({
    iris[, c(input$X, input$Y)]
  })
 
  output$plot <- renderPlot({plot(selectedData())})
  
  
}
#Knit the two objects together into a Shiny App
shinyApp(ui = ui, server = server)

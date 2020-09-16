#This code was written to teach Carthage College Data Science Club about R Shiny dasboards

#Plot mtcars data

#import the library
library(shiny)


#Sets up a UI Object
ui <- fluidPage(
  selectInput(inputId = "cartype", label= "Car Type", choices = row.names(mtcars)),
  textOutput(outputId = "carinfo"),
  tableOutput(outputId = "cartable")
)


#Sets up a Server Object
server <- function(input, output) {
  
  #Display their choice
  output$carinfo <- renderText({
    paste("You chose", input$cartype)
  })
  
  #Make a datatable of the row they chose
  output$cartable <- renderTable({
    mtcars[input$cartype,]
  })
  
}
#Knit the two objects together into a Shiny App
shinyApp(ui = ui, server = server)

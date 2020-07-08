#The below 4 lines are the "Template" For a Shiny App 
#import the library
library(shiny)

#plotting only works with numerical data
vars <- setdiff(names(iris), "Species")

#Sets up a UI Object
ui <- fluidPage(
  selectInput(inputId = "category", label = "Iris Category", choices = vars ),
  plotOutput(outputId = "barplot")
)



#Sets up a Server Object
server <- function(input, output) {
  
  #separate out the 3 species into separate dataframes
  #then, only select the column that the User chose
  setosa <- reactive({filter(iris, Species == "setosa")[, input$category]})
  versicolor <- reactive({filter(iris, Species == "versicolor")[, input$category]})
  virginica <- reactive({filter(iris, Species == "virginica")[, input$category]})
    
  output$barplot <- renderPlot({barplot(c(mean(setosa()), mean(versicolor()), mean(virginica())), 
                                        main = paste("Mean", input$category, "by Species"), xlab  = "Species", ylab = "Mean (cm)", 
                                        col = c('light grey', 'light blue', 'dark blue'), names.arg = c("Setosa", "Versicolor", "Virginica"))})
  
  
}
#Knit the two objects together into a Shiny App
shinyApp(ui = ui, server = server)

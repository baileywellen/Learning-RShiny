#The below 4 lines are the "Template" For a Shiny App 
#import the library
library(shiny)
#Sets up a UI Object
ui <- fluidPage(
  
  titlePanel("Police Shootings Dashboard"),
  sidebarPanel(
    checkboxGroupInput(inputId = "states", label = "Choose State(s) to Compare", choices = sort(unique(police_shootings$state)), selected = c("WI", "IL"))
  ),
  mainPanel(
    textOutput(outputId = "pie_label"),
    plotOutput(outputId = "piechart"),
    textOutput(outputId = "table_label"),
    tableOutput(outputId = "table")
  )
  
)
#Sets up a Server Object
server <- function(input, output) {
  
  output$pie_label <- renderText("Comparison of Fatal Shootings by State")
  output$table_label <- renderText("Total Fatal Shootings of Selected States")
  states <- reactive({
    chosen_states <- filter(police_shootings, state %in% input$states)
    states_table <- table(chosen_states$state)
    })
  
  
  output$piechart <- renderPlot(pie(states()))
  #Show the counts and rename the Column names
  output$table <- renderTable({df <- data.frame(states())
                              df <- df %>% rename(State = Var1, Count = Freq)
    })
  
}
#Knit the two objects together into a Shiny App
shinyApp(ui = ui, server = server)

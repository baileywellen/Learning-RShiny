#The below 4 lines are the "Template" For a Shiny App 
#import the library
library(shiny)

possible_graphs <- c("gender", fleeing = "flee", "armed", "race", 'suspected mental illness' = "signs_of_mental_illness", 'body camera on' = "body_camera", "age" = 'age_group', 'threat level' = "threat_level")

#rename the race column for clarity 
police_shootings$race[police_shootings$race == "W"] <- "White"
police_shootings$race[police_shootings$race == "B"] <- "Black"
police_shootings$race[police_shootings$race == "H"] <- "Hispanic"
police_shootings$race[police_shootings$race == "N"] <- "Native American"
police_shootings$race[police_shootings$race == "A"] <- "Asian"
police_shootings$race[police_shootings$race == "O"] <- "Other"

#put the ages into "Bins" 
police_shootings$age_group <- ifelse(police_shootings$age < 18, "Under 18",
                       ifelse(police_shootings$age < 30, "18 - 30",
                              ifelse(police_shootings$age < 40, "30 - 40",
                                     ifelse(police_shootings$age < 50, "40 - 50", 
                                            ifelse(police_shootings$age < 60, "50 - 60", "Over 60")))))
#Sets up a UI Object
ui <- fluidPage(
  
  titlePanel("Fatal Police Shootings Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      box(width = 12,
          splitLayout(
             checkboxGroupInput(inputId = "states", label = "State(s)", choices = sort(unique(police_shootings$state)), selected = c("WI", "IL")),
                    radioButtons(inputId = "details", label = "Case Detail", choices = sort(possible_graphs))
          )
       ),width= 3,
      
    ),
   position = "right",
  
  
  mainPanel(
    textOutput(outputId = "state_label"),
    textOutput(outputId = "details_label"),
    plotOutput(outputId = "piechart"),
    textOutput(outputId = "table_label"),
    tableOutput(outputId = "table")
  )
  
  
  )
 
  
)


#Sets up a Server Object
server <- function(input, output) {
  
  output$state_label <- renderText({input$states})
  output$details_label <- renderText(input$details)
  output$table_label <- renderText("Total Fatal Shootings of Selected States")
  chosen_states <- reactive({
    chosen_states <- filter(police_shootings, state %in% input$states)})
  
  states_table <- reactive({
    table(chosen_states()$state)
    })
  
  chosen_data <- reactive({
    this_data <- chosen_states()[,input$details]
    table(this_data)
  })
  
  output$piechart <- renderPlot(pie(chosen_data()))
  #Show the counts and rename the Column names
  output$table <- renderTable({df <- data.frame(states_table())
                              df <- df %>% rename(State = Var1, Count = Freq)
    })
  
}
#Knit the two objects together into a Shiny App
shinyApp(ui = ui, server = server)

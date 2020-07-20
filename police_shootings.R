#Kaggle Source - https://www.kaggle.com/mrmorj/data-police-shootings

#The below 4 lines are the "Template" For a Shiny App 
#import the library
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(readxl)

police_shootings <- read_excel("fatal-police-shootings.xlsx")

possible_graphs <- c("gender", fleeing = "flee", 'armed' = "weapon_category", "race", 'suspected mental illness' = "signs_of_mental_illness", 'body camera on' = "body_camera", "age" = 'age_group', 'threat level' = "threat_level")

#rename the race column for clarity 
police_shootings$race[police_shootings$race == "W"] <- "White"
police_shootings$race[police_shootings$race == "B"] <- "Black"
police_shootings$race[police_shootings$race == "H"] <- "Hispanic"
police_shootings$race[police_shootings$race == "N"] <- "Native American"
police_shootings$race[police_shootings$race == "A"] <- "Asian"
police_shootings$race[police_shootings$race == "O"] <- "Other"

#rename the gender column for clarity
police_shootings$gender[police_shootings$gender == "M"] <- "Male"
police_shootings$gender[police_shootings$gender == "F"] <- "Female"


#put the ages into "Bins" 
police_shootings$age_group <- ifelse(police_shootings$age < 18, "Under 18",
                                     ifelse(police_shootings$age < 30, "18 - 30",
                                            ifelse(police_shootings$age < 40, "30 - 40",
                                                   ifelse(police_shootings$age < 50, "40 - 50", 
                                                          ifelse(police_shootings$age < 60, "50 - 60", "Over 60")))))

#Put the weapons into "Bins"
police_shootings$weapon_category <- ifelse(grepl("unarmed", tolower(police_shootings$armed)), "unarmed", 
                                           ifelse(grepl("gun", tolower(police_shootings$armed)), "gun", 
                                                  ifelse(grepl("knife", tolower(police_shootings$armed)), "knife", "other")))


#Sets up a UI Object
ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("Fatal Police Shootings Dashboard"),
  
  sidebarLayout(
    #user choices - side by side
    sidebarPanel(
      shinydashboard::box(width = 12,
          splitLayout(
            checkboxGroupInput(inputId = "states", label = "State(s)", choices = sort(unique(police_shootings$state)), selected = c("WI", "IL")),
            radioButtons(inputId = "details", label = "Case Detail", choices = sort(possible_graphs))
          )
      )
      
    ),
    position = "right",
    
    mainPanel(
      textOutput(outputId = "state_label"),
      textOutput(outputId = "details_label"),
      textOutput(outputId = "total_shootings"),
      plotOutput(outputId = "piechart"),
      textOutput(outputId = "table_label"),
      tableOutput(outputId = "table"),
      textOutput(outputId = "disclaimer")
    )
  )
)


#Sets up a Server Object
server <- function(input, output) {
  
  #make a reactive dataframe of just the states the user chose
  chosen_states <- reactive({
    chosen_states <- dplyr::filter(police_shootings, state %in% input$states)})
  
  #make a reactive dataframe that shows the frequency of the states the user chose
  states_table <- reactive({
    table(chosen_states()$state)
  })
  
  #make a reactive dataframe of just the case details that the user chose in the chosen states
  chosen_data <- reactive({
    this_data <- chosen_states()[,input$details]
    table(this_data)
  })
  
  #add the text about the current pie chart
  output$state_label <- renderText({paste(input$states, sep = ",")})
  output$details_label <- renderText(input$details)
  output$total_shootings <- renderText({paste(nrow(chosen_states()), "total fatal police shootings between", min(police_shootings$date), " and ", max(police_shootings$date))})
  
  output$table_label <- renderText("Total Fatal Shootings of Selected States")
  
  #add the pie chart with percentages
  output$piechart <- renderPlot({
    #we cannot plot a pie chart with no data - if the user unselects all states, it will not display anything 
    if(length(input$states) > 0)
    {
      lbls <-names(chosen_data())
      #calculate the percentages
      pct <- round(chosen_data()/sum(chosen_data())*100)
      #add the percentage and a % to the labels
      lbls <- paste(lbls, pct, sep = "\n")
      lbls <- paste(lbls,"%",sep="") 
      pie(chosen_data(), labels = lbls)
    }
    
  })
  
  
  #Show the counts and rename the Column names
  output$table <- renderTable({df <- data.frame(states_table())
  df <- df %>% rename(State = Var1, Count = Freq)
  })
  
  #add the disclaimer from data source about inaccuracy of dataset                         
  output$disclaimer <- renderText("The FBI and the Centers for Disease Control and Prevention log fatal shootings by police, but officials acknowledge that their data is incomplete. In 2015, The Post documented more than two times more fatal shootings by police than had been recorded by the FBI. Last year, the FBI announced plans to overhaul how it tracks fatal police encounters. (Kaggle)")
  
  
}
#Knit the two objects together into a Shiny App
shinyApp(ui = ui, server = server)


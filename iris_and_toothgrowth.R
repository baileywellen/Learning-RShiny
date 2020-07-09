
#The below 4 lines are the "Template" For a Shiny App 
#import the library
library(shiny)

#provides an interface to JavaScript library DataTables
library(DT)

#plotting only works with numerical data
iris_vars <- setdiff(names(iris), "Species")

#all possible colors - it will default to the first one if nothing is selected
colors <- c('white', 'red', 'blue', 'black','green', 'grey', 'light grey','brown', "olive" =  'darkolivegreen', 'cyan', "pink" = 'deeppink', 'coral')

#Sets up a UI Object
ui <- fluidPage(
  
  titlePanel("Iris and Tooth Growth Dashboard"),
  
  sidebarLayout(
    
    #Allow the User to Choose Between the Iris Dataset and the Tooth Growth dataset
    sidebarPanel(
      selectInput(inputId = "dataset", label = "Choose Dataset", choices = c("Iris", "Tooth Growth")),
      
      #If they chose the Iris, we will allow them to pick the features and the colors
      conditionalPanel(
        condition = "input.dataset == 'Iris'",
        selectInput(inputId = "iris_category", label = "Iris Category", choices = iris_vars),
        selectInput(inputId = "setosa_color", label = "Setosa Color", choices = colors),
        selectInput(inputId = "versicolor_color", label = "Versicolor Color", choices = colors),
        selectInput(inputId = "virginica_color", label = "Virginica Color", choices = colors)
        
      ),
      
      #If they chose the Tooth Growth, we will allow them to pick the features and the colors
      conditionalPanel(
        condition = "input.dataset == 'Tooth Growth'",
        selectInput(inputId = "supp_category_1", label = "Tooth Growth Supplement - 1", choices = unique(ToothGrowth$supp)),
        selectInput(inputId = "dose_category_1", label = "Tooth Growth Dose - 1 ", choices = unique(ToothGrowth$dose)),
        selectInput(inputId = "color_1", label = "Color 1", choices = colors),
        selectInput(inputId = "supp_category_2", label = "Tooth Growth Supplement - 2", choices = unique(ToothGrowth$supp)),
        selectInput(inputId = "dose_category_2", label = "Tooth Growth Dose - 2 ", choices = unique(ToothGrowth$dose)),
        selectInput(inputId = "color_2", label = "Color 2", choices = colors)
      )
    ),
    
    mainPanel(
      
      conditionalPanel(
        condition = "input.dataset == 'Iris'",
        plotOutput(outputId = "iris_barplot"),
        tableOutput(outputId = "iris_table")
      ),
      conditionalPanel(
        condition = "input.dataset == 'Tooth Growth'",
        plotOutput(outputId = "tooth_barplot"),
        tableOutput(outputId = "tooth_table")
      )
    )
  )
)

#Sets up a Server Object
server <- function(input, output) {
  
  #separate out the 3 Iris species into separate dataframes
  #then, only select the column that the User chose
  setosa <- reactive({filter(iris, Species == "setosa")[, input$iris_category]})
  versicolor <- reactive({filter(iris, Species == "versicolor")[, input$iris_category]})
  virginica <- reactive({filter(iris, Species == "virginica")[, input$iris_category]})
  
  #Separate the Two Tooth Growth Dataframes that we want into separate dataframes with only the selection that User chose
  tooth_1 <- reactive({filter(ToothGrowth, supp == input$supp_category_1, dose == input$dose_category_1)[,"len"]})
  tooth_2 <- reactive({filter(ToothGrowth, supp == input$supp_category_2, dose == input$dose_category_2)[,"len"]})
  
  iris_means <- reactive({c(mean(setosa()), mean(versicolor()), mean(virginica()))})
  #find a y lim that is tall enough to show the values above the vars
  iris_ylim <- reactive(c(0, 1.1*max(iris_means())))
  
  #plot Iris Data with Labels
  output$iris_barplot <- renderPlot({p<- barplot(iris_means() ,main = paste("Mean", input$iris_category, "by Species"), xlab  = "Species", ylab = "Mean (cm)", 
                                        col = c(input$setosa_color, input$versicolor_color, input$virginica_color), names.arg = c("Setosa", "Versicolor", "Virginica"), ylim = iris_ylim())
                                    #Add a text label to display the height of each bar
                                      text(x = p, y = iris_means(), label = iris_means(), pos = 3)#pos = 3 tells us we want to adjust the label on the top
  })
  
  #Show table with the means
  output$iris_table <- renderTable({data.frame(setosa(), versicolor(), virginica())})
  
  
  tooth_means <- reactive({c(mean(tooth_1()), mean(tooth_2()))})
  tooth_ylim <- reactive(c(0, 1.1 * max(tooth_means())))
  
  #Plot Tooth Growth Data with Labels
  output$tooth_barplot <- renderPlot({p <- barplot(tooth_means(), main = paste("Mean Length by Supplement and Dosage"), xlab  = "Supplement Type and Dosage (mg / day)", ylab = "Mean Length", ylim = tooth_ylim(),
                                              col = c(input$color_1, input$color_2),  names.arg = c(paste(input$supp_category_1, ",", input$dose_category_1), paste(input$supp_category_2, ",", input$dose_category_2)))
                                      text(x = p, y = tooth_means(), label = tooth_means(), pos = 3)
  })
  
  #Show Table with the means
  output$tooth_table <- renderTable({data.frame(tooth_1(), tooth_2())})
  
}
#Knit the UI and the Server together with a Shiny Object
shinyApp(ui = ui, server = server)


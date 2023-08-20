library(pacman)
p_load(dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, shinythemes, stringr, tidyr)

emp_data <- rio::import("C:/Users/cohan/OneDrive - University of the Sunshine Coast/Bachelor of Engineering (Mechanical)/Year 3/2023 - SEM 2/ENG304 Engineering Research Methodology/GOVHACK/Datasets/FINAL.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "H2Optimise",
                  tabPanel("Home",
                           sidebarPanel(
                             tags$h3("Employee Information:"),
                             numericInput("emp_number", "Employee Number:", NULL, min = 1),
                             actionButton("submitbutton", 
                                          "Submit", 
                                          class = "btn btn-primary")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Monthly Summary"),
                             h4("Summary of your monthly driving statistics"),
                             verbatimTextOutput("txtout"),
                             plotOutput(outputId = "bar")              
                           ) # mainPanel
                  ), # tabPanel
                  
                  tabPanel("My Daily Footprint", 
                           sidebarLayout(
                             sidebarPanel(
                               numericInput("distance", "Driving Distance (kms)", value = 0),
                               numericInput("idling_time", "Idling Time (mins)", value = 0),
                               actionButton("calculate", "Calculate CO2 Emission")
                             ),
                             mainPanel(
                               plotlyOutput("emission_chart")
                             )
                           )
                  ),
                  
                  tabPanel("About", 
                           titlePanel("About"), 
                           div(includeMarkdown("about.md"), 
                               align="justify")
                  )
                  
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output) {
  
  observeEvent(input$submitbutton, {
    emp_number <- input$emp_number
    emp_data_subset <- subset(emp_data, EmployeeNumber == emp_number)
    
    if (nrow(emp_data_subset) > 0) {
      output$txtout <- renderText({
        paste("Employee Number:", emp_number)
      })
      
      output$bar <- renderPlot({
        # Barplot data preparation
        data_to_plot <- emp_data_subset[, c("Driving Duration", "Idling Duration", "Stop Duration")]
        color <- c("blue", "red", "green")
        
        # Barplot creation
        barplot(as.matrix(data_to_plot),
                beside = TRUE,
                main = "Monthly Driving Statistics",
                ylab = "Hours",
                xlab = "Driving Time Metrics",
                col = color,
                names.arg = c("Driving Duration", "Idling Duration", "Stop Duration"))
      })
    } else {
      output$txtout <- renderText({
        "Invalid Employee Number"
      })
      
      output$bar <- renderPlot(NULL)
    }
  })
  
  observeEvent(input$calculate, {
    # Calculate CO2 emissions
    total_distance <- input$distance
    total_idling_time <- input$idling_time
    total_time <- total_distance + total_idling_time
    driving_percentage <- (total_distance * 207 / total_time) * 100
    idling_percentage <- (total_idling_time * 87.5 / total_time) * 100
    
    # Create a pie chart
    emission_data <- data.frame(
      Category = c("Driving", "Idling"),
      Percentage = c(driving_percentage, idling_percentage),
      Color = c("dodgerblue1", "lawngreen")
    )
    
    output$emission_chart <- renderPlotly({
      pie_chart <- plot_ly(emission_data, labels = ~Category, values = ~Percentage, type = "pie",
                           marker = list(colors = ~Color))
      pie_chart <- pie_chart %>% layout(title = "CO2 Emission Breakdown")
      pie_chart
    })
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
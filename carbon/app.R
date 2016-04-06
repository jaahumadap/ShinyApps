# R code for carbon app

library(shiny)
library(shinydashboard)

ui <- fluidPage(
        withMathJax(),
        fluidRow(
                
        tags$img(height = 100, width =300, src = "CI_Logo_Standard_English_French.png"),tags$strong("Moore Center for Science"),tags$hr()),
        fluidRow(
        tags$h2("What happens to forests when we loose wildlife?"),tags$hr(),tags$p("Some interesting text describing what we are showing")),
        fluidRow(tags$br(), tags$hr(),
                column(3,
                       sliderInput(inputId = "def", label = tags$a("Choose a value for defaunation"),
                                   value = 25, min = 0, max = 100, step = 25),
                       selectInput(inputId = "country", label = "Select a country, a region or all",choices = c("Australia" = "Australia","Cameroon" = "Cameroon", "Congo" = "Congo","Costa Rica"="Costa Rica","India" = "India", "Indonesia" = "Indonesia", "Malaysia" = "Malaysia","Panama" = "Panama","Peru" = "Peru", "Tanzania" = "Tanzania", "All" = "All")),
                       checkboxGroupInput(inputId = "variable", label = "Choose a variable", choices = c("Carbon" = "carbon", "Value" = "value", "Relative abundance" = "ra"), selected = "Carbon"),
                       tags$hr(),
                       checkboxInput(inputId = "rawdata",label = "See the raw data?", value = TRUE)
                ),
                column(9,plotOutput("plot"))
        )
        
)

library(ggplot2)
library(reshape)
library(dplyr)
carbon <- read.csv("data/sim_result_carbon.csv", header = T)

server <- function(input, output){
        
        
        data <- reactive({
                data <- filter(carbon, country == input$country)
                # get deforestation level
                deflevel <- numeric()
                
                if (input$def == 0) {
                        deflevel <- "original"
                } else if (input$def == 25) {
                        deflevel <- 5
                } else if (input$def == 50) {
                        deflevel <- 6
                } else if (input$def == 75) {
                        deflevel <- 7
                } else 
                        deflevel <- 8
                
                select(data, c(1:4,deflevel))
                
        })
        
        
        
        output$plot <- renderPlot({
                ggplot(data, aes)
        })
        
}

shinyApp(ui = ui, server = server)
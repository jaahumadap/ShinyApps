# R code for carbon app

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
        #titlePanel(),
        fluidRow(
                column(4,tags$img(height = 100, width =300, src = "CI_Logo_Standard_English_French.png"),tags$strong("Moore Center for Science"),tags$hr()),
                 column(8,tags$h1("What happens to forests when we lose wildlife?"),tags$hr(),tags$p("Losing wildlife has serious consequences for tropical forests"))),
        fluidRow(
                column(4,
                               sliderInput(inputId = "def", label = tags$a("% of animal-dispersed trees lost"), value = 25, min = 25, max = 100, step = 25),
                               selectInput(inputId = "country", label = "Select a country, a region or all",choices = c("Cameroon" = "Cameroon", "Congo" = "Congo","Costa Rica"="Costa Rica","India" = "India", "Indonesia" = "Indonesia", "Malaysia" = "Malaysia","Australia" = "Australia","Panama" = "Panama","Peru" = "Peru", "Tanzania" = "Tanzania", "All" = "All")),
                               selectInput(inputId = "variable", label = "Choose a variable", choices = c("Carbon Biomass" = "carbon", "Carbon Value" = "value"), selected = "carbon"),
                               tags$hr(),
                               checkboxInput(inputId = "rawdata",label = "See the raw data?", value = FALSE)
                        ),
                column(8,plotOutput("plot"))
        )
)

carbon <- read.csv("data/sim_result_carbon.csv", header = T)
carbon <- filter(carbon, scenario != "control_2")
carbon <- droplevels(carbon)
value <- cbind(carbon[,1:4],carbon[,5:8]*0.46)

server <- function(input, output){
        
        #load the appropiate data set
        mainData <- reactive({
                if(input$variable == "carbon")
                        carbon
                else
                        value
        })
        
        #filter by country
        ctryData <- reactive({
                if(input$country == "All")
                        mainData()
                else
                        filter(mainData(), country == input$country)
        })
                
        # filter by deforestation level
        defData <- reactive({
                def <- paste("rem_",input$def,sep="")
                #size_means <- summarise(group_by(ctryData(),scenario), mean(ctryData()[,def]))
                #size_means <- as.numeric(size_means$mean)
                size_means <- as.numeric(tapply(ctryData()[,def], ctryData()[,"scenario"], mean))
                list(values=ctryData(), size_means=size_means, def = def)
        })       
        
        
        
        output$plot <- renderPlot({
                if(input$variable == "carbon")
                        ytitle <- "Tons of carbon stored per hectare"
                else
                        ytitle <- "Value of carbon in $US"
                
                def_label <- paste(input$def,"% of AD trees removed")
                
                yvalues <- defData()$values[,defData()$def]
                ylim <- range(defData()$values[,defData()$def])
                
                p <- ggplot(defData()$values, aes(x=scenario, y = yvalues)) + xlab("State of the forest") + ylab(ytitle) + ggtitle(input$country) + scale_x_discrete(labels = c("Intact Forest",def_label)) + theme_linedraw() +  expand_limits(y=c(min(yvalues),max(yvalues))) + stat_summary(fun.y = "mean", geom="point")#, alpha = 0.5, fill = c("green","red")) + geom_hline(yintercept = defData()$size_means, color = c("green","red"), size = 1, linetype = 2) 
                
                if (!input$rawdata)
                        p
                else
                        p + geom_jitter(alpha = 0.2)
                
                
        })
        
}

shinyApp(ui = ui, server = server)
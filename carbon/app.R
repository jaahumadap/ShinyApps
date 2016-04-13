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
                column(4,wellPanel(
                               sliderInput(inputId = "def", label = tags$a("Loss of trees that rely on animals for their seed dispersal"), value = 0, min = 0, max = 100, step = 25, animate = T),
                               selectInput(inputId = "country", label = "Select a country, a region or all",choices = c("Cameroon" = "Cameroon", "Congo" = "Congo","Costa Rica"="Costa Rica","India" = "India", "Indonesia" = "Indonesia", "Malaysia" = "Malaysia","Australia" = "Australia","Panama" = "Panama","Peru" = "Peru", "Tanzania" = "Tanzania", "All" = "All")),
                              selectInput(inputId = "variable", label = "Choose a variable", choices = c("Carbon Biomass" = "carbon", "Carbon Value" = "value"), selected = "carbon"),
                               tags$hr(),
                               checkboxInput(inputId = "rawdata",label = "See the raw data?", value = FALSE)
                        )),
                column(8,plotOutput("plot"))
        )
)

carbon <- read.csv("data/sim_result_carbon.csv", header = T)
carbon <- filter(carbon, scenario != "control_2" & scenario != "control_1")
carbon <- droplevels(carbon)
carbon <- melt(carbon,variable_name = "data",id.vars = c("scenario","country"),measure.vars = 4:8)
names(carbon)[3] <- "def"
# Temporary until I can get the value data
value <- carbon

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
                        filter(mainData(), country %in% input$country)
                        
        })
                
        # filter by deforestation level
        #defData <- reactive({
        #        def <- paste("rem_",input$def,sep="")
        #        #size_means <- summarise(group_by(ctryData(),scenario), mean(ctryData()[,def]))
        #        #size_means <- as.numeric(size_means$mean)
        #        size_means <- as.numeric(tapply(ctryData()[,def], ctryData()[,"scenario"], mean))
        #        list(values=ctryData(), size_means=size_means, def = def)
        #})       
        
        
        
        output$plot <- renderPlot({
                if(input$variable == "carbon")
                        ytitle <- "Carbon stored in the forest (tons/ha)"
                else
                        ytitle <- "Value of carbon stored in the forest ($US)"
                
                
                means <- as.numeric(tapply(ctryData()$value, ctryData()$def, mean))
                pcts <- round(means[-1]/means[1]*100 - 100,1)
                ylim <- range(ctryData()$value)
                
                
                balpha <- 0.05
                
                if(input$def == 0) {
                        alpha <- c(1,rep(balpha,4))
                        ipcts <- ""
                }
                else if(input$def == 25) {
                        alpha <- c(1,1,rep(balpha,3))
                        ipcts <- paste(pcts[1],"%")
                }
                        
                else if(input$def == 50) {
                        alpha <- c(1,1,1,rep(balpha,2))
                        ipcts <- paste(pcts[2],"%")
                }
                else if(input$def == 75) {
                        alpha <- c(1,1,1,1,rep(balpha,1))
                        ipcts <- paste(pcts[3],"%")
                }
                else {
                        alpha <- 1
                        ipcts <- paste(pcts[4],"%")
                }
                
                p <- ggplot(ctryData(), aes(x=def, y = value)) + xlab("Percent of Animal-dispersed trees removed") + ylab(ytitle) + ggtitle(input$country) + scale_x_discrete(labels = c("0","25%","50%","75%","100%")) + theme_linedraw() +  coord_cartesian(ylim=ylim) + stat_summary(fun.y = "mean", geom="bar", fill = c("green","red","red","red","red"), alpha = alpha) + theme(axis.title.y = element_text(size = rel(1.5), angle = 90),axis.title.x = element_text(size = rel(1.5), angle = 0), axis.text = element_text(size = rel(1.5)), plot.title = element_text(size = rel(2.5),color = "grey")) + annotate("text", label = ipcts, x = 5, y =ylim[2]*.98, size = 12, color = "red")
#, alpha = 0.5, fill = c("green","red")) + geom_hline(yintercept = defData()$size_means, color = c("green","red"), size = 1, linetype = 2) 
                
                if (!input$rawdata)
                        p
                else
                        p + geom_jitter(alpha = 0.2)
                
                
        })
        
        
}

shinyApp(ui = ui, server = server)
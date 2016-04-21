# R code for carbon app

library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(png)
library(grid)

ui <- fluidPage(
        #titlePanel(),
        fluidRow(
                column(4,tags$img(height = 200, width =200*1.33, src = "ci_48769095.jpg")),#tags$strong("Moore Center for Science")),
                 column(8,tags$h1("What happens to forests when we lose wildlife?"),tags$h2("We loose carbon but it depends where"),tags$hr())),
                fluidRow(wellPanel(tags$p("Loosing wildlife can have important consequences on how ecosystems work. How can we know this without actually removing wildlife from a forest? Anand Osuri, a scientist with the National Centre for Biological Sciences in India in collaboration with several scientists from the TEAM network, devised an ingenious simulation to explore the effects of removing wildlife on carbon storage in tropical forest plots. They first calculated the amount of carbon stored in the plots, then 'removed' trees that need animals for their survival, replaced them with a random selection of trees, and finally, re-calculated the amount of carbon in these new plots."),tags$p("What happened? Choose a country from the menu and move the lever on the left to simulate the removal of wildlife from the forest and see what happens with 1) the amount of carbon stored in the forest and 2) the value of that carbon in the current market."))),
        
        fluidRow(
                column(4,wellPanel(
                               
                               selectInput(inputId = "country", label = "Select a country, a region or all",choices = c("Cameroon" = "Cameroon", "Congo" = "Congo","Costa Rica"="Costa Rica","India" = "India", "Indonesia" = "Indonesia", "Malaysia" = "Malaysia","Australia" = "Australia","Panama" = "Panama","Peru" = "Peru", "Tanzania" = "Tanzania", "All" = "All")),
                               sliderInput(inputId = "def", label = tags$a("Animal-dispersed trees lost"), value = 0, min = 0, max = 100, step = 25, animate = T, post = "%"),
                              selectInput(inputId = "variable", label = "Choose a variable", choices = c("Carbon Biomass" = "carbon", "Carbon Value" = "value"), selected = "carbon"),
                               tags$hr(),
                               checkboxInput(inputId = "rawdata",label = "See the raw data?", value = FALSE)
                        )),
                column(8,plotOutput("plot")),
                fluidRow(column(4),column(8,wellPanel(textOutput("stats"))))
        )
)

carbon <- read.csv("data/sim_result_carbon-$-.csv", header = T)
carbon <- filter(carbon, scenario != "control_2" & scenario != "control_1")
carbon <- droplevels(carbon)
value <- select(carbon,-c(4,6,8,10,12))
carbon <- select(carbon, c(1:4,6,8,10,12))
carbon <- melt(carbon,id.vars = c("scenario","country"),measure.vars = 4:8)
value <- melt(value,id.vars = c("scenario","country"),measure.vars = 4:8)
names(carbon)[3] <- "def"
names(value)[3] <- "def"
value$value <- as.numeric(value$value)


server <- function(input, output, session){
        
        observe({
                input$country
                updateSliderInput(session, inputId = "def", value = 0)
        })
        
        
        
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
                
        
        output$plot <- renderPlot({
                if(input$variable == "carbon")
                        ytitle <- "Carbon stored in the forest (tons/ha)\n"
                else
                        ytitle <- "Value of carbon stored in the forest ($US)\n"
                
                
                means <- as.numeric(tapply(ctryData()$value, ctryData()$def, mean))
                pcts <- round(means[-1]/means[1]*100 - 100,1)
                raws <- round(means[-1] - means[1],1)
                ylim <- range(ctryData()$value)
                
                
                balpha <- 0.05
                
                if(input$def == 0) {
                        alpha <- c(1,rep(balpha,4))
                        ipcts <- ""
                }
                else if(input$def == 25) {
                        alpha <- c(1,1,rep(balpha,3))
                        indx <- 1
                        ipcts <- paste(pcts[indx],"%")
                }
                        
                else if(input$def == 50) {
                        alpha <- c(1,1,1,rep(balpha,2))
                        indx <- 2
                        ipcts <- paste(pcts[indx],"%")
                }
                else if(input$def == 75) {
                        alpha <- c(1,1,1,1,rep(balpha,1))
                        indx <- 3
                        ipcts <- paste(pcts[indx],"%")
                }
                else {
                        alpha <- 1
                        indx <- 4
                        ipcts <- paste(pcts[indx],"%")
                }
                
                p <- ggplot(ctryData(), aes(x=def, y = value)) + xlab("\nAnimal-dispersed trees lost") + ylab(ytitle) + ggtitle(paste(input$country)) + scale_x_discrete(labels = c("0","25%","50%","75%","100%")) + theme_linedraw() +  coord_cartesian(ylim=ylim) + stat_summary(fun.y = "mean", geom="bar", fill = c("green","red","red","red","red"), alpha = alpha) + theme(axis.title.y = element_text(size = rel(1.5), angle = 90),axis.title.x = element_text(size = rel(1.5), angle = 0), axis.text = element_text(size = rel(1.5)), plot.title = element_text(size = rel(2.5),color = "grey")) + annotate("text", label = ipcts, x = 5, y =ylim[2]*.98, size = 12, color = "red")
#, alpha = 0.5, fill = c("green","red")) + geom_hline(yintercept = defData()$size_means, color = c("green","red"), size = 1, linetype = 2) 
                
                if (!input$rawdata)
                        p
                else
                        p + geom_jitter(alpha = 0.2)
                
                
        })
        output$stats <- renderText("Source: Osuri et al. 2016. Contrasting effects of defaunation on aboveground carbon storage across the global tropics. Nature Communications, XXX:YYY")
        
        
}

shinyApp(ui = ui, server = server)
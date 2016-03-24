library(shiny)

ui <- fluidPage(
        withMathJax(),
        fluidRow(
                
        column(2, tags$img(height = 180, width =150, src = "lorenz.png")),
        column(10,tags$h2("Exploring complexity with a simple equation"),tags$h4("by Jorge A. Ahumada"),
               tags$a("The quadratic map is a simple difference equation describing the dynamics of a population of individuals in time:"),uiOutput("eq1"),
               tags$a("The equation only has one parameter ",tags$em("r")," that controls the dynamics of the system. By just changing the value of ",tags$em("r"), "the system goes from stable equilibrium, to dampening cycles, stable cycles and finally chaos! (for ",tags$em("r")," values bigger than 3.5). Slide the r scale to see how the dynamics changes before your eyes!  Set ",tags$em("r")," to 3.9. See how the the system becomes chaotic (e.g. irregular). Now, without changing ",tags$em("r"),", change the initial value, and watch how the trajectory varies! This an important property of chaotic systems; ",tags$strong("their sensitivity to initial conditions."))
        )
),
        fluidRow(tags$br(), tags$hr(),
                column(3,
                       sliderInput(inputId = "r", label = tags$a("Choose a value of", tags$em("r")),
                                   value = 2.01, min = 1, max = 3.99, step = 0.01),
                       sliderInput(inputId = "t", label = "Select number of time units",
                                   value = 100, min = 100, max = 500),
                       sliderInput(inputId = "n0", label = "Select initial value",
                                   value =0.5, min = 0, max = 1, step = 0.01)
                ),
                column(9,plotOutput("tsplot"))
        ),
        fluidRow(tags$br(),tags$br(),
                column(5,plotOutput("cobweb")),
                column(7,
                       tags$h2("Another way of looking at the system"),
                       tags$a("We can look at the dynamics through a diagram called a cobweb.
                              A cobweb essentially has three elements:"),tags$br(),
                       tags$ol(
                      tags$li("A map of the function, in this case represented by the dark parabola. This is the function that maps the value of ",tags$em("N"),"at ",tags$em("t+1")," from the value of ",tags$em("N"),"at ",tags$em("t")),
                       tags$li("A line with slope 1 and intercept 0 which represents the equality function; in this case the dash straight line. Whenever the parabola intersects this line the value of ",tags$em("N")," is at equilibrium - it does not change between time steps."),
                      tags$li("The red lines map the trajectory of the system from the initial condition (first segment starting from the bottom of the diagram), to its next value, and continuing on.  When the system settles in a steady state the red lines will finish in the intersection between the parabola and the equality line (the steady state)")),
                      tags$p("As the dynamics gets more complex, you will see the red lines bouncing off the map in all different directions without settling in. This is what is called a ",tags$strong("chaotic attractor."),"Enjoy your exploration!")
        )
)
)
server <- function(input, output){
        
        data <- reactive({
                time <- 1:input$t
                n <- numeric()
                n[1] <- input$n0
                for(i in 2:input$t)
                        n[i] <- input$r*n[i-1]*(1-n[i-1])
                data.frame(time = time, n = n)
        })
        
        output$eq1 <- renderUI({
                withMathJax(helpText('$$N_{t+1} = rN_t(1 - N_t)$$'))
        })
        
        output$tsplot <- renderPlot({
                par(bg = "lightblue1")
                plot(data()$time, data()$n, type = "l", lwd = 3, main = "Population trajectory",
                     xlab = "time", ylab = "N")
        })
        output$cobweb <- renderPlot({
                par(bg = "lightgreen")
                n<-seq(from=0,to=1,length.out=100)
                np1<-array(dim=100)
                for(i in 1:length(n))
                        np1[i]<-input$r*n[i]*(1-n[i])
                
                plot(n,np1,type='l',xlab=expression(N[t]),ylab=expression(N[t+1]),
                     main = "Cobweb diagram",lwd=2)
                abline(0,1,lty = 2)
                
                start <- input$n0
                vert <- FALSE
                lines(x=c(start,start), y=c(0,input$r*start*(1-start)), col="red",lwd=1.5)
                for(i in 1:(2*input$t)) {
                        if(vert)
                        {
                                lines(x=c(start,start),y=c(start,input$r*start*(1-start)),co="red", lwd=1.5)
                                vert=FALSE
                        }
                        else
                        {
                                lines(x=c(start,
                                          input$r*start*(1-start)),
                                      y=c(input$r*start*(1-start),
                                          input$r*start*(1-start)),col="red", lwd = 1.5)
                                vert=TRUE
                                start=input$r*start*(1-start)
                        }
                }
        })
        
}

shinyApp(ui = ui, server = server)
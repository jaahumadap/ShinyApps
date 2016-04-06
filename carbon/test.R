# test code
input <- list(def = 25, country = "Australia")

carbon <- read.csv("data/sim_result_carbon.csv", header = T)
carbon <- tbl_df(carbon)
carbon <- filter(carbon, scenario != "control_2")

data <- filter(carbon, country == input$country)
data 
# get deforestation level
deflevel <- numeric()

if (input$def == 0) {
        deflevel <- 4
} else if (input$def == 25) {
        deflevel <- 5
} else if (input$def == 50) {
        deflevel <- 6
} else if (input$def == 75) {
        deflevel <- 7
} else 
        deflevel <- 8

data <- select(data, c(1:3,deflevel))

ggplot(data, aes(x=scenario,y=data[,4])) + geom_boxplot() + geom_jitter(alpha =0.2)

data2 <- melt(data[,-c(1,4)])
data2 <- filter(data2, scenario !="control_2")
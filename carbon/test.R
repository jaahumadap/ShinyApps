# test code
input <- list(def = 25, country = "Australia")

carbon <- read.csv("data/sim_result_carbon.csv", header = T)
#carbon <- tbl_df(carbon)
carbon <- filter(carbon, scenario != "control_2")

data <- filter(carbon, country == input$country)
#data 
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
size_means <- summarise(group_by(data,scenario), mean(rem_25))
size_means <- as.numeric(size_means$mean)
ggplot(data, aes(x=scenario, y = data[,4])) + stat_summary(fun.y = "mean", geom="point", size = size_means/10, alpha =0.5, color = c("green","red")) + geom_jitter(alpha =0.2) + xlab("") + ylab("carbon (tons/ha)")

data2 <- melt(data[,-c(1,4)])
data2 <- filter(data2, scenario !="control_2")

head(carbon)
tapply(carbon[,"rem_25"], carbon[,"scenario"], mean)

################################ 
##  ASA NBA Site Content      ##
##  Auction Draft Helper App  ##
##  Stewart Gibson            ##
##  10/9/17                   ##
################################

### Set working directory
#setwd("~/Documents/ASA/ASA Site Content/NBA/Auction Draft Shiny App")

### Load data and packages
load("NBA Players.RData")
require(rsconnect)
require(shiny)
require(ggplot2)

### Edit data
# NBA.players <- NBA.players[-which(is.na(NBA.players$Player)),-5]
# 
# for (i in 1:13) {
#   NBA.players[,i] <- as.character(NBA.players[,i])
# }
# for (i in 1:300) {
#   NBA.players$`League Value`[i] <- substr(
#     as.character(NBA.players$`League Value`[i]), start = 2, 
#     stop = nchar(as.character(NBA.players$`League Value`[i])))
#   NBA.players$`Proj Value`[i] <- substr(
#     as.character(NBA.players$`Proj Value`[i]), start = 2, 
#     stop = nchar(as.character(NBA.players$`Proj Value`[i])))
#   NBA.players$`Avg Cost`[i] <- substr(
#     as.character(NBA.players$`Avg Cost`[i]), start = 2,
#     stop = nchar(as.character(NBA.players$`Avg Cost`[i])))
#   NBA.players$`FGM/A`[i] <- unlist(strsplit(NBA.players$`FGM/A`[i],"/"))[1]
#   
# }
#   
# for (i in 2:13) {
#   NBA.players[,i] <- as.numeric(NBA.players[,i])
# }
# 
# colnames(NBA.players)[5] <- "FGM"
# 
# NBA.players <- cbind(NBA.players, matrix(NA, nrow = 300, ncol = 9))
# 
# for (i in which(!is.na(NBA.players$FGM))) {
#   for (j in 5:13)
#   NBA.players[i,(j+9)] <- length(which(NBA.players[,j] <= NBA.players[i,j]))/
#       length(which(!is.na(NBA.players$FGM)))
# }
# 
# colnames(NBA.players)[14:22] <- c("FGM Percentile","FG% Percentile","FTM Percentile",
#                                   "3PTM Percentile","PTS Percentile","REB Percentile",
#                                   "AST Percentile","STL Percentile","BLK Percentile")

################# The App ########################
ui <- fluidPage(
  titlePanel("NBA Yahoo Auction Draft App"),
  # Compare players
  selectInput(inputId = "Players", label = "Select player(s)",
              choices = unique(NBA.players$Player), multiple = F),
  
  # Establish barplot output
  plotOutput(outputId = "Barplot")
  )


server <- function(input, output) {

  output$Barplot <- renderPlot({
    dat <- t(NBA.players[which(NBA.players$Player %in% input$Players),-1])
    
    dotchart(x = dat[c(13:21),1], xlim = c(0,1), 
             main = paste(unlist(strsplit(input$Players," "))[1],
                          unlist(strsplit(input$Players," "))[2], "Statistic Percentiles"))
    abline(v = mean(dat[13:21,1]), col = 2, lty = 2)
    
  })
}

shinyApp(ui = ui, server = server)



#############################
# dat <- t(NBA.players[1,-1])
# 
# dotchart(x = dat[c(13:21),1], xlim = c(0,1), 
#          main = paste(unlist(strsplit(input$Players," "))[1:2], "Statistic Percentiles"))
# abline(v = mean(dat[13:21,1]), col = 2, lty = 2)

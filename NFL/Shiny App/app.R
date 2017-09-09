############################# 
##  ASA NFL Site Content   ##
##  Player Comparison App  ##
##  Stewart Gibson         ##
##  9/1/17                 ##
#############################

### Set working directory
#setwd("~/Documents/ASA/ASA Site Content/NFL/Shiny App")

### Load data and packages
load("clean_data.RData")
require(rsconnect)
require(shiny)
require(ggplot2)

### Drop David Johson (TE) from dataset
Fantasy.2016 <- Fantasy.2016[-which(Fantasy.2016$First.Last == "David Johnson" & Fantasy.2016$Pos == "TE"),]

## Create data frame of teams' total offensive fantasy points, RB fantasy points, and WR/TE
## fantasy points for each week
Team.Fantasy.totals.2016 <- unique(Fantasy.2016[c("Week", "Team")])
Team.Fantasy.totals.2016$Offensive.Fantasy.Points <- NA
Team.Fantasy.totals.2016$RB.Fantasy.Points <- NA
Team.Fantasy.totals.2016$WR_TE.Fantasy.Points <- NA

for (i in c(1:nrow(Team.Fantasy.totals.2016))) {
  Team.Fantasy.totals.2016$Offensive.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])], na.rm = T)
  
  Team.Fantasy.totals.2016$RB.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i] &
      Fantasy.2016$Pos == "RB")], na.rm = T)
  
  Team.Fantasy.totals.2016$WR_TE.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i] &
      Fantasy.2016$Pos %in% c("WR","TE"))], na.rm = T)
}

Fantasy.2016.proportions <- merge(Fantasy.2016[which(Fantasy.2016$Pos != 'Def'),c(1,6,7,10,14,15)], 
                                  Team.Fantasy.totals.2016, by = c("Team", "Week"))
Fantasy.2016.proportions$Offense.proportion <- Fantasy.2016.proportions$DK.points/
  Fantasy.2016.proportions$Offensive.Fantasy.Points
Fantasy.2016.proportions$RB.proportion <- NA
Fantasy.2016.proportions$RB.proportion[which(Fantasy.2016.proportions$Pos == "RB")] <-
  Fantasy.2016.proportions$DK.points[which(Fantasy.2016.proportions$Pos == "RB")]/
  Fantasy.2016.proportions$RB.Fantasy.Points[which(Fantasy.2016.proportions$Pos == "RB")]
Fantasy.2016.proportions$WR_TE.proportion <- NA
Fantasy.2016.proportions$WR_TE.proportion[which(Fantasy.2016.proportions$Pos %in% c("WR","TE"))] <-
  Fantasy.2016.proportions$DK.points[which(Fantasy.2016.proportions$Pos %in% c("WR","TE"))]/
  Fantasy.2016.proportions$WR_TE.Fantasy.Points[which(Fantasy.2016.proportions$Pos %in% c("WR","TE"))]

ui <- fluidPage(
  # Player dropdown
  selectInput(inputId = "players", label = "Select players to compare:",
              choices = sort(unique(Fantasy.2016$First.Last)), multiple = T),
  selectInput(inputId = "chart.type", label = "Select chart type:",
              choices = c("DKP Distributions","DKP vs. Point Total","DKP vs. Spread",
                          "DKP Proportions of Total Offense")),
  #actionButton(inputId = "create.chart", label = "Create Chart"),
  
  plotOutput(outputId = "chart")
)

server <- function(input, output) {
  
    output$chart <- renderPlot({
      if (input$chart.type == "DKP Distributions") {
        ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
               aes(x = DK.points)) +
          geom_density(aes(color = Initial.Last,fill = Initial.Last),alpha = 0.5) +
          ggtitle("Fantasy Points Distributions") +
          xlab("Fantasy Points") +
          ylab("Density") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_x_continuous(limits = c(-10,60))
        } else if (input$chart.type == "DKP vs. Point Total") {
          ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
               aes(x = Actual.Points, y = DK.points)) +
            geom_point(aes(color = Initial.Last)) +
            geom_smooth(aes(color = Initial.Last), se = F, method = 'loess') +
            ggtitle("Fantasy Points vs. Actual Points") +
            xlab("Actual Points") +
            ylab("Fantasy Points") +
            theme(plot.title = element_text(hjust = 0.5))
         } else if (input$chart.type == "DKP vs. Spread") {
           ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
                  aes(x = Actual.Spread, y = DK.points)) +
             geom_point(aes(color = Initial.Last)) +
             geom_smooth(aes(color = Initial.Last), se = F, method = 'loess') +
             ggtitle("Fantasy Points vs. Spread") +
             xlab("Spread") +
             ylab("Fantasy Points") +
             theme(plot.title = element_text(hjust = 0.5))
         } else if (input$chart.type == "DKP Proportions of Total Offense") {
           ggplot(data = Fantasy.2016.proportions[which(
             Fantasy.2016.proportions$First.Last %in% input$players),]) +
             geom_boxplot(aes(x = factor(Initial.Last, 
                                         levels=levels(factor(Fantasy.2016.proportions$Initial.Last[which(
                                           Fantasy.2016.proportions$First.Last %in% input$players)]))[
                                             order(tapply(Fantasy.2016.proportions$Offense.proportion[which(
                                               Fantasy.2016.proportions$First.Last %in% input$players)], 
                                               factor(Fantasy.2016.proportions$Initial.Last[which(
                                                 Fantasy.2016.proportions$First.Last %in% input$players)]), 
                                               median), 
                                               decreasing = T)]), y = Offense.proportion), 
                          outlier.alpha = 0, coef = 0) +
             theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
             xlab("") +
             ylab("% of Team Fantasy Points") +
             ggtitle("Proportion of Team Offensive Fantasy Points") +
             theme(plot.title = element_text(hjust = 0.5))
         } else {}
      })
  }

shinyApp(ui = ui, server = server)


ui <- fluidPage(
  titlePanel("Advanced Sports Analytics Player & Team Comparison App"),
  # Compare players or teams
  selectInput(inputId = "PlayerOrTeam", label = "Compare players or teams?",
                     choices = c("Players","Teams"), multiple = F),
  uiOutput("ui"),
  plotOutput(outputId = "chart")
)

server <- function(input, output) {
  output$ui <- renderUI({
    switch(input$PlayerOrTeam,
         "Players" =   list(selectInput(inputId = "players", label = "Select players to compare:",
                                   choices = sort(unique(Fantasy.2016$First.Last)), multiple = T),
                         selectInput(inputId = "chart.type", label = "Select chart type:",
                                     choices = c("DKP Distributions","Salary Distributions","DKP/$1K Distributions",
                                                 "DKP vs. Point Total","DKP vs. Spread",
                                                 "DKP Proportions of Total Offense"))),
         "Teams" = list(selectInput(inputId = "teams", label = "Select team:",
                              choices = toupper(sort(unique(Fantasy.2016$Team)))),
                       selectInput(inputId = "chart.type", label = "Select chart type:",
                                   choices = c("Position DKP vs. Opponent Average",
                                               "Team Fantasy Points per Actual Point",
                                               "Correlation Matrix - By Depth",
                                               "Fantasy Points Home/Away Split"))
         )
    )
  })
  output$chart <- renderPlot({
    if (input$chart.type == "DKP Distributions") {
      ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
             aes(x = DK.points), adjust = 0.5) +
        geom_density(aes(color = Initial.Last,fill = Initial.Last),alpha = 0.5) +
        ggtitle("Fantasy Points Distributions") +
        xlab("Fantasy Points") +
        ylab("Density") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_continuous(limits = c(-10,60))
    } else if (input$chart.type == "Salary Distributions") {
      ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players &
                                         Fantasy.2016$DK.salary > 0),],
             aes(x = DK.salary), adjust = 0.5) +
        geom_density(aes(color = Initial.Last,fill = Initial.Last),alpha = 0.5) +
        ggtitle("Salary Distributions") +
        xlab("Salary") +
        ylab("Density") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_continuous(limits = c(1000, 12000))
    } else if (input$chart.type == "DKP/$1K Distributions") {
      ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
             aes(x = DK.points/(DK.salary/1000)), adjust = 0.5) +
        geom_density(aes(color = Initial.Last,fill = Initial.Last),alpha = 0.5) +
        ggtitle("Fantasy Points/$1K Salary Distributions") +
        xlab("DKP/$1K") +
        ylab("Density") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_continuous(limits = c(-1, 12))
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
    } else if (input$chart.type == "Position DKP vs. Opponent Average") {
      ggplot(data = Fantasy.vs.Oppt[which(Fantasy.vs.Oppt$Pos %in% 
                                            c("QB","RB","WR/TE") &
                                            Fantasy.vs.Oppt == tolower(input$teams)),], 
             aes (x = Oppt.FP.allowed, y = Total.FP, group = Pos, color = Pos)) +
        geom_point() +
        geom_smooth(method = 'lm', formula = y ~ x) +
        ggtitle("Fantasy Points vs. Opponent Average FP/Game") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Opponent Average FP/Game") +
        ylab("Total Positional Fantasy Points") +
        scale_color_discrete(name = "Position")
      
    } else if (input$chart.type == "Team Fantasy Points per Actual Point") {
      ggplot(data = Team.Fantasy.totals.Pos.2016[which(
        Team.Fantasy.totals.Pos.2016$Team == tolower(input$teams)),], 
        aes(x = Actual.Points, y = Offensive.Fantasy.Points)) +
        geom_point(aes(group = Pos, color = Pos), size = 0.5) + 
        geom_smooth(aes(group = Pos, color = Pos), method = 'lm', formula = y ~ x) +
        ggtitle("Team Fantasy Points vs. Actual Points By Position") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Actual Points") +
        ylab("Total Positional Fantasy Points")
    } else if (input$chart.type == "Correlation Matrix - By Depth") {
      corr.mat <- Weekly.depth[which(Weekly.depth$Team == tolower(input$teams)),]
      corrplot(cor(as.matrix(corr.mat[,3:9])), method = 'number', type = 'lower', 
               col = colorRampPalette(c("red","gray90","green"))(100),
               title = "Correlation by Depth",
               mar=c(0,0,1,0))
    } else if (input$chart.type == "Fantasy Points Home/Away Split") {
      ggplot(data = Team.Fantasy.totals.2016[which(
        Team.Fantasy.totals.2016$Team == tolower(input$teams)),],
             aes(x=Offensive.Fantasy.Points, group=H.A, color=H.A, fill = H.A)) +
        geom_density(alpha=0.5) +
        xlab("Offensive Fantasy Points") +
        scale_fill_discrete(name = "Home/Away") +
        scale_color_discrete(name = "Home/Away") +
        ggtitle("Total Fantasy Points Home/Away Splits") +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else {}
  })
  
}

shinyApp(ui = ui, server = server)



#############################


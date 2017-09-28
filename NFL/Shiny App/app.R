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
require(corrplot)

################ Data Prep #######################
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

## Create dataframe of defenses and how many total offensive, QB, RB, and WR/TE fantasy points
## they allow per game
Defense.FP <- data.frame(Oppt = sort(unique(Fantasy.2016$Team)),
                         Offense.FP = NA, QB.FP = NA, RB.FP = NA, WR_TE.FP = NA)

uniq.teams <- sort(unique(Fantasy.2016$Team))

for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.2016[which(Fantasy.2016$Oppt == uniq.teams[i]),]
  Defense.FP$Offense.FP[i] <- mean(tapply(dat$DK.points, dat$Week, sum), na.rm = T)
  Defense.FP$QB.FP[i] <- mean(tapply(dat$DK.points[which(dat$Pos == "QB")], 
                                     dat$Week[which(dat$Pos == "QB")], sum), na.rm = T)
  Defense.FP$RB.FP[i] <- mean(tapply(dat$DK.points[which(dat$Pos == "RB")], 
                                     dat$Week[which(dat$Pos == "RB")], sum), na.rm = T)
  Defense.FP$WR_TE.FP[i] <- mean(tapply(dat$DK.points[which(dat$Pos %in% c("WR","TE"))], 
                                        dat$Week[which(dat$Pos %in% c("WR","TE"))], sum), na.rm = T)
}

## Create dataframe with the sum of each teams' total and weekly positional fantasy production and
## the average fantasy production allowed by the opponent.
Fantasy.vs.Oppt <- unique(Fantasy.2016[,c("Team","Week","Pos")])
Fantasy.vs.Oppt$Pos <- as.character(Fantasy.vs.Oppt$Pos)
Fantasy.vs.Oppt$Pos[which(Fantasy.vs.Oppt$Pos == "WR")] <- "WR/TE"
Fantasy.vs.Oppt$Pos[which(Fantasy.vs.Oppt$Pos == "TE")] <- "All"
Fantasy.vs.Oppt$Oppt <- NA
Fantasy.vs.Oppt$Oppt.FP.allowed <- NA
Fantasy.vs.Oppt$Total.FP <- NA

for (i in 1:nrow(Fantasy.vs.Oppt)) {
  Fantasy.vs.Oppt$Oppt[i] <- as.character(Fantasy.2016$Oppt)[which(
    Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i])][1]
  if (Fantasy.vs.Oppt$Pos[i] == "QB") {
    Fantasy.vs.Oppt$Oppt.FP.allowed[i] <- Defense.FP$QB.FP[which(
      Defense.FP$Oppt == Fantasy.vs.Oppt$Oppt[i])]
    Fantasy.vs.Oppt$Total.FP[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i] &
        Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
        as.character(Fantasy.2016$Pos) == "QB")], na.rm = T)
  } else if (Fantasy.vs.Oppt$Pos[i] == "RB") {
    Fantasy.vs.Oppt$Oppt.FP.allowed[i] <- Defense.FP$RB.FP[which(
      Defense.FP$Oppt == Fantasy.vs.Oppt$Oppt[i])]
    Fantasy.vs.Oppt$Total.FP[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i] &
        Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
        as.character(Fantasy.2016$Pos) == "RB")], na.rm = T)
  } else if (Fantasy.vs.Oppt$Pos[i] == "WR/TE") {
    Fantasy.vs.Oppt$Oppt.FP.allowed[i] <- Defense.FP$WR_TE.FP[which(
      Defense.FP$Oppt == Fantasy.vs.Oppt$Oppt[i])]
    Fantasy.vs.Oppt$Total.FP[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Week == Fantasy.vs.Oppt$Week[i] &
        Fantasy.2016$Team == Fantasy.vs.Oppt$Team[i] &
        as.character(Fantasy.2016$Pos) %in% c("WR","TE"))], na.rm = T)
  } else {}
}

Fantasy.vs.Oppt <- Fantasy.vs.Oppt[which(Fantasy.vs.Oppt$Pos %in% c("QB","RB","WR/TE")),]
Fantasy.vs.Oppt$Pos <- as.factor(Fantasy.vs.Oppt$Pos)

## Create a dataframe of each teams' weekly actual point and offensive fantasy point totals
## BY POSITONAL UNIT (QB, RB, WR/TE)
Team.Fantasy.totals.Pos.2016 <- unique(Fantasy.2016[c("Week", "Team", "Pos")])
Team.Fantasy.totals.Pos.2016$Actual.Points <- NA
Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points <- NA

Team.Fantasy.totals.Pos.2016 <- Team.Fantasy.totals.Pos.2016[
  which(Team.Fantasy.totals.Pos.2016$Pos == "QB" |
          Team.Fantasy.totals.Pos.2016$Pos == "RB" |
          Team.Fantasy.totals.Pos.2016$Pos == "WR")
  ,]

Team.Fantasy.totals.Pos.2016$Pos <- as.character(Team.Fantasy.totals.Pos.2016$Pos)
Team.Fantasy.totals.Pos.2016$Pos[which(
  Team.Fantasy.totals.Pos.2016$Pos %in% c('WR','TE'))] <- "WR/TE"

for (i in c(1:nrow(Team.Fantasy.totals.Pos.2016))) {
  if (Team.Fantasy.totals.Pos.2016$Pos[i] == "QB") {
    Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
      Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
        Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i] &
        Fantasy.2016$Pos == 'QB')], na.rm = T)} else if (
          Team.Fantasy.totals.Pos.2016$Pos[i] == "RB") {
          Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points[i] <- 
            sum(Fantasy.2016$DK.points[which(
              Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
                Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i] &
                Fantasy.2016$Pos == 'RB')], na.rm = T)} else {
                  Team.Fantasy.totals.Pos.2016$Offensive.Fantasy.Points[i] <- 
                    sum(Fantasy.2016$DK.points[which(
                      Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
                        Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i] &
                        Fantasy.2016$Pos %in% c("WR","TE"))], na.rm = T)}
  
  Team.Fantasy.totals.Pos.2016$Actual.Points[i] <- Fantasy.2016$Actual.Points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.Pos.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.Pos.2016$Week[i])][1]
}

## Create dataset for each team/week combination.  Fill in QB1, RB1, RB2, WR1, WR2, WR3, TE1
## to be top, second, third fantasy point scorer at each position for each week.  This will
## create a correlation matrix slightly different from the one above, allowing for flexibility
## in which player fills each depth spot on a given week.
Weekly.depth <- unique(Fantasy.2016[c("Week","Team")])
Weekly.depth <- cbind(Weekly.depth, matrix(NA, ncol = 8, nrow = nrow(Weekly.depth)))
colnames(Weekly.depth)[3:10] <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1", "Actual.Points")

for (i in 1:nrow(Weekly.depth)) {
  dat <- Fantasy.2016[which(Fantasy.2016$Week == Weekly.depth$Week[i] &
                                   Fantasy.2016$Team == Weekly.depth$Team[i]),]
  # Fill out matrix
  Weekly.depth$QB1[i] <- sort(dat$DK.points[which(dat$Pos == "QB")], decreasing = T)[1]
  Weekly.depth$RB1[i] <- sort(dat$DK.points[which(dat$Pos == "RB")], decreasing = T)[1]
  Weekly.depth$RB2[i] <- sort(dat$DK.points[which(dat$Pos == "RB")], decreasing = T)[2]
  Weekly.depth$WR1[i] <- sort(dat$DK.points[which(dat$Pos == "WR")], decreasing = T)[1]
  Weekly.depth$WR2[i] <- sort(dat$DK.points[which(dat$Pos == "WR")], decreasing = T)[2]
  Weekly.depth$WR3[i] <- sort(dat$DK.points[which(dat$Pos == "WR")], decreasing = T)[3]
  Weekly.depth$TE1[i] <- sort(dat$DK.points[which(dat$Pos == "TE")], decreasing = T)[1]
  Weekly.depth$Actual.Points[i] <- dat$Actual.Points[1]
}
Weekly.depth$RB2[which(is.na(Weekly.depth$RB2))] <- 0

## Create a dataframe of each teams' weekly spread and offensive fantasy point totals
Team.Fantasy.totals.2016 <- unique(Fantasy.2016[c("Week", "Team")])
Team.Fantasy.totals.2016$Spread <- NA
Team.Fantasy.totals.2016$Offensive.Fantasy.Points <- NA
Team.Fantasy.totals.2016$H.A <- NA

for (i in c(1:nrow(Team.Fantasy.totals.2016))) {
  Team.Fantasy.totals.2016$Spread[i] <- Fantasy.2016$Actual.Spread[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])][1]
  
  Team.Fantasy.totals.2016$Offensive.Fantasy.Points[i] <- sum(Fantasy.2016$DK.points[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])], na.rm = T)
  
  Team.Fantasy.totals.2016$H.A[i] <- as.character(Fantasy.2016$h.a[which(
    Fantasy.2016$Team == Team.Fantasy.totals.2016$Team[i] &
      Fantasy.2016$Week == Team.Fantasy.totals.2016$Week[i])])[1]
}
Team.Fantasy.totals.2016$H.A[which(Team.Fantasy.totals.2016$H.A == 'h')] <- "Home"
Team.Fantasy.totals.2016$H.A[which(Team.Fantasy.totals.2016$H.A == 'a')] <- "Away"

################# The App ########################
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
        scale_x_continuous(limits = c(0,220)) +
        scale_fill_discrete(name = "Home/Away") +
        scale_color_discrete(name = "Home/Away") +
        ggtitle("Total Fantasy Points Home/Away Splits") +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else {}
  })
  
}

shinyApp(ui = ui, server = server)



#############################


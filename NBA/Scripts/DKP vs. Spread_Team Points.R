############################################################# 
##  ASA NBA Site Content                                   ##
##  Player Fantasy points vs. spread and team point total  ##
##  Stewart Gibson                                         ##
##  10/31/17                                               ##
#############################################################

### Load packages
require(ggplot2)

### Load data
load("NBA/data/NBA_2017.RData")

### Create dataframe of each players season point totals, only include players who scored above
### desired cutoff levels or have an average salary above a desired level.
season.totals <- cbind(unique(NBA_17[,c("First..Last","Team")]),
                       data.frame(DK.points.total = NA, Avg.DK.salary = NA, N.games = NA))

for  (i in 1:nrow(season.totals)) {
  season.totals$DK.points.total[i] <- sum(NBA_17$DKP[which(
    NBA_17$First..Last == season.totals$First..Last[i] &
      NBA_17$Team == season.totals$Team[i])], na.rm = T)
  
  season.totals$Avg.DK.salary[i] <- mean(NBA_17$DK.Sal[which(
    NBA_17$First..Last == season.totals$First..Last[i] &
      NBA_17$Team == season.totals$Team[i])], na.rm = T)
  
  season.totals$N.games[i] <- length(which(
    NBA_17$First..Last == season.totals$First..Last[i] &
      NBA_17$Team == season.totals$Team[i]))
}

### Create charts for each team.  Include only top 9 players (by total fantasy points) on
### each team chart
uniq.teams <- sort(unique(NBA_17$Team))

for (i in 1:length(uniq.teams)) {
  Players.df <- season.totals[which(season.totals$Team == uniq.teams[i]),]
  Players.df <- Players.df[order(Players.df$DK.points.total, decreasing = T),]

  dat <- NBA_17[which(NBA_17$Team == uniq.teams[i] &
                                   #Fantasy.2016_2017$Current_team == 1 &
                                   #Fantasy.2016_2017$N_Current_team >= 3
                        as.character(NBA_17$First..Last) %in% 
                        c(as.character(Players.df$First..Last[1:9])) &
                        !is.na(NBA_17$DKP)),]
  
  ggplot(data = dat, aes(x = Team.pts, y = DKP, color = First..Last, group = First..Last)) +
    geom_point(size = 0.5) +
    geom_smooth(se = F, size = 0.5) +
    ylab("Fantasy Points") +
    xlab("Team Point Total") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Team Point Total", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
  ggsave(paste("NBA/Visualizations/Fantasy Point Comp Charts/Vs. Point Total/",uniq.teams[i],
               ".png", sep = ""))
  
  ggplot(data = dat, aes(x = Opp.pts - Team.pts, y = DKP, color = First..Last, group = First..Last)) +
    geom_point(size = 0.5) +
    geom_smooth(se = F, size = 0.5) +
    ylab("Fantasy Points") +
    xlab("Game Spread") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Game Spread", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
  ggsave(paste("NBA/Visualizations/Fantasy Point Comp Charts/Vs. Spread/",uniq.teams[i],
               ".png", sep = ""))
}
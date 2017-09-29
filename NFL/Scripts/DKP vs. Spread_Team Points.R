############################################################# 
##  ASA NFL Site Content                                   ##
##  Player Fantasy points vs. spread and team point total  ##
##  Stewart Gibson                                         ##
##  7/9/17                                                 ##
#############################################################

## Load packages
require(ggplot2)

## Load data
load("NFL/data/clean_data_2016_2017.RData")

## Create charts
uniq.teams <- sort(unique(Fantasy.2016_2017$Team))

for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.2016_2017[which(Fantasy.2016_2017$Team == uniq.teams[i] &
                                   Fantasy.2016_2017$Current_team == 1 &
                                   Fantasy.2016_2017$N_Current_team >= 3),]
  
  ggplot(data = dat, aes(x = Actual.Points, y = DK.points, color = Initial.Last, group = First.Last)) +
    geom_point() +
    geom_smooth(method = "lm",formula = y ~ poly(x,max(1,(most.current-2))),se = F) +
    ylab("Fantasy Points") +
    xlab("Team Point Total") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Team Point Total", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
    
  ggsave(paste("NFL/Visualizations/Fantasy Point Comp Charts/Vs. Point Total/",uniq.teams[i],
               ".png", sep = ""))
  
  ggplot(data = dat, aes(x = Spread, y = DK.points, color = Initial.Last, group = First.Last)) +
    geom_point() +
    geom_smooth(method = "lm",formula = y ~ poly(x,max(1,(most.current-2))),se = F) +
    ylab("Fantasy Points") +
    xlab("Game Spread") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Game Spread", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("NFL/Visualizations/Fantasy Point Comp Charts/Vs. Spread/",uniq.teams[i],
               ".png", sep = ""))
  }
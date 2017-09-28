############################################################# 
##  ASA NFL Site Content                                   ##
##  Player Fantasy points vs. spread and team point total  ##
##  Stewart Gibson                                         ##
##  7/9/17                                                 ##
#############################################################

## Load packages
require(ggplot2)

## Load data
load("data/clean_data_2016_2017.RData")

## Create charts
uniq.teams <- sort(unique(Fantasy.2016_2017$Team))

for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.2016_2017[which(Fantasy.2016_2017$Team == uniq.teams[i]),]
  
  ggplot(data = dat, aes(x = Actual.Points, y = DK.points, color = Initial.Last, group = First.Last)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ poly(x,3), se = F) +
    ylab("Fantasy Points") +
    xlab("Team Point Total") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Team Point Total", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
    
  ggsave(paste("Visualizations/Fantasy Point Comp Charts/Vs. Point Total/",uniq.teams[i],
               ".png", sep = ""))
  
  ggplot(data = dat, aes(x = Actual.Spread, y = DK.points, color = Initial.Last, group = First.Last)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ poly(x,3), se = F) +
    ylab("Fantasy Points") +
    xlab("Game Spread") +
    ggtitle(paste(toupper(uniq.teams[i]), "Player Fantasy Points vs. Game Spread", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_discrete(name = "Player")
  
  ggsave(paste("Visualizations/Fantasy Point Comp Charts/Vs. Spread/",uniq.teams[i],
               ".png", sep = ""))
  }
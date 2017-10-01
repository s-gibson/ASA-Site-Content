########################################################## 
##  ASA NFL Site Content                                ##
##  Ch. 4: How do share positional fantasy production?  ##
##  Stewart Gibson                                      ##
##  8/1/17                                              ##
##########################################################

## Load packages
require(ggplot2)

## Load data
load("NFL/data/clean_data_2016_2017.RData")

# ## Create trendlines for all teams and their positional units, only charting players who are 
# ## above positional cutoff threshold
# uniq.teams <- sort(unique(Fantasy.2016$Team))
# 
# for (i in 1:length(uniq.teams)) {
#   dat.QB <- QB.2016[which(QB.2016$Team == uniq.teams[i] & 
#                       QB.2016$First.Last %in% QB.list),]
#   dat.RB <- RB.2016[which(RB.2016$Team == uniq.teams[i] & 
#                             RB.2016$First.Last %in% RB.list),]
#   dat.WR_TE <- WR_TE.2016[which(WR_TE.2016$Team == uniq.teams[i] & 
#                                   WR_TE.2016$First.Last %in% WR_TE.list),]
# 
#   # Plot QB trendline
#   ggplot(data = dat.QB, aes(x = Week, y = DK.points, group = Initial.Last, 
#                             color = Initial.Last)) +
#     geom_line() +
#     geom_point() +
#     xlab('Week') +
#     ylab('Fantasy Points') +
#     ggtitle(paste(toupper(uniq.teams[i]), "QB Fantasy Trendline", sep = " ")) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_continuous(breaks = c(1:17)) +
#     scale_color_discrete(name = "Player")
#   
#   ggsave(paste("Visualizations/Ch_4/Restrictive/QB/QB_", uniq.teams[i],".png", sep = "") )
#   
#   # Plot RB trendline
#   ggplot(data = dat.RB, aes(x = Week, y = DK.points, group = Initial.Last, 
#                             color = Initial.Last)) +
#     geom_line() +
#     geom_point() +
#     xlab('Week') +
#     ylab('Fantasy Points') +
#     ggtitle(paste(toupper(uniq.teams[i]), "RB Fantasy Trendline", sep = " ")) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_continuous(breaks = c(1:17)) +
#     scale_color_discrete(name = "Player")
#   
#   ggsave(paste("Visualizations/Ch_4/Restrictive/RB/RB_", uniq.teams[i],".png", sep = "") )
#   
#   # Plot WR/TE trendline
#   ggplot(data = dat.WR_TE, aes(x = Week, y = DK.points, group = Initial.Last, 
#                                color = Initial.Last)) +
#     geom_line() +
#     geom_point() +
#     xlab('Week') +
#     ylab('Fantasy Points') +
#     ggtitle(paste(toupper(uniq.teams[i]), "WR/TE Fantasy Trendline", sep = " ")) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_continuous(breaks = c(1:17)) +
#     scale_color_discrete(name = "Player")
#     
#   ggsave(paste("Visualizations/Ch_4/Restrictive/WR_TE/WR_TE_", uniq.teams[i],".png", sep = "") )
#   
# }
# 
# 
# ## Create trendlines for all teams and their positional units, NOT RESTRICTING BY TOTAL DKP
# ## CUTOFF.  Can possibly include these charts in appendix
# for (i in 1:length(uniq.teams)) {
#   dat.QB <- QB.2016[which(QB.2016$Team == uniq.teams[i]),]
#   dat.RB <- RB.2016[which(RB.2016$Team == uniq.teams[i]),]
#   dat.WR_TE <- WR_TE.2016[which(WR_TE.2016$Team == uniq.teams[i]),]
#   
#   # Plot QB trendline
#   ggplot(data = dat.QB, aes(x = Week, y = DK.points, group = Initial.Last, 
#                             color = Initial.Last)) +
#     geom_line() +
#     geom_point() +
#     xlab('Week') +
#     ylab('Fantasy Points') +
#     ggtitle(paste(toupper(uniq.teams[i]), "QB Fantasy Trendline", sep = " ")) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_continuous(breaks = c(1:17)) +
#     scale_color_discrete(name = "Player")
#   
#   ggsave(paste("Visualizations/Ch_4/Non_Restrictive/QB/QB_", uniq.teams[i],".png", sep = "") )
#   
#   # Plot RB trendline
#   ggplot(data = dat.RB, aes(x = Week, y = DK.points, group = Initial.Last, 
#                             color = Initial.Last)) +
#     geom_line() +
#     geom_point() +
#     xlab('Week') +
#     ylab('Fantasy Points') +
#     ggtitle(paste(toupper(uniq.teams[i]), "RB Fantasy Trendline", sep = " ")) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_continuous(breaks = c(1:17)) +
#     scale_color_discrete(name = "Player")
#   
#   ggsave(paste("Visualizations/Ch_4/Non_Restrictive/RB/RB_", uniq.teams[i],".png", sep = "") )
#   
#   # Plot WR/TE trendline
#   ggplot(data = dat.WR_TE, aes(x = Week, y = DK.points, group = Initial.Last, 
#                                color = Initial.Last)) +
#     geom_line() +
#     geom_point() +
#     xlab('Week') +
#     ylab('Fantasy Points') +
#     ggtitle(paste(toupper(uniq.teams[i]), "WR/TE Fantasy Trendline", sep = " ")) +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_continuous(breaks = c(1:17)) +
#     scale_color_discrete(name = "Player")
#   
#   ggsave(paste("Visualizations/Ch_4/Non_Restrictive/WR_TE/WR_TE_", uniq.teams[i],".png", sep = "") )
#   
# }

## Another way to understand how an offense's total fantasy point total is distributed among
## is players is to chart the proportion of total offensive fantasy points accounted for by 
## individual players.

uniq.teams <- sort(unique(Fantasy.2016_2017$Team))

for (i in 1:length(uniq.teams)) {
  dat <- Fantasy.2016_2017[which(Fantasy.2016_2017$Team == uniq.teams[i] &
                                               Fantasy.2016_2017$Current_team == 1),]
  
  # set color scheme
  if (dat$Team[length(dat$Team)] == "ari") {col.set = c("darkred","white")
  } else if (dat$Team[length(dat$Team)] == "atl") {col.set = c("black","red")
  } else if (dat$Team[length(dat$Team)] == "bal") {col.set = c("darkorchid4","goldenrod")
  } else if (dat$Team[length(dat$Team)] == "buf") {col.set = c("mediumblue","white")
  } else if (dat$Team[length(dat$Team)] == "car") {col.set = c("black","deepskyblue")
  } else if (dat$Team[length(dat$Team)] == "chi") {col.set = c("darkblue","orange")
  } else if (dat$Team[length(dat$Team)] == "cin") {col.set = c("darkorange","black")
  } else if (dat$Team[length(dat$Team)] == "cle") {col.set = c("darkorange2","chocolate4")
  } else if (dat$Team[length(dat$Team)] == "dal") {col.set = c("gray60","midnightblue")
  } else if (dat$Team[length(dat$Team)] == "den") {col.set = c("navy","darkorange2")
  } else if (dat$Team[length(dat$Team)] == "det") {col.set = c("dodgerblue","grey")
  } else if (dat$Team[length(dat$Team)] == "gnb") {col.set = c("green4","yellow")
  } else if (dat$Team[length(dat$Team)] == "hou") {col.set = c("red3","royalblue4")
  } else if (dat$Team[length(dat$Team)] == "ind") {col.set = c("white","blue3")
  } else if (dat$Team[length(dat$Team)] == "jac") {col.set = c("turquoise","khaki")
  } else if (dat$Team[length(dat$Team)] == "kan") {col.set = c("red","gold")
  } else if (dat$Team[length(dat$Team)] == "lac") {col.set = c("deepskyblue","yellow")
  } else if (dat$Team[length(dat$Team)] == "lar") {col.set = c("navy","white")
  } else if (dat$Team[length(dat$Team)] == "mia") {col.set = c("turquoise","orange")
  } else if (dat$Team[length(dat$Team)] == "min") {col.set = c("purple","gold")
  } else if (dat$Team[length(dat$Team)] == "nor") {col.set = c("lightgoldenrod","black")
  } else if (dat$Team[length(dat$Team)] == "nwe") {col.set = c("royalblue4","red4")
  } else if (dat$Team[length(dat$Team)] == "nyg") {col.set = c("blue","red")
  } else if (dat$Team[length(dat$Team)] == "nyj") {col.set = c("darkgreen","white")
  } else if (dat$Team[length(dat$Team)] == "oak") {col.set = c("grey","black")
  } else if (dat$Team[length(dat$Team)] == "phi") {col.set = c("seagreen4","black")
  } else if (dat$Team[length(dat$Team)] == "pit") {col.set = c("black", "yellow")
  } else if (dat$Team[length(dat$Team)] == "sea") {col.set = c("navy","chartreuse")
  } else if (dat$Team[length(dat$Team)] == "sfo") {col.set = c("red","darkgoldenrod")
  } else if (dat$Team[length(dat$Team)] == "tam") {col.set = c("red3","burlywood4")
  } else if (dat$Team[length(dat$Team)] == "ten") {col.set = c("steelblue1","steelblue4")
  } else {col.set = c("orangered4","gold")}
  
  # Team Offense
  ggplot(data = dat) +
    geom_boxplot(aes(x = factor(Initial.Last, 
                                levels=levels(factor(dat$Initial.Last))[
                                  order(tapply(dat$DK.points/dat$Total.DKP, factor(dat$Initial.Last), 
                                               median), 
                                        decreasing = T)]), y = DK.points/Total.DKP), 
                 outlier.alpha = 0, coef = 0, color = col.set[2], fill = col.set[1]) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("% of Team Fantasy Points") +
    ggtitle(paste(toupper(dat$Team[i]), "Proportion of Team Offensive Fantasy Points", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
    
  ggsave(paste("NFL/Visualizations/Player Fantasy Proportions/Total Offense/",
               uniq.teams[i],".png", sep = ""))
  
  # RB
  ggplot(data = dat[which(dat$Pos == "RB"),]) +
    geom_boxplot(aes(x = factor(Initial.Last, 
                                levels=levels(factor(dat$Initial.Last))[
                                  order(tapply(dat$DK.points/dat$RB.DKP, factor(dat$Initial.Last), 
                                               median),decreasing = T)]), y = DK.points/RB.DKP), 
                 outlier.alpha = 0, coef = 0, color = col.set[2], fill = col.set[1]) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("% of RB Fantasy Points") +
    ggtitle(paste(toupper(dat$Team[i]), "Proportion of Team RB Fantasy Points", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("NFL/Visualizations/Player Fantasy Proportions/RB/",
               dat$Team[i],".png", sep = ""))
  
  # WR/TE
  ggplot(data = dat[which(dat$Pos %in% c("WR","TE")),]) +
    geom_boxplot(aes(x = factor(Initial.Last, 
                                levels=levels(factor(dat$Initial.Last))[
                                  order(tapply(dat$DK.points/dat$WR_TE.DKP, factor(dat$Initial.Last), 
                                               median),decreasing = T)]), y = DK.points/WR_TE.DKP), 
                 outlier.alpha = 0, coef = 0, color = col.set[2], fill = col.set[1]) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") +
    ylab("% of WR/TE Fantasy Points") +
    ggtitle(paste(toupper(dat$Team[i]), "Proportion of Team WR/TE Fantasy Points", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("NFL/Visualizations/Player Fantasy Proportions/WR_TE/",
               dat$Team[i],".png", sep = ""))
}
  

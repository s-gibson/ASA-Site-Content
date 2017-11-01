############################################################## 
##  ASA NBA Site Content                                    ##
##  Player Fantasy point, salary, and FP/$1K distributions  ##
##  Stewart Gibson                                          ##
##  10/31/17                                                ##
##############################################################

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

DKP.cutoff <- 20
DK.Sal.cutoff <- 5000
N.games.cutoff <- 2

## Set unique players list (only considering players who scored above their positional cutoff)
uniq.players <- as.character(season.totals$First..Last[which(
  (season.totals$DK.points.total > DKP.cutoff | season.totals$Avg.DK.salary > DK.Sal.cutoff) &
    season.totals$N.games > N.games.cutoff)])

for (i in 1:length(uniq.players)) {
  dat <- NBA_17[which(NBA_17$First..Last == uniq.players[i]),]
  
  # set color scheme
  if (dat$Team[length(dat$Team)] == "atl") {col.set = c("red","yellow")
  } else if (dat$Team[length(dat$Team)] == "bkn") {col.set = c("black","grey")
  } else if (dat$Team[length(dat$Team)] == "bos") {col.set = c("green4","white")
  } else if (dat$Team[length(dat$Team)] == "cha") {col.set = c("darkslateblue","darkcyan")
  } else if (dat$Team[length(dat$Team)] == "chi") {col.set = c("red","black")
  } else if (dat$Team[length(dat$Team)] == "cle") {col.set = c("darkred","gold")
  } else if (dat$Team[length(dat$Team)] == "dal") {col.set = c("blue","navy")
  } else if (dat$Team[length(dat$Team)] == "den") {col.set = c("steelblue1","gold")
  } else if (dat$Team[length(dat$Team)] == "det") {col.set = c("red","blue")
  } else if (dat$Team[length(dat$Team)] == "gsw") {col.set = c("dodgerblue","gold")
  } else if (dat$Team[length(dat$Team)] == "hou") {col.set = c("red","grey")
  } else if (dat$Team[length(dat$Team)] == "ind") {col.set = c("navy","gold")
  } else if (dat$Team[length(dat$Team)] == "lac") {col.set = c("blue","red")
  } else if (dat$Team[length(dat$Team)] == "lal") {col.set = c("purple3","gold")
  } else if (dat$Team[length(dat$Team)] == "mem") {col.set = c("royalblue4","steelblue")
  } else if (dat$Team[length(dat$Team)] == "mia") {col.set = c("orangered4","black")
  } else if (dat$Team[length(dat$Team)] == "mil") {col.set = c("forestgreen","wheat")
  } else if (dat$Team[length(dat$Team)] == "min") {col.set = c("royalblue4","forestgreen")
  } else if (dat$Team[length(dat$Team)] == "nor") {col.set = c("royalblue4","gold3")
  } else if (dat$Team[length(dat$Team)] == "nyk") {col.set = c("blue","orange")
  } else if (dat$Team[length(dat$Team)] == "okc") {col.set = c("steelblue3","orange")
  } else if (dat$Team[length(dat$Team)] == "orl") {col.set = c("dodgerblue","black")
  } else if (dat$Team[length(dat$Team)] == "phi") {col.set = c("blue","white")
  } else if (dat$Team[length(dat$Team)] == "pho") {col.set = c("blueviolet","orange")
  } else if (dat$Team[length(dat$Team)] == "por") {col.set = c("black","red")
  } else if (dat$Team[length(dat$Team)] == "sac") {col.set = c("darkviolet","grey")
  } else if (dat$Team[length(dat$Team)] == "sas") {col.set = c("black","grey")
  } else if (dat$Team[length(dat$Team)] == "tor") {col.set = c("darkviolet","red")
  } else if (dat$Team[length(dat$Team)] == "uta") {col.set = c("midnightblue","seagreen")
  } else {col.set = c("blue4","red")}
  
  # Plot distributions
  ggplot(data = dat) +
    stat_density(aes(DKP), adjust = 0.5, color = col.set[2], fill = col.set[1], size = 1) +
    xlab("DK Fantasy Points") +
    scale_x_continuous(limits = c(-2,100)) +
    scale_y_continuous(limits = c(0, .2)) +
    ggtitle(paste(dat$First..Last[1], "DK Points Distribution", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste('NBA/Visualizations/Distributions/DK Points/', 
               dat$First..Last[1],'.png', sep = ''))

  ggplot(data = dat[which(dat$DK.Sal > 0),]) +
    stat_density(aes(DK.Sal), adjust = 0.5, color = col.set[2], fill = col.set[1], size = 1) +
    xlab("DK Salary") +
    scale_x_continuous(limits = c(2500, 13000)) +
    scale_y_continuous(limits = c(0, 0.005)) +
    ggtitle(paste(dat$First.Last[1], "DK Salary Distribution", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste('NBA/Visualizations/Distributions/Salary/',
               dat$First..Last[1],'.png', sep = ''))

  ggplot(data = dat[which(dat$DK.Sal > 0),]) +
    stat_density(aes(DKP/(DK.Sal/1000)), adjust = 0.5, color = col.set[2], fill = col.set[1], size = 1) +
    xlab("DK Points/$1K") +
    scale_x_continuous(limits = c(-0.5, 15)) +
    scale_y_continuous(limits = c(0, .75)) +
    ggtitle(paste(dat$First.Last[1], "DK Fantasy Points/$1K", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste('NBA/Visualizations/Distributions/DKP per $1K/', 
               dat$First..Last[1],'.png', sep = ''))

}
################################## 
##  ASA NFL Site Content        ##
##  Top Positional Probability  ##
##  Stewart Gibson              ##
##  8/31/17                     ##
##################################

### Import data, separate into 2016 and 2015 datasets
load("NFL/data/clean_data.RData")

### Create list of unique offensive players
uniq.players <- unique(Fantasy.2016[which(Fantasy.2016$Pos != "Def"),c("First.Last","Team")])
                                                              
### Read in projected players .csv
DraftKings.players <- read.csv("NFL/data/DraftKings.Lineup.csv")
# Keep only players who aren't injured
DraftKings.players <- DraftKings.players[which(is.na(DraftKings.players$Injury)),]

### Create large for loop that runs n simulations of drawing a fantasy total from every
### player's fantasy distribution function.  Store in a data frame for each position.
iter <- 10000

### Quarterback (QB)
uniq.QB <- unique(DraftKings.players$Name[which(DraftKings.players$Pos == "QB")])
samp.QB <- matrix(NA, nrow = iter, ncol = length(uniq.QB))
colnames(samp.QB) <- uniq.QB
for (i in c(1:length(uniq.QB))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.QB[i])),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK.points)
    
    samp.QB[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else (samp.QB[,i] <- 0)
}

Top.QB <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.QB[i,1] <- which.max(samp.QB[i,])
}

Top.QB.probs <- data.frame(Player = uniq.QB, Matchup = NA, Salary = NA, Prob = NA)
for (i in c(1:nrow(Top.QB.probs))) {
  Top.QB.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                             Top.QB.probs$Player[i])]
  Top.QB.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                 Top.QB.probs$Player[i])])
  Top.QB.probs$Prob[i] <- length(Top.QB[which(Top.QB[,1] == i)])/iter
}
Top.QB.probs <- cbind(data.frame(Rank = 1:nrow(Top.QB.probs)), Top.QB.probs[order(Top.QB.probs$Prob,decreasing = T),])
write.csv(Top.QB.probs, file = "NFL/TPPs/QB.csv", row.names = F)

### Running Back (RB)
uniq.RB <- unique(DraftKings.players$Name[which(DraftKings.players$Pos == "RB")])
samp.RB <- matrix(NA, nrow = iter, ncol = length(uniq.RB))
colnames(samp.RB) <- uniq.RB
for (i in c(1:length(uniq.RB))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.RB[i])),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK.points)
    
    samp.RB[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else (samp.RB[,i] <- 0)
}

Top.RB <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.RB[i,1] <- which.max(samp.RB[i,])
}

Top.RB.probs <- data.frame(Player = uniq.RB, Matchup = NA, Salary = NA, Prob = NA)
for (i in c(1:nrow(Top.RB.probs))) {
  Top.RB.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.RB.probs$Player[i])]
  Top.RB.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.RB.probs$Player[i])])
  Top.RB.probs$Prob[i] <- length(Top.RB[which(Top.RB[,1] == i)])/iter
}
Top.RB.probs <- cbind(data.frame(Rank = 1:nrow(Top.RB.probs)), Top.RB.probs[order(Top.RB.probs$Prob,decreasing = T),])
write.csv(Top.RB.probs, file = "NFL/TPPs/RB.csv", row.names = F)

### Wide Receiver (WR)
uniq.WR <- unique(DraftKings.players$Name[which(DraftKings.players$Pos == "WR")])
samp.WR <- matrix(NA, nrow = iter, ncol = length(uniq.WR))
colnames(samp.WR) <- uniq.WR
for (i in c(1:length(uniq.WR))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.WR[i])),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK.points)
    
    samp.WR[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else (samp.WR[,i] <- 0)
}

Top.WR <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.WR[i,1] <- which.max(samp.WR[i,])
}

Top.WR.probs <- data.frame(Player = uniq.WR, Matchup = NA, Salary = NA, Prob = NA)
for (i in c(1:nrow(Top.WR.probs))) {
  Top.WR.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.WR.probs$Player[i])]
  Top.WR.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.WR.probs$Player[i])])
  Top.WR.probs$Prob[i] <- length(Top.WR[which(Top.WR[,1] == i)])/iter
}
Top.WR.probs <- cbind(data.frame(Rank = 1:nrow(Top.WR.probs)), Top.WR.probs[order(Top.WR.probs$Prob,decreasing = T),])
write.csv(Top.WR.probs, file = "NFL/TPPs/WR.csv", row.names = F)

### Tight End (TE)
uniq.TE <- unique(DraftKings.players$Name[which(DraftKings.players$Pos == "TE")])
samp.TE <- matrix(NA, nrow = iter, ncol = length(uniq.TE))
colnames(samp.TE) <- uniq.TE
for (i in c(1:length(uniq.TE))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.TE[i]) &
                              Fantasy.2016$Pos == "TE"),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK.points)
    
    samp.TE[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else (samp.TE[,i] <- 0)
}

Top.TE <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.TE[i,1] <- which.max(samp.TE[i,])
}

Top.TE.probs <- data.frame(Player = uniq.TE, Matchup = NA, Salary = NA, Prob = NA)
for (i in c(1:nrow(Top.TE.probs))) {
  Top.TE.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.TE.probs$Player[i])]
  Top.TE.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.TE.probs$Player[i])])
  Top.TE.probs$Prob[i] <- length(Top.TE[which(Top.TE[,1] == i)])/iter
}
Top.TE.probs <- cbind(data.frame(Rank = 1:nrow(Top.TE.probs)), Top.TE.probs[order(Top.TE.probs$Prob,decreasing = T),])
write.csv(Top.TE.probs, file = "NFL/TPPs/TE.csv", row.names = F)

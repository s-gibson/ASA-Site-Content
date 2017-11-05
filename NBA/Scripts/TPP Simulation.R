###################################### 
##  ASA NBA Site Content            ##
##  NBA Top Positional Probability  ##
##  Stewart Gibson                  ##
##  11/4/17                         ##
######################################

### Import data, separate into 2016 and 2015 datasets
load("NBA/data/NBA_2017.RData")

### Read in .csv of available DraftKings players
DraftKings.players <- read.csv("NBA/data/DraftKings.players.csv")

### Keep only players who aren't "Out" (Injury == 2)
DraftKings.players$Injury[which(is.na(DraftKings.players$Injury))] <- 0
DraftKings.players <- DraftKings.players[-which(DraftKings.players$Injury == 2),]

### Set all non-starters to 0
DraftKings.players$Start[which(is.na(DraftKings.players$Start))] <- 0

### TPP Simulation loop
iter <- 1000

## PG
uniq.PG <- DraftKings.players[which(grepl("PG", DraftKings.players$Pos)),
                                     c("Name","Start","Injury")]
samp.PG <- matrix(NA, nrow = iter, ncol = nrow(uniq.PG))
colnames(samp.PG) <- uniq.PG$Name

for (i in c(1:nrow(uniq.PG))) {
  dat <- NBA_17[which(NBA_17$First..Last == uniq.PG$Name[i] &
                        NBA_17$Start == uniq.PG$Start[i] &
                        NBA_17$GP == 1),]
  
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DKP)
    samp.PG[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else {
    dat <- NBA_17[which(NBA_17$First..Last == uniq.PG$First..Last[i]),]
    if (nrow(dat) > 1) {
      player.dens <- density(dat$DK.points)
      samp.PG[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
    } else {samp.PG[,i] <- 0}
  }
  
}

# Replace half of sample DKP's with 0 for players who are "Questionable" ($Injury == 1)
samp.PG[1:iter/2, which(uniq.PG$Injury == 1)] <- 0

# Determine top probability
Top.PG <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.PG[i,1] <- which.max(samp.PG[i,])
}

Top.PG.probs <- data.frame(Player = uniq.PG$Name, Matchup = NA, Salary = NA, Avg = NA, Prob = NA,
                           Status = NA)
for (i in c(1:nrow(Top.PG.probs))) {
  Top.PG.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.PG.probs$Player[i])]
  Top.PG.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.PG.probs$Player[i])])
  Top.PG.probs$Matchup[i] <- unlist(strsplit(Top.PG.probs$Matchup[i], split = " "))[1]
  Top.PG.probs$Avg[i] <- DraftKings.players$AvgPointsPerGame[which(DraftKings.players$Name ==
                                                                              Top.PG.probs$Player[i])]
  Top.PG.probs$Prob[i] <- length(Top.PG[which(Top.PG[,1] == i)])/iter
  Top.PG.probs$Status[i] <- if(DraftKings.players$Injury[which(
    DraftKings.players$Name == Top.PG.probs$Player[i])] == 1) {"GTD"} else {""}
}
Top.PG.probs <- cbind(data.frame(Rank = 1:nrow(Top.PG.probs)), Top.PG.probs[order(Top.PG.probs$Prob,decreasing = T),])
write.csv(Top.PG.probs, file = "NBA/TPPs/PG.csv", row.names = F)

## SG
uniq.SG <- DraftKings.players[which(grepl("SG", DraftKings.players$Pos)),
                              c("Name","Start","Injury")]
samp.SG <- matrix(NA, nrow = iter, ncol = nrow(uniq.SG))
colnames(samp.SG) <- uniq.SG$Name

for (i in c(1:nrow(uniq.SG))) {
  dat <- NBA_17[which(NBA_17$First..Last == uniq.SG$Name[i] &
                        NBA_17$Start == uniq.SG$Start[i] &
                        NBA_17$GP == 1),]
  
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DKP)
    samp.SG[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else {
    dat <- NBA_17[which(NBA_17$First..Last == uniq.SG$First..Last[i]),]
    if (nrow(dat) > 1) {
      player.dens <- density(dat$DK.points)
      samp.SG[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
    } else {samp.SG[,i] <- 0}
  }
  
}

# Replace half of sample DKP's with 0 for players who are "Questionable" ($Injury == 1)
samp.SG[1:iter/2, which(uniq.SG$Injury == 1)] <- 0

# Determine top probability
Top.SG <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.SG[i,1] <- which.max(samp.SG[i,])
}

Top.SG.probs <- data.frame(Player = uniq.SG$Name, Matchup = NA, Salary = NA, Avg = NA, Prob = NA,
                           Status = NA)
for (i in c(1:nrow(Top.SG.probs))) {
  Top.SG.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.SG.probs$Player[i])]
  Top.SG.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.SG.probs$Player[i])])
  Top.SG.probs$Matchup[i] <- unlist(strsplit(Top.SG.probs$Matchup[i], split = " "))[1]
  Top.SG.probs$Avg[i] <- DraftKings.players$AvgPointsPerGame[which(DraftKings.players$Name ==
                                                                     Top.SG.probs$Player[i])]
  Top.SG.probs$Prob[i] <- length(Top.SG[which(Top.SG[,1] == i)])/iter
  Top.SG.probs$Status[i] <- if(DraftKings.players$Injury[which(
    DraftKings.players$Name == Top.SG.probs$Player[i])] == 1) {"GTD"} else {""}
}
Top.SG.probs <- cbind(data.frame(Rank = 1:nrow(Top.SG.probs)), Top.SG.probs[order(Top.SG.probs$Prob,decreasing = T),])
write.csv(Top.SG.probs, file = "NBA/TPPs/SG.csv", row.names = F)

## SF
uniq.SF <- DraftKings.players[which(grepl("SF", DraftKings.players$Pos)),
                              c("Name","Start","Injury")]
samp.SF <- matrix(NA, nrow = iter, ncol = nrow(uniq.SF))
colnames(samp.SF) <- uniq.SF$Name

for (i in c(1:nrow(uniq.SF))) {
  dat <- NBA_17[which(NBA_17$First..Last == uniq.SF$Name[i] &
                        NBA_17$Start == uniq.SF$Start[i] &
                        NBA_17$GP == 1),]
  
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DKP)
    samp.SF[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else {
    dat <- NBA_17[which(NBA_17$First..Last == uniq.SF$First..Last[i]),]
    if (nrow(dat) > 1) {
      player.dens <- density(dat$DK.points)
      samp.SF[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
    } else {samp.SF[,i] <- 0}
  }
  
}

# Replace half of sample DKP's with 0 for players who are "Questionable" ($Injury == 1)
samp.SF[1:iter/2, which(uniq.SF$Injury == 1)] <- 0

# Determine top probability
Top.SF <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.SF[i,1] <- which.max(samp.SF[i,])
}

Top.SF.probs <- data.frame(Player = uniq.SF$Name, Matchup = NA, Salary = NA, Avg = NA, Prob = NA,
                           Status = NA)
for (i in c(1:nrow(Top.SF.probs))) {
  Top.SF.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.SF.probs$Player[i])]
  Top.SF.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.SF.probs$Player[i])])
  Top.SF.probs$Matchup[i] <- unlist(strsplit(Top.SF.probs$Matchup[i], split = " "))[1]
  Top.SF.probs$Avg[i] <- DraftKings.players$AvgPointsPerGame[which(DraftKings.players$Name ==
                                                                     Top.SF.probs$Player[i])]
  Top.SF.probs$Prob[i] <- length(Top.SF[which(Top.SF[,1] == i)])/iter
  Top.SF.probs$Status[i] <- if(DraftKings.players$Injury[which(
    DraftKings.players$Name == Top.SF.probs$Player[i])] == 1) {"GTD"} else {""}
}
Top.SF.probs <- cbind(data.frame(Rank = 1:nrow(Top.SF.probs)), Top.SF.probs[order(Top.SF.probs$Prob,decreasing = T),])
write.csv(Top.SF.probs, file = "NBA/TPPs/SF.csv", row.names = F)

## PF
uniq.PF <- DraftKings.players[which(grepl("PF", DraftKings.players$Pos)),
                              c("Name","Start","Injury")]
samp.PF <- matrix(NA, nrow = iter, ncol = nrow(uniq.PF))
colnames(samp.PF) <- uniq.PF$Name

for (i in c(1:nrow(uniq.PF))) {
  dat <- NBA_17[which(NBA_17$First..Last == uniq.PF$Name[i] &
                        NBA_17$Start == uniq.PF$Start[i] &
                        NBA_17$GP == 1),]
  
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DKP)
    samp.PF[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else {
    dat <- NBA_17[which(NBA_17$First..Last == uniq.PF$First..Last[i]),]
    if (nrow(dat) > 1) {
      player.dens <- density(dat$DK.points)
      samp.PF[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
    } else {samp.PF[,i] <- 0}
  }
  
}

# Replace half of sample DKP's with 0 for players who are "Questionable" ($Injury == 1)
samp.PF[1:iter/2, which(uniq.PF$Injury == 1)] <- 0

# Determine top probability
Top.PF <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.PF[i,1] <- which.max(samp.PF[i,])
}

Top.PF.probs <- data.frame(Player = uniq.PF$Name, Matchup = NA, Salary = NA, Avg = NA, Prob = NA,
                           Status = NA)
for (i in c(1:nrow(Top.PF.probs))) {
  Top.PF.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.PF.probs$Player[i])]
  Top.PF.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.PF.probs$Player[i])])
  Top.PF.probs$Matchup[i] <- unlist(strsplit(Top.PF.probs$Matchup[i], split = " "))[1]
  Top.PF.probs$Avg[i] <- DraftKings.players$AvgPointsPerGame[which(DraftKings.players$Name ==
                                                                     Top.PF.probs$Player[i])]
  Top.PF.probs$Prob[i] <- length(Top.PF[which(Top.PF[,1] == i)])/iter
  Top.PF.probs$Status[i] <- if(DraftKings.players$Injury[which(
    DraftKings.players$Name == Top.PF.probs$Player[i])] == 1) {"GTD"} else {""}
}
Top.PF.probs <- cbind(data.frame(Rank = 1:nrow(Top.PF.probs)), Top.PF.probs[order(Top.PF.probs$Prob,decreasing = T),])
write.csv(Top.PF.probs, file = "NBA/TPPs/PF.csv", row.names = F)

## C
uniq.C <- DraftKings.players[which(grepl("C", DraftKings.players$Pos)),
                              c("Name","Start","Injury")]
samp.C <- matrix(NA, nrow = iter, ncol = nrow(uniq.C))
colnames(samp.C) <- uniq.C$Name

for (i in c(1:nrow(uniq.C))) {
  dat <- NBA_17[which(NBA_17$First..Last == uniq.C$Name[i] &
                        NBA_17$Start == uniq.C$Start[i] &
                        NBA_17$GP == 1),]
  
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DKP)
    samp.C[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
  } else {
    dat <- NBA_17[which(NBA_17$First..Last == uniq.C$First..Last[i]),]
    if (nrow(dat) > 1) {
      player.dens <- density(dat$DK.points)
      samp.C[,i] <- sample(player.dens$x, iter, replace = T, prob = player.dens$y)
    } else {samp.C[,i] <- 0}
  }
  
}

# Replace half of sample DKP's with 0 for players who are "Questionable" ($Injury == 1)
samp.C[1:iter/2, which(uniq.C$Injury == 1)] <- 0

# Determine top probability
Top.C <- matrix(NA, nrow = iter, ncol = 1)
for (i in c(1:iter)) {
  Top.C[i,1] <- which.max(samp.C[i,])
}

Top.C.probs <- data.frame(Player = uniq.C$Name, Matchup = NA, Salary = NA, Avg = NA, Prob = NA,
                           Status = NA)
for (i in c(1:nrow(Top.C.probs))) {
  Top.C.probs$Salary[i] <- DraftKings.players$Salary[which(DraftKings.players$Name ==
                                                              Top.C.probs$Player[i])]
  Top.C.probs$Matchup[i] <- as.character(DraftKings.players$GameInfo[which(DraftKings.players$Name ==
                                                                              Top.C.probs$Player[i])])
  Top.C.probs$Matchup[i] <- unlist(strsplit(Top.C.probs$Matchup[i], split = " "))[1]
  Top.C.probs$Avg[i] <- DraftKings.players$AvgPointsPerGame[which(DraftKings.players$Name ==
                                                                     Top.C.probs$Player[i])]
  Top.C.probs$Prob[i] <- length(Top.C[which(Top.C[,1] == i)])/iter
  Top.C.probs$Status[i] <- if(DraftKings.players$Injury[which(
    DraftKings.players$Name == Top.C.probs$Player[i])] == 1) {"GTD"} else {""}
}
Top.C.probs <- cbind(data.frame(Rank = 1:nrow(Top.C.probs)), Top.C.probs[order(Top.C.probs$Prob,decreasing = T),])
write.csv(Top.C.probs, file = "NBA/TPPs/C.csv", row.names = F)

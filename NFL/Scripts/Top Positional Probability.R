
### Import data, separate into 2016 and 2015 datasets
load("~/Documents/ASA/ASA Site Content/NFL/data/clean_data.RData")

### Create list of unique offensive players
uniq.players <- unique(Fantasy.2016[which(Fantasy.2016$Pos != "Def"),c("First.Last","Team")])
                                                              
### Read in projected players .csv
DraftKings.players <- read.csv("NFL/data/DraftKings.Lineup.csv")
# Keep only players who aren't injured
DraftKings.players <- DraftKings.players[which(is.na(DraftKings.players$Injury)),]

### Create large for loop that runs n simulations of drawing a fantasy total from every
### player's fantasy distribution function.  Store in a data frame for each position.
iter <- 1000

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
Top.QB.probs <- Top.QB.probs[order(Top.QB.probs$Prob,decreasing = T),]
write.csv(Top.QB.probs, file = "NFL/TPPs/QB.csv", row.names = F)

### First Base (1B)
uniq.1B <- unique(DraftKings.players$Name[which(DraftKings.players$Position == "1B" |
                                                       DraftKings.players$Position == "1B/2B" |
                                                       DraftKings.players$Position == "1B/3B" |
                                                       DraftKings.players$Position == "1B/OF" )])
samp.1B <- matrix(NA, nrow = 5000, ncol = length(uniq.1B))
colnames(samp.1B) <- uniq.1B
for (i in c(1:length(uniq.1B))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.1B[i]) &
                           Fantasy.2016$GS == 1),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK_points)
    
    samp.1B[,i] <- sample(player.dens$x, 5000, replace = T, prob = player.dens$y)
  } else (samp.1B[,i] <- 0)
}

Top.1B <- matrix(NA, nrow = 5000, ncol = 1)
for (i in c(1:5000)) {
  Top.1B[i,1] <- which.max(samp.1B[i,])
}

Top.1B.probs <- data.frame(Player = uniq.1B, Prob = NA)
for (i in c(1:nrow(Top.1B.probs))) {
  Top.1B.probs$Prob[i] <- length(Top.1B[which(Top.1B[,1] == i)])/5000
}
Top.1B.probs <- Top.1B.probs[order(Top.1B.probs$Prob,decreasing = T),]

write.csv(Top.1B.probs, file = "~/Documents/ASA/MLB Positional Probability/First Base.csv")

### Second Base (2B)
uniq.2B <- unique(DraftKings.players$Name[which(DraftKings.players$Position == "2B" |
                                               DraftKings.players$Position == "2B/3B" |
                                               DraftKings.players$Position == "2B/C" |
                                               DraftKings.players$Position == "2B/OF" |
                                               DraftKings.players$Position == "2B/SS")])
samp.2B <- matrix(NA, nrow = 5000, ncol = length(uniq.2B))
colnames(samp.2B) <- uniq.2B
for (i in c(1:length(uniq.2B))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.2B[i]) &
                           Fantasy.2016$GS == 1),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK_points)
    
    samp.2B[,i] <- sample(player.dens$x, 5000, replace = T, prob = player.dens$y)
  } else (samp.2B[,i] <- 0)
}

Top.2B <- matrix(NA, nrow = 5000, ncol = 1)
for (i in c(1:5000)) {
  Top.2B[i,1] <- which.max(samp.2B[i,])
}

Top.2B.probs <- data.frame(Player = uniq.2B, Prob = NA)
for (i in c(1:nrow(Top.2B.probs))) {
  Top.2B.probs$Prob[i] <- length(Top.2B[which(Top.2B[,1] == i)])/5000
}
Top.2B.probs <- Top.2B.probs[order(Top.2B.probs$Prob,decreasing = T),]

write.csv(Top.2B.probs, file = "~/Documents/ASA/MLB Positional Probability/Second Base.csv")

### Third Base (3B)
uniq.3B <- unique(DraftKings.players$Name[which(DraftKings.players$Position == "3B" |
                                               DraftKings.players$Position == "3B/OF" |
                                               DraftKings.players$Position == "3B/SS" |
                                               DraftKings.players$Position == "2B/3B" |
                                               DraftKings.players$Position == "1B/3B")])
samp.3B <- matrix(NA, nrow = 5000, ncol = length(uniq.3B))
colnames(samp.3B) <- uniq.3B
for (i in c(1:length(uniq.3B))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.3B[i]) &
                           Fantasy.2016$GS == 1),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK_points)
    
    samp.3B[,i] <- sample(player.dens$x, 5000, replace = T, prob = player.dens$y)
  } else (samp.3B[,i] <- 0)
}

Top.3B <- matrix(NA, nrow = 5000, ncol = 1)
for (i in c(1:5000)) {
  Top.3B[i,1] <- which.max(samp.3B[i,])
}

Top.3B.probs <- data.frame(Player = uniq.3B, Prob = NA)
for (i in c(1:nrow(Top.3B.probs))) {
  Top.3B.probs$Prob[i] <- length(Top.3B[which(Top.3B[,1] == i)])/5000
}
Top.3B.probs <- Top.3B.probs[order(Top.3B.probs$Prob,decreasing = T),]

write.csv(Top.3B.probs, file = "~/Documents/ASA/MLB Positional Probability/Third Base.csv")

### Shortstop (SS)
uniq.SS <- unique(DraftKings.players$Name[which(DraftKings.players$Position == "SS" |
                                               DraftKings.players$Position == "2B/SS" |
                                               DraftKings.players$Position == "3B/SS" |
                                               DraftKings.players$Position == "OF/SS" )])
samp.SS <- matrix(NA, nrow = 5000, ncol = length(uniq.SS))
colnames(samp.SS) <- uniq.SS
for (i in c(1:length(uniq.SS))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.SS[i]) &
                           Fantasy.2016$GS == 1),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK_points)
    
    samp.SS[,i] <- sample(player.dens$x, 5000, replace = T, prob = player.dens$y)
  } else (samp.SS[,i] <- 0)
}

Top.SS <- matrix(NA, nrow = 5000, ncol = 1)
for (i in c(1:5000)) {
  Top.SS[i,1] <- which.max(samp.SS[i,])
}

Top.SS.probs <- data.frame(Player = uniq.SS, Prob = NA)
for (i in c(1:nrow(Top.SS.probs))) {
  Top.SS.probs$Prob[i] <- length(Top.SS[which(Top.SS[,1] == i)])/5000
}
Top.SS.probs <- Top.SS.probs[order(Top.SS.probs$Prob,decreasing = T),]

write.csv(Top.SS.probs, file = "~/Documents/ASA/MLB Positional Probability/Shortstop.csv")

### Outfield (OF)
uniq.OF <- unique(DraftKings.players$Name[which(DraftKings.players$Position == "OF" |
                                               DraftKings.players$Position == "1B/OF" |
                                               DraftKings.players$Position == "2B/OF" |
                                               DraftKings.players$Position == "3B/OF" |
                                               DraftKings.players$Position == "C/OF" |
                                               DraftKings.players$Position == "OF/SS")])
samp.OF <- matrix(NA, nrow = 5000, ncol = length(uniq.OF))
colnames(samp.OF) <- uniq.OF
for (i in c(1:length(uniq.OF))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.OF[i]) &
                           Fantasy.2016$GS == 1),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK_points)
    
    samp.OF[,i] <- sample(player.dens$x, 5000, replace = T, prob = player.dens$y)
  } else (samp.OF[,i] <- 0)
}

Top.OF <- matrix(NA, nrow = 5000, ncol = 1)
for (i in c(1:5000)) {
  Top.OF[i,1] <- which.max(samp.OF[i,])
}

Top.OF.probs <- data.frame(Player = uniq.OF, Prob = NA)
for (i in c(1:nrow(Top.OF.probs))) {
  Top.OF.probs$Prob[i] <- length(Top.OF[which(Top.OF[,1] == i)])/5000
}
Top.OF.probs <- Top.OF.probs[order(Top.OF.probs$Prob,decreasing = T),]

write.csv(Top.OF.probs, file = "~/Documents/ASA/MLB Positional Probability/Outfield.csv")

### Starting Pitcher (SP)
uniq.SP <- unique(DraftKings.players$Name[which(DraftKings.players$Position == "SP" |
                                               DraftKings.players$Position == "SP/2B" |
                                               DraftKings.players$Position == "SP/3B" |
                                               DraftKings.players$Position == "SP/OF" )])
samp.SP <- matrix(NA, nrow = 5000, ncol = length(uniq.SP))
colnames(samp.SP) <- uniq.SP
for (i in c(1:length(uniq.SP))) {
  dat <- Fantasy.2016[which(Fantasy.2016$First.Last == as.character(uniq.SP[i]) &
                           Fantasy.2016$GS == 1),]
  if (nrow(dat) > 1) {
    player.dens <- density(dat$DK_points)
    
    samp.SP[,i] <- sample(player.dens$x, 5000, replace = T, prob = player.dens$y)
  } else (samp.SP[,i] <- 0)
}

Top.SP <- matrix(NA, nrow = 5000, ncol = 1)
for (i in c(1:5000)) {
  Top.SP[i,1] <- which.max(samp.SP[i,])
}

Top.SP.probs <- data.frame(Player = uniq.SP, Prob = NA)
for (i in c(1:nrow(Top.SP.probs))) {
  Top.SP.probs$Prob[i] <- length(Top.SP[which(Top.SP[,1] == i)])/5000
}
Top.SP.probs <- Top.SP.probs[order(Top.SP.probs$Prob,decreasing = T),]

write.csv(Top.SP.probs, file = "~/Documents/ASA/MLB Positional Probability/Starting Pitcher.csv")


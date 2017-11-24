################################## 
##  ASA NBA Site Content        ##
##  Player Fantasy Correlation  ##
##  Stewart Gibson              ##
##  11/21/17                    ##
##################################

## Load packages
require(corrplot)

## Load data
load("NBA/data/NBA_2017.RData")

## Consideration cutoff: How many DKP must a player score to be considered for correlation?
cutoff <- 50

## Create correlation matrix for each team.
uniq.teams <- sort(unique(NBA_17$Team))

for (i in 1:length(uniq.teams)) {
  dat <- NBA_17[which(NBA_17$Team == uniq.teams[i]),]
  uniq.players <- unique(as.character(dat$Last..First))
  player.points <- data.frame(Player = uniq.players,
                              Initial.Last = "Fill",
                              Total.DKP = 0,
                              stringsAsFactors = F)
  for (j in 1:length(uniq.players)) {
    player.points$Initial.Last[j] <- paste(substr(unlist(strsplit(
      as.character(player.points$Player[j]), split = ","))[2], start = 2, stop = 2),". ",
      unlist(strsplit(
        as.character(player.points$Player[j]), split = ","))[1], sep = "")
    
    player.points$Total.DKP[j] <- sum(NBA_17$DKP[which(
      as.character(NBA_17$Last..First) == as.character(player.points$Player[j]))],na.rm = T)
  }
  
  player.points <- player.points[order(player.points$Total.DKP, decreasing = T),]
  player.points <- player.points[which(player.points$Total.DKP > cutoff),]
  
  # build matrix for correlation plot
  corr.mat <- matrix(NA, ncol = nrow(player.points), nrow = nrow(player.points))
  colnames(corr.mat) <- player.points$Initial.Last
  rownames(corr.mat) <- player.points$Initial.Last
  
  for (r in 1:nrow(corr.mat)) {
    for (c in 1:ncol(corr.mat)) {
      
      corr.dat <- data.frame(GameID = unique(dat$GameID))
      corr.dat <- merge(corr.dat, NBA_17[which(NBA_17$Last..First == player.points$Player[c] &
                                                 NBA_17$active == 1),
                                       c("GameID","DKP")], 
                       by = "GameID")
      corr.dat <- merge(corr.dat, NBA_17[which(NBA_17$Last..First == player.points$Player[r] &
                                                 NBA_17$active == 1),
                                         c("GameID","DKP")], 
                        by = "GameID")
      
      corr.dat <- corr.dat[which(!is.na(corr.dat$DKP.x) & !is.na(corr.dat$DKP.y)),]
      
      corr.mat[r,c] <- cor(corr.dat$DKP.x, corr.dat$DKP.y)
      
    }
  }
  
  # create correlation plot
  f1 <- paste('NBA/Visualizations/Player Correlations/', 
              uniq.teams[i], sep="")
  f2 <- paste(f1, '.png', sep = "")
  png(f2, height = 450, width = 550, pointsize = 22-ncol(corr.mat))
  corrplot(corr.mat, method = 'number', type = 'lower', 
           col = colorRampPalette(c("red","gray90","green"))(100),
           title = paste(toupper(uniq.teams[i]),"Player Correlation", sep = " "),
           mar=c(0,0,1,0))
  dev.off()
}


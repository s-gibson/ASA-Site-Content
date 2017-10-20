### Load data and packages
load("NBA/data/NBA Players.RData")

# ### Edit data
# NBA.players <- NBA.players[-which(is.na(NBA.players$Player)),-5]
# 
# for (i in 1:13) {
#   NBA.players[,i] <- as.character(NBA.players[,i])
# }
# for (i in 1:300) {
#   NBA.players$`League Value`[i] <- substr(
#     as.character(NBA.players$`League Value`[i]), start = 2,
#     stop = nchar(as.character(NBA.players$`League Value`[i])))
#   NBA.players$`Proj Value`[i] <- substr(
#     as.character(NBA.players$`Proj Value`[i]), start = 2,
#     stop = nchar(as.character(NBA.players$`Proj Value`[i])))
#   NBA.players$`Avg Cost`[i] <- substr(
#     as.character(NBA.players$`Avg Cost`[i]), start = 2,
#     stop = nchar(as.character(NBA.players$`Avg Cost`[i])))
#   NBA.players$`FGM/A`[i] <- unlist(strsplit(NBA.players$`FGM/A`[i],"/"))[1]
# 
# }
# 
# for (i in 2:13) {
#   NBA.players[,i] <- as.numeric(NBA.players[,i])
# }
# 
# colnames(NBA.players)[5] <- "FGM"
# 
# NBA.players <- cbind(NBA.players, matrix(NA, nrow = 300, ncol = 9))
# 
for (i in which(!is.na(NBA.players$FGM))) {
  for (j in 5:13)
  NBA.players[i,(j+9)] <- length(which(NBA.players[,j] <= NBA.players[i,j]))/
      length(which(!is.na(NBA.players$FGM)))
}

colnames(NBA.players)[14:22] <- c("FGM Percentile","FG% Percentile","FTM Percentile",
                                  "3PTM Percentile","PTS Percentile","REB Percentile",
                                  "AST Percentile","STL Percentile","BLK Percentile")

for (i in which(!is.na(NBA.players$FGM))) {
  for (j in 5:13) {
    NBA.players[i,(j+18)] <- (NBA.players[i,j] - mean(NBA.players[,j], na.rm = T))/
      sqrt(var(NBA.players[,j], na.rm = T))
  }
}

colnames(NBA.players)[23:31] <- c("FGM Z-Score","FG% Z-Score","FTM Z-Score",
                                  "3PTM Z-Score","PTS Z-Score","REB Z-Score",
                                  "AST Z-Score","STL Z-Score","BLK Z-Score")

NBA.players$`Mean Percentile` <- NA
NBA.players$`Mean Percentile/Avg Cost` <- NA
NBA.players$`Mean Z-Score` <- NA
NBA.players$`Mean Z-Score/Avg Cost` <- NA
for (i in which(!is.na(NBA.players$FGM) & NBA.players$`Avg Cost` > 1
                )) {
  NBA.players$`Mean Percentile`[i] <- mean(as.numeric(NBA.players[i,14:22]))
  NBA.players$`Mean Percentile/Avg Cost`[i] <- mean(as.numeric(NBA.players[i,14:22]))*100/
    log(NBA.players$`Avg Cost`[i])
  
  NBA.players$`Mean Z-Score`[i] <- mean(as.numeric(NBA.players[i,23:31]))
  NBA.players$`Mean Z-Score/Avg Cost`[i] <- mean(as.numeric(NBA.players[i,23:31]))*100/
    log(NBA.players$`Avg Cost`[i])
}


ggplot(data = NBA.players[which(NBA.players$`Avg Cost` > 10),], aes(x = `Avg Cost`,y = `Mean Z-Score`)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x)

cor(NBA.players$`Avg Cost`[which(!is.na(NBA.players$`Mean Percentile/Avg Cost`))],
    NBA.players$`Mean Percentile/Avg Cost`[which(!is.na(NBA.players$`Mean Percentile/Avg Cost`))])


### Regression
reg.z <- lm(data = NBA.players[which(!is.na(NBA.players$`Mean Z-Score`)),], formula = `Avg Cost`~ poly(`Mean Z-Score`,7)) # Better
#reg.p <- lm(data = NBA.players, formula = `Mean Percentile` ~ `Avg Cost`)

NBA.players$`Fitted Avg Cost` <- NA
NBA.players$`Fitted Avg Cost`[which(!is.na(NBA.players$`Mean Z-Score`))] <- 
  as.numeric(fitted(reg.z))

NBA.players$`Avg Cost Residaual` <- NA
NBA.players$`Avg Cost Residaual`[which(!is.na(NBA.players$`Mean Z-Score`))] <- 
  NBA.players$`Avg Cost`[which(!is.na(NBA.players$`Mean Z-Score`))] - 
  as.numeric(fitted(reg.z)) 

plot(NBA.players$`Mean Z-Score`,NBA.players$`Avg Cost Residaual`)

write.csv(NBA.players,"~/Documents/ASA/ASA Site Content/NBA/data/NBA Players.csv",row.names = F)


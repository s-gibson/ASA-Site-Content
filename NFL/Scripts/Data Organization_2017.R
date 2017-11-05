##############################
##  Data Oganization: 2017  ##
##  Stewart Gibson          ##
##  7/26/17                 ##
##############################

## Set data's most current week
most.current <- 8

######################## 2017
## Import data of weekly scores, O/U's
OU.2017 <- read.csv("NFL/data/2017_DonBest_VegasData_NFL_Week.csv")

# Create variables for year and week
OU.2017$Year <- 2017
OU.2017$Week <- NA
OU.2017$Information <- as.character(OU.2017$Information)
for (i in c(1:most.current)) {
  OU.2017$Week[which(grepl(paste('WEEK ', i, " ", sep = ''), OU.2017$Information))] <- 
    rep(i, length(which(grepl(paste('WEEK ', i, " ", sep = ''), OU.2017$Information))))
} 
rm(i)

OU.2017 <- OU.2017[-which(is.na(OU.2017$Week)),]

# Import players' weekly fantasy points data
Fantasy.2017 <- read.csv("NFL/data/2017_NFL_Fantasy_Points.csv")
Fantasy.2017 <- Fantasy.2017[-which(Fantasy.2017$Pos == "Def"),]
Fantasy.2017$First.Name <- as.character(Fantasy.2017$First.Name)
Fantasy.2017$Last.Name <- as.character(Fantasy.2017$Last.Name)

# Remove [space] in front of players' first and last names
Fantasy.2017$First.Name <- gsub(" ", "", Fantasy.2017$First.Name)
Fantasy.2017$Last.Name <- gsub(" ", "", Fantasy.2017$Last.Name)

# Change class of Team Name (Home, Away) columns (OU.2015, OU.2017) to "character"
OU.2017$Home.Team <- as.character(OU.2017$Home.Team)
OU.2017$Away.Team <- as.character(OU.2017$Away.Team)

# Replace elongated teams names with abbreviations (i.e. "Arizona Cardinals" -> "ari")
full.names <- unique(OU.2017$Home.Team)[order(unique(OU.2017$Home.Team))][c(1:20, 22, 21, 23:27, 29, 28, 30:32)]
for (i in c(1:32)) {
  OU.2017$Home.Team[which(OU.2017$Home.Team == 
                                 full.names[i])] <- as.character(levels(Fantasy.2017$Team)[i])
  OU.2017$Away.Team[which(OU.2017$Away.Team == 
                            full.names[i])] <- as.character(levels(Fantasy.2017$Team)[i])
}

rm(i, full.names)

## Create "team-level" dataframe
OU.2017$Actual.Spread..Relative.to.Home.Team. <- 
    as.numeric(levels(OU.2017$Actual.Spread..Relative.to.Home.Team.)[
      as.numeric(OU.2017$Actual.Spread..Relative.to.Home.Team.)])

Team.2017_Home <- data.frame(unique(OU.2017[,c("Year","Week","Home.Team","Home.Team.Score",
                                               "Actual.Spread..Relative.to.Home.Team.")]))
colnames(Team.2017_Home)[3:5] <- c("Team","Actual.Points","Spread")
Team.2017_Home <- cbind(Team.2017_Home,
                        data.frame(H.A = "H",
                                   Total.DKP = NA,
                                   QB.DKP = NA,
                                   RB.DKP = NA,
                                   WR_TE.DKP = NA,
                                   Oppt.Avg.DKP = NA,
                                   Oppt.Avg.QB.DKP = NA,
                                   Oppt.Avg.RB.DKP = NA,
                                   Oppt.Avg.WR_TE.DKP = NA))

Team.2017_Away <- data.frame(unique(OU.2017[,c("Year","Week","Away.Team","Away.Team.Score",
                                               "Actual.Spread..Relative.to.Home.Team.")]))
colnames(Team.2017_Away)[3:5] <- c("Team","Actual.Points","Spread")
Team.2017_Away$Spread <- -1*Team.2017_Away$Spread
Team.2017_Away <- cbind(Team.2017_Away,
                        data.frame(H.A = "A",
                                   Total.DKP = NA,
                                   QB.DKP = NA,
                                   RB.DKP = NA,
                                   WR_TE.DKP = NA,
                                   Oppt.Avg.DKP = NA,
                                   Oppt.Avg.QB.DKP = NA,
                                   Oppt.Avg.RB.DKP = NA,
                                   Oppt.Avg.WR_TE.DKP = NA))

Team.2017 <- rbind(Team.2017_Home,Team.2017_Away)
rm(Team.2017_Away,Team.2017_Home)

for (i in 1:nrow(Team.2017)) {
  dat <- Fantasy.2017[which(Fantasy.2017$Team == Team.2017$Team[i] &
                              Fantasy.2017$Week == Team.2017$Week[i]),]
  
  Oppt.dat <- Fantasy.2017[which(Fantasy.2017$Oppt == dat$Oppt[1]),]
  
  Team.2017$Total.DKP[i] <- sum(dat$DK.points, na.rm = T)
  Team.2017$QB.DKP[i] <- sum(dat$DK.points[which(dat$Pos == "QB")], na.rm = T)
  Team.2017$RB.DKP[i] <- sum(dat$DK.points[which(dat$Pos == "RB")], na.rm = T)
  Team.2017$WR_TE.DKP[i] <- sum(dat$DK.points[which(dat$Pos %in% c("WR","TE"))], na.rm = T)
  Team.2017$Oppt.Avg.DKP[i] <- sum(Oppt.dat$DK.points,na.rm = T)/length(unique(Oppt.dat$Week))
  Team.2017$Oppt.Avg.QB.DKP[i] <- sum(Oppt.dat$DK.points[which(Oppt.dat$Pos == "QB")]
                                     ,na.rm = T)/length(unique(Oppt.dat$Week))
  Team.2017$Oppt.Avg.RB.DKP[i] <- sum(Oppt.dat$DK.points[which(Oppt.dat$Pos == "RB")]
                                      ,na.rm = T)/length(unique(Oppt.dat$Week))
  Team.2017$Oppt.Avg.WR_TE.DKP[i] <- sum(Oppt.dat$DK.points[which(Oppt.dat$Pos %in% c("WR","TE"))]
                                      ,na.rm = T)/length(unique(Oppt.dat$Week))
  
  
}

## Merge Team.2017 to Fantasy.2017
Fantasy.2017 <- merge(Fantasy.2017, Team.2017, by = c("Team","Week","Year"))

## Create columns that store players' names as their full name (First Last) and as their first
## initial + last name
Fantasy.2017$First.Last <- paste(Fantasy.2017$First.Name, Fantasy.2017$Last.Name)
Fantasy.2017$Initial.Last <- paste(substr(Fantasy.2017$First.Last, start = 1, stop = 1),
                                   ". ", Fantasy.2017$Last.Name, sep = "")


######################## 2016
## Import data of weekly scores, O/U's
OU.2016 <- read.csv("NFL/data/2016_DonBest_VegasData_NFL_Week.csv")
OU.2016 <- OU.2016[which(OU.2016$Regular.Season == 1),]

# Create variables for year and week
OU.2016$Year <- 2016
OU.2016$Week <- NA
OU.2016$Information <- as.character(OU.2016$Information)
for (i in c((most.current+1):17)) {
  OU.2016$Week[which(grepl(paste('WEEK ', i, " ", sep = ''), OU.2016$Information))] <- 
    rep(i, length(which(grepl(paste('WEEK ', i, " ", sep = ''), OU.2016$Information))))
} 
rm(i)

OU.2016 <- OU.2016[-which(is.na(OU.2016$Week)),]

# Import players' weekly fantasy points data
Fantasy.2016 <- read.csv("NFL/data/2016_NFL_Fantasy_Points.csv")
Fantasy.2016 <- Fantasy.2016[-which(Fantasy.2016$Pos == "Def"),]
Fantasy.2016 <- Fantasy.2016[which(Fantasy.2016$Week > most.current),]
Fantasy.2016$First.Name <- as.character(Fantasy.2016$First.Name)
Fantasy.2016$Last.Name <- as.character(Fantasy.2016$Last.Name)

# Remove [space] in front of players' first and last names
Fantasy.2016$First.Name <- gsub(" ", "", Fantasy.2016$First.Name)
Fantasy.2016$Last.Name <- gsub(" ", "", Fantasy.2016$Last.Name)

# Change class of Team Name (Home, Away) columns (OU.2015, OU.2016) to "character"
OU.2016$Home.Team <- as.character(OU.2016$Home.Team)
OU.2016$Away.Team <- as.character(OU.2016$Away.Team)

# Replace elongated teams names with abbreviations (i.e. "Arizona Cardinals" -> "ari")
full.names <- unique(OU.2016$Home.Team)[order(unique(OU.2016$Home.Team))][c(1:19, 21, 20, 22:27, 29, 28, 30:32)]
for (i in c(1:32)) {
  OU.2016$Home.Team[which(OU.2016$Home.Team == 
                            full.names[i])] <- as.character(levels(Fantasy.2016$Team)[i])
  OU.2016$Away.Team[which(OU.2016$Away.Team == 
                            full.names[i])] <- as.character(levels(Fantasy.2016$Team)[i])
}

rm(i, full.names)

## Change "sdg" to "lac"
OU.2016$Away.Team[which(OU.2016$Away.Team == "sdg")] <- "lac"
OU.2016$Home.Team[which(OU.2016$Home.Team == "sdg")] <- "lac"
Fantasy.2016$Team <- as.character(Fantasy.2016$Team)
Fantasy.2016$Team[which(Fantasy.2016$Team == "sdg")] <- "lac"

## Create "team-level" dataframe
Team.2016_Home <- data.frame(unique(OU.2016[,c("Year","Week","Home.Team","Home.Team.Score",
                                               "Actual.Spread..Relative.to.Home.Team.")]))
colnames(Team.2016_Home)[3:5] <- c("Team","Actual.Points","Spread")
Team.2016_Home <- cbind(Team.2016_Home,
                        data.frame(H.A = "H",
                                   Total.DKP = NA,
                                   QB.DKP = NA,
                                   RB.DKP = NA,
                                   WR_TE.DKP = NA,
                                   Oppt.Avg.DKP = NA,
                                   Oppt.Avg.QB.DKP = NA,
                                   Oppt.Avg.RB.DKP = NA,
                                   Oppt.Avg.WR_TE.DKP = NA))

Team.2016_Away <- data.frame(unique(OU.2016[,c("Year","Week","Away.Team","Away.Team.Score",
                                               "Actual.Spread..Relative.to.Home.Team.")]))
colnames(Team.2016_Away)[3:5] <- c("Team","Actual.Points","Spread")
Team.2016_Away$Spread <- -1*Team.2016_Away$Spread
Team.2016_Away <- cbind(Team.2016_Away,
                        data.frame(H.A = "A",
                                   Total.DKP = NA,
                                   QB.DKP = NA,
                                   RB.DKP = NA,
                                   WR_TE.DKP = NA,
                                   Oppt.Avg.DKP = NA,
                                   Oppt.Avg.QB.DKP = NA,
                                   Oppt.Avg.RB.DKP = NA,
                                   Oppt.Avg.WR_TE.DKP = NA))

Team.2016 <- rbind(Team.2016_Home,Team.2016_Away)
rm(Team.2016_Away,Team.2016_Home)

for (i in 1:nrow(Team.2016)) {
  dat <- Fantasy.2016[which(Fantasy.2016$Team == Team.2016$Team[i] &
                              Fantasy.2016$Week == Team.2016$Week[i]),]
  
  Oppt.dat <- Fantasy.2016[which(Fantasy.2016$Oppt == dat$Oppt[1]),]
  
  Team.2016$Total.DKP[i] <- sum(dat$DK.points, na.rm = T)
  Team.2016$QB.DKP[i] <- sum(dat$DK.points[which(dat$Pos == "QB")], na.rm = T)
  Team.2016$RB.DKP[i] <- sum(dat$DK.points[which(dat$Pos == "RB")], na.rm = T)
  Team.2016$WR_TE.DKP[i] <- sum(dat$DK.points[which(dat$Pos %in% c("WR","TE"))], na.rm = T)
  Team.2016$Oppt.Avg.DKP[i] <- sum(Oppt.dat$DK.points,na.rm = T)/length(unique(Oppt.dat$Week))
  Team.2016$Oppt.Avg.QB.DKP[i] <- sum(Oppt.dat$DK.points[which(Oppt.dat$Pos == "QB")]
                                      ,na.rm = T)/length(unique(Oppt.dat$Week))
  Team.2016$Oppt.Avg.RB.DKP[i] <- sum(Oppt.dat$DK.points[which(Oppt.dat$Pos == "RB")]
                                      ,na.rm = T)/length(unique(Oppt.dat$Week))
  Team.2016$Oppt.Avg.WR_TE.DKP[i] <- sum(Oppt.dat$DK.points[which(Oppt.dat$Pos %in% c("WR","TE"))]
                                         ,na.rm = T)/length(unique(Oppt.dat$Week))
  
  
}

## Merge Team.2016 to Fantasy.2016
Fantasy.2016 <- merge(Fantasy.2016, Team.2016, by = c("Team","Week","Year"))

## Create columns that store players' names as their full name (First Last) and as their first
## initial + last name
Fantasy.2016$First.Last <- paste(Fantasy.2016$First.Name, Fantasy.2016$Last.Name)
Fantasy.2016$Initial.Last <- paste(substr(Fantasy.2016$First.Last, start = 1, stop = 1),
                                   ". ", Fantasy.2016$Last.Name, sep = "")

########## Combine 2016 & 2017
Fantasy.2016_2017 <- rbind(Fantasy.2016,Fantasy.2017)
Team.2016_2017 <- rbind(Team.2016,Team.2017)

## Add columns for number of data points (games) & total fantasy points in last 17 weeks for each player
uniq.players <- unique(Fantasy.2016_2017[which(Fantasy.2016_2017$Year == 2017),c('First.Last','Team')])
Fantasy.2016_2017$N_Current_team <- 0
Fantasy.2016_2017$Current_team <- 0
Fantasy.2016_2017$Cum.DKP <- 0
Fantasy.2016_2017$Avg.DKP <- 0
for (i in 1:nrow(uniq.players)) {
  dat <- Fantasy.2016_2017[which(Fantasy.2016_2017$First.Last == uniq.players[i,1]),]
  
  Fantasy.2016_2017$N_Current_team[which(Fantasy.2016_2017$First.Last == uniq.players[i,1])] <- 
    length(which(dat$Team == uniq.players[i,2]))
  Fantasy.2016_2017$Current_team[which(Fantasy.2016_2017$First.Last == uniq.players[i,1] &
                                         Fantasy.2016_2017$Team == uniq.players[i,2])] <- 1
  Fantasy.2016_2017$Cum.DKP[which(Fantasy.2016_2017$First.Last == uniq.players[i,1])] <- 
    sum(dat$DK.points, na.rm = T)
  Fantasy.2016_2017$Avg.DKP[which(Fantasy.2016_2017$First.Last == uniq.players[i,1])] <- 
    mean(dat$DK.points, na.rm = T)
}


## Adjust names of certain players
Fantasy.2016_2017$First.Last[which(Fantasy.2016_2017$First.Last == "Odell BeckhamJr.")] <- "Odell Beckham Jr."
Fantasy.2016_2017$First.Last[which(Fantasy.2016_2017$First.Last == "Ted GinnJr.")] <- "Ted Ginn Jr."
Fantasy.2016_2017$First.Last[which(Fantasy.2016_2017$First.Last == "Todd Gurley")] <- "Todd Gurley II"
Fantasy.2016_2017$First.Last[which(grepl("Will Fuller",Fantasy.2016_2017$First.Last))] <- "Will Fuller V"
## Keep a full dataset, without removing players who have scored fewer than X fantasy points
Fantasy.2016_2017.full <- Fantasy.2016_2017

## Keep only players who have scored at least N fantasy points in the last 17 weeks or have averaged
## greater than A fantasy points in the last 17 weeks and have also played at least 3 weeks.
Fantasy.2016_2017 <- Fantasy.2016_2017[which(
  Fantasy.2016_2017$Cum.DKP > 30 | Fantasy.2016_2017$Avg.DKP > 10),]

## Change classes
Fantasy.2016_2017$Actual.Points <- as.numeric(Fantasy.2016_2017$Actual.Points)

## Remove insignifficant players who have the same name as significant players
Fantasy.2016_2017 <- Fantasy.2016_2017[-which(Fantasy.2016_2017$First.Last == "David Johnson" &
                                                Fantasy.2016_2017$Pos == "TE"),]
Fantasy.2016_2017 <- Fantasy.2016_2017[-which(Fantasy.2016_2017$First.Last == "Chris Thompson" &
                                                Fantasy.2016_2017$Pos == "WR"),]


## Save data
save(Fantasy.2016_2017,Team.2016_2017,Fantasy.2016_2017.full,most.current,
     file = "NFL/data/clean_data_2016_2017.RData")

save(Fantasy.2016_2017,Team.2016_2017,Fantasy.2016_2017.full,most.current,
     file = "NFL/Shiny App/clean_data_2016_2017.RData")

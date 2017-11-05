###########################
## ASA NBA Site Content  ##
## NBA Data Organization ##
## Stewart Gibson        ##
## 10/31/17              ##
###########################

### Import data
NBA_17 <- read.csv("NBA/data/nba_2017_to_date.csv")
NBA_17$First..Last <- as.character(NBA_17$First..Last)

## Assign more descriptive positions to players
NBA_17$DK.pos <- as.character(NBA_17$DK.pos)
NBA_17 <- cbind(NBA_17, data.frame(DK_PG = 0,
                                   DK_SG = 0,
                                   DK_SF = 0,
                                   DK_PF = 0,
                                   DK_C = 0))
NBA_17$DK_PG[which(grepl("1",NBA_17$DK.pos))] <- 1
NBA_17$DK_SG[which(grepl("2",NBA_17$DK.pos))] <- 1
NBA_17$DK_SF[which(grepl("3",NBA_17$DK.pos))] <- 1
NBA_17$DK_PF[which(grepl("4",NBA_17$DK.pos))] <- 1
NBA_17$DK_C[which(grepl("5",NBA_17$DK.pos))] <- 1

## Save data
save.image("~/Documents/ASA/ASA Site Content/NBA/data/NBA_2017.RData")

############################################################## 
##  ASA Site Content                                        ##
##  Ch. 7: How how does matchup affect fantasy production?  ##
##  Stewart Gibson                                          ##
##  10/1/17                                                 ##
##############################################################

## Load packages
require(ggplot2)

## Load data
load("NFL/data/clean_data_2016_2017.RData")
uniq.teams <- sort(unique(Fantasy.2016_2017.full$Team))

## Plot each teams' fantasy point total (total offense and positional units) vs. the average
## point total allowed by each opponent
for (i in 1:length(uniq.teams)) {
   dat <- Team.2016_2017[which(Team.2016_2017$Team == uniq.teams[i]),]
  # 
  # FP.total.reg <- lm(dat$Total.FP[which(dat$Pos == "All")] ~ 
  #                     dat$Oppt.FP.allowed[which(dat$Pos == "All")])
  # reg.eq <- paste("Y = ", round(FP.total.reg$coefficients[1], 2), " + ", 
  #                 round(FP.total.reg$coefficients[2], 2), " * X", sep = '')
  # 
  # # Total offense
  # ggplot(data = dat[which(dat$Pos == "All"),], aes (x = Oppt.FP.allowed, 
  #                                                   y = Total.FP)) +
  #   geom_point() +
  #   geom_smooth(method = 'lm', formula = y ~x) +
  #   ggtitle(paste(toupper(uniq.teams[i]), 
  #                         "Total Fantasy Points vs. Opponent Average FP/Game", sep = " ")) +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   xlab("Opponent Average FP/Game") +
  #   ylab("Total Offensive Fantasy Points") +
  #   geom_text(x = 85, y = max(Fantasy.vs.Oppt$Total.FP), label = reg.eq,
  #             color = 'blue', size = 5) +
  #   scale_x_continuous(limits = c(min(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
  #     Fantasy.vs.Oppt$Pos == "All")]), max(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
  #     Fantasy.vs.Oppt$Pos == "All")]) + 3)) +
  #   scale_y_continuous(limits = c(min(Fantasy.vs.Oppt$Total.FP[which(
  #     Fantasy.vs.Oppt$Pos == "All")] - 5),
  #                                 max(Fantasy.vs.Oppt$Total.FP[which(
  #                                   Fantasy.vs.Oppt$Pos == "All")] + 5)))
  # 
  # ggsave(paste("Visualizations/Ch_7/Total Offense/",uniq.teams[i],".png", sep = ""))
  
  # Positional units
  # ggplot(data = dat, aes (x = Oppt.FP.allowed, y = Total.FP, group = Pos, color = Pos)) +
    
  ggplot(dat) +
    geom_point(aes(x = Oppt.Avg.QB.DKP, y = QB.DKP, color = "QB")) +
    geom_smooth(aes(x = Oppt.Avg.QB.DKP, y = QB.DKP, color = "QB"), method = 'lm', 
                formula = y ~ x) +
    geom_point(aes(x = Oppt.Avg.RB.DKP, y = RB.DKP, color = "RB")) +
    geom_smooth(aes(x = Oppt.Avg.RB.DKP, y = RB.DKP, color = "RB"), method = 'lm', formula = y ~ x) +
    geom_point(aes(x = Oppt.Avg.WR_TE.DKP, y = WR_TE.DKP, color = "WR/TE")) +
    geom_smooth(aes(x = Oppt.Avg.WR_TE.DKP, y = WR_TE.DKP, color = "WR/TE"), method = 'lm', formula = y ~ x) +
    ggtitle(paste(toupper(uniq.teams[i]), 
                  "Total Fantasy Points vs. Opponent Average FP/Game", sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Opponent Average FP/Game") +
    ylab("Total Positiona Unit Fantasy Points") #+

  ggsave(paste("NFL/Visualizations/DKP vs. Opponent Average/",uniq.teams[i],".png", sep = ""))
  
}

# ## Plot teams' fantasy point total (total offense and positional units) vs. the average
# ## point total allowed by each opponent at the league-aggregate level.
# FP.total.reg <- lm(Fantasy.vs.Oppt$Total.FP[which(Fantasy.vs.Oppt$Pos == "All")] ~ 
#                      Fantasy.vs.Oppt$Oppt.FP.allowed[which(Fantasy.vs.Oppt$Pos == "All")])
# reg.eq <- paste("Y = ", round(FP.total.reg$coefficients[1], 2), " + ", 
#                 round(FP.total.reg$coefficients[2], 2), " * X", sep = '')
# 
# # Total offense
# ggplot(data = Fantasy.vs.Oppt[which(Fantasy.vs.Oppt$Pos == "All"),], aes (x = Oppt.FP.allowed, 
#                                                   y = Total.FP)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~x) +
#   ggtitle("Total Fantasy Points vs. Opponent Average FP/Game") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   xlab("Opponent Average FP/Game") +
#   ylab("Total Offensive Fantasy Points") +
#   geom_text(x = 85, y = max(Fantasy.vs.Oppt$Total.FP), label = reg.eq,
#             color = 'blue', size = 5) +
#   scale_x_continuous(limits = c(min(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
#     Fantasy.vs.Oppt$Pos == "All")]), max(Fantasy.vs.Oppt$Oppt.FP.allowed[which(
#       Fantasy.vs.Oppt$Pos == "All")]) + 3)) +
#   scale_y_continuous(limits = c(min(Fantasy.vs.Oppt$Total.FP[which(
#     Fantasy.vs.Oppt$Pos == "All")] - 5),
#     max(Fantasy.vs.Oppt$Total.FP[which(
#       Fantasy.vs.Oppt$Pos == "All")] + 5)))
# 
# ggsave("Visualizations/Ch_7/League-Aggregate/Total Offense.png")
# 
# # Positional units
# ggplot(data = Fantasy.vs.Oppt[which(Fantasy.vs.Oppt$Pos != "All"),], aes (x = Oppt.FP.allowed, 
#                                                   y = Total.FP, group = Pos, color = Pos)) +
#   geom_point(size = 0.4, alpha = 0.4) +
#   geom_smooth(method = 'lm', formula = y ~x) +
#   ggtitle("Total Fantasy Points vs. Opponent Average FP/Game") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   xlab("Opponent Average FP/Game") +
#   ylab("Total Positiona Unit Fantasy Points") #+
# 
# ggsave("Visualizations/Ch_7/League-Aggregate/Positional Units.png")
# 
# 

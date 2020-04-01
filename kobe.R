# import dataset
kobe <- read.csv(file = 'data.csv')
summary(kobe)
dim(kobe)
kobe=na.omit(kobe)
dim(kobe)
attach(kobe)

# visualize data  
dotsCol = rep("red", length(shot_made_flag))
madeShots = which(shot_made_flag==TRUE)
dotsCol[madeShots] = "blue"
rm(madeShots)
plot(loc_y, loc_x, type="p", cex=0.1, pch=20, col=dotsCol)

# prune columns
time_remaining = minutes_remaining*60+seconds_remaining
feet_x = abs(loc_x)/10
feet_y = loc_y/10
kobe$shot_distance = sqrt(feet_x^2 + feet_y^2)
shot_angle = atan2(feet_y, feet_x)
shot_side = ifelse(loc_x >= 0, "Right", "Left")
kobe = cbind(kobe, time_remaining, shot_angle, feet_x, feet_y, shot_side)
season = substr(season, 1, 4)
season = strtoi(season)
kobe$season = season
kobe = kobe[,-which(names(kobe) %in% c("minutes_remaining","seconds_remaining","team_name",
                                      "team_id", "matchup", "shot_zone_range", "game_event_id", "game_id",
                                      "game_date", "shot_id", "lat", "lon", "loc_x", "loc_y"))]

kobe = kobe[,-which(names(kobe) %in% c("combined_shot_type", "opponent"))]

# Logistic regression
logreg = glm(shot_made_flag~., family=binomial, data=kobe)
probs = predict(logreg, type="response")
preds = (probs>.45)
table(shot_made_flag, preds)
truep = sum(preds2==shot_made_flag)
acc = truep / nrow(kobe)
summary(logreg)
logreg2 = glm(shot_made_flag~action_type+period+season+shot_distance+time_remaining+shot_angle, family=binomial, data=kobe)
summary(logreg2)
probs2 = predict(logreg2, type="response")
preds2 = (probs2>.5)
truep2 = sum(preds2==shot_made_flag)
acc2 = truep2 / nrow(kobe)
acc2
logreg3 = glm(shot_made_flag~action_type+feet_x+feet_y+feet_x*feet_y+shot_side+period+time_remaining, family=binomial, data=kobe)
summary(logreg3)
probs3 = predict(logreg3, type="response")
pred3 = (probs3>.5)
truep3 = sum(pred3==shot_made_flag)
acc3 = truep3 / nrow(kobe)
acc3

# Interacting terms (feet_x*feet_y)


# Decision tree

# Bagging

# Random forest

# Lda

# Svm

# Knn
library(knitr)
setwd("report")
knit2pdf(input="report.Rnw", 
         output=paste0("report",'.tex'))
detach(kobe)
rm(list=ls())

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
                                      "team_id", "matchup", "game_event_id", "game_id",
                                      "game_date", "shot_id", "lat", "lon", "loc_x", "loc_y"))]

# Logistic regression
logreg = glm(shot_made_flag~., family=binomial, data=kobe)
probs = predict(logreg, type="response")
preds = (probs>.45)
table(shot_made_flag, preds)
summary(logreg)
summary(shot_distance)
summary(loc_y)
summary(loc_x)

detach(kobe)
rm(list=ls())

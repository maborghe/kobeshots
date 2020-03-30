# import dataset
kobe <- read.csv(file = 'data.csv')
summary(kobe)
dim(kobe)
kobe=na.omit(kobe)
dim(kobe)
attach(kobe)

# prune columns
time_remaining = minutes_remaining*60+seconds_remaining
kobe = cbind(kobe, time_remaining)
rm(time_remaining)
kobe = kobe[,-which(names(kobe) %in% c("minutes_remaining","seconds_remaining","team_name",
                                       "team_id", "matchup", "game_event_id", "game_id", "game_date", "shot_id"))]

fix(kobe)

unique(combined_shot_type)
unique(action_type)
unique(shot_type)

logreg = glm(shot_made_flag~., family=binomial, data=kobe)
probs = predict(logreg, type="response")
preds = (probs>.45)
table(shot_made_flag, preds)
summary(logreg)
fix(kobe)
summary(shot_distance)
summary(loc_y)
summary(loc_x)

# visualize data  
dotsCol = rep("red", length(shot_made_flag))
madeShots = which(shot_made_flag==TRUE)
dotsCol[madeShots] = "blue"
rm(madeShots)
plot(loc_y, loc_x, type="p", cex=0.1, pch=20, col=dotsCol)
hist(shot_distance)


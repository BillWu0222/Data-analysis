
library(dplyr)


Player_Data <- read.csv("player_cleaned2.csv")
Match_Star_Data <- read.csv("match_star.csv")
Match_Data <- read.csv("match_cleaned2.csv")
Win_Lose_Data <- read.csv("win_lose_signs.csv")

Player_Data <- Player_Data[, -c(1, 3:8, 39:46)]
Goal_Data <- subset(Match_Star_Data, select = c("match_api_id", "home_team_api_id", "away_team_api_id", "home_team_goal", "away_team_goal"))
Match_Data <- Match_Data[, -c(1:5,11:54)]

Merged_Data <- merge(Win_Lose_Data, Goal_Data, by = 'match_api_id', all.x = TRUE)
Merged_Data$TeamGoal <- ifelse(Merged_Data$team_api_id == Merged_Data$home_team_api_id, Merged_Data$home_team_goal, Merged_Data$away_team_goal)
Merged_Data <- Merged_Data[, !(names(Merged_Data) %in% c("home_team_api_id", "away_team_api_id", "home_team_goal", "away_team_goal"))]


has_na1 <- any(is.na(Merged_Data))
has_na2<- any(is.na(Player_Data))
has_na3 <- any(is.na(Match_Data))

is.na(Merged_Data)
na_column <- colSums(is.na(Merged_Data))
print(na_column)

write.csv(Merged_Data, file = "C:\\Users\\user\\Desktop\\巨量資料\\期末報告\\資料前處理資料\\Team_Goal_Data.csv", row.names = FALSE)
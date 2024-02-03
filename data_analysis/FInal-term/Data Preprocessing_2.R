
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)

Player_Data <- read.csv("player_cleaned2.csv")
Match_Star_Data <- read.csv("match_star.csv")
Match_Data <- read.csv("match_cleaned2.csv")
Win_Lose_Signs_Data <- read.csv("win_lose_signs.csv")

Goal_Data <- subset(Match_Star_Data, select = c("match_api_id", "home_team_api_id", "away_team_api_id", "home_team_goal", "away_team_goal"))

Win_Lose_Signs_Data <- Win_Lose_Signs_Data[, -c(4,6:21)]

win_dataset <- subset(Win_Lose_Signs_Data, status == "win")
lose_draw_dataset <- subset(Win_Lose_Signs_Data, status %in% c("lose", "a draw"))


win_home_dataset <- data.frame(match_api_id = numeric(0))
win_away_draw_dataset <- data.frame(match_api_id = numeric(0))

for (i in 1:nrow(win_dataset)) {
  current_team_type <- win_dataset$team_type[i]
  current_match_id <- win_dataset$match_api_id[i]
  
  if (current_team_type == "home") {
    win_home_dataset <- rbind(win_home_dataset, data.frame(match_api_id = current_match_id))
  } else if (current_team_type %in% c("away", "a draw")) {
    win_away_draw_dataset <- rbind(win_away_draw_dataset, data.frame(match_api_id = current_match_id))
  }
}

win_away_draw_dataset
win_home_dataset

n <- nrow(win_away_draw_dataset)
player_win_11 <- data.frame(matrix(nrow = n, ncol = 11))

for (i in 1:n) {
  current <- win_away_draw_dataset$match_api_id[i]
  match_indices <- which(current == Match_Data$match_api_id)
  
  if (length(match_indices) > 0) {
    player_win_11[i, ] <- Match_Data[match_indices[1], c(55:65)]
  }
}

element_frequencies <- table(unlist(player_win_11))
playerID_num_win_1 <- as.data.frame(element_frequencies)

n <- nrow(win_home_dataset)
player_win_11_2 <- data.frame(matrix(nrow = n, ncol = 11))

for (i in 1:n) {
  current <- win_home_dataset$match_api_id[i]
  match_indices <- which(current == Match_Data$match_api_id)
  
  if (length(match_indices) > 0) {
    player_win_11_2[i, ] <- Match_Data[match_indices[1], c(55:65)]
  }
}

element_frequencies <- table(unlist(player_win_11_2))
playerID_num_win_2 <- as.data.frame(element_frequencies)


Win_num <- left_join(playerID_num_win_1, playerID_num_win_2, by = "Var1")
Win_num <- Win_num %>%
  mutate(Freq = ifelse(is.na(Freq.x), Freq.y, ifelse(is.na(Freq.y), Freq.x, Freq.x + Freq.y))) %>%
  select(Var1, Freq)

colnames(Win_num) <- c("PlayerID", "Frequency")


lose_home_dataset <- data.frame(match_api_id = numeric(0))
lose_away_draw_dataset <- data.frame(match_api_id = numeric(0))

for (i in 1:nrow(lose_draw_dataset)) {
  current_team_type <- lose_draw_dataset$team_type[i]
  current_match_id <- lose_draw_dataset$match_api_id[i]
  
  if (current_team_type == "home") {
    lose_home_dataset <- rbind(lose_home_dataset, data.frame(match_api_id = current_match_id))
  } else if (current_team_type %in% c("away", "a draw")) {
    lose_away_draw_dataset <- rbind(lose_away_draw_dataset, data.frame(match_api_id = current_match_id))
  }
}

lose_away_draw_dataset
lose_home_dataset

n <- nrow(lose_away_draw_dataset)
player_lose_11 <- data.frame(matrix(nrow = n, ncol = 11))

for (i in 1:n) {
  current <- lose_away_draw_dataset$match_api_id[i]
  match_indices <- which(current == Match_Data$match_api_id)
  
  if (length(match_indices) > 0) {
    player_lose_11[i, ] <- Match_Data[match_indices[1], c(55:65)]
  }
}

element_frequencies <- table(unlist(player_lose_11))
playerID_num_lose_1 <- as.data.frame(element_frequencies)

n <- nrow(lose_home_dataset)
player_lose_11_2 <- data.frame(matrix(nrow = n, ncol = 11))

for (i in 1:n) {
  current <- lose_home_dataset$match_api_id[i]
  match_indices <- which(current == Match_Data$match_api_id)
  
  if (length(match_indices) > 0) {
    player_lose_11_2[i, ] <- Match_Data[match_indices[1], c(55:65)]
  }
}

element_frequencies <- table(unlist(player_lose_11_2))
playerID_num_lose_2 <- as.data.frame(element_frequencies)


Lose_num <- left_join(playerID_num_lose_1, playerID_num_lose_2, by = "Var1")
Lose_num <- Lose_num %>%
  mutate(Freq = ifelse(is.na(Freq.x), Freq.y, ifelse(is.na(Freq.y), Freq.x, Freq.x + Freq.y))) %>%
  select(Var1, Freq)

colnames(Lose_num) <- c("PlayerID", "Frequency")


Rate_data <- merge(Win_num, Lose_num, by = "PlayerID", all = TRUE)
colnames(Rate_data) <- c("player_api_id", "Frequency_Win", "Frequency_Lose")

Rate_data[is.na(Rate_data)] <- 0
Rate_data$Total_Frequency <- Rate_data$Frequency_Win / (Rate_data$Frequency_Win + Rate_data$Frequency_Lose)

temp <- any(is.na(Lose_num))
na_column <- colSums(is.na(Lose_num))
print(na_column)



Player_Data <- merge(Player_Data, Rate_data, by = "player_api_id", all = TRUE)
Player_Data <- Player_Data %>%
  mutate(
    Frequency_Win = ifelse(is.na(Frequency_Win), 0, Frequency_Win),
    Frequency_Lose = ifelse(is.na(Frequency_Lose), 0, Frequency_Lose),
    Total_Frequency = ifelse(is.na(Total_Frequency), 0, Total_Frequency)
  )

Player_Cleaned_Data <- Player_Data[, -c(2:7, 39:46)]


dummy_data <- dummyVars(" ~ star_sign", data = Player_Cleaned_Data)
one_hot_encoded <- predict(dummy_data, newdata = Player_Cleaned_Data)

Player_Cleaned_Data <- cbind(Player_Cleaned_Data, one_hot_encoded)
Player_Cleaned_Data <- Player_Cleaned_Data[, -which(names(Player_Cleaned_Data) %in% c("star_sign"))]

is.na(Player_Cleaned_Data)
na_column <- colSums(is.na(Player_Cleaned_Data))
print(na_column)


library(DMwR2)

Player_Cleaned_Data <- knnImputation(Player_Cleaned_Data, k = 5, scale = TRUE)
has_na2<- any(is.na(Player_Cleaned_Data))

write.csv(Player_Cleaned_Data, file = "C:\\Users\\user\\Desktop\\Cleaned\\Total_Frequency_Data.csv", row.names = FALSE)


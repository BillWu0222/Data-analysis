#Import dataset
League_Data <- read.csv("league_cleaned.csv")
Player_Data <- read.csv("player_cleaned_ver2.csv")
Match_Data <- read.csv("match_cleaned.csv")


library(ggplot2)
library(dplyr)
library(imputeTS)
library(nortest)
library(stats)


#Read data column names
Player_Column <- colnames(Player_Data)
Match_Column <- colnames(Match_Data)



#Check for NA values
is.na(Player_Data)
na_column <- colSums(is.na(Player_Data))
print(na_column)


#Draw QQ chart
save_col <- subset(Player_Data, select = sapply(Player_Data, is.numeric))

QQ_Plot <- function(var){
  qqnorm(var)
  qqline(var, col = 2)
  title(sub = var)
}
lapply(save_col, QQ_Plot)


#Use "mean" to impute values
Player_Data <- Player_Data %>%
  mutate(volleys = ifelse(is.na(volleys), round(mean(volleys, na.rm = TRUE)), volleys))

Player_Data <- Player_Data %>%
  mutate(curve = ifelse(is.na(curve), round(mean(curve, na.rm = TRUE)), curve))

Player_Data <- Player_Data %>%
  mutate(agility = ifelse(is.na(agility), (mean(agility, na.rm = TRUE)), agility))

Player_Data <- Player_Data %>%
  mutate(balance = ifelse(is.na(balance), round(mean(balance, na.rm = TRUE)), balance))

Player_Data <- Player_Data %>%
  mutate(jumping = ifelse(is.na(jumping), round(mean(jumping, na.rm = TRUE)), jumping))

Player_Data <- Player_Data %>%
  mutate(vision = ifelse(is.na(vision), round(mean(vision, na.rm = TRUE)), vision))


#Use "Probability Density Function (PDF)" to fill NA values
mean_value <- mean(Player_Data$sliding_tackle, na.rm = TRUE)
std_dev <- sd(Player_Data$sliding_tackle, na.rm = TRUE)
random_values <- round(rnorm(sum(is.na(Player_Data$sliding_tackle)), mean = mean_value, sd = std_dev))
Player_Data$sliding_tackle[is.na(Player_Data$sliding_tackle)] <- random_values


#List narrative statistics for each data field
summary(League_Data)
summary(Player_Data)
summary(Match_Data)
summary(Try_Data)



#Univariate analysis graph
lapply(names(save_col), function(var){
  ggplot(Player_Data, aes(x = .data[[var]])) + 
    geom_bar(stat = "count", fill = "lightblue") + 
    geom_density(stat = "count") + 
    geom_point(stat = "count") + 
    ggtitle(paste(title = "Density and Bar plot of", var))
})

hist(Player_Data$height, main = "Player's height", xlab = "height", col = c("lightblue", "lightyellow"))
hist(Player_Data$weight, main = "Player's weight", xlab = "weight", col = c("lightblue", "lightyellow"))

#Preferred_foot
Preferred_foot_barplot <- table(Player_Data$preferred_foot)
barplot(Preferred_foot_barplot, main = "Preferred_foot Barplot", xlab = "Preferred_foot", ylab = "Frequency", col = c("lightgreen", "lightblue"))

#Attacking_work_rate
Attacking_work_rate_barplot <- table(Player_Data$attacking_work_rate)
barplot(Attacking_work_rate_barplot, main = "Attacking work rate Barplot", xlab = "Attacking work rate", ylab = "Frequency", col = c("lightgreen", "lightblue"))

#Defensive_work_rate
Defensive_work_rate_barplot <- table(Player_Data$defensive_work_rate)
barplot(Defensive_work_rate_barplot, main = "Defensive work rate Barplot", xlab = "Defensive work rate", ylab = "Frequency", col = c("lightgreen", "lightblue"))

#Star sign
Star_sign_barplot <- table(Player_Data$star_sign)
barplot(Star_sign_barplot, main = "Player's star sign", xlab = "star_sign", ylab = "Frequency", col = c("lightgreen", "lightblue"))






# 2.
# Please load the given Titanic dataset ("Titanic.csv"). Consider the following data analytics
# questions.
# PassengerID - The ID of each passenger
# Survived - Survival (0 = No; 1 = Yes)
# Pclass - Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd)
# Name - Name
# Sex - Sex
# Age - Age
# Sibsp - Number of Siblings/Spouses Aboard
# Parch - Number of Parents/Children Aboard
# Ticket - Ticket Number
# Fare - Passenger Fare
# Embarked - Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)

dataset <- read.csv("C:/Users/User/Desktop/Titanic.csv")


# 2.1. [5 pts] Please convert the categorical variables into R factors or Python Pandas
# Categoricals .

#Categorical variables : Survived, Pclass, Sex, Embarked
categorical_variables <- c("Survived","Pclass","Sex","Embarked")
#using lapply() to convert those categorical variables into R factors
dataset[categorical_variables] <- lapply(dataset[categorical_variables], factor)
#see the result
str(dataset)


# 2.2. [10 pts] Calculate the number of NA values in each column with any
# “apply”functions in R or Python, and remove those records (rows) with NA values.

#get the NA position in each column (for later codes to use)
col_na <- apply(dataset,2,function(x){is.na(x)})

#get the numbers of NA values in each column and see it
col_na_numbers <- apply(col_na,2,sum)
print(col_na_numbers)

#remove rows with NA values, by the way, dataset <- na.omit(dataset) also works
#get the boolean for rows, TRUE for rows without NA values, FALSE for otherwise.
test <- apply(col_na,1,function(x){!sum(x)})
dataset <- dataset[test,]

#see if the rows had been removed successfully or not (they had been removed)
col_na_after <- apply(dataset,2,function(x){is.na(x)})
col_na_numbers_after <- apply(col_na_after,2,sum)
print(col_na_numbers_after)
str(dataset)

# 2.3. [5 pts] Calculate the average fare (‘Fare’) among the different classes (‘Pclass’).
# Please sort the average fare in ascending order and show the result.

average_fare <- aggregate(Fare ~ Pclass, FUN = mean, data = dataset)#Group fare by pclass and calculate the average
average_fare <- average_fare[order(average_fare$Fare), ]#Sort by fare from low to high
average_fare

# 2.4 correlation matrix
dataset$Survived <- as.numeric(dataset$Survived)#Convert back to numerical data
dataset$Pclass <- as.numeric(dataset$Pclass)

correlation_matrix <- cor(dataset[, c("Survived", "Pclass", "Age", "SibSp", "Parch", "Fare")])#Grab the numerical variables and make a correlation matrix
correlation_matrix

# the strongest positive correlation
diag(correlation_matrix) <- 0  #Set the diagonal to 0 first to eliminate the correlation between yourself and yourself.
#Find the position with the maximum value and put it into the matrix
max_corr <- which(correlation_matrix == max(correlation_matrix, na.rm = TRUE), arr.ind = TRUE)

#the strongest negative correlation
#Find the position of the minimum value and put it into the matrix
min_corr <- which(correlation_matrix == min(correlation_matrix, na.rm = TRUE), arr.ind = TRUE)

#print the result
cat("Strongest Positive Correlation:\n", rownames(correlation_matrix)[max_corr[1, 1]], "&", rownames(correlation_matrix)[max_corr[1, 2]])
cat("Strongest Negative Correlation:\n", rownames(correlation_matrix)[min_corr[1, 1]], "&", rownames(correlation_matrix)[min_corr[1, 2]])

##From the above results, it can be seen that Parch & SibSp is the strongest positive correlation
##Fare & Pclass are the strongest negative correlations

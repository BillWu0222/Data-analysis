# 1.1. #Perform an inner join on the "Car_Merge_A'' and "Car_Merge_B" datasets
#using "car_ID" as the primary key. Then, concatenate the merged dataset with the
#"Car_Concat" dataset along the row axis. Convert any character columns to factor in
#R and categorical in Pythons. Note that the remaining 'car_ID' is not one of our
#variables, delete it or convert it into the index.

# Load the necessary libraries if not already loaded
install.packages("readr")
library(dplyr,readr)

Car_Merge_A <- read_csv("Desktop/dataset/Car_Merge_A.csv")
Car_Merge_B <- read_csv("Desktop/dataset/Car_Merge_B.csv")

# Perform an inner join on Car_Merge_A and Car_Merge_B using "car_ID" as the primary key
merged_dataset<- inner_join(Car_Merge_A, Car_Merge_B, by = "car_ID")

# Concatenate the merged dataset with Car_Concat along the row axis
final_dataset <- bind_rows(merged_dataset, Car_Concat)

# Convert character columns to factors
character_columns <- sapply(final_dataset, is.character)
final_dataset[character_columns] <- lapply(final_dataset[character_columns], as.factor)

# Optionally, you can remove the 'car_ID' column or set it as the index
final_dataset <- final_dataset %>% select(-car_ID)  # 


# 1.2. For continuous variables, create density plots to understand the
#distribution of the data. For categorical/factor variables, generate frequency tables
#and bar charts to summarize the counts of each category.

library(ggplot2)

# Define a function to generate density plots or bar charts based on column type
plot_density_or_bar <- function(data, col_name) {
  if (is.factor(data[[col_name]])) {
    print(col_name)
    print(table(data[[col_name]]))
    p <- ggplot(data, aes(x = .data[[col_name]])) +
      geom_bar() +
      xlab(col_name)
    print(p)
  } else {
    p <- ggplot(data, aes(x = .data[[col_name]])) +
      geom_density() +
      xlab(col_name)
    print(p)
  }
}

# Apply the function to all columns in the dataset
lapply(colnames(final_dataset), function(x) {
  plot_density_or_bar(final_dataset, x)
})



# 1.3. Consider doing a series of bivariate analyses on "PRICE vs. the rest of
# variables". Specifically, plot your data and perform bivariate statistical tests to
# understand the relationships among the variables.

library(ggplot2)

# Load the dataset (replace 'Car_Merge_B.csv' with your actual dataset file path)
Car_Merge_B <- read_csv("Desktop/dataset/Car_Merge_B.csv")

# Create a function to perform bivariate analysis
bivariate_analysis <- function(x) {
  if (x != "PRICE") {
    # Create scatterplots for continuous variables
    if (is.numeric(final_dataset[[x]])) {
      print(
        ggplot(data = final_dataset, aes(x = .data[[x]], y = PRICE)) +
          geom_point() +
          labs(x = x, y = "PRICE") +
          ggtitle(paste("Scatterplot of PRICE vs.", x))
      )
      
      # Perform a correlation test (e.g., Pearson correlation)
      correlation <- cor(final_dataset[[x]], final_dataset$PRICE)
      print(paste("Correlation between PRICE and", x, ":", correlation))
    }
    
    # Create boxplots for categorical variables
    if (is.factor(final_dataset[[x]])) {
      print(
        ggplot(data = final_dataset, aes(x = .data[[x]], y = PRICE)) +
          geom_boxplot() +
          labs(x = x, y = "PRICE") +
          ggtitle(paste("Boxplot of PRICE by", x))
      )
      
      # Perform a bivariate statistical test (e.g., t-test or ANOVA)
      # Adjust the test type based on your specific needs
      # For example, for a t-test:
      # t_test <- t.test(Car_Merge_B$PRICE ~ Car_Merge_B[[x]])
      # print(t_test)
      # For ANOVA:
      # anova_result <- aov(PRICE ~ .data[[x]], data = Car_Merge_B)
      # print(summary(anova_result))
    }
  }
}

# Apply the function to all columns in Car_Merge_B (except PRICE)
lapply(colnames(final_dataset), bivariate_analysis)


# 1.4 Please perform normality tests on PRICE. Does it seem "normal"? If not, do
# you think fitting general linear models to predict or explain the outcome is
#.appropriate?
install.packages("nortest")

nortest::lillie.test(final_dataset$PRICE)
nortest::ad.test(final_dataset$PRICE)
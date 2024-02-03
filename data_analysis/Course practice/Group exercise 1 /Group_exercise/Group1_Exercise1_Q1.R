library(dplyr)
library(caret)
library(ggplot2)
library(readr)
library(nortest)

## =========================================資料前處理=========================================##

## 1.1
## 設定到本地路徑
#setwd("/Users/changchenyu/Desktop/中山大學/巨量資料分析/Group_exercise/a1/")
## 讀入 data
Car_Merge_A <- read.csv("~/Desktop/巨量課/dataset/Car_Merge_A.csv")
Car_Merge_B <- read.csv("~/Desktop/巨量課/dataset/Car_Merge_B.csv")
Car_Concat <- read.csv("~/Desktop/巨量課/dataset/Car_Concat.csv")

## 合併資料集 A, B
merged_data <- inner_join(Car_Merge_A, Car_Merge_B, by = "car_ID")

## 與 Concat 資料 cancat
final_data <- bind_rows(merged_data, Car_Concat)

## 將 character 欄位轉換成 factor，表示離散類型的資料
character_columns <- sapply(final_data, is.character)
final_data[character_columns] <- lapply(final_data[character_columns], as.factor)

## 移除 ID 欄位
rownames(final_data) <- final_data$car_ID
final_data <- final_data[, !(names(final_data) %in% "car_ID")]

## =========================================analyses=========================================##

## 1.2.For continuous variables, create density plots to understand the
## distribution of the data. For categorical/factor variables, generate frequency tables
## and bar charts to summarize the counts of each category.

lapply(colnames(final_data), function(x){
  # categorical/factor variables
  if(is.factor(final_data[, x] )){
    print(x); print(table(final_data[, x])) 
    qplot(final_data[, x], geom = "bar") + xlab(x)
  # continuous variables
  }else{
    qplot(final_data[, x], geom = "density",ylab = "Density") + xlab(x)
  }
})

## 1.3
# seperate "PRICE" from other variables
car_var = setdiff(colnames(final_data), "PRICE")
lapply(car_var, function(x) {
  ## simple linear model
  lm_xy = lm(eval(parse(text = paste("PRICE ~", x))), data = final_data)
  if (is.factor(final_data[, x])) {
    ## for categorical
    anova_result <- aov(as.formula(paste("PRICE ~", x)), data = final_data)
    print(summary(anova_result))
    ggplot(data = final_data, aes_string(x = x, y = "PRICE")) + geom_boxplot() + labs(x = x, y = "PRICE")
    # type 3 代表所有自變量的影響
  } else {
    ## scatter for numeric
    print(summary(lm_xy))
    ggplot(data = final_data, aes_string(x = x, y = "PRICE")) + geom_point() + labs(x = x, y = "PRICE")
  }
})

## 1.4
nortest::lillie.test(final_data$PRICE)
#常態分佈檢測 p-value = 8.092e-13 < 0.05, 不符合常態分佈
ad.test(final_data$PRICE)
#ad-test 常態分佈檢測 p-value < 2.2e-16 < 0.05, 不符合常態分佈
##  If not, do you think fitting general linear models to predict or explain the outcome is appropriate?  Ans: not appropriate

## =========================================選擇變數=========================================##
# 1.5
# all
lm_all <- lm(PRICE ~ ., data = final_data)
summary(lm_all)
# select
selected_vars <- c("wheelbase", "enginesize", "boreratio", "stroke", "compressionratio", "horsepower", "peakrpm", "mpg")
formula <- as.formula(paste("PRICE ~", paste(selected_vars, collapse = " + ")))
lm_select <- lm(formula, data = final_data)
summary(lm_select)

## =========================================切分資料=========================================##
# 1.6
# Split the data into training (70%) and testing (30%) sets
set.seed(20230929)
train_idx = sample(1:nrow(final_data), 0.7 * nrow(final_data))
train_d = final_data[train_idx,]
test_d = final_data[setdiff(1:nrow(final_data), train_idx),]

## =========================================RMSE=========================================##
# 1.7
# Function to calculate RMSE
calculate_rmse <- function(pred, target) {
  rmse <- sqrt(mean((pred - target)^2))
  return(rmse)
}

## =========================================fit model=========================================##
# all vars
lm_all <- lm(PRICE ~ ., data = train_d)
# select
selected_vars <- c("wheelbase", "enginesize", "boreratio", "stroke", "compressionratio", "horsepower", "peakrpm", "mpg")
formula <- as.formula(paste("PRICE ~", paste(selected_vars, collapse = " + ")))
lm_select <- lm(formula, data = train_d)
# 2-way interaction
lm_2way <- lm(PRICE ~ (.)^2, data = train_d)


# model all
pred = lm_all$fitted.values
print(calculate_rmse(pred, train_d$PRICE)) # RMSE: 2251.896
pred = predict(lm_all, test_d)
print(calculate_rmse(pred, test_d$PRICE)) # RMSE: 1876.706

# model select
pred = lm_select$fitted.values
print(calculate_rmse(pred, train_d$PRICE)) # RMSE: 2283.604
pred = predict(lm_select, test_d)
print(calculate_rmse(pred, test_d$PRICE)) # RMSE: 1834.355

# model 2-way
pred = lm_2way$fitted.values
print(calculate_rmse(pred, train_d$PRICE)) # RMSE: 946.6921
pred = predict(lm_2way, test_d)
print(calculate_rmse(pred, test_d$PRICE)) # RMSE: 10361.03

# best model: lm_select, RMSE: 1834.355

## =========================================summary=========================================##
# 1.8
## summary models
model_summary = summary(lm_all)
significant_vars <- model_summary$coefficients[, "Pr(>|t|)"] < 0.05
print("Variables with p-value < 0.05:")
print(names(significant_vars[significant_vars == TRUE]))
model_summary = summary(lm_select)
significant_vars <- model_summary$coefficients[, "Pr(>|t|)"] < 0.05
print("Variables with p-value < 0.05:")
print(names(significant_vars[significant_vars == TRUE]))
model_summary = summary(lm_2way)
significant_vars <- model_summary$coefficients[, "Pr(>|t|)"] < 0.05
print("Variables with p-value < 0.05:")
print(names(significant_vars[significant_vars == TRUE]))

## variables with p-value < 0.05: "wheelbase","enginesize","boreratio","stroke","horsepower","peakrpm"

# Calculate correlation coefficients
correlation_matrix <- cor(final_data[, c("wheelbase", "enginesize", "boreratio", "stroke", "horsepower", "peakrpm", "PRICE")])
# high correlations var: wheelbase, enginesize, horsepower
# Do you think the correlation coefficient is a good measurement for variable importance ranking?
#   是，但變數的重要性會受到係數大小、標準誤差和實際意義等因素的影響。 因此，在評估預測模型中變數的重要性時，必須考慮多種因素，而不僅僅是相關係數。 
#   雖然相關係數可以顯示變數之間關係的強度，但在評估線性模型中變數的重要性時，應將相關係數與其他因素結合使用。


# 1.5 Consider fitting linear models with manually selected variables (i.e.,
# multivariate analysis). What is your best model? You may consider those variables
# with "p < 0.05"

# Seems like the following model with the predictors is ok, based on "p < 0.05"
summary(lm(PRICE ~ wheelbase + drivewheel + enginesize + boreratio + stroke + compressionratio + horsepower + peakrpm + mpg, data = All_Cars))


# 1.6 Split the Car dataset into 70% training and 30% testing sets using random
# seed “20230929”. Using the training set, build multiple linear regression models
# with the predictor variables selected in previous analyses.

set.seed(20230929)
train_idx = sample(1:nrow(All_Cars), 0.7 * nrow(All_Cars))
train_d = All_Cars[train_idx,]
test_d = All_Cars[setdiff(1:nrow(All_Cars), train_idx),]

# Build linear models with training set train_d. 
# You may consider reusing your selected variables in previous questions.

# Model with selected variables.
All_Cars_lm_select = lm(PRICE ~ wheelbase + drivewheel + enginesize + boreratio + stroke + compressionratio + horsepower + peakrpm + mpg, train_d)
# Model with all variables
All_Cars_lm_all = lm(PRICE ~ ., train_d) 
# Model with all variables with 2-way interactions
All_Cars_lm_all2way = lm(PRICE ~ (.)^2, train_d)








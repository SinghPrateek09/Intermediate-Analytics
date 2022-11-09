# ALY6015 Module 4 R Practice: Singh Prateek ------------------------------------------------
#----------------- Author: Prateek Singh
#----------------- Submission Date: 14th Feb, 2022
#----------------- Tutor: Jiyoung Yun


# Step: Installing New Libraries ------------------------------------------------

install.packages("Metrics")
install.packages("glmnet")


# Step: Importing Libraries ------------------------------------------------

library(ISLR)
library(dplyr)
library(glmnet)
library(caret)
library(Metrics)
library(psych)


# Step: Load the inbuilt Dataset and do the EDA ------------------------------------------------

data("College")

str(College)
describe(College)
summary(College)


# Step: Split the data into a train and test set ------------------------------------------------

set.seed(3456)
Index_Train <- createDataPartition(College$Grad.Rate, p = 0.7, list = FALSE, times = 1)
Col_train <- College[Index_Train,]
Col_test <- College[-Index_Train,]

# Create a model matrix for both train and test data

Col_train_x <- model.matrix(Grad.Rate ~.,Col_train)[,-1]
Col_test_x <- model.matrix(Grad.Rate ~.,Col_test)[,-1]

Col_train_y <- Col_train$Grad.Rate
Col_test_y <- Col_test$Grad.Rate


# Step: Ridge Regression ------------------------------------------------

# Step: Use the cv.glmnet function to estimate the lambda.min and lambda.1se values ------------------------------------------------

set.seed(3456)
cv_ridge <- cv.glmnet(Col_train_x, Col_train_y, nfolds = 10)
log(cv_ridge$lambda.min)
log(cv_ridge$lambda.1se)


# Step: Plot the results from the cv.glmnet function ------------------------------------------------

plot(cv_ridge)


# Step: Fit a Ridge regression model against the training set and report on the coefficients ------------------------------------------------

Ridge_model_min <- glmnet(Col_train_x, Col_train_y, alpha = 0, lambda = cv_ridge$lambda.min)
Ridge_model_min

coef(Ridge_model_min)

Ridge_model_1se <- glmnet(Col_train_x, Col_train_y, alpha = 0, lambda = cv_ridge$lambda.1se)
Ridge_model_1se

coef(Ridge_model_1se)


# Step: Determine the performance of the fit model against the training set by calculating the root mean square error (RMSE). sqrt(mean((actual - predicted)^2)) ------------------------------------------------

ols <- lm(Grad.Rate ~.,data = Col_train)
coef(ols)                                                                       # Coefficient of ols model with no regularization

pred.ols <- predict(ols, new = Col_test)
rmse(Col_test$Grad.Rate, pred.ols)                                              # Root Mean Square Error

pred.train <- predict(Ridge_model_1se, newx = Col_train_x)                      # College Train Set Predictions
Col_train_rmse_Ridge <- rmse(Col_train_y, pred.train)
Col_train_rmse_Ridge


# Step: Determine the performance of the fit model against the test set by calculating the root mean square error (RMSE). Is your model overfit? ------------------------------------------------

pred.test <- predict(Ridge_model_1se, newx = Col_test_x)                        # College Test Set Predictions
Col_test_rmse_Ridge <- rmse(Col_test_y, pred.test)
Col_test_rmse_Ridge


# Step: LASSO Regression ------------------------------------------------

# Step: Use the cv.glmnet function to estimate the lambda.min and lambda.1se values ------------------------------------------------

set.seed(3456)
cv_lasso <- cv.glmnet(Col_train_x, Col_train_y, nfolds = 10)
log(cv_lasso$lambda.min)
log(cv_lasso$lambda.1se)


# Step: Plot the results from the cv.glmnet function ------------------------------------------------

plot(cv_lasso)


# Step: Fit a Lasso regression model against the training set and report on the coefficients. Do any coefficients reduce to zero? If so, which ones?------------------------------------------------

Lasso_model_min <- glmnet(Col_train_x, Col_train_y, alpha = 1, lambda = cv_lasso$lambda.min)
Lasso_model_min

coef(Lasso_model_min)

Lasso_model_1se <- glmnet(Col_train_x, Col_train_y, alpha = 1, lambda = cv_lasso$lambda.1se)
Lasso_model_1se

coef(Lasso_model_1se)


# Step: Determine the performance of the fit model against the training set by calculating the root mean square error (RMSE). sqrt(mean((actual - predicted)^2)) ------------------------------------------------

ols <- lm(Grad.Rate ~.,data = Col_train)
coef(ols)                                                                       # Coefficient of ols model with no regularization

pred_ols <- predict(ols, new = Col_test)
rmse(Col_test$Grad.Rate, pred_ols)                                              # Root Mean Square Error

pred_train <- predict(Lasso_model_1se, newx = Col_train_x)                      # College Train Set Predictions
Col_train_rmse_Lasso <- rmse(Col_train_y, pred_train)
Col_train_rmse_Lasso


# Step: Determine the performance of the fit model against the test set by calculating the root mean square error (RMSE). Is your model overfit? ------------------------------------------------

pred_test <- predict(Lasso_model_1se, newx = Col_test_x)                        # College Test Set Predictions
Col_test_rmse_Lasso <- rmse(Col_test_y, pred_test)
Col_test_rmse_Lasso


# Step: Fit a model and compare ------------------------------------------------

set.seed(123)

Train_Index <- createDataPartition(College$Grad.Rate, p=0.7, list = FALSE, times = 1)

train_model <- College[Train_Index,]
test_model <- College[-Train_Index,]

Model <- step(lm(Grad.Rate ~., data = test_model), direction = 'both')
coef(Model)
summary(Model)

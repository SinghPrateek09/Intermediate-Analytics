# ALY6015 Module 3 R Practice: Singh Prateek ------------------------------------------------
#----------------- Author: Prateek Singh
#----------------- Submission Date: 9th Feb, 2022
#----------------- Tutor: Jiyoung Yun


# Step: Installing New Libraries ------------------------------------------------

install.packages("ISLR")
install.packages("caret")
install.packages("lattice")


# Step: Importing Libraries ------------------------------------------------

library(ISLR)
library(dplyr)
library(psych)
library(caret)
library(ggplot2)
library(lattice)
library(pROC)


# Step: Load the inbuilt Dataset and do the EDA ------------------------------------------------

data("College")

str(College)
describe(College)
summary(College)

head(arrange(College, Enroll))                                                  # Lowest Enrollment in the colleges
head(arrange(College, desc(Enroll)))                                            # Highest Enrolling colleges

head(arrange(College, desc(PhD)))                                               # To investigate the percentage going above 100% for faculty

ggplot(data = College, aes(x = Apps,fill = Private)) +                          
  geom_histogram() + 
  labs(title = "Private vs Public University Applications")

ggplot(data = College, aes(x = Accept,fill = Private)) +                          
  geom_histogram() + 
  labs(title = "Private vs Public University Acceptance Rate")

ggplot(data = College, aes(x = Enroll,fill = Private)) +                          
  geom_histogram() + 
  labs(title = "Private vs Public University Enrollment")

xyplot(F.Undergrad + P.Undergrad ~ S.F.Ratio, College, auto.key = TRUE, 
       ylab = "FullTime & PartTime Undergrad Students",
       xlab = "Student Faculty Ratio",
       xlim=c(0,45),
       scales = list(
         x = list(
           at=seq(0,45,3)
                 )))

ggplot(College, aes(x = rownames(College), Outstate)) + 
  geom_bar(stat='identity') +
  labs(title = "Out of State Tuition Fees Distribution") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(name = "Colleges",  guide = guide_axis(check.overlap = TRUE))


# Step: Split the data into a train and test set ------------------------------------------------

set.seed(3456)
Index_Train <- createDataPartition(College$Private, p = 0.7, list = FALSE, times = 1)
Col_train <- College[Index_Train,]
Col_test <- College[-Index_Train,]


# Step: Fitting Logistic Regression Model to a training set ------------------------------------------------

model1 <- glm(Private ~., data = Col_train , family = binomial(link = "logit"))
summary(model1)

model2 <- glm(Private ~ F.Undergrad + Outstate + perc.alumni, data = Col_train , family = binomial(link = "logit"))
summary(model2)

coef(model2)
exp(coef(model2))

# Train set predictions

Train_Prob <- predict(model2, newdata = Col_train, type = "response")

predicted.classes.min <- as.factor(ifelse(Train_Prob >= 0.5, "Yes", "No"))


# Step: Creating a confusion matrix for Train data set ------------------------------------------------

confusionMatrix(predicted.classes.min, Col_train$Private, positive = 'Yes')


# Step: Calculating Accuracy, Precision, Recall, and Specificity ------------------------------------------------

TP <- 381
TN <- 128
FP <- 21
FN <- 15

Accuracy = (TP+TN)/(TP+TN+FP+FN)
Accuracy

Precison = TP/(FP+TP)
Precison

Recall = TP/(TP+FN)
Recall

Specificity = TN/(TN+FP)
Specificity


# Step: Creating a confusion matrix for the test set ------------------------------------------------

# Test set predictions

Test_Prob <- predict(model2, newdata = Col_test, type = "response")

predicted.classes.min <- as.factor(ifelse(Test_Prob >= 0.5, "Yes", "No"))


confusionMatrix(predicted.classes.min, Col_test$Private, positive = 'Yes')


# Step: Plotting ROC Curve ------------------------------------------------

Test_ROC <- roc(Col_test$Private, Test_Prob)
plot(Test_ROC, col = "cyan4", ylab = "Sensitivity - TP Rate", xlab = "Specificity - FP Rate")


# Step: Calculating the Area under the ROC Curve ------------------------------------------------

Test_AUC <- auc(Test_ROC)
Test_AUC

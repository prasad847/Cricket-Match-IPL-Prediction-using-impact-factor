setwd("F:/Masters/Semester 2/Advanced Data Mining/IPL/IPL_ADM/")
library(dplyr)
library(e1071)
# Import Data from CSV
data = read.csv("./dataset/clean datasets/x15_over_2innings.csv")
str(data)

# Partition
set.seed(3696)
index <- sample(1:dim(data)[1],dim(data)[1]*.75,replace=FALSE)
training  <- data[index, ]
testing  <- data[-index, ]

# Logistic Regression
# Training
logistic <-glm(Win ~ TossWin+x+RunsNeeded+RunRateNeeded,data=training,family="binomial")
summary(logistic)
res <- predict(logistic,newdata=training,type='response')
confusion = table(Actual_Value = training$Win,Predicted_Value = res>0.5)
# Accuracy
(confusion[[1,1]] + confusion[[2,2]]) / sum(confusion)

# Testing
res_testing <- predict(logistic,newdata=testing,type='response')
confusion = table(Actual_Value = testing$Win,Predicted_Value = res_testing>0.5)
# Accuracy
(confusion[[1,1]] + confusion[[2,2]]) / sum(confusion)

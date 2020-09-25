setwd("F:/Masters/Semester 2/Advanced Data Mining/IPL/IPL_ADM/")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
win = read.csv("./dataset/clean datasets/x5_over_2innings.csv")
str(win)
xtabs(~Win+Team, data=win)

# Convert into Factor
win$Win<- as.factor(win$Win)
win$match_id <- as.factor(win$Match_Id)
win$Team_Id <- as.factor(win$Team)
win$match_winner <- as.factor(win$match_winner)

# Visualization
win %>%
  ggplot(aes(x=Win, y=x, fill = Win))+
  geom_boxplot() +
  ggtitle("Box Plot")
win %>%
  ggplot(aes(x=Win, y=RunsNeeded, fill = Win))+
  geom_boxplot() +
  ggtitle("Box Plot")
win %>%
  ggplot(aes(x=Win, y=RunRateNeeded, fill = Win))+
  geom_boxplot() +
  ggtitle("Box Plot")


# Data Partition
library(e1071)
set.seed(3696)
index <- sample(1:dim(win)[1],dim(win)[1]*.75,replace=FALSE)
training  <- win[index, ]
testing  <- win[-index, ]

############################# Naive Bayes' Model ############################## 
model <- naive_bayes(Win ~ TossWin+x+RunsNeeded+RunRateNeeded,data = training)
model
plot(model)


# Prediction
p <- predict(model, training, type = 'prob')
head(cbind(p,training))


# Confusion Matrix#
# Training Data
p1 <- predict(model,training)
(tab1 <- table(p1,training$Win))
# Misclassification
1-sum(diag(tab1)) / sum(tab1)
# Accuracy
sum(diag(tab1)) / sum(tab1)

# Testing Data
p2 <- predict(model,testing)
(tab2 <- table(p2,testing$Win))
## Misclassification
1-sum(diag(tab2)) / sum(tab2)
# Accuracy
sum(diag(tab2)) / sum(tab2)


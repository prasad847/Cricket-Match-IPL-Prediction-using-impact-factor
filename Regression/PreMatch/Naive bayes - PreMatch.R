setwd("F:/Masters/Semester 2/Advanced Data Mining/IPL/IPL_ADM/")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
win = read.csv("./dataset/clean datasets/Match_Impact_Teams.csv")
str(win)
xtabs(~Win+Team_Id, data=win)

# Convert into Factor
win$Win<- as.factor(win$Win)
win$match_id <- as.factor(win$match_id)
win$Team_Id <- as.factor(win$Team_Id)
win$match_winner <- as.factor(win$match_winner)

# Visualization
pairs.panels(win[-8])
win %>%
  ggplot(aes(x=Win, y=Avg_PreMatch_Impact_Fielding, fill = Win))+
  geom_boxplot() +
  ggtitle("Box Plot")
win %>%
  ggplot(aes(x=Win, y=Avg_PreMatch_Impact_Batting, fill = Win))+
  geom_boxplot() +
  ggtitle("Box Plot")
win %>%
  ggplot(aes(x=Win, y=Avg_PreMatch_Impact_Bowling, fill = Win))+
  geom_boxplot() +
  ggtitle("Box Plot")

win %>% ggplot(aes(x=Avg_PreMatch_Impact_Fielding, fill = Win))+
  geom_density(alpha=0.8, color='black') +
  ggtitle("Density Plot")
win %>% ggplot(aes(x=Avg_PreMatch_Impact_Batting, fill = Win))+
  geom_density(alpha=0.8, color='black') +
  ggtitle("Density Plot")
win %>% ggplot(aes(x=Avg_PreMatch_Impact_Bowling, fill = Win))+
  geom_density(alpha=0.8, color='black') +
  ggtitle("Density Plot")

# Data Partition
library(e1071)
set.seed(3696)
index <- sample(1:dim(win)[1],dim(win)[1]*.75,replace=FALSE)
training  <- win[index, ]
testing  <- win[-index, ]

############################# Naive Bayes' Model ############################## 
model <- naive_bayes(Win ~ TossWin+Avg_PreMatch_Impact_Batting+Avg_PreMatch_Impact_Bowling+Avg_PreMatch_Impact_Fielding,data = training)
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


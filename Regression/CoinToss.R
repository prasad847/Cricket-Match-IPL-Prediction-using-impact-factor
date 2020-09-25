setwd("F:/Masters/Semester 2/Advanced Data Mining/IPL/IPL_ADM/")
library(caret)
library(e1071)
library(ModelMetrics)
library(dplyr)
library(ggplot2)
library(reshape)
library(gmodels)

#toss winning
win = read.csv("./dataset/clean datasets/Match_Impact_Teams.csv")
y <- win$Win
table(y)
y <- factor(y, levels = c(0,1), labels = c("No", "Yes"))
table(y)
prop.table(table(y))
barplot(table(y), main = "Distribution of winning team", ylab="Frequency")
set.seed(1337)
index <- sample(1:length(y), length(y) * .25, replace=FALSE)
testing <- y[index]
perishModel <- rep("No", length(testing))
coinModel <- round(runif(length(testing), min=0, max=1))
coinModel <- factor(coinModel, levels = c(0,1), labels = c("No", "Yes"))
perishModel <- factor(perishModel, levels = c("No", "Yes"), labels = c("No", "Yes"))
table(testing, perishModel)
table(testing, coinModel)
(coinAccuracy <- 1 - mean(coinModel != testing))
(perishAccuracy <- 1 - mean(perishModel != testing))
perish <- c()
coin <- c()
for (i in 1:1000) {
  index <- sample(1:length(y), length(y) * .25, replace=FALSE)
  testing <- y[index]
  coinModel <- round(runif(length(testing), min=0, max=1))
  coinModel <- factor(coinModel, levels = c(0,1), labels = c("No", "Yes"))
  coin[i] <- 1 - mean(coinModel != testing)
  perish[i] <- 1 - mean(perishModel != testing)
}
results <- data.frame(coin, perish)
names(results) <- c("Coin Toss Accuracy", "Everyone Perishes Accuracy")
summary(results)

ggplot(melt(results), mapping = aes (fill = variable, x = value)) + geom_density (alpha = .5)
boxplot(results)

df <- win[, c("Win", "TossWin")]
df$Win <- factor(df$Win, levels = c(0,1), labels = c("No", "Yes"))
index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
training <- df[index, ]
testing <- df[-index, ]
table(training$Win, training$TossWin)

predictWin <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$TossWin == 1] <- "Yes"
  return(model)
}
win <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
  testing <- df[-index, ]
  winModel <- predictWin(testing)
  win[i] <- 1 - mean(winModel != testing$Win)
}
results$`Winning Accuracy` <- win
names(results) <- c("Coin", "All Perish", "toss winning")
boxplot(results)

CrossTable(testing$Win, winModel)
CrossTable(testing$Win, winModel, prop.chisq = F, prop.c = F, prop.r = F)

confusionMatrix(table(testing$Win,winModel))

winModel1 = as.factor(winModel)
auc(testing$Win, winModel1)

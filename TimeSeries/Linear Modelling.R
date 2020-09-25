setwd("F:/Masters/Semester 2/Advanced Data Mining/IPL/IPL_ADM/")
library(dplyr)
library(tidyverse)
library(plyr)
over_by_over = read.csv("./dataset/clean datasets/Over_by_Over.csv")
actual = read.csv("./dataset/clean datasets/Match_Impact_Teams.csv")
m1 = over_by_over[!duplicated(over_by_over$Match_Id),]
m1 = m1 [,c(-2,-4:-12)]

m2 = subset(over_by_over, Innings == 2)
m2 = m2 [,c(-2,-3,-5:-7,-9,-11,-12)]

m1 = m1 %>%
  left_join(m2, by=c("Match_Id"))
m1 = m1 [,c(-3,-4)]
m1 = m1[!duplicated(m1$Match_Id),]

# Use cast to get what we want
newdat <- ddply(m2, .(Match_Id), transform, idx = paste("Over", 1:length(Over), sep = ""))
# Use cast to get what we want
m2_test = cast(newdat, Match_Id ~ idx, value = .(x))

m2 <- m2_test[,c(1,2,10,11,12,13,14,15,16,17,3,4,5,6,7,8,9,18,19,20,21)]
m2 = m2 [,c(-17:-21)]
df <- m2 %>% na.omit()

linear_modelling = function(x){
  x1 = c(x$Over1,x$Over2,x$Over3,x$Over4,x$Over5,x$Over6,x$Over7,x$Over8,x$Over9,x$Over10,x$Over11,x$Over12,x$Over13,x$Over14,x$Over15)
  dat <- data.frame(x=seq_along(x1), y=x1)
  m = predict(lm(y ~ x, data=dat), newdata=list(x=20))
  return(m)
}

df1 = df
df1 = df1[,c(-3:-16)]

for (i in 1:nrow(df)){
  df1[i,]$Over1 = round(linear_modelling(df[i,]),digits=0)
}

m1 <- m1 %>% left_join(df1, by=c("Match_Id"))
output <- m1 %>% na.omit()
colnames(output) <- c("Match_Id", "Team","Target","Predicted Score")

output = output %>% mutate(`Predicted Win` = ifelse(output$`Predicted Score`>output$Target, 0, 1))

output <- output %>% left_join(actual, by=c("Match_Id"="match_id","Team"="Team_Id"))
output = output[,c(-6:-13,-15:-26)]

output = output %>% mutate(`Perfect Prediction` = ifelse(output$`Predicted Win`== output$Win, 1, 0))

accuracy = sum(output$`Perfect Prediction`)/nrow(output)
accuracy

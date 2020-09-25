require(randomForest)
df = read.csv("F:/Masters/Semester 2/Advanced Data Mining/IPL/raghu543-ipl-data-till-2017/clean datasets/Match_Impact_Teams.csv")
set.seed(1234)
dim(df)
set.seed(3696)
index <- sample(1:dim(df)[1],dim(df)[1]*.75,replace=FALSE)
train  <- df[index, ]
test  <- df[-index, ]

data.rf=randomForest(Win ~ TossWin+Avg_Impact_Batting+Avg_Impact_Bowling+Avg_Impact_Fielding,data=train)
data.rf
plot(data.rf)

oob.err=double(13)
test.err=double(13)

for(mtry in 1:13) 
{
  rf=randomForest(Win ~ TossWin+Avg_Impact_Batting+Avg_Impact_Bowling+Avg_Impact_Fielding,data=train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test, mean( (Win - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
test.err
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


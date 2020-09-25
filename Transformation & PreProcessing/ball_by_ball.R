setwd("F:/Masters/Semester 2/Advanced Data Mining/IPL/IPL_ADM/")
library(dplyr)
ball_by_ball = read.csv("./dataset/Ball_By_Ball.csv")
#Striker_Batting_Position
ball_by_ball$Striker_Batting_Position[is.na(ball_by_ball$Striker_Batting_Position)] = 0
#Team_Batting
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Delhi Daredevils"] = 6
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Gujarat Lions"] = 13
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Kings XI Punjab"] = 4
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Kolkata Knight Riders"] = 1
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Mumbai Indians"] = 7
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Rising Pune Supergiants"] = 12
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Royal Challengers Bangalore"] = 2
ball_by_ball$Team_Batting[ball_by_ball$Team_Batting == "Sunrisers Hyderabad"] = 11
table(droplevels(ball_by_ball$Team_Batting))
#Team_Bowling
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Delhi Daredevils"] = 6
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Gujarat Lions"] = 13
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Kings XI Punjab"] = 4
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Kolkata Knight Riders"] = 1
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Mumbai Indians"] = 7
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Rising Pune Supergiants"] = 12
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Royal Challengers Bangalore"] = 2
ball_by_ball$Team_Bowling[ball_by_ball$Team_Bowling == "Sunrisers Hyderabad"] = 11
table(droplevels(ball_by_ball$Team_Bowling))
#remove Extra_Type
ball_by_ball$validBall = ifelse(ball_by_ball$Extra_Type=="No Extras", 1, ifelse(ball_by_ball$Extra_Type=="byes", 1, ifelse(ball_by_ball$Extra_Type=="Byes", 1, ifelse(ball_by_ball$Extra_Type=="legbyes", 1, ifelse(ball_by_ball$Extra_Type=="Legbyes", 1, 0)))))
ball_by_ball <- ball_by_ball[,-8]
#remove Match_date
ball_by_ball <- ball_by_ball[,-27]
#replace blank with 0 in Player_Out
ball_by_ball$Player_Out[is.na(ball_by_ball$Player_Out)] = 0 
#replace blank with 0 in Fielders
ball_by_ball$Fielders[is.na(ball_by_ball$Fielders)] = 0 
write.csv(ball_by_ball,"./dataset/clean datasets/Ball_By_Ball_New.csv",row.names = FALSE)





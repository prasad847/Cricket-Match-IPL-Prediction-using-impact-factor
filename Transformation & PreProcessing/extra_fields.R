setwd("F:/Masters/Semester 2/Advanced Data Mining/IPL/IPL_ADM/")
library(dplyr)
library(tidyverse)
# Load Ball by Ball
ball_by_ball = read.csv("./dataset/clean datasets/Ball_By_Ball_New.csv")
match_players = read.csv("./dataset/Player_match.csv")
Players <- read.csv("./dataset/Player.csv")
Match <- read.csv("./dataset/Match.csv")
Teams <- read.csv("./dataset/Team.csv")
Teams = Teams[,c(-1)]

ball_by_ball = ball_by_ball[!(ball_by_ball$MatcH_id=="419119" | ball_by_ball$MatcH_id=="392195" | ball_by_ball$MatcH_id=="392202" | ball_by_ball$MatcH_id=="392207" | ball_by_ball$MatcH_id=="419126" | ball_by_ball$MatcH_id=="598009" | ball_by_ball$MatcH_id=="598022" | ball_by_ball$MatcH_id=="729320" | ball_by_ball$MatcH_id=="829746" | ball_by_ball$MatcH_id=="1082625"),]


# Runs scored by player
#Batting score
df = setNames(aggregate(ball_by_ball$Runs_Scored,by=list(ball_by_ball$Striker,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Runs Scored"))
df2 = setNames(aggregate(ball_by_ball$validBall,by=list(ball_by_ball$Striker,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Balls Faced"))
#Strike rate
df3 <- data.frame(df,df2$`Balls Faced`)
df3 <- df3 %>% mutate(Strike_rate = Runs.Scored/df2..Balls.Faced.*100)
df3$Strike_rate <- round(df3$Strike_rate,digits = 2)

#Bowling
df4 = setNames(aggregate(ball_by_ball$Runs_Scored,by=list(ball_by_ball$Bowler,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Runs Conceded"))
df11 = setNames(aggregate(ball_by_ball$validBall,by=list(ball_by_ball$Bowler,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Balls Bowled"))
df5 <- setNames(aggregate(ball_by_ball$Bowler_Wicket,by=list(ball_by_ball$Bowler,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Wickets")) 

df12 <- setNames(aggregate(ball_by_ball$Runs_Scored,by=list(ball_by_ball$Bowler,ball_by_ball$Over_id,ball_by_ball$MatcH_id,ball_by_ball$Innings_No),sum),c("Player_Id","Over_id","Match_Id","Innings_No","Runs_Scored_Over"))
df12 = df12 %>% mutate(`maiden` = ifelse((df12$Runs_Scored_Over==0), 1, 0))

df13 <- setNames(aggregate(df12$maiden,by=list(df12$Player_Id,df12$Match_Id),sum),c("Player_Id","Match_Id","Maidens"))


# Run Out
df6 <- setNames(aggregate(ball_by_ball$Run_out,by=list(ball_by_ball$Fielders,ball_by_ball$MatcH_id),length),c("Player_Id","Match_Id","Run Out")) 

# Catches
df7 <- setNames(aggregate(ball_by_ball$caught_and_bowled,by=list(ball_by_ball$Fielders,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Catches1")) 
df8 <- setNames(aggregate(ball_by_ball$Caught,by=list(ball_by_ball$Fielders,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Catches2")) 
df9 <- setNames(aggregate(ball_by_ball$Keeper_Catch,by=list(ball_by_ball$Fielders,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Catches3")) 

# Stumping
df10 <- setNames(aggregate(ball_by_ball$Stumped,by=list(ball_by_ball$Fielders,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Stumping")) 

#4 and 6
df16 <- setNames(aggregate(ball_by_ball$Runs_Scored == 4,by=list(ball_by_ball$Striker,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Fours"))
df17 <- setNames(aggregate(ball_by_ball$Runs_Scored == 6,by=list(ball_by_ball$Striker,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Sixes"))
#batting dot balls
df14 <- setNames(aggregate(ball_by_ball$Runs_Scored == 0,by=list(ball_by_ball$Striker,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Dot balls Batsmen"))
#Bowling dot balls
df15 <- setNames(aggregate(ball_by_ball$Runs_Scored == 0,by=list(ball_by_ball$Bowler,ball_by_ball$MatcH_id),sum),c("Player_Id","Match_Id","Dot balls Bowler"))

# Merge Columns
match_players = match_players %>%
  left_join(df3, by=c("Player_Id","Match_Id")) %>% 
  left_join(df4, by=c("Player_Id","Match_Id")) %>%
  left_join(df11, by=c("Player_Id","Match_Id")) %>%   
  left_join(df5, by=c("Player_Id","Match_Id")) %>%
  left_join(df6, by=c("Player_Id","Match_Id")) %>% 
  left_join(df7, by=c("Player_Id","Match_Id")) %>%
  left_join(df8, by=c("Player_Id","Match_Id")) %>%
  left_join(df9, by=c("Player_Id","Match_Id")) %>% 
  left_join(df10, by=c("Player_Id","Match_Id")) %>%
  left_join(df14, by=c("Player_Id","Match_Id")) %>%
  left_join(df15, by=c("Player_Id","Match_Id")) %>%
  left_join(df16, by=c("Player_Id","Match_Id")) %>%
  left_join(df17, by=c("Player_Id","Match_Id")) %>%
  left_join(df13, by=c("Player_Id","Match_Id"))  

match_players = match_players %>% mutate(Catches = Catches1+Catches2+Catches3)

# Remove Extra Catches Columns
match_players = match_players[ , -which(names(match_players) %in% c("Catches1","Catches2","Catches3"))]

#Remove NA
match_players$Runs.Scored[is.na(match_players$Runs.Scored)] = 0
match_players$df2..Balls.Faced.[is.na(match_players$df2..Balls.Faced.)] = 0
match_players$Strike_rate[is.na(match_players$Strike_rate)] = 0
match_players$`Runs Conceded`[is.na(match_players$`Runs Conceded`)] = 0
match_players$Wickets[is.na(match_players$Wickets)] = 0
match_players$`Run Out`[is.na(match_players$`Run Out`)] = 0
match_players$Stumping[is.na(match_players$Stumping)] = 0
match_players$Catches[is.na(match_players$Catches)] = 0
match_players$`Balls Bowled`[is.na(match_players$`Balls Bowled`)] = 0
match_players$`Dot balls Batsmen`[is.na(match_players$`Dot balls Batsmen`)] = 0
match_players$`Dot balls Bowler`[is.na(match_players$`Dot balls Bowler`)] = 0
match_players$Sixes[is.na(match_players$Sixes)] = 0
match_players$Fours[is.na(match_players$Fours)] = 0
match_players$Maidens[is.na(match_players$Maidens)] = 0
match_players <- match_players[-1,]

# 100s
match_players = match_players %>% mutate(`100s` = ifelse(match_players$Runs.Scored>=100, 1, 0))
# 50s
match_players = match_players %>% mutate(`50s` = ifelse((match_players$Runs.Scored>=50)&(match_players$Runs.Scored<100), 1, 0))
# 30s
match_players = match_players %>% mutate(`30s` = ifelse((match_players$Runs.Scored>=30)&(match_players$Runs.Scored<50), 1, 0))



#Economy
No_of_overs<- match_players$`Balls Bowled`/6
match_players <- cbind(match_players,No_of_overs)
economy <- match_players$`Runs Conceded`/match_players$No_of_overs

match_players<- cbind(match_players,economy)
#remove "Na" and roundoff 

match_players$economy[is.na(match_players$economy)] = 0
match_players$economy <- round(match_players$economy,digits=2)

#No_of_overs roundoff
match_players$No_of_overs <- round(match_players$No_of_overs,digits=2)

#Player_team
# match_players$Player_team = as.numeric(match_players$Player_team)
# match_players$Player_team[is.na(match_players$Player_team)] = 0
match_players$Player_team = ifelse(match_players$Player_team=="Delhi Daredevils", 6, ifelse(match_players$Player_team=="Chennai Super Kings", 3, ifelse(match_players$Player_team=="Deccan Chargers", 8, ifelse(match_players$Player_team=="Gujarat Lions", 13, ifelse(match_players$Player_team=="Kings XI Punjab" | match_players$Player_team=="kings XI Punjab", 4,ifelse(match_players$Player_team=="Kochi Tuskers Kerala",9,ifelse(match_players$Player_team=="Kolkata Knight Riders", 1,ifelse(match_players$Player_team=="Mumbai Indians",7,ifelse(match_players$Player_team=="Pune Warriors",10,ifelse(match_players$Player_team=="Rajasthan Royals",5,ifelse(match_players$Player_team=="Rising Pune Supergiants",12,ifelse(match_players$Player_team=="Royal Challengers Bangalore",2,ifelse(match_players$Player_team=="Sunrisers Hyderabad" | match_players$Player_team=="sunrisers Hyderabad",11, 0)))))))))))))
#Opposit_Team
# match_players$Opposit_Team = as.numeric(match_players$Opposit_Team)
# match_players$Opposit_Team[is.na(match_players$Opposit_Team)] = 0
match_players$Opposit_Team = ifelse(match_players$Opposit_Team=="Delhi Daredevils", 6, ifelse(match_players$Opposit_Team=="Chennai Super Kings", 3, ifelse(match_players$Opposit_Team=="Deccan Chargers", 8, ifelse(match_players$Opposit_Team=="Gujarat Lions", 13, ifelse(match_players$Opposit_Team=="Kings XI Punjab" | match_players$Player_team=="kings XI Punjab", 4,ifelse(match_players$Opposit_Team=="Kochi Tuskers Kerala",9,ifelse(match_players$Opposit_Team=="Kolkata Knight Riders", 1,ifelse(match_players$Opposit_Team=="Mumbai Indians",7,ifelse(match_players$Opposit_Team=="Pune Warriors",10,ifelse(match_players$Opposit_Team=="Rajasthan Royals",5,ifelse(match_players$Opposit_Team=="Rising Pune Supergiants",12,ifelse(match_players$Opposit_Team=="Royal Challengers Bangalore",2,ifelse(match_players$Opposit_Team=="Sunrisers Hyderabad" | match_players$Player_team=="sunrisers Hyderabad",11, 0)))))))))))))

#Winner
Match$match_winner = ifelse(Match$match_winner=="Delhi Daredevils", 6, ifelse(Match$match_winner=="Chennai Super Kings", 3, ifelse(Match$match_winner=="Deccan Chargers", 8, ifelse(Match$match_winner=="Gujarat Lions", 13, ifelse(Match$match_winner=="Kings XI Punjab" | match_players$Player_team=="kings XI Punjab", 4,ifelse(Match$match_winner=="Kochi Tuskers Kerala",9,ifelse(Match$match_winner=="Kolkata Knight Riders", 1,ifelse(Match$match_winner=="Mumbai Indians",7,ifelse(Match$match_winner=="Pune Warriors",10,ifelse(Match$match_winner=="Rajasthan Royals",5,ifelse(Match$match_winner=="Rising Pune Supergiants",12,ifelse(Match$match_winner=="Royal Challengers Bangalore",2,ifelse(Match$match_winner=="Sunrisers Hyderabad" | match_players$Player_team=="sunrisers Hyderabad",11, 0)))))))))))))

#TossWin
Match$Toss_Winner = ifelse(Match$Toss_Winner=="Delhi Daredevils", 6, ifelse(Match$Toss_Winner=="Chennai Super Kings", 3, ifelse(Match$Toss_Winner=="Deccan Chargers", 8, ifelse(Match$Toss_Winner=="Gujarat Lions", 13, ifelse(Match$Toss_Winner=="Kings XI Punjab" | match_players$Player_team=="kings XI Punjab", 4,ifelse(Match$Toss_Winner=="Kochi Tuskers Kerala",9,ifelse(Match$Toss_Winner=="Kolkata Knight Riders", 1,ifelse(Match$Toss_Winner=="Mumbai Indians",7,ifelse(Match$Toss_Winner=="Pune Warriors",10,ifelse(Match$Toss_Winner=="Rajasthan Royals",5,ifelse(Match$Toss_Winner=="Rising Pune Supergiants",12,ifelse(Match$Toss_Winner=="Royal Challengers Bangalore",2,ifelse(Match$Toss_Winner=="Sunrisers Hyderabad" | match_players$Player_team=="sunrisers Hyderabad",11, 0)))))))))))))

#Opposit_captain
Players$Captain_Id = Players$Player_Id
df18 <- Players[,c(-1,-2,-4,-5,-6,-7)]
df18$Player_Name = as.character(df18$Player_Name)
match_players <- match_players %>% left_join(df18, by=c("Opposit_captain"="Player_Name"))
match_players=match_players[,c(-20)]
match_players$Opposit_captain = match_players$Captain_Id
match_players=match_players[,c(-36)]


#Player_keeper
df19 <- df18
df19$Player_keeper_new = df19$Captain_Id
match_players <- match_players %>% left_join(df19, by=c("Player_keeper"="Player_Name"))
match_players=match_players[,c(-20)]
match_players$Player_keeper = match_players$Player_keeper_new
match_players=match_players[,c(-36,-37)]
#Opposit_keepar
df20 <- df19
df20$Opposit_keepar_new = df20$Captain_Id
match_players <- match_players %>% left_join(df20, by=c("Opposit_keeper"="Player_Name"))
match_players=match_players[,c(-20)]
match_players$Opposit_keeper = match_players$Opposit_keepar_new
match_players=match_players[,c(-36,-37,-38)]
#Player_Captain
df21 <- df20
df21$Player_Captain_new = df21$Captain_Id
match_players <- match_players %>% left_join(df21, by=c("Player_Captain"="Player_Name"))
match_players=match_players[,c(-19)]
match_players$Player_Captain = match_players$Player_Captain_new
match_players=match_players[,c(-36,-37,-38,-39)]

match_players = match_players[,c(-1,-5,-2,-6,-7,-8,-17,-18)]



impact_factor <- function(match_players){
  match_players = match_players %>% mutate(`im1` = ifelse(match_players$Strike_rate>=170,10,ifelse(match_players$Strike_rate>=150,9,ifelse(match_players$Strike_rate>=140,8,ifelse(match_players$Strike_rate>=120,7,ifelse(match_players$Strike_rate>=100,6,ifelse(match_players$Strike_rate>=80,3,2)))))))
  match_players = match_players %>% mutate(`im2` = ifelse(match_players$Fours>=6,10,ifelse(match_players$Fours>=4,9,ifelse(match_players$Fours>=3,8,ifelse(match_players$Fours>=2,7,ifelse(match_players$Fours>0,6,3))))))
  match_players = match_players %>% mutate(`im3` = ifelse(match_players$Sixes>=4,10,ifelse(match_players$Sixes>=3,9,ifelse(match_players$Sixes>=2,7,ifelse(match_players$Sixes>=1,6,4)))))  
  # match_players = match_players %>% mutate(`im4` = ifelse(match_players$`Dot balls Batsmen`>=20,6,ifelse(match_players$`Dot balls Batsmen`>=15,7,ifelse(match_players$`Dot balls Batsmen`>=10,8,ifelse(match_players$`Dot balls Batsmen`>=5,9,10)))))
  match_players = match_players %>% mutate(`im5` = ifelse(match_players$Runs.Scored>=35,10,ifelse(match_players$Runs.Scored>=25,9,ifelse(match_players$Runs.Scored>=20,8,ifelse(match_players$Runs.Scored>=15,7,ifelse(match_players$Runs.Scored>=10,6,3))))))  
  # match_players = match_players %>% mutate(`impact_batting` = (match_players$im1+match_players$im2+match_players$im3+match_players$im4+match_players$im5)/5)  
  match_players = match_players %>% mutate(`impact_batting` = (match_players$im1+match_players$im2+match_players$im3+match_players$im5)/4)  
  
  match_players = match_players %>% mutate(`im16` = ifelse(match_players$economy>10,5,ifelse(match_players$economy>9,6,ifelse(match_players$economy>8,7,ifelse(match_players$economy>7,8,ifelse(match_players$economy>6,9,10))))))
  match_players = match_players %>% mutate(`im11` = ifelse(match_players$`Dot balls Bowler`==0,0,ifelse(match_players$`Dot balls Bowler`<=5,5,ifelse(match_players$`Dot balls Bowler`<=6,6,ifelse(match_players$`Dot balls Bowler`<=8,7,ifelse(match_players$`Dot balls Bowler`<=12,8,ifelse(match_players$`Dot balls Bowler`<=15,9,10)))))))
  match_players = match_players %>% mutate(`im12` = ifelse(match_players$Wickets==0,5,ifelse(match_players$Wickets==1,7,ifelse(match_players$Wickets==2,8,ifelse(match_players$Wickets==3,9,10)))))
  match_players = match_players %>% mutate(`impact_bowling` = (match_players$im16+match_players$im11+match_players$im12)/3)  
  match_players$impact_bowling = round(match_players$impact_bowling,2)
  
  match_players = match_players %>% mutate(`im13` = ifelse(match_players$Catches==0,6,ifelse(match_players$Catches==1,8,ifelse(match_players$Catches==2,9,10))))  
  match_players = match_players %>% mutate(`im17` = ifelse(match_players$`Run Out`==0,6,ifelse(match_players$`Run Out`==1,8,ifelse(match_players$`Run Out`==2,9,10))))  
  match_players = match_players %>% mutate(`impact_fielding` = (match_players$im13+match_players$im17)/2)    
  
  # Remove Extra Catches Columns
  match_players = match_players[ , -which(names(match_players) %in% c("im1","im2","im3","im4","im5","im16","im11","im12","im17","im13"))]
  return(match_players) 
}

match_players = impact_factor(match_players)
match_players = match_players %>% mutate(`match_played` = 1)

impact_factor_player_batting = setNames(aggregate(match_players$impact_batting,by=list(match_players$Player_Id),sum),c("Player_Id","Total_Impact_Batting"))
impact_factor_player_bowling = setNames(aggregate(match_players$impact_bowling,by=list(match_players$Player_Id),sum),c("Player_Id","Total_Impact_Bowling"))
impact_factor_player_fielding = setNames(aggregate(match_players$impact_fielding,by=list(match_players$Player_Id),sum),c("Player_Id","Total_Impact_Fielding"))

impact_factor_player1 = setNames(aggregate(match_players$match_played,by=list(match_players$Player_Id),sum),c("Player_Id","Match Played"))

impact_factor_player = impact_factor_player_batting %>%
  left_join(impact_factor_player_bowling, by=c("Player_Id")) %>% 
  left_join(impact_factor_player_fielding, by=c("Player_Id")) %>% 
  left_join(impact_factor_player1, by=c("Player_Id"))


rm(impact_factor_player1,impact_factor_player_batting,impact_factor_player_bowling,impact_factor_player_fielding)

runs_scored = setNames(aggregate(df$`Runs Scored`,by=list(df$Player_Id),sum),c("Player_Id","Runs Scored"))
impact_factor_player = impact_factor_player %>%
  left_join(runs_scored, by=c("Player_Id")) 
rm(runs_scored)

impact_factor_player = impact_factor_player %>% mutate(`Average_Batting` = impact_factor_player$`Runs Scored`/impact_factor_player$`Match Played`)
impact_factor_player = impact_factor_player %>% mutate(`Average_Impact_Batting` = impact_factor_player$Total_Impact_Batting/impact_factor_player$`Match Played`)
impact_factor_player = impact_factor_player %>% mutate(`Average_Impact_Bowling` = impact_factor_player$Total_Impact_Bowling/impact_factor_player$`Match Played`)
impact_factor_player = impact_factor_player %>% mutate(`Average_Impact_Fielding` = impact_factor_player$Total_Impact_Fielding/impact_factor_player$`Match Played`)

impact_factor_player$Average_Impact_Batting <- round(impact_factor_player$Average_Impact_Batting,digits = 2)
impact_factor_player$Average_Impact_Bowling <- round(impact_factor_player$Average_Impact_Bowling,digits = 2)
impact_factor_player$Average_Impact_Fielding <- round(impact_factor_player$Average_Impact_Fielding,digits = 2)

rm(df,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21)


df = setNames(aggregate(match_players$impact_batting,by=list(match_players$Player_team,match_players$Match_Id),sum),c("Team_Id","match_id","Impact Batting"))
df1 = setNames(aggregate(match_players$impact_bowling,by=list(match_players$Player_team,match_players$Match_Id),sum),c("Team_Id","match_id","Impact Bowling"))
df2 = setNames(aggregate(match_players$impact_fielding,by=list(match_players$Player_team,match_players$Match_Id),sum),c("Team_Id","match_id","Impact Fielding"))


df = df %>% mutate(`Avg_Impact_Batting` = (df$`Impact Batting`)/11)
df1 = df1 %>% mutate(`Avg_Impact_Bowling` = (df1$`Impact Bowling`)/11)
df2 = df2 %>% mutate(`Avg_Impact_Fielding` = (df2$`Impact Fielding`)/11)

df = merge(df,Match,by="match_id")

df = df[,c(-5,-8,-10,-11,-12,-15,-16,-17,-18,-19,-20)]

#Team 1
df$Team1 = ifelse(df$Team1=="Delhi Daredevils", 6, ifelse(df$Team1=="Chennai Super Kings", 3, ifelse(df$Team1=="Deccan Chargers", 8, ifelse(df$Team1=="Gujarat Lions", 13, ifelse(df$Team1=="Kings XI Punjab" | match_players$Player_team=="kings XI Punjab", 4,ifelse(df$Team1=="Kochi Tuskers Kerala",9,ifelse(df$Team1=="Kolkata Knight Riders", 1,ifelse(df$Team1=="Mumbai Indians",7,ifelse(df$Team1=="Pune Warriors",10,ifelse(df$Team1=="Rajasthan Royals",5,ifelse(df$Team1=="Rising Pune Supergiants",12,ifelse(df$Team1=="Royal Challengers Bangalore",2,ifelse(df$Team1=="Sunrisers Hyderabad" | match_players$Player_team=="sunrisers Hyderabad",11, 0)))))))))))))

#Team 2
df$Team2 = ifelse(df$Team2=="Delhi Daredevils", 6, ifelse(df$Team2=="Chennai Super Kings", 3, ifelse(df$Team2=="Deccan Chargers", 8, ifelse(df$Team2=="Gujarat Lions", 13, ifelse(df$Team2=="Kings XI Punjab" | match_players$Player_team=="kings XI Punjab", 4,ifelse(df$Team2=="Kochi Tuskers Kerala",9,ifelse(df$Team2=="Kolkata Knight Riders", 1,ifelse(df$Team2=="Mumbai Indians",7,ifelse(df$Team2=="Pune Warriors",10,ifelse(df$Team2=="Rajasthan Royals",5,ifelse(df$Team2=="Rising Pune Supergiants",12,ifelse(df$Team2=="Royal Challengers Bangalore",2,ifelse(df$Team2=="Sunrisers Hyderabad" | match_players$Player_team=="sunrisers Hyderabad",11, 0)))))))))))))

df = df %>% mutate(`TossWin` = ifelse(df$Team_Id == df$Toss_Winner,1,0))
df = df %>% mutate(`Win` = ifelse(df$Team_Id == df$match_winner,1,0))


df = df %>%
  left_join(df1, by=c("Team_Id","match_id")) %>% 
  left_join(df2, by=c("Team_Id","match_id"))



# over by over score
a = ball_by_ball
b <- a %>%
  group_by(MatcH_id, Over_id) %>%
  summarise(a = sum(Runs_Scored),b = distinct(Team_Batting))

dfa = setNames(aggregate(a$Runs_Scored,by=list(a$MatcH_id,a$Innings_No,a$Team_Batting,a$Over_id),sum),c("Match_Id", "Innings","Team","Over","Runs Scored"))
dfa1 = setNames(aggregate(a$Extra_runs,by=list(a$MatcH_id,a$Innings_No,a$Team_Batting,a$Over_id),sum),c("Match_Id", "Innings","Team","Over","Extras"))



dfa = dfa %>%
  left_join(dfa1, by=c("Match_Id", "Innings","Team","Over"))

dfa$Overall <- dfa$`Runs Scored`+dfa$Extras
dfa2 = setNames(aggregate(dfa$Overall,by=list(dfa$Match_Id,dfa$Innings,dfa$Team),sum),c("Match_Id", "Innings","Team","Score"))


dfa3 = dfa %>%
  group_by(Match_Id,Innings)%>%
  mutate(x = cumsum(Overall))

dfa3 = dfa3 %>%
  mutate(RunRate = x/Over)

dfa4 = setNames(aggregate(dfa3$x, by=list(dfa3$Match_Id,dfa3$Innings),function(x) max(x)+1),c("Match_Id","Innings","Target"))
dfa5 = dfa4[!(dfa4$Innings=="2"),]
dfa5$Innings = 2
dfa3 = dfa3 %>%
  left_join(dfa5, by=c("Match_Id", "Innings"))

dfa3$Target[is.na(dfa3$Target)] <- 0


dfa3 = dfa3 %>%
  mutate(RunsNeeded = ifelse(Innings == 1,0,Target-x))

dfa3 = dfa3 %>%
  mutate(RunRateNeeded = ifelse(Innings == 1,0,RunsNeeded/(20-Over)))


# df = df[c(-1:-797),]

################# Impact Team before match ####################


Impact_Batting = function(x){
  y = c(0,cumsum(x$Avg_Impact_Batting))
  return(round(y,digits = 2))
}

Impact_Bowling = function(x){
  y = c(0,cumsum(x$Avg_Impact_Bowling))
  return(round(y,digits = 2))
}

Impact_Fielding = function(x){
  y = c(0,cumsum(x$Avg_Impact_Fielding))
  return(round(y,digits = 2))
}

df$Avg_PreMatch_Impact_Batting = 6.5
df$Avg_PreMatch_Impact_Bowling = 6.5
df$Avg_PreMatch_Impact_Fielding = 6.5

t1 = df[df$Team_Id==1,]
t2 = df[df$Team_Id==2,]
t3 = df[df$Team_Id==3,]
t4 = df[df$Team_Id==4,]
t5 = df[df$Team_Id==5,]
t6 = df[df$Team_Id==6,]
t7 = df[df$Team_Id==7,]
t8 = df[df$Team_Id==8,]
t9 = df[df$Team_Id==9,]
t10 = df[df$Team_Id==10,]
t11 = df[df$Team_Id==11,]
t12 = df[df$Team_Id==12,]
t13 = df[df$Team_Id==13,]

t =  as.data.frame(Impact_Batting(t1))
t1$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t1))
t1$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t1))
t1$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t2))
t2$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t2))
t2$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t2))
t2$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t3))
t3$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t3))
t3$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t3))
t3$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t4))
t4$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t4))
t4$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t4))
t4$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t5))
t5$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t5))
t5$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t5))
t5$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t6))
t6$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t6))
t6$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t6))
t6$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t7))
t7$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t7))
t7$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t7))
t7$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t8))
t8$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t8))
t8$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t8))
t8$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t9))
t9$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t9))
t9$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t9))
t9$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t10))
t10$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t10))
t10$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t10))
t10$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t11))
t11$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t11))
t11$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t11))
t11$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t12))
t12$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t12))
t12$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t12))
t12$PreMatch_Impact_Fielding =  t[-nrow(t),]  

t =  as.data.frame(Impact_Batting(t13))
t13$PreMatch_Impact_Batting =  t[-nrow(t),]
t =  as.data.frame(Impact_Bowling(t13))
t13$PreMatch_Impact_Bowling =  t[-nrow(t),]
t =  as.data.frame(Impact_Fielding(t13))
t13$PreMatch_Impact_Fielding =  t[-nrow(t),]  
rm(t)



for (t in 1:nrow(t1)) {
  t1[t,]$Avg_PreMatch_Impact_Batting = round(((t1[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t1[t,]$Avg_PreMatch_Impact_Bowling = round(((t1[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t1[t,]$Avg_PreMatch_Impact_Fielding = round(((t1[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t2)) {
  t2[t,]$Avg_PreMatch_Impact_Batting = round(((t2[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t2[t,]$Avg_PreMatch_Impact_Bowling = round(((t2[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t2[t,]$Avg_PreMatch_Impact_Fielding = round(((t2[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t3)) {
  t3[t,]$Avg_PreMatch_Impact_Batting = round(((t3[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t3[t,]$Avg_PreMatch_Impact_Bowling = round(((t3[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t3[t,]$Avg_PreMatch_Impact_Fielding = round(((t3[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t4)) {
  t4[t,]$Avg_PreMatch_Impact_Batting = round(((t4[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t4[t,]$Avg_PreMatch_Impact_Bowling = round(((t4[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t4[t,]$Avg_PreMatch_Impact_Fielding = round(((t4[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t5)) {
  t5[t,]$Avg_PreMatch_Impact_Batting = round(((t5[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t5[t,]$Avg_PreMatch_Impact_Bowling = round(((t5[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t5[t,]$Avg_PreMatch_Impact_Fielding = round(((t5[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t6)) {
  t6[t,]$Avg_PreMatch_Impact_Batting = round(((t6[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t6[t,]$Avg_PreMatch_Impact_Bowling = round(((t6[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t6[t,]$Avg_PreMatch_Impact_Fielding = round(((t6[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t7)) {
  t7[t,]$Avg_PreMatch_Impact_Batting = round(((t7[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t7[t,]$Avg_PreMatch_Impact_Bowling = round(((t7[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t7[t,]$Avg_PreMatch_Impact_Fielding = round(((t7[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t8)) {
  t8[t,]$Avg_PreMatch_Impact_Batting = round(((t8[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t8[t,]$Avg_PreMatch_Impact_Bowling = round(((t8[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t8[t,]$Avg_PreMatch_Impact_Fielding = round(((t8[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t9)) {
  t9[t,]$Avg_PreMatch_Impact_Batting = round(((t9[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t9[t,]$Avg_PreMatch_Impact_Bowling = round(((t9[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t9[t,]$Avg_PreMatch_Impact_Fielding = round(((t9[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t10)) {
  t10[t,]$Avg_PreMatch_Impact_Batting = round(((t10[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t10[t,]$Avg_PreMatch_Impact_Bowling = round(((t10[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t10[t,]$Avg_PreMatch_Impact_Fielding = round(((t10[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t11)) {
  t11[t,]$Avg_PreMatch_Impact_Batting = round(((t11[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t11[t,]$Avg_PreMatch_Impact_Bowling = round(((t11[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t11[t,]$Avg_PreMatch_Impact_Fielding = round(((t11[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t12)) {
  t12[t,]$Avg_PreMatch_Impact_Batting = round(((t12[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t12[t,]$Avg_PreMatch_Impact_Bowling = round(((t12[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t12[t,]$Avg_PreMatch_Impact_Fielding = round(((t12[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}

for (t in 1:nrow(t13)) {
  t13[t,]$Avg_PreMatch_Impact_Batting = round(((t13[t,]$PreMatch_Impact_Batting)/(t-1)), digits = 2)
  t13[t,]$Avg_PreMatch_Impact_Bowling = round(((t13[t,]$PreMatch_Impact_Bowling)/(t-1)), digits = 2)
  t13[t,]$Avg_PreMatch_Impact_Fielding = round(((t13[t,]$PreMatch_Impact_Fielding)/(t-1)), digits = 2)
}


t1 <- replace(t1, is.na(t1), 6.5)
t2 <- replace(t2, is.na(t2), 6.5)
t3 <- replace(t3, is.na(t3), 6.5)
t4 <- replace(t4, is.na(t4), 6.5)
t5 <- replace(t5, is.na(t5), 6.5)
t6 <- replace(t6, is.na(t6), 6.5)
t7 <- replace(t7, is.na(t7), 6.5)
t8 <- replace(t8, is.na(t8), 6.5)
t9 <- replace(t9, is.na(t9), 6.5)
t10 <- replace(t10, is.na(t10), 6.5)
t11 <- replace(t11, is.na(t11), 6.5)
t12 <- replace(t12, is.na(t12), 6.5)
t13 <- replace(t13, is.na(t13), 6.5)

t1 = t1[c(-3:-13)]
t2 = t2[c(-3:-13)]
t3 = t3[c(-3:-13)]
t4 = t4[c(-3:-13)]
t5 = t5[c(-3:-13)]
t6 = t6[c(-3:-13)]
t7 = t7[c(-3:-13)]
t8 = t8[c(-3:-13)]
t9 = t9[c(-3:-13)]
t10 = t10[c(-3:-13)]
t11 = t11[c(-3:-13)]
t12 = t12[c(-3:-13)]
t13 = t13[c(-3:-13)]

df = df[c(-16:-18)]

result<-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)
df = df %>%
  left_join(result, by=c("match_id", "Team_Id"))

df$Avg_Impact_Batting = round(df$Avg_Impact_Batting,digits = 2)
df$Avg_Impact_Bowling = round(df$Avg_Impact_Bowling,digits = 2)
df$Avg_Impact_Fielding = round(df$Avg_Impact_Fielding,digits = 2)

df$`Impact Batting` = round(df$`Impact Batting`,digits = 2)
df$`Impact Bowling`= round(df$`Impact Bowling`,digits = 2)
df$`Impact Fielding` = round(df$`Impact Fielding`,digits = 2)


x5_over = dfa3[dfa3[,4]==5,]
x10_over = dfa3[dfa3[,4]==10,]
x15_over = dfa3[dfa3[,4]==15,]

x5_over_2innings = x5_over[x5_over[,2]==2,]
x10_over_2innings = x10_over[x10_over[,2]==2,]
x15_over_2innings = x15_over[x15_over[,2]==2,]


winner = df
winner = winner[c(-3,-4,-7,-12:-23)]

colnames(winner)= c("Match_Id","Team","Team1","Team2","Toss_Winner","match_winner","TossWin","Win")

x5_over_2innings = x5_over_2innings %>%
  left_join(winner, by=c("Match_Id", "Team"))

x10_over_2innings = x10_over_2innings %>%
  left_join(winner, by=c("Match_Id", "Team"))

x15_over_2innings = x15_over_2innings %>%
  left_join(winner, by=c("Match_Id", "Team"))


write.csv(dfa3, "./dataset/clean datasets/Over_by_Over.csv",row.names = FALSE)
write.csv(df, "./dataset/clean datasets/Match_Impact_Teams.csv",row.names = FALSE)
write.csv(match_players, "./dataset/clean datasets/Player_match.csv",row.names = FALSE)

write.csv(x5_over_2innings, "./dataset/clean datasets/x5_over_2innings.csv",row.names = FALSE)
write.csv(x10_over_2innings, "./dataset/clean datasets/x10_over_2innings.csv",row.names = FALSE)
write.csv(x15_over_2innings, "./dataset/clean datasets/x15_over_2innings.csv",row.names = FALSE)

rm(df,dfa,dfa1,dfa2,dfa3,a,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,df1,df2,dfa4,dfa5,result,winner,x5_over_2innings,x10_over_2innings,x15_over_2innings,x5_over,x10_over,x15_over)

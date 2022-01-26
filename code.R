library(plyr)
library(dplyr)
library(ggplot2)
library(teamcolors)

#nfl <- read.csv("/home/msieder/Dokumente/Unilife/VisualDS/nfl09-18full.csv")
#nrow(nfl)
#ncol(nfl)
#names(nfl)
#names(nfl)
#test <- nfl[c(2:40,119:120,133:166)]
#write.csv(test, "/home/msieder/Dokumente/Unilife/VisualDS/nflsmall.csv",row.names = F)
nfl <- read.csv("/home/msieder/Documents/Unilife/VisualDataScience/nfl/nflsmall.csv")
nfl$posteam <- mapvalues(nfl$posteam, 
                         from=c("LA","STL","JAC","SD"), 
                         to=c("LAR","LAR","JAX","LAC"),
                         warn_missing=F)
levels(nfl$posteam)
abbr <- levels(sort(unique(nfl$posteam)))[2:33]
full_names <- c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", 
                "Cleveland Browns", "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers", "Houston Texans", "Indianapolis Colts", 
                "Jacksonville Jaguars","Kansas City Chiefs", "Los Angeles Rams", "Los Angeles Chargers", "Miami Dolphins", "Minnesota Vikings", 
                "New England Patriots", "New Orleans Saints", "New York Giants", "New York Jets", "Oakland Raiders", "Philadelphia Eagles", "Pittsburgh Steelers",
                "Seattle Seahawks", "San Francisco 49ers", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Redskins")

for (i in c(2009:2018)) {
  nfl$season[season(i)]<-i
}

season <- function(n){
  n1 <- (n+0.05)*1000000
  n2 <- (n+1.05)*1000000
  s <- nfl$game_id>n1 &  nfl$game_id<n2
  return(s)
}


nfl$comple
nfl$rush
head(quarterbacks)
passing <- nfl[season(2015),]  %>% 
  filter( !is.na(yards_gained), !is.na(passer_player_name), sack==0 ,qb_dropback == 1) %>% 
  group_by(passer_player_name) %>% 
  summarise(Tot_PassYards = sum(yards_gained),
            Avg_PassYards = round(mean(yards_gained),2),
            Pass_Attempts = sum(pass_attempt),
            Completion_Rate = round(mean(pass_attempt==1&complete_pass==1),2),
            Pass_Touchdowns = sum(pass_touchdown),
            Tot_RushYards = sum(),
            Avg_RushYards = sum(),
            Rush_Attempts = sum(),
            Rush_Touchdowns = sum(rush_touchdown),
            Tot_Yards = sum(),
            Tot_Touchdowns = sum(),
            Team = names(table(posteam))[which.max(table(posteam))]) %>% 
  filter(Pass_Attempts >= 100) %>% 
  arrange(passer_player_name) %>% 
  rename(Player = passer_player_name)


passingrush <- nfl[season(2015),]  %>% 
  filter( !is.na(yards_gained), !is.na(rusher_player_name)) %>% 
  group_by(rusher_player_name) %>% 
  summarise(Tot_RushYards = sum(yards_gained),
            Avg_RushYards = round(mean(yards_gained),2),
            Rush_Touchdowns = sum(rush_touchdown),
            Rush_Attempts = sum(rush_attempt),
            Team = names(table(posteam))[which.max(table(posteam))]) %>% 
  arrange(rusher_player_name) %>% 
  filter(rusher_player_name %in% c(as.character(passing$Player))) %>% 
  rename(Player = rusher_player_name)

noquote("Player")

quarterbacks <- passing %>%
  mutate(Tot_RushYards = passingrush$Tot_RushYards,
         Avg_RushYards = passingrush$Avg_RushYards,
         Rush_Attempts = passingrush$Rush_Attempts,
         Rush_Touchdowns = passingrush$Rush_Touchdowns,
         Tot_Yards = Tot_PassYards+Tot_RushYards,
         Tot_Touchdowns = Rush_Touchdowns+Pass_Touchdowns) %>% 
  arrange(desc(!!sym(x)))
x <- "Tot_Yards"
quarterbacks
quarterbacks["fullname"] <- mapvalues(quarterbacks$Team, 
                            from=abbr, 
                            to=full_names,
                            warn_missing = F) 

df <- as.data.frame(quarterbacks[1:7,])
df["fullname"] <- mapvalues(df$Team, 
                            from=abbr, 
                            to=full_names,
                            warn_missing = F) 
df["interest"] <- df[x]


ggplot(df, aes(x=reorder(Player, -interest, sum), y=interest, fill=fullname))+
  geom_bar(stat="identity", color="black")+
  scale_fill_teams(name = "Team")+
  scale_color_teams(name = "Team")+
  xlab("Player")+
  ylab(x)+
  theme_minimal()











unique(nfl[season(2018),"game_id"])

test <- nfl[season(2018),]
rush <- test[test$rush_attempt==1,]
rush <- rush[!is.na(rush$rusher_player_name),]
#rush <- rush[!is.na(rush$epa),]

players <- rush %>% 
  group_by(rusher_player_name) %>% 
  summarize(c = names(table(posteam))[which.max(table(posteam))],
            d = sum(yards_gained))

df <- data.frame(players)
names(df) <- c("Name","Team","Yards")

df["fullname"] <- mapvalues(df$Team, 
                            from=abbr, 
                            to=full_names,
                            warn_missing=F)
df <- df[order(-df$Yards),]
df[1:10,]

ggplot(df[1:5,], aes(x=reorder(Name, -df$Yards[1:5], sum), y=Yards, fill=fullname))+
  geom_bar(stat="identity", color="black")+
  scale_fill_teams(name = "Team")+
  scale_color_teams(name = "Team")+
  xlab("Player")+
  theme_minimal()

test <- nfl[season(2012),]
rush <- test[test$rush_attempt==1,c("yards_gained","posteam")]
rush <- rush[!is.na(rush$yards_gained),]
pass <- test[test$pass_attempt==1,c("yards_gained","posteam")]
pass <- pass[!is.na(pass$yards_gained),]

rushyds <- rush %>% 
  group_by(posteam) %>% 
  summarize(d = sum(yards_gained))
passyds <- pass %>% 
  group_by(posteam) %>% 
  summarize(d = sum(yards_gained))
df <- data.frame(rushyds,passyds$d)
names(df)<-c("Team","RushYards","PassYards")
df["fullname"] <- mapvalues(df$Team, 
                            from=abbr, 
                            to=full_names,
                            warn_missing=F)

ggplot(df, aes(x=PassYards, y=RushYards, abbr=Team, fill=fullname)) +
  geom_point(size=2, shape=23)+
  geom_text(label=abbr,hjust=1, vjust=-0.7,size=2.3)+
  scale_fill_teams(name = "Team")+
  scale_color_teams(name = "Team")+
  theme_minimal()+
  theme(legend.position = "none")



team <- c("ARI","PIT","NO")
rush <- nfl[nfl$posteam %in% team & nfl$rush_attempt==1,c("yards_gained","posteam","season")]
rush <- rush[!is.na(rush$yards_gained),]
pass <- nfl[nfl$posteam %in%team & nfl$pass_attempt==1,c("yards_gained","posteam","season")]
pass <- pass[!is.na(pass$yards_gained),]

rushyds <- rush %>% 
  group_by(season,posteam) %>% 
  summarize(d = sum(yards_gained))
passyds <- pass %>% 
  group_by(season,posteam) %>% 
  summarize(d = sum(yards_gained))
df <- data.frame(rushyds)
names(df)<-c("Season","posteam","TotalYards")
df$teamname <- mapvalues(team, 
                      from=abbr, 
                      to=full_names,
                      warn_missing=F)
df



ggplot(df, aes(Season,TotalYards,color=teamname,group=teamname)) +
  geom_line() +
  ggtitle(df$teamname)+
  xlab('Yards') +
  ylab('Season') +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 8000))+
  scale_color_manual(values=as.character(team_pal()[unique(df$teamname)]))



as.character(team_pal()["Arizona Cardinals"])
ggplot(df, aes(x=Season, y=Yards, group=Type, color=Type)) +
  geom_line() +
  ggtitle(df$teamname)+
  xlab('Yards') +
  ylab('Season') +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 7000))+
  scale_color_manual(values=c("red","blue", as.character(team_pal()[df$teamname])))



as.character(team_pal()[unique(df$teamname)])
df$teamname             

nfl$qb_scramble

test <- nfl[season(2013),]
rush <- test[test$pass_attempt==1,]
rush <- rush[!is.na(rush$passer_player_name),]
rush <- rush[rush$sack!=1,]
rush$yards_gained[is.na(rush$yards_gained)]<-0

players <- rush %>% 
  group_by(passer_player_name) %>% 
  summarize(c = names(table(posteam))[which.max(table(posteam))],
            d = sum(yards_gained))

df <- data.frame(players)
names(df) <- c("Name","Team","Yards")

df["fullname"] <- mapvalues(df$Team, 
                            from=abbr, 
                            to=full_names,
                            warn_missing=F)
df <- df[order(-df$Yards),]
df
df[1:5,]
ggplot(df[1:5,], aes(x=reorder(Name, -df$Yards[1:5], sum), y=Yards, fill=fullname))+
  geom_bar(stat="identity", color="black")+
  scale_fill_teams(name = "Team")+
  scale_color_teams(name = "Team")+
  xlab("Player")+
  theme_minimal()

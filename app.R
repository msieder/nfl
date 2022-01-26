#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries
{
    library(shiny)
    library(plyr)
    library(dplyr)
    library(ggplot2)
    library(teamcolors)
    library(viridis)
}
#Preliminaries:
{
#nfl <- read.csv("/home/msieder/Dokumente/Unilife/VisualDS/nflsmall.csv")
nfl$posteam <- mapvalues(nfl$posteam, 
                         from=c("LA","STL","JAC","SD"), 
                         to=c("LAR","LAR","JAX","LAC"),
                         warn_missing=F)
unique(nfl$posteam)
abbr <- levels(sort(unique(nfl$posteam)))[2:33]
full_names <- c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", 
                "Cleveland Browns", "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers", "Houston Texans", "Indianapolis Colts", 
                "Jacksonville Jaguars","Kansas City Chiefs", "Los Angeles Rams", "Los Angeles Chargers", "Miami Dolphins", "Minnesota Vikings", 
                "New England Patriots", "New Orleans Saints", "New York Giants", "New York Jets", "Oakland Raiders", "Philadelphia Eagles", "Pittsburgh Steelers",
                "Seattle Seahawks", "San Francisco 49ers", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Redskins")


season <- function(n){
    n1 <- (n+0.05)*1000000
    n2 <- (n+1.05)*1000000
    s <- nfl$game_id>n1 &  nfl$game_id<n2
    return(s)

}

for (i in c(2009:2018)) {
    nfl$season[season(i)]<-i
}

nfl$season<-as.factor(nfl$season)
levels(nfl$season)
}


# Define UI for application that draws a histogram
ui <- navbarPage("NFL Data",
               tabPanel("Players", value = "players",
                        {sidebarLayout(
                            sidebarPanel(
                                radioButtons("type",
                                             "What to aggregate:",
                                             c("Receiving Yards" = "receiver_player_name",
                                               "Rushing Yards" = "rusher_player_name",
                                               "Passing Yards" = "passer_player_name")),
                                sliderInput("season1",
                                            "Aggregate data for season:",
                                            min = 2009,
                                            max = 2018,
                                            value = 2018),
                                sliderInput("top",
                                            "Number of Players displayed:",
                                            min = 3,
                                            max = 10,
                                            value = 5),
                                checkboxInput("nfl1", "NFL-Colours", TRUE)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Plot",plotOutput("playerplot")),
                                    tabPanel("Data",tableOutput("playertable")))
                            )
                        )}),
               tabPanel("Teams", value = "teams",
                         {sidebarLayout(
                             sidebarPanel(
                                 sliderInput("season2",
                                             "Aggregate data for season:",
                                             min = 2009,
                                             max = 2018,
                                             value = 2018),
                                 selectInput("team","Or select a team for detailed information:",
                                             {c("All Teams" = "ALL",
                                                "Arizona Cardinals" = "ARI",
                                                "Atlanta Falcons" = "ATL", 
                                                "Baltimore Ravens" = "BAL", 
                                                "Buffalo Bills" = "BUF", 
                                                "Carolina Panthers" = "CAR", 
                                                "Chicago Bears" = "CHI", 
                                                "Cincinnati Bengals" = "CIN", 
                                                "Cleveland Browns" = "CLE", 
                                                "Dallas Cowboys" = "DAL", 
                                                "Denver Broncos" = "DEN", 
                                                "Detroit Lions" = "DET", 
                                                "Green Bay Packers" = "GB", 
                                                "Houston Texans" = "HOU", 
                                                "Indianapolis Colts" = "IND", 
                                                "Jacksonville Jaguars" = "JAX",
                                                "Kansas City Chiefs" = "KC", 
                                                "Los Angeles Rams" = "LAR", 
                                                "Los Angeles Chargers" = "LAC", 
                                                "Miami Dolphins" = "MIA", 
                                                "Minnesota Vikings" = "MIN", 
                                                "New England Patriots" = "NE", 
                                                "New Orleans Saints" = "NO", 
                                                "New York Giants" = "NYG", 
                                                "New York Jets" = "NYJ", 
                                                "Oakland Raiders" = "OAK", 
                                                "Philadelphia Eagles" = "PHI", 
                                                "Pittsburgh Steelers" = "PIT",
                                                "Seattle Seahawks" = "SEA", 
                                                "San Francisco 49ers" = "SF", 
                                                "Tampa Bay Buccaneers" = "TB", 
                                                "Tennessee Titans" = "TEN", 
                                                "Washington Redskins" = "WAS")})
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                                 tabsetPanel(
                                     tabPanel("Plot",plotOutput("team")),
                                     tabPanel("Data",tableOutput("teamtable")))
                             )
                         )}),
               navbarMenu("In-Depth",
                          tabPanel("Quarterbacks",value = "quarterback",
                                   {
                                       sidebarLayout(
                                           sidebarPanel(
                                               selectInput("qb_stat","Sort by:",
                                                           {c("Passing Yards (Total)" = "Tot_PassYards",
                                                              "Passing Yards (Avg)" = "Avg_PassYards", 
                                                              "Pass Attempts" = "Pass_Attempts", 
                                                              "Completion Rate" = "Completion_Rate", 
                                                              "Pass Touchdowns" = "Pass_Touchdowns", 
                                                              "Rush Yards (Total)" = "Tot_RushYards", 
                                                              "Rush Yards (Avg)" = "Avg_RushYards", 
                                                              "Rush Attempts" = "Rush_Attempts", 
                                                              "Rush Touchdowns" = "Rush_Touchdowns", 
                                                              "Total Yards" = "Tot_Yards", 
                                                              "Total Touchdowns" = "Tot_Touchdowns")}),
                                               sliderInput("qb_season",
                                                           "Aggregate data for season:",
                                                           min = 2009,
                                                           max = 2018,
                                                           value = 2018),
                                               sliderInput("qb_top",
                                                           "Number of Players displayed:",
                                                           min = 3,
                                                           max = 10,
                                                           value = 5),
                                               checkboxInput("nfl2", "NFL-Colours", TRUE)
                                               
                                               ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                               tabsetPanel(
                                                   tabPanel("Plot",plotOutput("quarterplot")),
                                                   tabPanel("Data",tableOutput("quartertable")))
                                           )
                                       )
                                   }),
                          tabPanel("Running Backs",value = "runningback",
                                   {
                                       sidebarLayout(
                                           sidebarPanel(
                                               selectInput("rush_stat","Sort by:",
                                                           {c("Rushing Yards (Total)" = "Tot_RushYards",
                                                              "Rushing Yards (Avg)" = "Avg_RushYards", 
                                                              "Attempts" = "Rush_Attempts", 
                                                              "Touchdowns" = "Rush_Touchdowns", 
                                                              "Fumbles" = "Fumbles")}),
                                               sliderInput("rush_season",
                                                           "Aggregate data for season:",
                                                           min = 2009,
                                                           max = 2018,
                                                           value = 2018),
                                               sliderInput("rush_top",
                                                           "Number of Players displayed:",
                                                           min = 3,
                                                           max = 10,
                                                           value = 5),
                                               checkboxInput("nflrush", "NFL-Colours", TRUE)
                                               
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                               tabsetPanel(
                                                   tabPanel("Plot",plotOutput("runningplot")),
                                                   tabPanel("Data",tableOutput("runningtable")))
                                           )
                                       )
                                   }),
                          tabPanel("Receivers",value = "receiver",
                                   {
                                       sidebarLayout(
                                           sidebarPanel(
                                               selectInput("rec_stat","Sort by:",
                                                           {c("Receiving Yards (Total)" = "Tot_RecYards",
                                                              "Receiving Yards (Avg)" = "Avg_RecYards", 
                                                              "Targets" = "Targets", 
                                                              "Completion Rate" = "Completion_Rate", 
                                                              "Touchdowns" = "Touchdowns")}),
                                               sliderInput("rec_season",
                                                           "Aggregate data for season:",
                                                           min = 2009,
                                                           max = 2018,
                                                           value = 2018),
                                               sliderInput("rec_top",
                                                           "Number of Players displayed:",
                                                           min = 3,
                                                           max = 10,
                                                           value = 5),
                                               checkboxInput("nflrec", "NFL-Colours", TRUE)
                                               
                                           ),
                                           
                                           # Show a plot of the generated distribution
                                           mainPanel(
                                               tabsetPanel(
                                                   tabPanel("Plot",plotOutput("receiverplot")),
                                                   tabPanel("Data",tableOutput("receivertable")))
                                           )
                                       )
                                   })
                          ),
               navbarMenu("Hypotheses",
                          tabPanel("Hypothesis 1", value = "hypo1",
                                   {plotOutput("hypo1plot")}),
                          tabPanel("Hypothesis 2", value = "hypo2",
                                   {plotOutput("hypo2plot")}),
                          tabPanel("Hypothesis 3", value = "hypo3",
                                   {sidebarLayout(
                                       sidebarPanel(
                                           radioButtons("cortype",
                                                        "What to show:",
                                                        c("Quarterback vs TD" = "quarterback",
                                                          "Team vs TD" = "team",
                                                          "Boxplot Comparison" = "comp"))
                                       ),
                                       mainPanel(
                                           plotOutput("hypo3plot")
                                       )
                                   )}
                                   ))

               )
    # Application title


    # Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    
    output$playertable <- renderTable({
        df <- nfl[season(input$season1),]
        if(input$type=="rusher_player_name"){df <- df[df$rush_attempt==1,c("yards_gained",input$type,"posteam")]}
        else{df <- df[df$pass_attempt==1,c("yards_gained",input$type,"posteam")]}
        
        
        players <- df[!is.na(df[,input$type]),]
        df <- df[df$sack!=1,]
        df$yards_gained[is.na(df$yards_gained)]<-0
        players <- players %>% 
            group_by(players[,2]) %>% 
            summarize(c = names(table(posteam))[which.max(table(posteam))],
                      d = sum(yards_gained))
        
        
        df <- data.frame(players)
        names(df) <- c("Name","Team","Yards")
        
        df$Teamname <- mapvalues(df$Team, 
                                    from=abbr, 
                                    to=full_names,
                                    warn_missing = F)
        
        df <- df[order(-df$Yards),]
        df
    })

    output$playerplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        df <- nfl[season(input$season1),]
        if(input$type=="rusher_player_name"){df <- df[df$rush_attempt==1,c("yards_gained",input$type,"posteam")]}
        else{df <- df[df$pass_attempt==1,c("yards_gained",input$type,"posteam")]}
        
        
        players <- df[!is.na(df[,input$type]),]
        players <- players %>% 
            group_by(players[,2]) %>% 
            summarize(c = names(table(posteam))[which.max(table(posteam))],
                      d = sum(yards_gained))
        
        
        df <- data.frame(players)
        names(df) <- c("Name","Team","Yards")
        
        df["fullname"] <- mapvalues(df$Team, 
                                    from=abbr, 
                                    to=full_names,
                                    warn_missing = F)
        
        df <- df[order(-df$Yards),]
        
        df <- df[1:input$top,]
        
        if(input$nfl1){
            ggplot(df, aes(x=reorder(Name, -df$Yards, sum), y=Yards, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_teams(name = "Team")+
                scale_color_teams(name = "Team")+
                xlab("Player")+
                theme_minimal()
        }else{
            ggplot(df, aes(x=reorder(Name, -df$Yards, sum), y=Yards, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_viridis_d(name="Team")+
                xlab("Player")+
                theme_minimal()
        }
    })
    
    output$team <- renderPlot({
        if(input$team=="ALL"){
            df <- nfl[season(input$season2),]
            rush <- df[df$rush_attempt==1,c("yards_gained","posteam")]
            rush <- rush[!is.na(rush$yards_gained),]
            pass <- df[df$pass_attempt==1,c("yards_gained","posteam")]
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
        }
        else{
            team <- input$team
            rush <- nfl[nfl$posteam==team & nfl$rush_attempt==1,c("yards_gained","posteam","season")]
            rush <- rush[!is.na(rush$yards_gained),]
            pass <- nfl[nfl$posteam==team & nfl$pass_attempt==1,c("yards_gained","posteam","season")]
            pass <- pass[!is.na(pass$yards_gained),]
            
            rushyds <- rush %>% 
                group_by(season) %>% 
                summarize(d = sum(yards_gained))
            passyds <- pass %>% 
                group_by(season) %>% 
                summarize(d = sum(yards_gained))
            df1 <- data.frame(rushyds)
            df2 <- data.frame(passyds)
            df3 <- df1
            df3$d <- df3$d+df2$d
            df <- df1 %>%  mutate(Type = 'RushYards') %>%
                bind_rows(df2 %>%
                              mutate(Type = 'PassYards'))
            
            names(df)<-c("Season","Yards","Type")
            df$teamname <- mapvalues(team, 
                                     from=abbr, 
                                     to=full_names,
                                     warn_missing=F)
            
            
            ggplot(df, aes(Season,Yards,fill=Type)) +
                geom_col() +
                ggtitle(df$teamname)+
                xlab('Yards') +
                ylab('Season') +
                theme_minimal()+
                scale_y_continuous(limits = c(0, 8000))+
                scale_color_manual(values=c("red","blue", as.character(team_pal()[df$teamname])))
        }
    })
    
    output$teamtable <- renderTable({
        if(input$team=="ALL"){
        df <- nfl[season(input$season2),]
        rush <- df[df$rush_attempt==1,c("yards_gained","posteam","season")]
        rush <- rush[!is.na(rush$yards_gained),]
        pass <- df[df$pass_attempt==1,c("yards_gained","posteam","season")]
        pass <- pass[!is.na(pass$yards_gained),]
        
        rushyds <- rush %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained))
        passyds <- pass %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained))
        
        df <- data.frame(rushyds)
        df$PassYards <- passyds$d
        df$TotalYards <- df$PassYards+df$d
        names(df)<-c("Season","Team","RushYards","PassYards","TotalYards")
        df$Teamname <- mapvalues(df$Team, 
                                 from=abbr, 
                                 to=full_names,
                                 warn_missing=F)
        df
        }
        else{
            team <- input$team
            rush <- nfl[nfl$posteam==team & nfl$rush_attempt==1,c("yards_gained","posteam","season")]
            rush <- rush[!is.na(rush$yards_gained),]
            pass <- nfl[nfl$posteam==team & nfl$pass_attempt==1,c("yards_gained","posteam","season")]
            pass <- pass[!is.na(pass$yards_gained),]
            
            rushyds <- rush %>% 
                group_by(season) %>% 
                summarize(d = sum(yards_gained))
            passyds <- pass %>% 
                group_by(season) %>% 
                summarize(d = sum(yards_gained))
            
            df <- data.frame(rushyds)
            df$PassYards <- passyds$d
            df$TotalYards <- df$d + df$PassYards

            
            names(df)<-c("Season","RushYards","PassYards","TotalYards")
            df$Teamname <- mapvalues(team, 
                                     from=abbr, 
                                     to=full_names,
                                     warn_missing=F)
            
            
            df
            
        }
    })

    output$quarterplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        df <- nfl[season(input$qb_season),]
        
        passing <- df  %>% 
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
        
        
        passingrush <- df  %>% 
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

        
        quarterbacks <- passing %>%
            mutate(Tot_RushYards = passingrush$Tot_RushYards,
                   Avg_RushYards = passingrush$Avg_RushYards,
                   Rush_Attempts = passingrush$Rush_Attempts,
                   Rush_Touchdowns = passingrush$Rush_Touchdowns,
                   Tot_Yards = Tot_PassYards+Tot_RushYards,
                   Tot_Touchdowns = Rush_Touchdowns+Pass_Touchdowns) %>% 
            arrange(desc(!!sym(input$qb_stat)))
        
        quarterbacks["fullname"] <- mapvalues(quarterbacks$Team, 
                                              from=abbr, 
                                              to=full_names,
                                              warn_missing = F) 
        
        df <- as.data.frame(quarterbacks[1:input$qb_top,])
        df["fullname"] <- mapvalues(df$Team, 
                                    from=abbr, 
                                    to=full_names,
                                    warn_missing = F)
        
        df["interest"] <- df[input$qb_stat]
        
        
        if(input$nfl2){
            ggplot(df, aes(x=reorder(Player, -interest, sum), y=interest, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_teams(name = "Team")+
                scale_color_teams(name = "Team")+
                xlab("Player")+
                ylab(input$stat)+
                theme_minimal()
        }else{
            ggplot(df, aes(x=reorder(Player, -interest, sum), y=interest, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_viridis_d(name="Team")+
                xlab("Player")+
                ylab(input$stat)+
                theme_minimal()
        }
        
    })
    
    output$quartertable <- renderTable({
        df <- nfl[season(input$qb_season),]
        
        passing <- df  %>% 
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
        
        
        passingrush <- df  %>% 
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
        
        
        quarterbacks <- passing %>%
            mutate(Tot_RushYards = passingrush$Tot_RushYards,
                   Avg_RushYards = passingrush$Avg_RushYards,
                   Rush_Attempts = passingrush$Rush_Attempts,
                   Rush_Touchdowns = passingrush$Rush_Touchdowns,
                   Tot_Yards = Tot_PassYards+Tot_RushYards,
                   Tot_Touchdowns = Rush_Touchdowns+Pass_Touchdowns) %>% 
            arrange(desc(!!sym(input$qb_stat)))
        
        quarterbacks
    })
    
    output$receiverplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        receiving <- nfl[season(input$rec_season),]  %>% 
            filter( !is.na(yards_gained), !is.na(receiver_player_name)) %>% 
            group_by(receiver_player_name) %>% 
            summarise(Tot_RecYards = sum(yards_gained),
                      Avg_RecYards = round(mean(yards_gained),2),
                      Targets = sum(pass_attempt),
                      Completion_Rate = round(mean(pass_attempt==1&complete_pass==1),2),
                      Touchdowns = sum(pass_touchdown),
                      Team = names(table(posteam))[which.max(table(posteam))]) %>% 
            filter(Targets >= 10) %>% 
            arrange(desc(!!sym(input$rec_stat))) %>% 
            rename(Player = receiver_player_name)
        
        
        df <- as.data.frame(receiving[1:input$rec_top,])
        df["fullname"] <- mapvalues(df$Team, 
                                    from=abbr, 
                                    to=full_names,
                                    warn_missing = F)
        
        df["interest"] <- df[input$rec_stat]
        
        
        if(input$nflrec){
            ggplot(df, aes(x=reorder(Player, -interest, sum), y=interest, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_teams(name = "Team")+
                scale_color_teams(name = "Team")+
                xlab("Player")+
                ylab(input$stat)+
                theme_minimal()
        }else{
            ggplot(df, aes(x=reorder(Player, -interest, sum), y=interest, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_viridis_d(name="Team")+
                xlab("Player")+
                ylab(input$stat)+
                theme_minimal()
        }
        
    })
    
    output$receivertable <- renderTable({
        receiving <- nfl[season(input$rec_season),]  %>% 
            filter( !is.na(yards_gained), !is.na(receiver_player_name)) %>% 
            group_by(receiver_player_name) %>% 
            summarise(Tot_RecYards = sum(yards_gained),
                      Avg_RecYards = round(mean(yards_gained),2),
                      Targets = sum(pass_attempt),
                      Completion_Rate = round(mean(pass_attempt==1&complete_pass==1),2),
                      Touchdowns = sum(pass_touchdown),
                      Team = names(table(posteam))[which.max(table(posteam))]) %>% 
            filter(Targets >= 10) %>% 
            arrange(desc(!!sym(input$rec_stat))) %>% 
            rename(Player = receiver_player_name)
        receiving
    })
    
    output$runningplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        rushers <- nfl[season(input$rush_season),]  %>% 
            filter( !is.na(yards_gained), !is.na(rusher_player_name)) %>% 
            group_by(rusher_player_name) %>% 
            summarise(Tot_RushYards = sum(yards_gained),
                      Avg_RushYards = round(mean(yards_gained),2),
                      Rush_Touchdowns = sum(rush_touchdown),
                      Rush_Attempts = sum(rush_attempt),
                      Fumbles = sum(fumble),
                      Team = names(table(posteam))[which.max(table(posteam))]) %>%
            filter(Rush_Attempts >= 10) %>% 
            arrange(desc(!!sym(input$rush_stat))) %>% 
            rename(Player = rusher_player_name)
        df <- as.data.frame(rushers[1:input$rush_top,])
        
        
        df["fullname"] <- mapvalues(df$Team, 
                                    from=abbr, 
                                    to=full_names,
                                    warn_missing = F)
        
        df["interest"] <- df[input$rush_stat]
        
        
        if(input$nflrush){
            ggplot(df, aes(x=reorder(Player, -interest, sum), y=interest, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_teams(name = "Team")+
                scale_color_teams(name = "Team")+
                xlab("Player")+
                ylab(input$stat)+
                theme_minimal()
        }else{
            ggplot(df, aes(x=reorder(Player, -interest, sum), y=interest, fill=fullname))+
                geom_bar(stat="identity", color="black")+
                scale_fill_viridis_d(name="Team")+
                xlab("Player")+
                ylab(input$stat)+
                theme_minimal()
        }
        
    })
    
    output$runningtable <- renderTable({
        rushers <- nfl[season(input$rush_season),]  %>% 
            filter( !is.na(yards_gained), !is.na(rusher_player_name)) %>% 
            group_by(rusher_player_name) %>% 
            summarise(Tot_RushYards = sum(yards_gained),
                      Avg_RushYards = round(mean(yards_gained),2),
                      Rush_Touchdowns = sum(rush_touchdown),
                      Rush_Attempts = sum(rush_attempt),
                      Fumbles = sum(fumble),
                      Team = names(table(posteam))[which.max(table(posteam))]) %>%
            filter(Rush_Attempts >= 10) %>% 
            arrange(desc(!!sym(input$rush_stat))) %>% 
            rename(Player = rusher_player_name)

        rushers
    })
    
    output$hypo1plot <- renderPlot({
        df <- nfl
        rush <- df[df$rush_attempt==1,c("yards_gained","rush_attempt","posteam","season","touchdown")]
        rush <- rush[!is.na(rush$yards_gained),]
        pass <- df[df$pass_attempt==1,c("yards_gained","pass_attempt","posteam","season","touchdown")]
        pass <- pass[!is.na(pass$yards_gained),]
        
        
        
        rushyds <- rush %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained),
                      e = sum(rush_attempt),
                      f = sum(touchdown))
        passyds <- pass %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained),
                      e = sum(pass_attempt),
                      f = sum(touchdown))
        
        rushyds["avg"] <- rushyds$d/rushyds$e
        passyds["avg"] <- passyds$d/passyds$e
        
        
        
        
        df <- data.frame(rushyds)
        df$PassYards <- passyds$d
        df$AvgPassYards <- passyds$avg
        df$Pass_Touchdowns <- passyds$f
        df$Total_Yards <- df$PassYards+df$d
        df$Total_Touchdowns <- df$f+df$Pass_Touchdowns
        df
        names(df)<-c("Season","Team","RushYards","RushAttempts","RushTouchdowns","AvgRushYards","PassYards","AvgPassYards","PassTouchdowns",
                     "TotalYards","TotalTouchdowns")
        df$Teamname <- mapvalues(df$Team, 
                                 from=abbr, 
                                 to=full_names,
                                 warn_missing=F)
        
        RushYards <- data.frame(group="RushYards",yards=df$RushYards)
        PassYards <- data.frame(group="PassYards",yards=df$PassYards)
        dat <- rbind(RushYards,PassYards)
        
        ggplot(dat, aes(x=group, y=yards, fill=group)) + geom_boxplot()
    })
    
    output$hypo2plot <- renderPlot({
        df <- nfl
        rush <- df[df$rush_attempt==1,c("yards_gained","rush_attempt","posteam","season","touchdown")]
        rush <- rush[!is.na(rush$yards_gained),]
        pass <- df[df$pass_attempt==1,c("yards_gained","pass_attempt","posteam","season","touchdown")]
        pass <- pass[!is.na(pass$yards_gained),]
        
        
        
        rushyds <- rush %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained),
                      e = sum(rush_attempt),
                      f = sum(touchdown))
        passyds <- pass %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained),
                      e = sum(pass_attempt),
                      f = sum(touchdown))
        
        rushyds["avg"] <- rushyds$d/rushyds$e
        passyds["avg"] <- passyds$d/passyds$e
        
        
        
        
        df <- data.frame(rushyds)
        df$PassYards <- passyds$d
        df$AvgPassYards <- passyds$avg
        df$Pass_Touchdowns <- passyds$f
        df$Total_Yards <- df$PassYards+df$d
        df$Total_Touchdowns <- df$f+df$Pass_Touchdowns
        
        names(df)<-c("Season","Team","RushYards","RushAttempts","RushTouchdowns","AvgRushYards","PassYards","AvgPassYards","PassTouchdowns",
                     "TotalYards","TotalTouchdowns")
        df$Teamname <- mapvalues(df$Team, 
                                 from=abbr, 
                                 to=full_names,
                                 warn_missing=F)
        
        RushYards <- data.frame(group="AvgRushYards",yards=df$AvgRushYards)
        PassYards <- data.frame(group="AvgPassYards",yards=df$AvgPassYards)
        dat <- rbind(RushYards,PassYards)
        
        ggplot(dat, aes(x=group, y=yards, fill=group)) + geom_boxplot()
    })
    
    output$hypo3plot <- renderPlot({
        {
        df <- nfl
        rush <- df[df$rush_attempt==1,c("yards_gained","rush_attempt","posteam","season","touchdown")]
        rush <- rush[!is.na(rush$yards_gained),]
        pass <- df[df$pass_attempt==1,c("yards_gained","pass_attempt","posteam","season","touchdown")]
        pass <- pass[!is.na(pass$yards_gained),]
        
        rushyds <- rush %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained),
                      e = sum(rush_attempt),
                      f = sum(touchdown))
        passyds <- pass %>% 
            group_by(season,posteam) %>% 
            summarize(d = sum(yards_gained),
                      e = sum(pass_attempt),
                      f = sum(touchdown))
        
        rushyds["avg"] <- rushyds$d/rushyds$e
        passyds["avg"] <- passyds$d/passyds$e}
        {
        df <- data.frame(rushyds)
        df$PassYards <- passyds$d
        df$AvgPassYards <- passyds$avg
        df$Pass_Touchdowns <- passyds$f
        df$Total_Yards <- df$PassYards+df$d
        df$Total_Touchdowns <- df$f+df$Pass_Touchdowns
        df
        names(df)<-c("Season","Team","RushYards","RushAttempts","RushTouchdowns","AvgRushYards","PassYards","AvgPassYards","PassTouchdowns",
                     "TotalYards","TotalTouchdowns")
        df$Teamname <- mapvalues(df$Team, 
                                 from=abbr, 
                                 to=full_names,
                                 warn_missing=F)
        RushYards <- data.frame(group="RushYards",yards=df$RushYards)
        PassYards <- data.frame(group="PassYards",yards=df$PassYards)}
        Team_Touchdowns <- A2 <- df$PassTouchdowns
        Team_Yards <- B2 <- df$PassYards
        
        for(i in c(2009:2018)){
            passing <- nfl[season(i),]  %>% 
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
                          Interceptions = sum(interception),
                          Team = names(table(posteam))[which.max(table(posteam))]) %>% 
                filter(Pass_Attempts >= 100) %>% 
                arrange(passer_player_name) %>% 
                rename(Player = passer_player_name)
            
            if(i==2009){quarter <- passing}
            else{quarter<-rbind(quarter,passing)}
        }
        
        Quarterback_Yards <- B1 <- quarter$Pass_Touchdowns
        Quarterback_Touchdowns <- A1 <- quarter$Tot_PassYards
        
        {B <- 1000
        cor1 <- cor2 <- rep(0, B)
        for(i in 1:B){
            samp1 <- sample(1:length(A1), length(A1), TRUE)  
            samp2 <- sample(1:length(A2), length(A2), TRUE)
            cor1[i] <- cor(A1[samp1], B1[samp1])
            cor2[i] <- cor(A2[samp2], B2[samp2])
        }}
        
        
        if(input$cortype=="quarterback"){plot(Quarterback_Touchdowns,Quarterback_Yards)}
        if(input$cortype=="team"){plot(Team_Touchdowns,Team_Yards)}
        if(input$cortype=="comp"){boxplot(cor1,cor2)}
        
    })
    
    

}

# Run the application 
if (interactive()){
    shinyApp(ui = ui, server = server)
}
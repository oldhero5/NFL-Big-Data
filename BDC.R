library(tidyverse)
library(MASS)
#Raw Data
file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.players <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
players.sum <- read_csv(file.players) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays)

games_to_download <- readr::read_csv("DATA/NFL/game_id.csv")

tracking_dat <- readr::read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091004.csv")
tracking_dat <- dplyr::select(tracking_dat,nflId,gameId,playId,s,event)
# Loop to download all play tracking data

for(g in 1:length(games_to_download$game_id)) {
    
    path <- paste0("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_",games_to_download$game_id[[g]],".csv")
    
    d <- readr::read_csv(path)
    
    d <- dplyr::select(d,nflId,gameId,playId,s,event)
    
    tracking_dat <- bind_rows(tracking_dat,d)
}   
#

play_data <- readr::read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv")

#

tracking_data <- left_join(play_data, tracking_dat)

tracking_data1 <- dplyr::select(tracking_data, nflId, playId, gameId, event, quarter,down, possessionTeam,s,isSTPlay,PlayResult,PositionAbbr)

game_data <- readr::read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv")
game_data <- dplyr::select(game_data,gameId,HomeScore,VisitorScore,homeTeamAbbr,visitorTeamAbbr,Temperature,Humidity) 

tracking_data <- left_join(
                 game_data,tracking_data
)

# remove special teams plays for only normal plays to be analyzed
speed_data <- readr::read_csv("DATA/NFL/speed_data.csv")
speed_data <- filter(speed_data, isSTPlay == FALSE)
#game sec recorded without special teams 10 obeservations per player per second
avg_game_length <- speed_data%>%group_by(gameId)%>%count(playId)
avg_game_length <- avg_game_length%>%group_by(gameId)%>%mutate(game_length = sum(n)/(220*60))%>%ungroup()
mean(avg_game_length$game_length)
sd(avg_game_length$game_length)
# parse out players who do not have enough plays
speed_data <- speed_data%>%group_by(nflId)%>%filter(nflId >= (mean(avg_game_length$game_length)*(220*60)))%>%ungroup()

# Add metric to calulate player field Average Speed
speed_data <- speed_data %>%group_by(nflId)%>%mutate(player_field_speed = mean(s, na.rm = T))%>%ungroup()
speed_data <- speed_data %>%group_by(nflId)%>%mutate(player_sD = sd(s, na.rm = T))%>%ungroup()
speed_data <- speed_data %>%group_by(nflId)%>%mutate(player_MAX_dat = max(s, na.rm = T))%>%ungroup()
# Player ID Speed
##written to here
Player_Speed <- dplyr::select(speed_data,nflId,player_field_speed,player_sD,player_MAX_dat,PositionAbbr)
Player_Speed <- distinct(Player_Speed)
# Add metric to calculate postional aveage speed field Average Speed
pos_speed <- dplyr::select(speed_data,PositionAbbr,nflId,s)
pos_speed <- pos_speed%>%group_by(PositionAbbr)%>%mutate(Pos_mean = mean(s, na.rm = T))
pos_speed <- pos_speed%>%group_by(PositionAbbr)%>%mutate(Pos_sd = sd(s, na.rm = T))
pos_speed <- pos_speed%>%group_by(PositionAbbr)%>%mutate(Pos_max = max(s, na.rm = T))%>%ungroup()
pos_speed <- distinct(pos_speed)
pos_speed <- dplyr::select(pos_speed, PositionAbbr, Pos_mean, Pos_sd, Pos_max)
pos_speed <- distinct(pos_speed)
#merge data to spot differences from mean position speed
Player_Diff <- left_join(Player_Speed,pos_speed)
#merge to see players names
Player_name_Team <- dplyr::select(players.sum, nflId, FirstName, LastName)
Player_Diff <- inner_join(Player_Diff,Player_name_Team)
Player_Diff <- inner_join(Player_Diff,pos_speed)
#Fastest Offenisive Team vs Average Team Offense
Team_O <- filter(speed_data, PositionAbbr == "QB"
                 |PositionAbbr=="RB" 
                 |PositionAbbr=="FB"
                 |PositionAbbr=="WR"
                 |PositionAbbr=="TE"
                 |PositionAbbr=="C"
                 |PositionAbbr=="G"
                 |PositionAbbr=="T"
)
# Total team Speed
Team_O<- Team_O %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_mean = mean(s, na.rm = T))
Team_O<- Team_O %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_sd = sd(s, na.rm = T))
Team_O<- Team_O %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by game AVGs
Team_O<- Team_O %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_mean = mean(s, na.rm = T))
Team_O<- Team_O %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_sd = sd(s, na.rm = T))
Team_O<- Team_O %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by qtr
Team_O<- Team_O %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_mean = mean(s, na.rm = T))
Team_O<- Team_O %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_sd = sd(s, na.rm = T))
Team_O<- Team_O %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
Team_O <- dplyr::select(Team_O, gameId,possessionTeam, Temperature,Humidity,team_Speed_mean, team_Speed_sd,team_Speed_max,gm_team_Speed_mean, gm_team_Speed_sd,gm_team_Speed_max,qt_team_Speed_mean,qt_team_Speed_sd, qt_team_Speed_max, HomeScore,VisitorScore,homeTeamAbbr,visitorTeamAbbr,quarter)

Team_O <- distinct(Team_O)
Team_O$TeamName <- 0
Team_O$TeamName <- Team_O$possessionTeam
Team_O$possessionTeam <- NULL
#adding win loss column
Team_O <- Team_O%>%mutate(WL = case_when(TeamName == homeTeamAbbr~1,TeamName == visitorTeamAbbr~0))
Team_O <- Team_O%>%mutate(Win = case_when(WL == 0 & VisitorScore < HomeScore~0,WL == 0 & VisitorScore > HomeScore~1,WL == 1 & VisitorScore > HomeScore~0,WL == 1 & VisitorScore < HomeScore~1 ))
#modeling the speed differnce
fit <- lm(Win~Temperature+
              Humidity+
              team_Speed_mean+
              team_Speed_sd+
              team_Speed_max+
              gm_team_Speed_mean+
              gm_team_Speed_sd+
              gm_team_Speed_max+
              qt_team_Speed_mean+
              qt_team_Speed_sd+
              qt_team_Speed_max,
          data=Team_O)
summary(fit)
step <- stepAIC(fit, direction="both")
step$anova # display results
#Fastest Offenisive Team vs Average Team Offense skill players no QB
Team_Os <- filter(speed_data, PositionAbbr=="RB" 
                 |PositionAbbr=="FB"
                 |PositionAbbr=="WR"
                 |PositionAbbr=="TE"
)
Team_Os<- Team_Os %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_mean = mean(s, na.rm = T))
Team_Os<- Team_Os %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_sd = sd(s, na.rm = T))
Team_Os<- Team_Os %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by game AVGs
Team_Os<- Team_Os %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_mean = mean(s, na.rm = T))
Team_Os<- Team_Os %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_sd = sd(s, na.rm = T))
Team_Os<- Team_Os %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by qtr
Team_Os<- Team_Os %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_mean = mean(s, na.rm = T))
Team_Os<- Team_Os %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_sd = sd(s, na.rm = T))
Team_Os<- Team_Os %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
Team_Os <- dplyr::select(Team_Os,
                  gameId,
                  possessionTeam, 
                  Temperature,
                  Humidity,
                  team_Speed_mean, 
                  team_Speed_sd,
                  team_Speed_max,
                  gm_team_Speed_mean, 
                  gm_team_Speed_sd,
                  gm_team_Speed_max,
                  qt_team_Speed_mean,
                  qt_team_Speed_sd,
                  qt_team_Speed_max, 
                  HomeScore,
                  VisitorScore,
                  homeTeamAbbr,
                  visitorTeamAbbr,
                  quarter
                  )

Team_Os <- distinct(Team_Os)
Team_Os$TeamName <- 0
Team_Os$TeamName <- Team_Os$possessionTeam
Team_Os$possessionTeam <- NULL
#Adding win_loss
Team_Os <- Team_Os%>%mutate(WL = case_when(TeamName == homeTeamAbbr~1,TeamName == visitorTeamAbbr~0))
Team_Os <- Team_Os%>%mutate(Win = case_when(WL == 0 & VisitorScore < HomeScore~0,WL == 0 & VisitorScore > HomeScore~1,WL == 1 & VisitorScore > HomeScore~0,WL == 1 & VisitorScore < HomeScore~1 ))
#modeling the speed differnce
fit_skill <- lm(Win~Temperature+
                    Humidity+
                    team_Speed_mean+
                    team_Speed_sd+
                    team_Speed_max+
                    gm_team_Speed_mean+
                    gm_team_Speed_sd+
                    gm_team_Speed_max+
                    qt_team_Speed_mean+
                    qt_team_Speed_sd+
                    qt_team_Speed_max,
                data=Team_Os)
summary(fit_skill)
step_skill <- stepAIC(fit_skill, direction="both")
summary(step_skill)
#Fastest Offenisive Team vs Average Team without TE and QB
Team_Os1 <- filter(speed_data, PositionAbbr=="RB" 
                  |PositionAbbr=="FB"
                  |PositionAbbr=="WR"
)
Team_Os1<- Team_Os1 %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_mean = mean(s, na.rm = T))
Team_Os1<- Team_Os1 %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_sd = sd(s, na.rm = T))
Team_Os1<- Team_Os1 %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by game AVGs
Team_Os1<- Team_Os1 %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_mean = mean(s, na.rm = T))
Team_Os1<- Team_Os1 %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_sd = sd(s, na.rm = T))
Team_Os1<- Team_Os1 %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by qtr
Team_Os1<- Team_Os1 %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_mean = mean(s, na.rm = T))
Team_Os1<- Team_Os1 %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_sd = sd(s, na.rm = T))
Team_Os1<- Team_Os1 %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
Team_Os1 <- dplyr::select(Team_Os1,
                         gameId,
                         possessionTeam, 
                         Temperature,
                         Humidity,
                         team_Speed_mean, 
                         team_Speed_sd,
                         team_Speed_max,
                         gm_team_Speed_mean, 
                         gm_team_Speed_sd,
                         gm_team_Speed_max,
                         qt_team_Speed_mean,
                         qt_team_Speed_sd,
                         qt_team_Speed_max, 
                         HomeScore,
                         VisitorScore,
                         homeTeamAbbr,
                         visitorTeamAbbr,
                         quarter
)

Team_Os1 <- distinct(Team_Os1)
Team_Os1$TeamName <- 0
Team_Os1$TeamName <- Team_Os$possessionTeam
Team_Os1$possessionTeam <- NULL
#Adding win_loss
Team_Os1 <- Team_Os%>%mutate(WL = case_when(TeamName == homeTeamAbbr~1,TeamName == visitorTeamAbbr~0))
Team_Os1 <- Team_Os%>%mutate(Win = case_when(WL == 0 & VisitorScore < HomeScore~0,WL == 0 & VisitorScore > HomeScore~1,WL == 1 & VisitorScore > HomeScore~0,WL == 1 & VisitorScore < HomeScore~1 ))
#modeling the speed differnce
fit_RBWR <- lm(Win~Temperature+
                    Humidity+
                    team_Speed_mean+
                    team_Speed_sd+
                    team_Speed_max+
                    gm_team_Speed_mean+
                    gm_team_Speed_sd+
                    gm_team_Speed_max+
                    qt_team_Speed_mean+
                    qt_team_Speed_sd+
                    qt_team_Speed_max,
                data=Team_Os1)
summary(fit_RBWR)
step_skillRW <- stepAIC(fit_RBWR, direction="forward")
summary(step_skillRW)
#O Lines
Team_OL <- filter(speed_data, PositionAbbr=="C" 
                   |PositionAbbr=="G"
                   |PositionAbbr=="T"
)
Team_OL<- Team_OL %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_mean = mean(s, na.rm = T))
Team_OL<- Team_OL %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_sd = sd(s, na.rm = T))
Team_OL<- Team_OL %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by game AVGs
Team_OL<- Team_OL %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_mean = mean(s, na.rm = T))
Team_OL<- Team_OL %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_sd = sd(s, na.rm = T))
Team_OL<- Team_OL %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
#by qtr
Team_OL<- Team_OL %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_mean = mean(s, na.rm = T))
Team_OL<- Team_OL %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_sd = sd(s, na.rm = T))
Team_OL<- Team_OL %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_max = max(s, na.rm = T))%>%
    ungroup()
Team_OL <- dplyr::select(Team_OL,
                          gameId,
                          possessionTeam, 
                          Temperature,
                          Humidity,
                          team_Speed_mean, 
                          team_Speed_sd,
                          team_Speed_max,
                          gm_team_Speed_mean, 
                          gm_team_Speed_sd,
                          gm_team_Speed_max,
                          qt_team_Speed_mean,
                          qt_team_Speed_sd,
                          qt_team_Speed_max, 
                          HomeScore,
                          VisitorScore,
                          homeTeamAbbr,
                          visitorTeamAbbr,
                          quarter
)

Team_OL <- distinct(Team_OL)
Team_OL$TeamName <- 0
Team_OL$TeamName <- Team_OL$possessionTeam
Team_OL$possessionTeam <- NULL
#Adding win_loss
Team_OL <- Team_Os%>%mutate(WL = case_when(TeamName == homeTeamAbbr~1,TeamName == visitorTeamAbbr~0))
Team_OL <- Team_Os%>%mutate(Win = case_when(WL == 0 & VisitorScore < HomeScore~0,WL == 0 & VisitorScore > HomeScore~1,WL == 1 & VisitorScore > HomeScore~0,WL == 1 & VisitorScore < HomeScore~1 ))
#modeling the speed differnce
fit_OL <- lm(Win~Temperature+
                   Humidity+
                   team_Speed_mean+
                   team_Speed_sd+
                   team_Speed_max+
                   gm_team_Speed_mean+
                   gm_team_Speed_sd+
                   gm_team_Speed_max+
                   qt_team_Speed_mean+
                   qt_team_Speed_sd+
                   qt_team_Speed_max,
               data=Team_OL)
summary(fit_OL)
step_OL <- stepAIC(fit_OL, direction="forward")
summary(step_OL)
fit_OL1 <- lm(Win~gm_team_Speed_mean+
              gm_team_Speed_max,
              data=Team_OL)
summary(fit_OL1)
#Defenses Speed
Team_DEF <- filter(speed_data, PositionAbbr== "DL"
                         |PositionAbbr=="DE"
                         |PositionAbbr=="DT"
                         |PositionAbbr=="NT"
                         |PositionAbbr=="LB"
                         |PositionAbbr=="ILB"
                         |PositionAbbr=="OLB"
                         |PositionAbbr=="MLB"
                         |PositionAbbr=="DB"
                         |PositionAbbr=="CB"
                         |PositionAbbr=="FS"
                         |PositionAbbr=="SS"
                         |PositionAbbr=="S"
)
#overall avgs
Team_DEF<- Team_DEF %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_meand = mean(s, na.rm = T))
Team_DEF<- Team_DEF %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_sdd = sd(s, na.rm = T))
Team_DEF<- Team_DEF %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_maxd = max(s, na.rm = T))%>%
    ungroup()
#by game AVGs
Team_DEF<- Team_DEF %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_meand = mean(s, na.rm = T))
Team_DEF<- Team_DEF %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_sdd = sd(s, na.rm = T))
Team_DEF<- Team_DEF %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_maxd = max(s, na.rm = T))%>%
    ungroup()
#by qtr
Team_DEF<- Team_DEF %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_meand = mean(s, na.rm = T))
Team_DEF<- Team_DEF %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_sdd = sd(s, na.rm = T))
Team_DEF<- Team_DEF %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_maxd = max(s, na.rm = T))%>%
    ungroup()
Team_DEF <- dplyr::select(Team_DEF,
                         gameId,
                         possessionTeam, 
                         Temperature,
                         Humidity,
                         team_Speed_meand, 
                         team_Speed_sdd,
                         team_Speed_maxd,
                         gm_team_Speed_meand, 
                         gm_team_Speed_sdd,
                         gm_team_Speed_maxd,
                         qt_team_Speed_meand,
                         qt_team_Speed_sdd,
                         qt_team_Speed_maxd, 
                         HomeScore,
                         VisitorScore,
                         homeTeamAbbr,
                         visitorTeamAbbr,
                         quarter
)

Team_DEF <- distinct(Team_DEF)
Team_DEF$TeamName <- 0
Team_DEF$TeamName <- Team_DEF$possessionTeam
Team_DEF$possessionTeam <- NULL
#Adding win_loss
Team_DEF <- Team_DEF%>%mutate(WL = case_when(TeamName == homeTeamAbbr~1,TeamName == visitorTeamAbbr~0))
Team_DEF <- Team_DEF%>%mutate(Win = case_when(WL == 0 & VisitorScore < HomeScore~0,WL == 0 & VisitorScore > HomeScore~1,WL == 1 & VisitorScore > HomeScore~0,WL == 1 & VisitorScore < HomeScore~1 ))
#modeling the speed differnce
fit_DEF <- lm(Win~Temperature+
                 Humidity+
                 team_Speed_meand+
                 team_Speed_sdd+
                 team_Speed_maxd+
                 gm_team_Speed_meand+
                 gm_team_Speed_sdd+
                 gm_team_Speed_maxd+
                 qt_team_Speed_meand+
                 qt_team_Speed_sdd+
                 qt_team_Speed_maxd,
             data=Team_DEF)
summary(fit_DEF)
step_DEF <- stepAIC(fit_DEF, direction="forward")
summary(step_DEF)
fit_DEF <- lm(Win~gm_team_Speed_meand+
                  gm_team_Speed_maxd,
              data=Team_DEF)
summary(fit_DEF)
#Defenses Speed backs
Team_NL <- filter(speed_data,PositionAbbr=="LB"
                   |PositionAbbr=="ILB"
                   |PositionAbbr=="OLB"
                   |PositionAbbr=="MLB"
                   |PositionAbbr=="DB"
                   |PositionAbbr=="CB"
                   |PositionAbbr=="FS"
                   |PositionAbbr=="SS"
                   |PositionAbbr=="S"
)
#overall avgs
Team_NL<- Team_NL %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_meannl = mean(s, na.rm = T))
Team_NL<- Team_NL %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_sdnl = sd(s, na.rm = T))
Team_NL<- Team_NL %>%
    group_by(possessionTeam)%>%
    mutate(team_Speed_maxnl = max(s, na.rm = T))%>%
    ungroup()
#by game AVGs
Team_NL<- Team_NL %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_meannl = mean(s, na.rm = T))
Team_NL<- Team_NL %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_sdnl = sd(s, na.rm = T))
Team_NL<- Team_NL %>%
    group_by(gameId,possessionTeam)%>%
    mutate(gm_team_Speed_maxnl = max(s, na.rm = T))%>%
    ungroup()
#by qtr
Team_NL<- Team_NL %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_meannl = mean(s, na.rm = T))
Team_NL<- Team_NL %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_sdnl = sd(s, na.rm = T))
Team_NL<- Team_NL %>%
    group_by(gameId,quarter,possessionTeam)%>%
    mutate(qt_team_Speed_maxnl = max(s, na.rm = T))%>%
    ungroup()
Team_NL <- dplyr::select(Team_NL,
                          gameId,
                          possessionTeam, 
                          Temperature,
                          Humidity,
                          team_Speed_meannl, 
                          team_Speed_sdnl,
                          team_Speed_maxnl,
                          gm_team_Speed_meannl, 
                          gm_team_Speed_sdnl,
                          gm_team_Speed_maxnl,
                          qt_team_Speed_meannl,
                          qt_team_Speed_sdnl,
                          qt_team_Speed_maxnl, 
                          HomeScore,
                          VisitorScore,
                          homeTeamAbbr,
                          visitorTeamAbbr,
                          quarter
)

Team_NL <- distinct(Team_NL)
Team_NL$TeamName <- 0
Team_NL$TeamName <- Team_NL$possessionTeam
Team_DEF$possessionTeam <- NULL
#Adding win_loss
Team_NL <- Team_NL%>%mutate(WL = case_when(TeamName == homeTeamAbbr~1,TeamName == visitorTeamAbbr~0))
Team_NL <- Team_NL%>%mutate(Win = case_when(WL == 0 & VisitorScore < HomeScore~0,WL == 0 & VisitorScore > HomeScore~1,WL == 1 & VisitorScore > HomeScore~0,WL == 1 & VisitorScore < HomeScore~1 ))
#modeling the speed differnce
fit_NL <- lm(Win~Temperature+
                  Humidity+
                  team_Speed_meannl+
                  team_Speed_sdnl+
                  team_Speed_maxnl+
                  gm_team_Speed_meannl+
                  gm_team_Speed_sdnl+
                  gm_team_Speed_maxnl+
                  qt_team_Speed_meannl+
                 qt_team_Speed_sdnl+
                  qt_team_Speed_maxnl,
              data=Team_NL)
summary(fit_NL)
step_NL <- stepAIC(fit_NL, direction="forward")
summary(step_NL)
fit_NL <- lm(Win~gm_team_Speed_meannnl+
                  gm_team_Speed_maxnl,
              data=Team_NL)
summary(fit_NL)
# merge O AND D
diffod <- left_join(Team_O,Team_DEF)
#need to build a way for teams to go against one another
diffod$teamdiffAVG <- Team_O$team_Speed_mean/Team_DEF$team_Speed_meand
diffod$teamdiffMAX <- Team_O$team_Speed_max/Team_DEF$team_Speed_maxd
diffod$gameDiffMean <- Team_O$gm_team_Speed_mean/Team_DEF$gm_team_Speed_meand
diffod$gameDiffMAX  <- Team_O$gm_team_Speed_max/Team_DEF$gm_team_Speed_maxd
diffod$qtrDiffMean <- Team_O$qt_team_Speed_mean/Team_DEF$team_Speed_meand
diffod$qtrDiffMax <- Team_O$gm_team_Speed_max/Team_DEF$gm_team_Speed_maxd

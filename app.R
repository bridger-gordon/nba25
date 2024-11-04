#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(rvest)
library(ggrepel)
library(lubridate)
library(janitor)
library(readr)
library(RCurl)
library(jpeg)
library(bbr)
library(fmsb)
library(conflicted)
library(wesanderson)
library(paletteer)
library(hoopR)
library(hrbrthemes)
library(stringr)
library(plotly)
library(ggbasketball)
library(scales)
library(ggthemes)
library(ggsci)
library(gt)
library(gtExtras)
library(psych)
library(RColorBrewer)
library(sportyR)
library(ggforce)
library(ggridges)
library(zoo)
library(pracma)
library(DT)

currentseason <- hoopR::most_recent_nba_season()

## PULLING IN THE LATEST DATA AND PUTTING IT IN ONE MASTER DATAFRAME
nba_pbp_2025_raw <- hoopR::load_nba_pbp(seasons = currentseason)
nba_box_2025 <- hoopR::load_nba_player_box(seasons = currentseason) %>%
  dplyr::filter(team_name != "All-Stars")
player_tibble <- nba_box_2025 %>%
  select(athlete_id, athlete_display_name) %>%
  unique()
team_tibble <- nba_box_2025 %>%
  select(team_id, team_name) %>%
  unique()
clean_PBP25 <- nba_pbp_2025_raw %>%
  mutate(assistedTrue = str_detect(text, "assists"),
         assister = ifelse(assistedTrue, athlete_id_2, NA),
         scorer = ifelse(assistedTrue, athlete_id_1, NA),
         threeAttemptedOrMade = str_detect(text, "three point"),
         threeMade = ifelse(score_value == 3, TRUE, FALSE),
         threeMissed = ifelse(threeMade == FALSE & threeAttemptedOrMade == TRUE, TRUE, FALSE),
         freeThrow = str_detect(text, "free throw"),
         shooter = ifelse(shooting_play, athlete_id_1, NA)) %>%
  left_join(player_tibble, by=c('assister'='athlete_id')) %>%
  left_join(player_tibble, by=c('scorer'='athlete_id')) %>%
  left_join(player_tibble, by=c('shooter'='athlete_id')) %>%
  mutate(assisterName = athlete_display_name.x, scorerName = athlete_display_name.y, shooterName = athlete_display_name) %>%
  left_join(team_tibble, by=c('team_id'='team_id')) %>%
  dplyr::filter(home_team_mascot != "All-Stars")

## END MAIN

## TODAYS TEAMS
today <- hoopR::nba_todays_scoreboard() %>%
  select(home_team_name, away_team_name, game_status_text)

todaysTeams <- c(today$home_team_name, today$away_team_name)


positionalSplits <- nba_box_2025 %>%
  dplyr::filter(starter == TRUE) %>%
  mutate(threePositions = ifelse(athlete_position_name == "Point Guard" | athlete_position_name == "Shooting Guard" | athlete_position_name == "Guard", "Guard", 
                                 ifelse(athlete_position_name == "Power Forward" | athlete_position_name == "Small Forward" | athlete_position_name == "Forward", "Forward", "Center"))) %>%
  group_by(threePositions, opponent_team_name) %>%
  summarize(pointsAllowedByPos = round( mean(points), 2),
            reboundsAllowedByPos = round( mean(rebounds), 2),
            assistsAllowedByPos = round( mean(assists), 2),
            threesAllowedByPos = round( mean(three_point_field_goals_made), 2)
  ) %>%
  ungroup() %>%
  group_by(threePositions) %>%
  mutate(rankPoints = order(order(pointsAllowedByPos, decreasing = T)),
         rankRebs = order(order(reboundsAllowedByPos, decreasing = T)),
         rankAssists = order(order(assistsAllowedByPos, decreasing = T)),
         rankThrees = order(order(threesAllowedByPos, decreasing = T))) %>%
  ungroup() %>%
  arrange(rankPoints)


top5sToday <- positionalSplits %>%
  dplyr::filter(rankPoints > 6 || rankRebs > 6 || rankAssists > 6 || rankThrees > 6) %>%
  dplyr::filter(opponent_team_name %in% todaysTeams)









earlySeasonGP <- nba_box_2025 %>%
  group_by(team_name) %>%
  summarize(gamesPlayed = n_distinct(game_id)) %>%
  pull(gamesPlayed) %>% min()

MAthreshold10 <- ifelse(earlySeasonGP >= 11, 10, earlySeasonGP - 1)

## TRENDING UP AND DOWN DESCRIPTIVE STATS
trending <- nba_box_2025 %>%
  dplyr::filter(!is.na(minutes)) %>%
  group_by(athlete_display_name) %>%
  arrange(game_date) %>%
  mutate(gamesPlayed = n()) %>%
  dplyr::filter(gamesPlayed >= MAthreshold10 + 1) %>%
  mutate(MAPoints = round( movavg(points, MAthreshold10, type = "s"), 2),
         MARebounds = round( movavg(rebounds, MAthreshold10, type = "s"), 2),
         MAAssists = round( movavg(assists, MAthreshold10, type = "s"), 2),
         MAFGvolume = round( movavg(field_goals_attempted, MAthreshold10, type = "s"), 2),
         MATvolume = round( movavg(three_point_field_goals_attempted, MAthreshold10, type = "s"), 2),
         MATmade = round( movavg(three_point_field_goals_made, MAthreshold10, type = "s"), 2),
         MAminutes = round( movavg(minutes, MAthreshold10, type = "s"), 2))  %>%
  mutate(chgMAPoints = round( MAPoints - dplyr::lag(MAPoints, MAthreshold10),2),
         chgMARebounds = round( MARebounds - dplyr::lag(MARebounds, MAthreshold10), 2),
         chgMAAssists = round( MAAssists - dplyr::lag(MAAssists, MAthreshold10), 2),
         chgMAFGvolume = round( MAFGvolume - dplyr::lag(MAFGvolume, MAthreshold10), 2),
         chgMATvolume = round( MATvolume - dplyr::lag(MATvolume, MAthreshold10), 2),
         chgMATmade = round( MATmade - dplyr::lag(MATmade, MAthreshold10), 2),
         chgMAminutes = round( MATmade - dplyr::lag(MAminutes, MAthreshold10), 2)) %>%
  mutate(player = athlete_display_name) %>%
  slice(tail(row_number(), 1)) %>%
  ungroup() %>%
  arrange(desc(MAPoints)) %>%
  select(player, team_name, MAPoints, chgMAPoints, MARebounds, chgMARebounds, MAAssists, chgMAAssists, 
         MAFGvolume, chgMAFGvolume, MATvolume, chgMATvolume, MATmade, chgMATmade, MAminutes, chgMAminutes)
  

## END OF TRENDING UP AND DOWN DESCRIPTIVE STATS


##### TRACKING DATA
## THREES TYPE
tictoc::tic()
progressr::with_progress({
threeCSSeason <- hoopR::nba_leaguedashptstats(league_id = '00', pt_measure_type = "CatchShoot", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "Season")
Sys.sleep(1)
threeCSL5 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 5, pt_measure_type = "CatchShoot", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L5")
Sys.sleep(1)
threeCSL1 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 1, pt_measure_type = "CatchShoot", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L1")
Sys.sleep(1)
PullUpSeason <- hoopR::nba_leaguedashptstats(league_id = '00', pt_measure_type = "PullUpShot", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "Season")
Sys.sleep(1)
PullUpL5 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 5, pt_measure_type = "PullUpShot", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L5")
Sys.sleep(1)
PullUpL1 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 1, pt_measure_type = "PullUpShot", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L1")

Season <- left_join(threeCSSeason, PullUpSeason, by = "PLAYER_NAME")
L5 <- left_join(threeCSL5, PullUpL5, by = "PLAYER_NAME")
L1 <- left_join(threeCSL1, PullUpL1, by = "PLAYER_NAME") 

threestype <- rbind(Season, L5, L1)

## Passing
passingSeason <- hoopR::nba_leaguedashptstats(league_id = '00', pt_measure_type = "Passing", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "Season")
Sys.sleep(1)
passingL5 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 5, pt_measure_type = "Passing", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L5")
Sys.sleep(1)
passingL1 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 1, pt_measure_type = "Passing", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L1")

passingtracking <- rbind(passingSeason, passingL5, passingL1)

## Rebounding
reboundingSeason <- hoopR::nba_leaguedashptstats(league_id = '00', pt_measure_type = "Rebounding", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "Season")
Sys.sleep(1)
reboundingL5 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 5, pt_measure_type = "Rebounding", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L5")
Sys.sleep(1)
reboundingL1 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 1, pt_measure_type = "Rebounding", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L1")

reboundtracking <- rbind(reboundingSeason, reboundingL5, reboundingL1)

## Scoring
scoringSeason <- hoopR::nba_leaguedashptstats(league_id = '00', pt_measure_type = "Efficiency", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "Season")
Sys.sleep(1)
scoringL5 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 5, pt_measure_type = "Efficiency", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L5")
Sys.sleep(1)
scoringL1 <- hoopR::nba_leaguedashptstats(league_id = '00', last_n_games = 1, pt_measure_type = "Efficiency", season = year_to_season(hoopR::most_recent_nba_season() - 1))$LeagueDashPtStats %>%
  mutate(range = "L1")

scoringtracking <- rbind(scoringSeason, scoringL5, scoringL1)


## Playtypes
Sys.sleep(1)
iso <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "Isolation", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 1))$SynergyPlayType
Sys.sleep(1)
trans <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "Transition", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
spotup <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "Spotup", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
offreb <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "OffRebound", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
prrollman <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "PRRollman", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
prballman <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "PRBallHandler", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
postup <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "Postup", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
offscre <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "OffScreen", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
misc <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "Misc", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
cut <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "Cut", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType
Sys.sleep(1)
ho <- hoopR::nba_synergyplaytypes(league_id = "00", per_mode = "PerGame", play_type = "Handoff", player_or_team = "P", season = year_to_season(hoopR::most_recent_nba_season() - 2))$SynergyPlayType

playtypes <- rbind(iso, trans, spotup, offreb, prrollman, prballman, postup, offscre, misc, cut, ho) %>%
  select(PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION, TEAM_NAME, PLAY_TYPE, PERCENTILE, POSS_PCT, PPP, POSS, PTS, FGM, FGMX, FGA) %>%
  mutate_at(c('PERCENTILE', 'POSS_PCT', 'PPP', 'POSS', 'PTS', 'FGM', 'FGMX', 'FGA'), as.numeric) %>%
  group_by(PLAYER_NAME) %>%
  mutate(TotalPlaytypePts = sum(PTS),
         TotalPlaytypePoss = sum(POSS),
         TotalPlaytypeFGA = sum(FGA)) %>%
  ungroup()

})
tictoc::toc()

###


## AESTHETIC SETTINGS
colorPalatte <- c("#9e0242", "#d53e4f", "#f56d44", "#fdae61", "#ffe08b", "#fffebf", "#e6f599", "#abdda5", "#66c2a5", "#3189bd", "#5e4fa2", "#EF798A", "#4A051C", "#D34E24", "#005E7C", "#E8C547",  "#01200F", "#ED474A", "#D3F9B5", "#FC2F00", "#016FB9", "#6D8EA0", "#F7A9A8", "red", "orange", "blue", "pink", "#7E2E84")
smallercolorPalatte <- c("#9e0242", "#66c2a5", "#3189bd", "#5e4fa2")
# General function
colfunc <- colorRampPalette(c("#66c2a5", "#e67582"))

# drawing the court in diagrams
crcl <- function(x0, y0, r, span=r, nsteps=100) {
  x <- seq(x0-span,x0+span,length.out=nsteps)
  ylo <- y0-sqrt(r^2-(x-x0)^2)
  yup <- y0+sqrt(r^2-(x-x0)^2)
  data.frame(x=x, ylo=ylo, yup=yup)
}

crcl3pt <- crcl(x0=0, y0=-41.75, r=23.75, span=22)
ln3pt <- data.frame(x=c(-22,-22,crcl3pt$x,22,22),
                    ylo=c(-47,-47+169/12,crcl3pt$ylo,-47+169/12,-47),
                    yup=c(-47,-47+169/12,crcl3pt$yup,-47+169/12,-47))

threePointLine <- ln3pt %>%
  mutate(yup2 = yup + 43,
         x2 = x + 25)
halfcourt <- data.frame(x=c(0,0,50,50,0),y=c(-4,47-4,47-4,-4,-4))
baseline <- data.frame(x=c(0,50),y=c(-4,-4))

headerCallbackRemoveHeaderFooter <- c(
  "function(thead, data, start, end, display){",
  "  $('th', thead).css('display', 'none');",
  "}"
)
# end
## AES END



## SOME INITIAL FILTERING FOR DIAGRAMS
assistsGamelog <- clean_PBP25 %>%
  dplyr::filter(assistedTrue == TRUE)

bPerGameByScore <- assistsGamelog %>%
  group_by(assisterName,scorerName,score_value) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = score_value, values_from = count, values_fill = 0) %>%
  clean_names() %>%
  mutate(assistedSum = x2 + x3)
## FILTER


## TABLE OF TEAM STATS -- FOR
teamstats <- nba_box_2025 %>%
  mutate(opponent_win = ifelse(team_winner == TRUE, FALSE, TRUE)) %>%
  dplyr::filter(!is.na(minutes)) %>%
  group_by(team_name, opponent_team_name, game_id) %>%
  summarize(PTS = sum(points),
            AST = sum(assists),
            REB = sum(rebounds),
            OREB = sum(offensive_rebounds),
            DREB = sum(defensive_rebounds),
            BLK = sum(blocks),
            STL = sum(steals),
            TOV = sum(turnovers),
            FGM = sum(field_goals_made),
            FGA = sum(field_goals_attempted),
            FG3M = sum(three_point_field_goals_made),
            FG3A = sum(three_point_field_goals_attempted),
            aWin = unique(team_winner),
            aLoss = unique(opponent_win)
  ) %>%
  ungroup()

teamstatsDatatable <- teamstats %>%
  group_by(team_name) %>%
  summarize(GP = n(),
            W = sum(aWin),
            L = sum(aLoss),
            PPG = round( mean(PTS) , 1),
            APG = round( mean(AST) , 1),
            RPG = round( mean(REB) , 1),
            ORPG = round( mean(OREB) , 1),
            DRPG = round( mean(DREB) , 1),
            BPG = round( mean(BLK) , 1),
            SPG = round( mean(STL) , 1),
            TOPG = round( mean(TOV) , 1),
            FGMG = round( mean(FGM) , 1),
            FGAG = round( mean(FGA) , 1),
            ThreePG = round( mean(FG3M) , 1),
            ThreeAPG = round( mean(FG3A) , 1)) %>%
  arrange(desc(APG)) %>%
  mutate(AST_RANK = row_number()) %>%
  arrange(desc(ThreePG)) %>%
  mutate(FG3M_RANK = row_number()) %>%
  select(team_name, GP, W, L, PPG, APG, RPG, ORPG, DRPG, BPG, SPG, TOPG, FGMG, FGAG, ThreePG, ThreeAPG, AST_RANK, FG3M_RANK) %>%
  arrange(desc(W))

teamstatsDatatable2 <- teamstatsDatatable %>%
  select(team_name, GP, W, L, PPG, APG, RPG, ORPG, DRPG, BPG, SPG, TOPG, FGMG, FGAG, ThreePG, ThreeAPG)

## END



## TABLE OF TEAM STATS -- AGAINST
teamstatsAgainstDatatable <- nba_box_2025 %>%
  mutate(opponent_win = ifelse(team_winner == TRUE, FALSE, TRUE)) %>%
  dplyr::filter(!is.na(minutes)) %>%
  group_by(team_name, opponent_team_name, game_id) %>%
  summarize(aPTS = sum(points),
            aASS = sum(assists),
            aREB = sum(rebounds),
            aOREB = sum(offensive_rebounds),
            aDREB = sum(defensive_rebounds),
            aBLK = sum(blocks),
            aSTL = sum(steals),
            aTO = sum(turnovers),
            aFGM = sum(field_goals_made),
            aFGA = sum(field_goals_attempted),
            aThM = sum(three_point_field_goals_made),
            aThA = sum(three_point_field_goals_attempted),
            aWin = unique(team_winner),
            aLoss = unique(opponent_win)
  ) %>%
  ungroup() %>%
  group_by(opponent_team_name) %>%
  summarize(aGP = n(),
            aW = sum(aWin),
            aL = sum(aLoss),
            aPPG = round( mean(aPTS) , 1),
            aAPG = round( mean(aASS) , 1),
            aRPG = round( mean(aREB) , 1),
            aORPG = round( mean(aOREB) , 1),
            aDRPG = round( mean(aDREB) , 1),
            aBPG = round( mean(aBLK) , 1),
            aSPG = round( mean(aSTL) , 1),
            aTOPG = round( mean(aTO) , 1),
            aFGMG = round( mean(aFGM) , 1),
            aFGAG = round( mean(aFGA) , 1),
            aThreePG = round( mean(aThM) , 1),
            aThreeAPG = round( mean(aThA) , 1)) %>%
  arrange(desc(aL))
## END



##### DEFINING THE UI THAT ACTUALLY DRAWS THE SHINY WEBPAGE
ui <- fluidPage(
  

  navbarPage(tags$b("NBA Play-by-Play Data"),
            
             tabPanel("Points",
                      titlePanel("Points for a Player, 2024 season data"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("playersP", "Choose a player:",
                                      choices = " "
                          ),
                          h3("##"),
                          h3("avg. points (season)"),
                          textOutput("pointsMean"),
                          h3("avg. points (last five)"),
                          textOutput("pointsL5"),
                          h3("50% of games are between"),
                          textOutput("pointsPercentiles"),
                          tags$style(
                            "#pointsMean {font-size: 25px;}
                             #pointsL5 {font-size: 25px;}
                             #pointsPercentiles {font-size: 25px;}"
                          ),
                          br(),
                          plotOutput("pointsPlayerShotChart", width="100%", height = "25vh"),
                          br(),
                          br(),
                          width = 3
                        ),
                        mainPanel(
                          plotlyOutput("pointsLineplot", height="50em"),
                          br(),
                          plotlyOutput("pointsHistplot", height="40em"),
                          br(),
                          plotlyOutput("fgLineplot", height="40em"),
                          br(),
                          plotlyOutput("twoOrThreeLineplot", height="40em"),
                          
                          # plotlyOutput("assistBarplot", height="40em"),
                          br(),
                          br(),
                          br(),
                          width = 9
                        ) )
                      
             ),
             
             tabPanel("Perception View",
                      titlePanel("Perception View"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("playersHome", "Choose a player:",
                                      choices = " "
                          ),
                          h3("##"),
                          br(),
                          htmlOutput("picture"),
                          br(),
                          width = 3
                        ),
                        mainPanel(
                          h3("who is scoring on their team"),
                          plotOutput("whoScoresTeam", height="40em"),
                          br(),
                          h3("what plays are they involved in"),
                          plotOutput("playtypesPlayer", height="40em"),
                          br(),
                          h3("team interactions in playtypes"),
                          plotOutput("playtypesTeam", height="40em"),
                          br(),
                          h3("how do they get their buckets generally"),
                          tableOutput("scoringTrackingTable"),
                          h3("loose heatmap"),
                          plotOutput("heatShotChart", height = "40em", width = "60em"),
                          br(),
                          width = 9
                        ) )
                      
             ),
             
             tabPanel("Rebounds",
                      titlePanel("Rebounds by player, 2024 season data"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("playersR", "Choose a player:",
                                      choices = " "
                          ),
                          h3("##"),
                          h3("avg. rebounds (season)"),
                          textOutput("reboundsMean"),
                          h3("avg. rebounds (last five)"),
                          textOutput("reboundsL5"),
                          h3("50% of games are between"),
                          textOutput("reboundsPercentiles"),
                          tags$style(
                            "#reboundsMean {font-size: 25px;}
                             #reboundsL5 {font-size: 25px;}
                             #reboundsPercentiles {font-size: 25px;}"
                          ),
                          br(),
                          br(),
                          br(),
                          br(),
                          width = 3
                        ),
                        mainPanel(
                          plotlyOutput("reboundsLineplot", height="40em"),
                          br(),
                          plotlyOutput("reboundsHistplot", height="40em"),
                          br(), br(),
                          # plotlyOutput("assistBarplot", height="40em"),
                          br(),
                          br(),
                          br(),
                          width = 9
                        ) ),
                      
                      h3("Tracking data of rebounds and potential rebounds"),
                      tableOutput("reboundTrackingTable"),
                      br(), br(), br()
             ),
             
             tabPanel("Rebounds (Team)",
                      titlePanel("Team rebound stats, 2024 season data"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("teamsR", "Choose a team:",
                                      choices = " "
                          ),
                          h3("##"),
                          p("must min averaging 10mins/game to appear in chart"),
                          width = 3
                        ),
                        mainPanel(
                          plotOutput("teamReboundPlot", height = "75em"),
                          br(),
                          br(),
                          width = 9
                        ) )
             ),
             
             tabPanel("Assists",
                      titlePanel("Assists by player, 2024 season data"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("playersA", "Choose a player:",
                                      choices = " "
                          ),
                          h3("##"),
                          h3("avg. assists (season)"),
                          textOutput("assistMean"),
                          h3("avg. assists (last five)"),
                          textOutput("assistL5"),
                          h3("50% of games are between"),
                          textOutput("assistPercentiles"),
                          tags$style(
                            "#assistMean {font-size: 25px;}
                             #assistL5 {font-size: 25px;}
                             #assistPercentiles {font-size: 25px;}"
                          ),
                          br(),
                          br(),
                          br(),
                          br(),
                          width = 3
                        ),
                      mainPanel(
                                plotlyOutput("assistLineplot", height="40em"),
                                br(),
                                plotlyOutput("assistHistplot", height="40em"),
                                br(),
                                plotlyOutput("assistBarplot", height="40em"),
                                br(), br(),
                                h3("Tracking data of passes and potential assists"),
                                tableOutput("assistTrackingTable"),
                                br(),
                                br(),
                          width = 9
                      ) )
             ),
             
             tabPanel("Assists (Team)",
                      titlePanel("Team assist stats, 2024 season data"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("teamsA", "Choose a team:",
                                      choices = " "
                          ),
                          h3("##"),
                          h3("this team ranks # in the league in assists"),
                          textOutput(paste("assistsTeamRank")),
                          h3("with this many assists per game"),
                          textOutput(paste("assistsTeam")),
                          p("must min averaging 10mins/game to appear in chart"),
                          tags$style(
                            "#assistsTeamRank {font-size: 25px;}
                             #assistsTeam {font-size: 25px;}"
                          ),
                          width = 3
                        ),
                        mainPanel(
                          plotOutput("teamAssistPlot", height = "75em"),
                          br(),br(),br(),
                          h3("who assists who, across the team"),
                          h4("number is avg. assists from x to y in a game together"),
                          plotOutput("teamAssistCrossPlot", height = "75em"),
                          br(),br(),br(),
                          h3("top assist combos for this team"),
                          DTOutput("tblAssistCombos"),
                          #plotlyOutput("assistHistplot", height="40em"),
                          br(),
                          #plotlyOutput("assistBarplot", height="40em"),
                          br(),
                          br(),
                          br(),
                          width = 9
                        ) )
             ),
             
             
             tabPanel("Threes",
                      titlePanel("threes by a player"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("players3", "Choose a player:",
                                      choices = " "
                          ),
                          br(),
                          h3("avg. threes made (season)"),
                          textOutput("threesMadeMean"),
                          h3("avg. threes made (last five)"),
                          textOutput("threesMadeL5"),
                          h3("50% of games are between"),
                          textOutput("threesMadePercentiles"),
                          tags$style(
                            "#threesMadeMean {font-size: 25px;}
                             #threesMadeL5 {font-size: 25px;}
                             #threesMadePercentiles {font-size: 25px;}"
                          ),
                          br(),
                          plotOutput("threesPlayerShotChart", width="100%", height = "25vh"),
                          br(),
                          br(),
                          width = 3
                        ),
                        mainPanel(
                          plotlyOutput("threesLineplot", height="40em"),
                          br(),
                          plotlyOutput("threesHistplot", height="40em"),
                          br(), br(),
                          plotlyOutput("threesAssistedThroughTime", height="40em"),
                          br(),
                          h3("who has assisted their 3s season-long"),
                          plotOutput("threesAssistedByPie", height = "50em"),
                          br(),
                          h3("threes type, during L1 L5 + Season Avg"),
                          plotOutput("threesType", width = "100em", height = "50em"),
                          h3("three point shooting percentage through time"),
                          plotlyOutput("threesShootingPerc", height="40em"),
                          br(), br(), br(), br(), br(), 
                          # this first plotly didnt include the Unassisted 3 in its graph
                          # plotlyOutput("threesBarplot", height="40em"),
                          width = 9
                        ) 
                      )
                      
            
                      
             ),
             
             tabPanel("Threes (Team)",
                      titlePanel("Team threes stats, 2024 season data"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("teams3", "Choose a team:",
                                      choices = " "
                          ),
                          h3("##"),
                          h3("this team ranks # in the league in threes"),
                          textOutput(paste("threesTeamRank")),
                          h3("with this many threes per game"),
                          textOutput(paste("threesTeam")),
                          p("must min averaging 10mins/game to appear in chart"),
                          tags$style(
                            "#threesTeamRank {font-size: 25px;}
                             #threesTeam {font-size: 25px;}"
                          ),
                          width = 3
                        ),
                        mainPanel(
                          h3("threes made in a game"),
                          plotOutput("teamThreesPlot", height = "75em"),
                          br(),br(),
                          h3("who is the most 3ball dependant? (min 6ppg)"),
                          plotOutput("teamThreesDependencePlot", height = "75em"),
                          br(),br(),br(),
                          h3("who assists threes on this team"),
                          plotOutput("teamThreeAssistCrossPlot", height = "75em"),
                          width = 9
                        ) )
             ),
             
             
             
             
             
             tabPanel("Dashboard",
                      titlePanel("State of the League"),
                      h3("Players Leading/Losing the League"),
                      p(em("10 game moving average of the stat; delta is the arithmetic change in that mov.avg series over the last 10 games (+/0/-)")), br(),
                      datatable(trending, options = list(pageLength = 30, lengthChange = FALSE), rownames = FALSE, width = "100%") %>%
                        # formatRound(which(sapply(trending,is.numeric)), digits=1) %>%
                        formatStyle("MAPoints", backgroundColor = styleEqual(sort(unique(trending$MAPoints), decreasing = TRUE), colfunc(length(unique(trending$MAPoints))))) %>%
                        formatStyle("chgMAPoints", backgroundColor = styleEqual(sort(unique(trending$chgMAPoints), decreasing = TRUE), colfunc(length(unique(trending$chgMAPoints)))))  %>%
                        formatStyle("MARebounds", backgroundColor = styleEqual(sort(unique(trending$MARebounds), decreasing = TRUE), colfunc(length(unique(trending$MARebounds)))))  %>%
                        formatStyle("chgMARebounds", backgroundColor = styleEqual(sort(unique(trending$chgMARebounds), decreasing = TRUE), colfunc(length(unique(trending$chgMARebounds)))))  %>%
                        formatStyle("MAAssists", backgroundColor = styleEqual(sort(unique(trending$MAAssists), decreasing = TRUE), colfunc(length(unique(trending$MAAssists)))))  %>%
                        formatStyle("chgMAAssists", backgroundColor = styleEqual(sort(unique(trending$chgMAAssists), decreasing = TRUE), colfunc(length(unique(trending$chgMAAssists)))))  %>%
                        formatStyle("MAFGvolume", backgroundColor = styleEqual(sort(unique(trending$MAFGvolume), decreasing = TRUE), colfunc(length(unique(trending$MAFGvolume)))))  %>%
                        formatStyle("chgMAFGvolume", backgroundColor = styleEqual(sort(unique(trending$chgMAFGvolume), decreasing = TRUE), colfunc(length(unique(trending$chgMAFGvolume)))))  %>%
                        formatStyle("MATvolume", backgroundColor = styleEqual(sort(unique(trending$MATvolume), decreasing = TRUE), colfunc(length(unique(trending$MATvolume)))))  %>%
                        formatStyle("chgMATvolume", backgroundColor = styleEqual(sort(unique(trending$chgMATvolume), decreasing = TRUE), colfunc(length(unique(trending$chgMATvolume)))))  %>%
                        formatStyle("MATmade", backgroundColor = styleEqual(sort(unique(trending$MATmade), decreasing = TRUE), colfunc(length(unique(trending$MATmade)))))  %>%
                        formatStyle("chgMATmade", backgroundColor = styleEqual(sort(unique(trending$chgMATmade), decreasing = TRUE), colfunc(length(unique(trending$chgMATmade)))))
                      ,
                      
                      
                      
                       br(), br(), br(), br(), br(), br(), br(),
                      
                      ## COLOR CODING; FILTER BY MPG, ETC
                      
             ), 
             
             tabPanel("Today's Matchups",
                      titlePanel("Today's Matchups"),
                       datatable(today, options = list(dom = "t", ordering = FALSE, paging = FALSE, searching = FALSE), selection = 'none', 
                                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"), class = 'row-border', escape = FALSE, rownames = FALSE, filter = "none", width = "100%"),
                      br(), br(),
                      h3("positional matchups that rank top 5 or better against opponent"),
                      datatable(top5sToday, options = list(paging = FALSE, lengthChange = FALSE), rownames = FALSE, width = "100%")  %>% 
                        formatStyle('rankPoints', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8'))) %>% 
                        formatStyle('rankRebs', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8'))) %>% 
                        formatStyle('rankAssists', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8'))) %>% 
                        formatStyle('rankThrees', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8'))),
                      br(), br(), br(), br(), br(), br(),
                      h3("all positional matchups, if u want"),
                      datatable(positionalSplits, options = list(pageLength = 30, lengthChange = FALSE), rownames = FALSE, width = "100%")  %>% 
                        formatStyle('rankPoints', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8'))) %>% 
                        formatStyle('rankRebs', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8'))) %>% 
                        formatStyle('rankAssists', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8'))) %>% 
                        formatStyle('rankThrees', backgroundColor = styleInterval(c(5,25), c('#abdda5', '#e6f599', '#F7A9A8')))
             ),
             
             tabPanel("Team Stats",
                      mainPanel(h1("Team Stats, 2024 season"),
                                br(),
                                h4("TEAM STATS -- this team's box scores"),
                                p("how many points, assists, etc, is this team scoring in their games this season?"),
                                datatable(teamstatsDatatable2, options = list(paging = FALSE, lengthChange = FALSE), rownames = FALSE) %>% 
                                  formatStyle('PPG', backgroundColor = styleInterval(c(110, 115, 120), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('APG', backgroundColor = styleInterval(c(25, 27, 29), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('RPG', backgroundColor = styleInterval(c(42, 43.5, 45), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('ORPG', backgroundColor = styleInterval(c(10, 11, 12), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('DRPG', backgroundColor = styleInterval(c(31, 33, 35), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('BPG', backgroundColor = styleInterval(c(4, 5, 6), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('SPG', backgroundColor = styleInterval(c(6, 7, 8), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('TOPG', backgroundColor = styleInterval(c(12, 13, 14), c('#abdda5', '#e6f599','#fffebf', '#F7A9A8'))) %>%
                                  formatStyle('FGMG', backgroundColor = styleInterval(c(41, 42.5, 44), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('FGAG', backgroundColor = styleInterval(c(87, 89, 91), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('ThreePG', backgroundColor = styleInterval(c(11.5, 13, 15), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('ThreeAPG', backgroundColor = styleInterval(c(32, 35, 40), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))),
                                br(),br(),br(),
                                h4("MATCHUP STATS -- stats put up AGAINST this team"),
                                p("how many points, assists, etc, is this team letting up in games against them this season?"),
                                p("meaning: weaker defenses let up more PPG; stronger defenses let up less PPG"),
                                datatable(teamstatsAgainstDatatable, options = list(paging = FALSE, lengthChange = FALSE), rownames = FALSE) %>% 
                                  formatStyle('aPPG', backgroundColor = styleInterval(c(110, 115, 120), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aAPG', backgroundColor = styleInterval(c(25, 27, 29), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aRPG', backgroundColor = styleInterval(c(42, 43.5, 45), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aORPG', backgroundColor = styleInterval(c(10, 11, 12), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aDRPG', backgroundColor = styleInterval(c(31, 33, 35), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aBPG', backgroundColor = styleInterval(c(4, 5, 6), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aSPG', backgroundColor = styleInterval(c(6, 7, 8), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aTOPG', backgroundColor = styleInterval(c(12, 13, 14), c('#abdda5', '#e6f599','#fffebf', '#F7A9A8'))) %>%
                                  formatStyle('aFGMG', backgroundColor = styleInterval(c(41, 42.5, 44), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aFGAG', backgroundColor = styleInterval(c(87, 89, 91), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aThreePG', backgroundColor = styleInterval(c(11.5, 13, 15), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) %>%
                                  formatStyle('aThreeAPG', backgroundColor = styleInterval(c(32, 35, 40), c('#F7A9A8', '#fffebf', '#e6f599', '#abdda5'))) ,
                                br(),br(),br(),
                      )
             )
            )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  #### THIS IS WHERE I CREATE THE ACTUAL CHARTS. USUALLY THEY ARE ORIGINALLY TESTED OUT IN MY RMARKDOWN FILES.
  
  
  
  ##### POINTS
  ##### PLOTS
  ## Plots Timeline for a player
  output$pointsLineplot <- renderPlotly({
    
    if (input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes)) %>%
      arrange(game_date) %>%
      mutate(movAvg = movavg(points, MAthreshold10, type = "s"))
    
    pointsPlotly <- playerBox %>%
      ggplot(aes(x = game_date)) +
      geom_line(aes(y = points), color = "#01200F", linewidth = 0.25) +
      geom_point(aes(y = points), color = "#01200F") + 
      geom_area(aes(y = points), fill = "#A2C3A4", alpha = 0.25) +
      geom_line(aes(y = movAvg), color = "#F7A9A8", linewidth = 0.25) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(breaks = pretty_breaks(8)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "points", title = "Points scored in 2024 season")
    
    ggplotly(pointsPlotly)
    
  })
  
  ## point type
  output$twoOrThreeLineplot <- renderPlotly({
    
    if (input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerBoxOneTwoThree <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes)) %>%
      mutate(PointsFromThree = three_point_field_goals_made * 3,
             PointsFromTwo = (field_goals_made - three_point_field_goals_made) * 2,
             PointsFromOne = free_throws_made) %>%
      select(game_id, game_date, athlete_display_name, points, PointsFromThree, PointsFromTwo, PointsFromOne)
    
    
    maxPoints <- max(playerBoxOneTwoThree$points)
    
    oneTwoThreePlotly <- playerBoxOneTwoThree %>%
      pivot_longer(cols = starts_with("PointsFrom"), names_to = "scoretype", names_prefix = "PointsFrom", values_to = "pointsScored") %>%
      ggplot(aes(x = game_date, y = pointsScored, fill = scoretype)) +
      geom_area() +
      scale_fill_manual(values = smallercolorPalatte) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(limits = c(0, maxPoints), breaks = pretty_breaks(7)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "points", title = "How do they get their points, 2024 season")
    
    ggplotly(oneTwoThreePlotly)
    
  })
  
  ## point type
  output$whoScoresTeam <- renderPlot({
    
    if (input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    
    playerTeam <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      select(team_name) %>%
      head(1) %>% pull()
    
    top10guys <- nba_box_2025 %>%
      dplyr::filter(team_name == playerTeam) %>%
      dplyr::filter(!is.na(minutes)) %>%
      group_by(athlete_display_name) %>%
      summarize(ppg = mean(points)) %>%
      arrange(desc(ppg)) %>%
      head(10) %>%
      pull(athlete_display_name)
    
    teamBoxes <- nba_box_2025 %>%
      dplyr::filter(team_name == playerTeam) %>%
      dplyr::filter(!is.na(minutes)) %>%
      mutate(nameIncludeOther = ifelse(athlete_display_name %in% top10guys, athlete_display_name, "Other")) %>%
      group_by(game_id, game_date, nameIncludeOther) %>%
      mutate(pointsIncludeOther = ifelse(athlete_display_name %in% top10guys, points, sum(points))) %>%
      select(game_id, game_date, nameIncludeOther, pointsIncludeOther) %>%
      unique() %>%
      group_by(nameIncludeOther) %>%
      mutate(avg = mean(pointsIncludeOther)) %>% ungroup()
    
    teamMax <- teamBoxes %>%
      group_by(game_date) %>%
      summarize(total = sum(pointsIncludeOther)) %>%
      pull(total) %>%
      max()
    
    teamPointsPlot <- teamBoxes %>%
      ggplot(aes(x = game_date, y = pointsIncludeOther, fill = reorder(nameIncludeOther, avg))) +
      geom_area() +
      scale_fill_manual(values = colorPalatte) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(limits = c(0, teamMax + 5), breaks = pretty_breaks(7)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "points", title = "who is scoring for the team")
    
    teamPointsPlot
    
  })
  
  
  ## Plots Timeline for a player
  output$fgLineplot <- renderPlotly({
    
    if (input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    
    maxFGA <- max(playerBox$field_goals_attempted)
    
    fgPlotly <- playerBox %>%
      ggplot(aes(x = game_date)) +
      geom_line(aes(y = field_goals_attempted), color = "#FC2F00", linewidth = 0.25) +
      geom_point(aes(y = field_goals_attempted), color = "#FC2F00") + 
      geom_area(aes(y = field_goals_attempted), fill = "#FC2F00", alpha = 0.25) +
      geom_line(aes(y = field_goals_made), color = "#01200F", linewidth = 0.25) +
      geom_point(aes(y = field_goals_made), color = "#01200F") + 
      geom_area(aes(y = field_goals_made), fill = "#A2C3A4", alpha = 0.8) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(limits = c(0, maxFGA + 2), breaks = pretty_breaks(maxFGA + 2)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "fgs", title = "FG volume in 2024 season")
    
    ggplotly(fgPlotly)
    
  })
  
  ## Points Distribution Chart / Histogram
  output$pointsHistplot <- renderPlotly({
    
    if (input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    pointsPlotly <- playerBox %>%
      ggplot(aes(x= points)) + 
      geom_density(aes(y=1.5 * ..count..), fill="#A2C3A4", color="white", alpha=0.4) + 
      geom_histogram(binwidth = 1, fill="#01200F", color="white", alpha=1) + 
      geom_vline(xintercept = mean(playerBox$points), color="#FC2F00", size = 1.5) +
      geom_vline(xintercept = mean(playerBox$points) - sd(playerBox$points), color="#FC2F00", size = 0.5) +
      geom_vline(xintercept = mean(playerBox$points) + sd(playerBox$points), color="#FC2F00", size = 0.5) +
      xlim(-0.5, max(playerBox$points) + 3) +
      labs(y = "number of times", title = "Range of Outcomes") + 
      theme_minimal()
    
    ggplotly(pointsPlotly)
    
  })
  
  output$pointsPlayerShotChart <- renderPlot({
    
    if (input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerShotsGamelog <- clean_PBP25 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(shooting_play == TRUE)
    
    pointsShotdiagram <- ggplot() +
      geom_jitter(data = playerShotsGamelog, aes(x = coordinate_x_raw, y = coordinate_y_raw, color = as.factor(score_value), shape = scoring_play), size = 3, show.legend = FALSE) +
      scale_shape_manual(values=c(4, 16)) + 
      scale_color_manual(values=c('#D64933', 'black', "#66c2a5",'#0C7C59')) +
      geom_path(data=threePointLine, aes(x=x2, y=yup2)) +
      geom_path(data=baseline, aes(x=x, y=y))+
      geom_circle(aes(x0 = 25, y0 = 5.24934 - 4, r = 0.75)) + 
      labs(x = "", y="") +
      ylim(-4, 34) +
      theme_void() +
      theme(panel.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"))
    
    pointsShotdiagram
    
  })
  
  ## END POINTS PLOTS
  
  
  ## CREATING TEXT TO OUTPUT KEY STATISTICS
  output$pointsMean <- renderText({
    if (input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    round(mean(playerBox$points),2)
  })
  
  output$pointsL5 <- renderText({
    if (input$playersP == " " | input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    lastfive <- playerBox$points %>%
      head(5)
    
    round(mean(lastfive),3)
  })
  
  output$pointsPercentiles <- renderText({
    if (input$playersP == " " | input$playersP == " ") {
      return()
    }
    
    playerName <- input$playersP
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    twentyfifth <- playerBox$points %>%
      quantile(probs=0.25)
    
    seventyfifth <- playerBox$points %>%
      quantile(probs=0.75)
    
    paste(twentyfifth, " and ", seventyfifth, " points", sep="")
  })
  ## END
  
  
  
  
  ##### REBOUNDS
  ##### PLOTS
  ## Rebounds Timeline for a player
  output$reboundsLineplot <- renderPlotly({
    
    if (input$playersR == " ") {
      return()
    }
    
    playerName <- input$playersR
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    reboundsPlotly <- playerBox %>%
      ggplot(aes(x = game_date, y = rebounds)) +
      geom_line(color = "#01200F", linewidth = 0.25) +
      geom_point(color = "#01200F") + 
      geom_area(fill = "#A2C3A4", alpha = 0.25) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(breaks = pretty_breaks(8)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "rebounds", title = "Boards grabbed in 2024 season")
    
    ggplotly(reboundsPlotly)
    
  })
  
  ## Points Distribution Chart / Histogram
  output$reboundsHistplot <- renderPlotly({
    
    if (input$playersR == " ") {
      return()
    }
    
    playerName <- input$playersR
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    reboundsPlotly <- playerBox %>%
      ggplot(aes(x= rebounds)) + 
      geom_density(aes(y=1.5 * ..count..), fill="#A2C3A4", color="white", alpha=0.4) + 
      geom_histogram(binwidth = 1, fill="#01200F", color="white", alpha=1) + 
      geom_vline(xintercept = mean(playerBox$rebounds), color="#FC2F00", size = 1.5) +
      geom_vline(xintercept = mean(playerBox$rebounds) - sd(playerBox$rebounds), color="#FC2F00", size = 0.5) +
      geom_vline(xintercept = mean(playerBox$rebounds) + sd(playerBox$rebounds), color="#FC2F00", size = 0.5) +
      xlim(-0.5, max(playerBox$rebounds) + 3) +
      labs(y = "number of times", title = "Range of Outcomes") + 
      theme_minimal()
    
    ggplotly(reboundsPlotly)
    
  })
  
  ## Who is rebounding
  output$reboundTrackingTable <- renderTable({
    
    if (input$playersR == " ") {
      return()
    }
    
    playerName <- input$playersA
    reboundtracking %>%
      dplyr::filter(PLAYER_NAME == playerName) %>%
      select(PLAYER_NAME, range, REB, REB_CONTEST, REB_UNCONTEST, REB_CHANCES, REB_CHANCE_DEFER, 
             OREB, OREB_CONTEST, OREB_UNCONTEST, OREB_CHANCES, OREB_CHANCE_DEFER, 
             DREB, DREB_CONTEST, DREB_UNCONTEST, DREB_CHANCES, DREB_CHANCE_DEFER)
    
  })
  
  
  ## CREATING TEXT TO OUTPUT KEY STATISTICS
  output$reboundsMean <- renderText({
    if (input$playersR == " " | input$playersR == " ") {
      return()
    }
    
    playerName <- input$playersR
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    round(mean(playerBox$rebounds),2)
  })
  
  output$reboundsL5 <- renderText({
    if (input$playersR == " " | input$playersR == " ") {
      return()
    }
    
    playerName <- input$playersR
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    lastfive <- playerBox$rebounds %>%
      head(5)
    
    round(mean(lastfive),3)
  })
  
  output$reboundsPercentiles <- renderText({
    if (input$playersR == " " | input$playersR == " ") {
      return()
    }
    
    playerName <- input$playersR
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    twentyfifth <- playerBox$rebounds %>%
      quantile(probs=0.25)
    
    seventyfifth <- playerBox$rebounds %>%
      quantile(probs=0.75)
    
    paste(twentyfifth, " and ", seventyfifth, " rebounds", sep="")
  })
  ## END
  
  
  #### TEAM REBOUND PLOTS
  output$teamReboundPlot <- renderPlot({
    
    if (input$teamsR == " ") {
      return()
    }
    
    teamName <- input$teamsR
    teamRebounds <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      dplyr::filter( !is.na(rebounds)) %>%
      group_by(athlete_display_name) %>%
      mutate(avgRebounds = mean(rebounds),
             avgMins = mean(minutes)) %>%
      arrange(desc(avgMins)) %>%
      dplyr::filter(avgMins >= 10)
    
    maxTeamRebounds <- max(teamRebounds$rebounds)
    
    ggplot(teamRebounds, aes(x = rebounds, y = reorder(athlete_display_name, avgRebounds), fill = athlete_display_name)) +
      geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.07, height = 0),
                          point_shape = 'X', point_size = 5, point_alpha = 1, alpha = 0.5, scale = 3) +
      scale_x_continuous(limits = c(0, maxTeamRebounds + 2), breaks = pretty_breaks(maxTeamRebounds + 2), position = "top") + 
      theme_ridges() +
      theme(legend.position = "none") +
      labs(y = "")
  })
  
  
  
  
  
  ##### ASSIST PLOTS
  ## Assists Timeline for a player
  output$assistLineplot <- renderPlotly({
    
    if (input$playersA == " " | input$playersA == " ") {
      return()
    }
    
    playerName <- input$playersA
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    assistsPlotly <- playerBox %>%
      ggplot(aes(x = game_date, y = assists)) +
      geom_line(color = "#01200F", linewidth = 0.25) +
      geom_point(color = "#01200F") + 
      geom_area(fill = "#A2C3A4", alpha = 0.25) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(breaks = pretty_breaks(8)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "assists", title = "Number of Assists in 2024 season")
    
    ggplotly(assistsPlotly)
    
  })
  
  ## Assists Distribution Chart / Histogram
  output$assistHistplot <- renderPlotly({
    
    if (input$playersA == " " | input$playersA == " ") {
      return()
    }
    
    playerName <- input$playersA
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    assistsPlotly <- playerBox %>%
      ggplot(aes(x= assists)) + 
      # geom_density(aes(y=1.5 * ..count..), fill="#A2C3A4", color="white", alpha=0.2) + 
      geom_histogram(binwidth = 1, fill="#01200F", color="white", alpha=1) + 
      geom_vline(xintercept = mean(playerBox$assists), color="#FC2F00", size = 1.5) +
      geom_vline(xintercept = mean(playerBox$assists) - sd(playerBox$assists), color="#FC2F00", size = 0.5) +
      geom_vline(xintercept = mean(playerBox$assists) + sd(playerBox$assists), color="#FC2F00", size = 0.5) +
      xlim(-0.5, max(playerBox$assists) + 3) +
      labs(y = "number of times", title = "Range of Outcomes") + 
      theme_minimal()
    
    ggplotly(assistsPlotly)
    
  })
  
  ## Who are they assigint plot
  output$assistBarplot <- renderPlotly({
    
    if (input$playersA == " " | input$playersA == " ") {
      return()
    }
    
    playerName <- input$playersA
    playerAssist <- assistsGamelog %>%
      dplyr::filter(assisterName == playerName) %>%
      group_by(game_id) %>%
      mutate(boxAssists = n()) %>%
      ungroup() %>%
      group_by(game_id, scorerName) %>%
      mutate(timesScorerAssistedByPlayer = n()) %>%
      ungroup()
    
    g <- playerAssist %>%
      select(scorerName, game_date, timesScorerAssistedByPlayer) %>%
      unique() %>%
      ggplot(aes(x = reorder(as.character(game_date), game_date), y = timesScorerAssistedByPlayer, fill = scorerName)) +
      geom_col() +
      # scale_fill_brewer(type = "div", palette = "Spectral") +
      scale_fill_manual(values = colorPalatte) +
      labs(x = "date", y = "assists", title = "Who are they assisting?") + 
      theme_minimal() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      scale_y_continuous(breaks = pretty_breaks(8))
    
    ggplotly(g)
    
  })
  
  ## Who are they assigint plot
  output$assistTrackingTable <- renderTable({
    
    if (input$playersA == " " | input$playersA == " ") {
      return()
    }
    
    playerName <- input$playersA
    passingtracking %>%
      dplyr::filter(PLAYER_NAME == playerName) %>%
      select(PLAYER_NAME, TEAM_ABBREVIATION, range, AST, POTENTIAL_AST, FT_AST, SECONDARY_AST, AST_TO_PASS_PCT, PASSES_MADE, PASSES_RECEIVED)
    
  })
  
  ## CREATING TEXT TO OUTPUT KEY STATISTICS
  output$assistMean <- renderText({
    if (input$playersA == " " | input$playersA == " ") {
      return()
    }
    
    playerName <- input$playersA
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    round(mean(playerBox$assists),2)
  })
  
  output$assistL5 <- renderText({
    if (input$playersA == " " | input$playersA == " ") {
      return()
    }
    
    playerName <- input$playersA
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    lastfive <- playerBox$assists %>%
      head(5)
    
    round(mean(lastfive),3)
  })
  
  output$assistPercentiles <- renderText({
    if (input$playersA == " " | input$playersA == " ") {
      return()
    }
    
    playerName <- input$playersA
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    twentyfifth <- playerBox$assists %>%
      quantile(probs=0.25)
    
    seventyfifth <- playerBox$assists %>%
      quantile(probs=0.75)
    
    paste(twentyfifth, " and ", seventyfifth, " assists", sep="")
  })
  ## END
  
  #### TEAM ASSIST PLOTS
  ## Assists summary for a team
  output$teamAssistPlot <- renderPlot({
    
    if (input$teamsA == " ") {
      return()
    }
    
    teamName <- input$teamsA
    teamAssists <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      dplyr::filter( !is.na(assists)) %>%
      group_by(athlete_display_name) %>%
      mutate(avgAssists = mean(assists),
             avgMins = mean(minutes)) %>%
      arrange(desc(avgMins)) %>%
      dplyr::filter(avgMins >= 10)
    
    maxTeamAssists <- max(teamAssists$assists)
    
    ggplot(teamAssists, aes(x = assists, y = reorder(athlete_display_name, avgAssists), fill = athlete_display_name)) +
      geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.07, height = 0),
                          point_shape = 'X', point_size = 5, point_alpha = 1, alpha = 0.5, scale = 3) +
      scale_x_continuous(limits = c(0, maxTeamAssists + 2), breaks = pretty_breaks(maxTeamAssists + 2), position = "top") + 
      theme_ridges() +
      theme(legend.position = "none") +
      labs(y = "")
    
    
  })
  
  ## Assists summary for a team
  output$teamAssistCrossPlot <- renderPlot({
    
    if (input$teamsA == " ") {
      return()
    }
    
    teamName <- input$teamsA
    
    ## ASSISTS COMPLETE LOG
    allcombinations <- assistsGamelog %>%
      dplyr::filter(team_name == teamName) %>%
      select(assisterName, scorerName, game_id) %>%
      group_by(assisterName,scorerName,game_id) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      tidyr::complete(assisterName, scorerName, game_id, fill = list(count = 0), explicit = FALSE) %>%
      dplyr::filter(assisterName != scorerName)
    
    minutesData <- nba_box_2025 %>%
      select(athlete_display_name, game_id, minutes)
    
    cleanAssistToFrom <- allcombinations %>%
      left_join(minutesData, by = c("assisterName"="athlete_display_name", "game_id"="game_id")) %>%
      left_join(minutesData, by = c("scorerName"="athlete_display_name", "game_id"="game_id")) %>%
      rename(assisterMins = minutes.x,
             scorerMins = minutes.y) %>%
      dplyr::filter(!is.na(assisterMins)  & !is.na(scorerMins)) 
    
    
    links <- cleanAssistToFrom %>%
      group_by(assisterName, scorerName) %>%
      summarize(avgTFapg = mean(count)) %>%
      arrange(desc(avgTFapg)) %>%
      group_by(assisterName) %>%
      mutate(sum = sum(avgTFapg),
             percAssist = avgTFapg / sum,
             label = round(avgTFapg, 1))
    
    t<- ggplot(links, aes(x = reorder(assisterName, -sum), y = reorder(scorerName, -sum), fill = avgTFapg)) +
      geom_tile(show.legend = FALSE) + 
      geom_text(aes(label = label), color = "black") +
      theme_bw() +
      theme(axis.text.x = element_text(size = rel(2), angle = 90, vjust = 0.5, hjust=1), axis.text.y = element_text(size = rel(2)))+
      scale_fill_gradient2(low = "#9e0242",
                           mid = "#ffe08b",
                           high = "#66c2a5", midpoint = 0.6) +
      coord_fixed() +
      labs(x = "assister", y = "scorer")
    
    t
    
    
  })
  
  
  output$tblAssistCombos = renderDT({
    
    if (input$teamsA == " ") {
      return()
    }
    
    teamName <- input$teamsA
    
    ## ASSISTS COMPLETE LOG
    allcombinations <- assistsGamelog %>%
      dplyr::filter(team_name == teamName) %>%
      select(assisterName, scorerName, game_id) %>%
      group_by(assisterName,scorerName,game_id) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      tidyr::complete(assisterName, scorerName, game_id, fill = list(count = 0), explicit = FALSE) %>%
      dplyr::filter(assisterName != scorerName)
    
    minutesData <- nba_box_2025 %>%
      select(athlete_display_name, game_id, minutes)
    
    cleanAssistToFrom <- allcombinations %>%
      left_join(minutesData, by = c("assisterName"="athlete_display_name", "game_id"="game_id")) %>%
      left_join(minutesData, by = c("scorerName"="athlete_display_name", "game_id"="game_id")) %>%
      rename(assisterMins = minutes.x,
             scorerMins = minutes.y) %>%
      dplyr::filter(!is.na(assisterMins)  & !is.na(scorerMins)) 
    
    
    links <- cleanAssistToFrom %>%
      group_by(assisterName, scorerName) %>%
      summarize(avgTFapg = mean(count)) %>%
      arrange(desc(avgTFapg)) %>%
      group_by(assisterName) %>%
      mutate(sum = sum(avgTFapg),
             percAssist = avgTFapg / sum,
             label = round(avgTFapg, 1))
    
    x <- links %>%
      mutate(assistsPerGameTF = round( avgTFapg, 2),
             assisterTotAPG = round( sum, 2),
             shareOfAssistersA = round( percAssist * 100, 2)) %>%
      select(assisterName, scorerName, assistsPerGameTF, assisterTotAPG, shareOfAssistersA)
    
    x
    
  }, options = list(pageLength = 25, lengthChange = FALSE, searching = FALSE), rownames = FALSE)
  
  
  
  ## CREATING TEXT TO OUTPUT KEY STATISTICS
  output$assistsTeamRank <- renderText({
    if (input$teamsA == " ") {
      return()
    }
    
    teamName <- input$teamsA
    realTeamname <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      select(team_display_name) %>%
      unique() %>%
      pull()
    
    assistRank <- teamstatsDatatable %>%
      dplyr::filter(team_name == realTeamname) %>%
      select(AST_RANK) %>% pull()
    
    assistRank
    
  })
  
  output$assistsTeam <- renderText({
    if (input$teamsA == " ") {
      return()
    }
    
    teamName <- input$teamsA
    realTeamname <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      select(team_display_name) %>%
      unique() %>%
      pull()
    
    assistPerGame <- teamstatsDatatable %>%
      dplyr::filter(team_name == realTeamname) %>%
      select(APG) %>% pull()
    
    assistPerGame
    
  })
  ## END
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##### 3PM+3PA PLOTS
  ## 3PM+3PA Timeline for a player
  output$threesLineplot <- renderPlotly({
    
    if (input$players3 == " " | input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    threesPlotly <- playerBox %>%
      ggplot() +
      geom_line(aes(x = game_date, y = three_point_field_goals_attempted), color = "#A2C3A4", linewidth = 0.25) +
      geom_point(aes(x = game_date, y = three_point_field_goals_attempted), color = "#A2C3A4") + 
      geom_area(aes(x = game_date, y = three_point_field_goals_attempted), fill = "#A2C3A4", alpha = 0.25) +
      # made
      geom_line(aes(x = game_date, y = three_point_field_goals_made), color = "#01200F", linewidth = 0.25) +
      geom_point(aes(x = game_date, y = three_point_field_goals_made), color = "#01200F") + 
      geom_area(aes(x = game_date, y = three_point_field_goals_made), fill = "#01200F", alpha = 0.25) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(breaks = pretty_breaks(16)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "threes", title = "Number of 3s made/attempted in 2024 season")
    
    ggplotly(threesPlotly)
    
  })
  
  ## 3PM+3PA Distribution Chart / Histogram
  output$threesHistplot <- renderPlotly({
    
    if (input$players3 == " " | input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    h <- playerBox %>%
      ggplot(aes(x= three_point_field_goals_made)) + 
      # geom_density(aes(y=1.5 * ..count..), fill="#A2C3A4", color="white", alpha=0.2) + 
      geom_histogram(binwidth = 1, fill="#01200F", color="white", alpha=1) + 
      geom_vline(xintercept = mean(playerBox$three_point_field_goals_made), color="#FC2F00", size = 1.5) +
      geom_vline(xintercept = mean(playerBox$three_point_field_goals_made) - sd(playerBox$three_point_field_goals_made), color="#FC2F00", size = 0.5) +
      geom_vline(xintercept = mean(playerBox$three_point_field_goals_made) + sd(playerBox$three_point_field_goals_made), color="#FC2F00", size = 0.5) +
      xlim(-0.5, max(playerBox$three_point_field_goals_made) + 3) +
      labs(y = "number of times", title = "Range of Outcomes") + 
      theme_minimal()
    
    ggplotly(h)
    
  })
  
  ## 3PM shooting percentage
  output$threesShootingPerc <- renderPlotly({
    
    if (input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    player3Box <- nba_box_2025 %>%
      dplyr::filter(!is.na(minutes)) %>%
      dplyr::filter(athlete_display_name == playerName)
    
    convChart <- player3Box %>%
      dplyr::filter(!is.na(minutes)) %>%
      arrange(game_date) %>%
      mutate(threepercentage = three_point_field_goals_made/three_point_field_goals_attempted,
             ma = movavg(threepercentage, 5, type = "s"),
             volume = three_point_field_goals_attempted) %>%
      ggplot() +
      # game percentage
      geom_line(aes(x = game_date, y = threepercentage), color = "red", linewidth = 0.25, alpha = 0.25) +
      geom_point(aes(x = game_date, y = threepercentage, size = volume), color = "red", alpha = 0.5, show.legend = FALSE) + 
      # moving average
      geom_line(aes(x = game_date, y = ma), color = "#01200F", linewidth = 0.25) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_continuous(limits = c(0,1), breaks = pretty_breaks(8)) + 
      theme_minimal() +
      labs(x = "Game Date", y = "three point percentage", title = "dot size is volume of 3s attempted")
    
    ggplotly(convChart)
    
  })
  
  
  
  
  
  ## 3PM+3PA Player Shot Chart
  output$threesPlayerShotChart <- renderPlot({
    
    if (input$players3 == " " | input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    player3sGamelog <- clean_PBP25 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(threeAttemptedOrMade == TRUE)
    
    threeShotdiagram <- ggplot() +
      geom_jitter(data = player3sGamelog, aes(x = coordinate_x_raw, y = coordinate_y_raw, color = threeMade, shape = threeMade), size = 3, show.legend = FALSE) +
      scale_shape_manual(values=c(4, 16)) + 
      scale_color_manual(values=c('#D64933','#0C7C59')) +
      geom_path(data=threePointLine, aes(x=x2, y=yup2)) +
      geom_path(data=baseline, aes(x=x, y=y))+
      geom_circle(aes(x0 = 25, y0 = 5.24934 - 4, r = 0.75)) + 
      labs(x = "", y="") +
      ylim(-4, 34) +
      theme_void() +
      theme(panel.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"))
    
    threeShotdiagram
    
  })
  
  ## Who are threes assigint plot
  output$threesBarplot <- renderPlotly({
    
    if (input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    playerAssist3 <- assistsGamelog %>%
      dplyr::filter(scorerName == playerName) %>%
      dplyr::filter(threeMade == TRUE) %>%
      group_by(game_id) %>%
      mutate(boxAssists = n()) %>%
      ungroup() %>%
      group_by(game_id, assisterName) %>%
      mutate(timesScorerAssistedByPlayer = n()) %>%
      ungroup()
    
    g <- playerAssist3 %>%
      select(assisterName, game_date, timesScorerAssistedByPlayer) %>%
      unique() %>%
      ggplot(aes(x = reorder(as.character(game_date), game_date), y = timesScorerAssistedByPlayer, fill = assisterName)) +
      geom_col() +
      # scale_fill_brewer(type = "div", palette = "Spectral") +
      scale_fill_manual(values = colorPalatte) +
      labs(x = "date", y = "assists", title = "Who assisting their threes?") + 
      theme_minimal() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      scale_y_continuous(breaks = pretty_breaks(8))
    
    ggplotly(g)
    
  })
  
  
  ## 3s assisted by, plotly chart
  output$threesAssistedThroughTime <- renderPlotly({
    
    if (input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    assistedOurPlayer <- clean_PBP25 %>%
      dplyr::filter(shooterName == playerName) %>%
      dplyr::filter(score_value >= 3) %>%
      mutate(assistCategory = ifelse(!is.na(assisterName),assisterName,"Unassisted")) %>%
      group_by(scorerName, assistCategory, game_date) %>%
      summarize(count = n()) %>%
      ungroup()
    
    
    g <- ggplot() +
      geom_col(data = assistedOurPlayer, aes(x = reorder(as.character(game_date), game_date), y = count, fill = assistCategory)) +
      scale_fill_manual(values = colorPalatte) +
      labs(x = "date", y = "assists", title = "who is assisting their threes?") + 
      theme_minimal() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      scale_y_continuous(breaks = pretty_breaks(8))
    
    ggplotly(g)
    
  })
  
  
  
  ## 3s assisted by, pie chart
  output$threesAssistedByPie <- renderPlot({
    
    if (input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    
    assistedOurPlayer <- clean_PBP25 %>%
      dplyr::filter(shooterName == playerName) %>%
      dplyr::filter(score_value >= 3) %>%
      mutate(assistCategory = ifelse(!is.na(assisterName),assisterName,"Unassisted")) %>%
      group_by(scorerName, assistCategory, game_date) %>%
      summarize(count = n()) %>%
      ungroup()
    
    propTable <- assistedOurPlayer %>%
      group_by(assistCategory) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      mutate(p = count/sum(count) * 100) %>%
      arrange(desc(p))
    
    pieyum <- ggplot(propTable, aes(x="", y=p, fill= reorder( assistCategory, -p) )) +
      geom_bar(stat="identity", width=1) +
      geom_text(aes(label = scales::percent(round(p) / 100)), position = position_stack(vjust = 0.5), size = rel(6)) +
      coord_polar("y", start=0) +
      theme_void() +
      scale_fill_manual(values = colorPalatte, name = "")
    
    pieyum
    
    # ggplotly(pieyum)
    
  })
  
  output$threesType <- renderPlot({
    
    if (input$players3 == " ") {
      return()
    }
    
    
    
    playerName <- input$players3
    
    threestype %>%
      dplyr::filter(PLAYER_NAME == playerName) %>%
      select(PLAYER_NAME, TEAM_ABBREVIATION.x, range.x, CATCH_SHOOT_FG3M, CATCH_SHOOT_FG3A, PULL_UP_FG3M, PULL_UP_FG3A) %>%
      pivot_longer(cols = CATCH_SHOOT_FG3M:PULL_UP_FG3A, names_to = "shotType", values_to = "count") %>%
      ggplot(aes(x = range.x, y = as.numeric(count), fill = shotType)) +
      geom_col(position = "dodge") +
      theme_bw() +
      labs(x = "range", y = "count") +
      scale_y_continuous(n.breaks = 10)
    
    
    
  })
  
  
  #### TEAM THREE PLOTS
  ## Threes summary for a team
  output$teamThreesPlot <- renderPlot({
    
    if (input$teams3 == " ") {
      return()
    }
    
    teamName <- input$teams3
    team3s <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      dplyr::filter( !is.na(minutes) ) %>%
      group_by(athlete_display_name) %>%
      mutate(avgThrees = mean(three_point_field_goals_made),
             avgMins = mean(minutes)) %>%
      arrange(desc(avgMins)) %>%
      dplyr::filter(avgMins >= 10)
    
    maxTeam3s <- max(team3s$three_point_field_goals_made)
    
    ggplot(team3s, aes(x = three_point_field_goals_made, y = reorder(athlete_display_name, avgThrees), fill = athlete_display_name)) +
      geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.07, height = 0),
                          point_shape = 'X', point_size = 5, point_alpha = 1, alpha = 0.5, scale = 3) +
      scale_x_continuous(limits = c(0, maxTeam3s + 1), breaks = pretty_breaks(maxTeam3s + 1), position = "top") + 
      theme_ridges() +
      theme(legend.position = "none") +
      labs(y = "")
    
    
  })
  
  ## Threes dependence on the team
  output$teamThreesDependencePlot <- renderPlot({
    
    if (input$teams3 == " ") {
      return()
    }
    
    teamName <- input$teams3
    team3Dependence <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      dplyr::filter( !is.na(minutes) ) %>%
      group_by(athlete_display_name) %>%
      mutate(avgThrees = mean(three_point_field_goals_made),
             avgMins = mean(minutes),
             avgPPG = mean(points)) %>%
      arrange(desc(avgMins)) %>%
      dplyr::filter(avgMins >= 10) %>%
      dplyr::filter(avgPPG >= 6) %>%
      mutate(pointsFromThree = three_point_field_goals_made * 3) %>%
      mutate(percPtsFThree = pointsFromThree / points)
    
    ggplot(team3Dependence, aes(x = percPtsFThree, y = reorder(athlete_display_name, percPtsFThree), fill = athlete_display_name)) +
      geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.07, height = 0),
                          point_shape = '|', point_size = 5, point_alpha = 1, alpha = 0.5, scale = 1) +
      scale_x_continuous(limits = c(0, 1), breaks = pretty_breaks(10), position = "top") + 
      theme_ridges() +
      theme(legend.position = "none") +
      labs(x="% of points from 3 in game", y = "")
    
    
  })
  
  ## ON THIS TEAM WHO ASSISTS THREES
  output$teamThreeAssistCrossPlot <- renderPlot({
    
    if (input$teams3 == " ") {
      return()
    }
    
    teamName <- input$teams3
    
    ## ASSISTS COMPLETE LOG
    allcombinations <- assistsGamelog %>%
      dplyr::filter(team_name == teamName) %>%
      dplyr::filter(threeMade == TRUE) %>%
      select(assisterName, scorerName, game_id) %>%
      group_by(assisterName,scorerName,game_id) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      tidyr::complete(assisterName, scorerName, game_id, fill = list(count = 0), explicit = FALSE) %>%
      dplyr::filter(assisterName != scorerName)
    
    minutesData <- nba_box_2025 %>%
      select(athlete_display_name, game_id, minutes)
    
    cleanAssistToFrom <- allcombinations %>%
      left_join(minutesData, by = c("assisterName"="athlete_display_name", "game_id"="game_id")) %>%
      left_join(minutesData, by = c("scorerName"="athlete_display_name", "game_id"="game_id")) %>%
      rename(assisterMins = minutes.x,
             scorerMins = minutes.y) %>%
      dplyr::filter(!is.na(assisterMins)  & !is.na(scorerMins)) 
    
    
    links <- cleanAssistToFrom %>%
      group_by(assisterName, scorerName) %>%
      summarize(avgTFapg = mean(count)) %>%
      arrange(desc(avgTFapg)) %>%
      group_by(assisterName) %>%
      mutate(sum = sum(avgTFapg),
             percAssist = avgTFapg / sum,
             label = round(avgTFapg, 1))
    
    t <- ggplot(links, aes(x = reorder(assisterName, -sum), y = reorder(scorerName, -sum), fill = avgTFapg)) +
      geom_tile(show.legend = FALSE) + 
      geom_text(aes(label = label), color = "black") +
      theme_bw() +
      theme(axis.text.x = element_text(size = rel(2), angle = 90, vjust = 0.5, hjust=1), axis.text.y = element_text(size = rel(2)))+
      scale_fill_gradient2(low = "#9e0242",
                           mid = "#ffe08b",
                           high = "#66c2a5", midpoint = 0.3) +
      coord_fixed() +
      labs(x = "assister", y = "scorer")
    
    t
    
    
  })
  
  ## CREATING TEXT TO OUTPUT KEY STATISTICS
  output$threesTeamRank <- renderText({
    if (input$teams3 == " ") {
      return()
    }
    
    teamName <- input$teams3
    realTeamname <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      select(team_display_name) %>%
      unique() %>%
      pull()
    
    threesRank <- teamstatsDatatable %>%
      dplyr::filter(team_name == realTeamname) %>%
      select(FG3M_RANK) %>% pull()
    
    threesRank
    
  })
  
  output$threesTeam <- renderText({
    if (input$teams3 == " ") {
      return()
    }
    
    teamName <- input$teams3
    realTeamname <- nba_box_2025 %>%
      dplyr::filter(team_name == teamName) %>%
      select(team_display_name) %>%
      unique() %>%
      pull()
    
    threesPerGame <- teamstatsDatatable %>%
      dplyr::filter(team_name == realTeamname) %>%
      select(ThreePG) %>% pull()
    
    threesPerGame
    
  })
  ## END
  
  
  

  
  
  ## CREATING TEXT TO OUTPUT KEY STATISTICS
  output$threesMadeMean <- renderText({
    if (input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    round(mean(playerBox$three_point_field_goals_made),2)
  })
  
  output$threesMadeL5 <- renderText({
    if (input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    lastfive <- playerBox$three_point_field_goals_made %>%
      head(5)
    
    round(mean(lastfive),3)
  })
  
  output$threesMadePercentiles <- renderText({
    if (input$players3 == " ") {
      return()
    }
    
    playerName <- input$players3
    playerBox <- nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      dplyr::filter(!is.na(minutes))
    
    twentyfifth <- playerBox$three_point_field_goals_made %>%
      quantile(probs=0.25)
    
    seventyfifth <- playerBox$three_point_field_goals_made %>%
      quantile(probs=0.75)
    
    paste(twentyfifth, " and ", seventyfifth, " threes", sep="")
  })
  ## END
  
  
  
  ### PLAYER VIEW PAGE ONE
  output$picture<-renderText({
    
    if (input$playersHome == " ") {
      return()
    }
    
    playerName <- input$playersHome
    src = nba_box_2025 %>%
      dplyr::filter(athlete_display_name == playerName) %>%
      select(athlete_headshot_href) %>%
      head(1) %>% pull()
    
    c('<img src="',src,'" style="width: 20vw; min-width: 50px;">')
    
  })
  
  output$heatShotChart <- renderPlot({
    
    if (input$playersHome == " ") {
      return()
    }
    
    
    
    playerName <- input$playersHome
    playerShotsGamelog <- clean_PBP25 %>%

      dplyr::filter(shooting_play == TRUE & score_value >= 2) %>%
      mutate(coorY = round(coordinate_y_raw / 3) * 3,
             coorX = round(coordinate_x_raw / 3) * 3) %>%
      group_by(shooterName, coorX, coorY) %>%
      summarize(count = n()) %>%
      ungroup() %>% group_by(shooterName) %>%
      mutate(buckets = sum(count),
             perc = count / buckets) %>%
      ungroup() %>% group_by(coorX, coorY) %>%
      mutate(avgperc = mean(perc),
             relperc = as.double(perc - avgperc)) %>%
      ungroup() %>%
      mutate(percentile = percent_rank(relperc)*100) %>%
      dplyr::filter(shooterName == playerName)
      
    
    ggplot(playerShotsGamelog, aes(x=coorX, y=coorY)) +
      geom_point(shape = 15, aes( size = perc * 100, color = percentile)) +
      geom_text(label = as.character(round(playerShotsGamelog$percentile, 0)), size = 3, color = "white") +
      scale_color_gradient(low = "#ed0000", high = "#2bdb25") +
      scale_fill_continuous(type = "viridis") +
      geom_path(data=threePointLine, aes(x=x2, y=yup2)) +
      geom_path(data=baseline, aes(x=x, y=y))+
      geom_circle(aes(x0 = 25, y0 = 5.24934 - 4, r = 0.75)) + 
      labs(x = "", y="") +
      ylim(-4, 29) +
      theme_void() +
      # theme(panel.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5")) +
      guides(size=guide_legend(title="%ofshots")) +
      scale_size_continuous(range = c(5, 10)) +
      guides(color = FALSE)
    
    
    
  })
  
  ## how are they scoring
  output$scoringTrackingTable <- renderTable({
    
    if (input$playersHome == " ") {
      return()
    }
    
    playerName <- input$playersHome
    scoringtracking %>% 
      dplyr::filter(PLAYER_NAME == playerName) %>%
      mutate(DRIVE = DRIVE_PTS, CATCHSHOOT = CATCH_SHOOT_PTS, PULLUP = PULL_UP_PTS, PAINT = PAINT_TOUCH_PTS, POSTUP = POST_TOUCH_PTS, ELBOW = ELBOW_TOUCH_PTS) %>%
      select(PLAYER_NAME, range, MIN, POINTS, DRIVE, CATCHSHOOT, PULLUP, PAINT, POSTUP, ELBOW)
    
    
  })
  
  ## playtypes
  output$playtypesPlayer <- renderPlot({
    
    if (input$playersHome == " ") {
      return()
    }
    
    playerName <- input$playersHome
    playtypes %>%
      dplyr::filter(PLAYER_NAME == playerName) %>%
      ggplot(aes(x=reorder(PLAY_TYPE, -POSS_PCT), y=POSS_PCT, fill = PPP)) +
      geom_bar(stat = "identity") +
      theme_bw() + 
      scale_fill_gradient2(low = "#e67582",
                           mid = "#ffe08b",
                           high = "#66c2a5", midpoint = 1) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      labs(x = "play-type", y = "%of player's possessions")
    
  })
  
  output$playtypesTeam <- renderPlot({
    
    if (input$playersHome == " ") {
      return()
    }
    
    playerName <- input$playersHome
    
    teamName <- playtypes %>%
      dplyr::filter(PLAYER_NAME == playerName) %>%
      head(1) %>%
      pull(TEAM_NAME)
    
    playtypes %>%
      dplyr::filter(TEAM_NAME == teamName) %>%
      ggplot(aes(y = reorder(PLAYER_NAME, -TotalPlaytypePts), x = PLAY_TYPE, fill = PTS)) + 
      geom_tile() + 
      scale_fill_gradient2(low = "#9e0242",
                           mid = "#ffe08b",
                           high = "#66c2a5", midpoint = 2) +
      geom_text(aes(label = PTS, size = PTS), show.legend = FALSE) +
      theme_classic() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) +
      labs(x = "Play Type", y = "")
    
  })
  

  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Players To Be filtered/selected
  observe({
    updateSelectInput(
      session,
      "playersHome",
      choices = unique(player_tibble$athlete_display_name)
    )
  })
  observe({
    updateSelectInput(
      session,
      "playersP",
      choices = unique(player_tibble$athlete_display_name)
    )
  })
  observe({
    updateSelectInput(
      session,
      "playersR",
      choices = unique(player_tibble$athlete_display_name)
    )
  })
  observe({
    updateSelectInput(
      session,
      "playersA",
      choices = unique(player_tibble$athlete_display_name)
    )
  })
  observe({
    updateSelectInput(
      session,
      "teamsA",
      choices = unique(team_tibble$team_name)
    )
  })
  observe({
    updateSelectInput(
      session,
      "teamsR",
      choices = unique(team_tibble$team_name)
    )
  })
  observe({
    updateSelectInput(
      session,
      "teams3",
      choices = unique(team_tibble$team_name)
    )
  })
  observe({
    updateSelectInput(
      session,
      "players3",
      choices = unique(player_tibble$athlete_display_name)
    )
  })
  
  

}


shinyApp(ui = ui, server = server)
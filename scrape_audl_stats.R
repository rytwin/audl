setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(RCurl)
library(rjson)

#############Scrape CURRENT YEAR PLAYER STATS################
year <- 2023
page_num <- 1
player_stats <- list()
while(page_num < 1000) {
  webpage <- fromJSON(getURL(paste0("https://audl-stat-server.herokuapp.com/web-api/player-stats?limit=20&year=", year, "&page=", page_num)))
  if(length(webpage[[1]]) == 0) {
    break
  } else {
    player_stats[[page_num]] <- webpage[[1]]
    page_num <- page_num + 1
  }
}
player_stats_all <- bind_rows(player_stats)

write.csv(player_stats_all, paste0("/Users/ryandrost/Documents/R projects/AUDL_stats_", year, ".csv"), row.names = FALSE)

###########Scrape CAREER PLAYER STATS#############

page_num <- 1
player_stats <- list()
while(page_num < 1000) {
  webpage <- fromJSON(getURL(paste0("https://audl-stat-server.herokuapp.com/web-api/player-stats?limit=20&page=", page_num)))
  if(length(webpage[[1]]) == 0) {
    break
  } else {
    player_stats[[page_num]] <- webpage[[1]]
    page_num <- page_num + 1
  }
}
player_stats_all <- bind_rows(player_stats)

write.csv(player_stats_all, "/Users/ryandrost/Documents/R projects/AUDL_stats_player_career.csv", row.names = FALSE)
###########Scrape PLAYER GAME LOGS################

seasons <- 2023
player_stats_all <- read.csv(paste0("/Users/ryandrost/Documents/R projects/AUDL_stats_", seasons, ".csv"))
player_ids <- player_stats_all %>%
  pull(playerID)
game_logs <- list()
count <- 1
for(id in player_ids) {
  for(y in seasons) {
    webpage <- fromJSON(getURL(paste0("https://audl-stat-server.herokuapp.com/web-api/roster-game-stats-for-player?playerID=",
                                      id, "&year=", y)))
    if(length(webpage[[1]]) != 0) {
      player_log <- bind_rows(webpage[[1]]) %>%
        mutate(playerID = id, year = y)
      game_logs[[count]] <- player_log
      count <- count + 1
    }
  }
}

game_logs_all <- bind_rows(game_logs) %>%
  mutate(tm_score = ifelse(isHome, scoreHome, scoreAway),
         opp_score = ifelse(isHome, scoreAway, scoreHome),
         result = ifelse(tm_score > opp_score, 1, ifelse(tm_score == opp_score, 0.5, 0)),
         year = substr(gameID, 1, 4),
         date = substr(gameID, 1, 10),
         tm = ifelse(isHome, sub(".*-", "", gameID), str_extract(gameID, "(?<=-)[[:alpha:]]*[[:alpha:]](?=-)")),
         opp = ifelse(isHome, str_extract(gameID, "(?<=-)[[:alpha:]]*[[:alpha:]](?=-)"), sub(".*-", "", gameID))) %>%
  select(year, date, gameID, playerID, tm, opp, isHome, result, tm_score, opp_score, assists:hucksAttempted) %>%
  arrange(playerID, date)

write.csv(game_logs_all, paste0("/Users/ryandrost/Documents/R projects/AUDL_player_game_logs_", seasons, ".csv"), row.names = FALSE)

###########Add player game logs to complete dataframe################

pgl <- read.csv("AUDL_player_game_logs_all.csv")
pgl_df <- rbind(pgl, game_logs_all)
#run below line -- commented out to prevent accidental overwriting of file
#write.csv(pgl_df, "/Users/ryandrost/Documents/R projects/AUDL_player_game_logs_all.csv", row.names = FALSE)

###########Scrape TEAM GAME LOGS################

page_num <- 1
team_games <- list()
while(page_num < 1000) {
  webpage <- fromJSON(getURL(paste0("https://audl-stat-server.herokuapp.com/web-api/team-game-stats?limit=20&page=",
                                    page_num)))
  if(length(webpage[[1]]) == 0) {
    break
  } else {
    team_games[[page_num]] <- webpage[[1]]
    page_num <- page_num + 1
  }
}
team_game_logs <- bind_rows(team_games) %>%
  mutate(#tmScore = ,
         #oppScore = ,
         result = ifelse(tm_score > opp_score, 1, ifelse(tm_score == opp_score, 0.5, 0)),
         year = substr(gameID, 1, 4),
         date = substr(gameID, 1, 10),
         #tm = str_extract(gameID, "(?<=-)[[:alpha:]]*[[:alpha:]](?=-)"),
         #opp = sub(".*-", "", gameID)) %>%
  select(year, date, gameID, tm, opp, isHome, ) %>%
  arrange(playerID, date)
  
write.csv(team_game_logs, paste0("/Users/ryandrost/Documents/R projects/AUDL_team_game_logs_", seasons, ".csv"), row.names = FALSE)

###########Scrape current year team game logs################

tm_names <- team_game_logs %>% distinct(teamID) %>% pull()
tm_abbr <- pgl %>% distinct(tm) %>% pull

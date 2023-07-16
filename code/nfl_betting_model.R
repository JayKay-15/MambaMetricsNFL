library(tidyverse)
library(nflfastR)


#### MambaMetrics NFL Model Video ####
weekly <- calculate_series_conversion_rates(pbp, weekly = TRUE)

weekly <- weekly %>%
    select(season, week, team, off_scr, def_scr)


df_off <- pbp %>%
    filter(season_type == "REG" & play_type %in% c("run","pass")) %>%
    group_by(game_id, season, week, posteam, posteam_type) %>%
    summarise(off_int = sum(interception),
              off_fum = sum(fumble_lost),
              off_pass_yards = sum(passing_yards, na.rm = T),
              off_rushing_yards = sum(rushing_yards, na.rm = T),
              off_total_yards = sum(yards_gained, na.rm = T),
              off_sacks = sum(sack),
              off_sack_yards = sum(if_else(sack == 1, yards_gained, 0)),
              off_qb_epa = sum(if_else(qb_dropback == 1, qb_epa, 0)),
              off_plays = sum(n())) %>%
    left_join(weekly, by = c("season" = "season", "week" = "week", "posteam" = "team"))


df_def <- pbp %>%
    filter(season_type == "REG" & play_type %in% c("run","pass")) %>%
    group_by(game_id, defteam) %>%
    summarise(def_int = sum(interception),
              def_fum = sum(fumble_lost),
              def_pass_yards = sum(passing_yards, na.rm = T),
              def_rushing_yards = sum(rushing_yards, na.rm = T),
              def_total_yards = sum(yards_gained, na.rm = T),
              def_sacks = sum(sack),
              def_sack_yards = sum(if_else(sack == 1, yards_gained, 0)),
              def_qb_epa = sum(if_else(qb_dropback == 1, qb_epa, 0)),
              def_plays = sum(n())) 

df <- df_off %>%
    left_join(df_def, by = c("game_id" = "game_id", "posteam" = "defteam"))


schedule <- schedule %>%
    filter(game_type == "REG") %>%
    mutate(away_team = if_else(season < 2020 & away_team == "OAK", "LV", away_team),
           away_team = if_else(season < 2017 & away_team == "SD", "LAC", away_team),
           away_team = if_else(season < 2016 & away_team == "STL", "LA", away_team),
           home_team = if_else(season < 2020 & home_team == "OAK", "LV", home_team),
           home_team = if_else(season < 2017 & home_team == "SD", "LAC", home_team),
           home_team = if_else(season < 2016 & home_team == "STL", "LA", home_team)) %>%
    select(game_id, season, week, away_team, home_team, location, away_score, home_score, result, total,
           away_moneyline, home_moneyline, spread_line, total_line) %>%
    mutate(result = result*-1)



# game stats
data <- df %>%
    left_join(schedule[,c(1,4,7,9,11,13,14)], by = c("game_id" = "game_id", "posteam" = "away_team")) %>%
    left_join(schedule[,c(1,5,8,9,12,13,14)], by = c("game_id" = "game_id", "posteam" = "home_team")) %>%
    mutate(result = if_else(is.na(result.x), result.y, result.x),
           score = if_else(is.na(away_score), home_score, away_score),
           spread = if_else(is.na(spread_line.x), spread_line.y, spread_line.x),
           moneyline = if_else(is.na(away_moneyline), home_moneyline, away_moneyline),
           total = if_else(is.na(total_line.x), total_line.y, total_line.x)) %>%
    select(game_id:def_scr, result:total) %>%
    mutate(spread = if_else(moneyline < 0 & spread < 0, spread,
                            if_else(moneyline > 0 & spread > 0, spread,
                                    if_else(moneyline > 0 & spread < 0, spread*-1,
                                            if_else(moneyline < 0 & spread > 0, spread*-1, 0)))),
           result = if_else(posteam_type == "away", result, result*-1)
    )


# predictive stats
nfl_df <- data.frame()
w <- 2

for (i in unique(df$season)) {
    
    season_df <- df %>% filter(season == i)
    
    for (w in unique(season_df$week)) {
        
        week_df <- season_df %>% filter(week < w) %>%
            group_by(posteam) %>%
            summarise(across(c(off_int:def_plays), mean)) %>%
            mutate(season = i,
                   week = w) %>%
            select(season, week, posteam:def_plays) %>%
            mutate(across(c(off_int:def_plays), \(x) round(x, 2)))
        
        nfl_df <- bind_rows(nfl_df, week_df)
        
    }
    
}



nfl_final <- nfl_df %>%
    left_join(schedule[,c(1:4,7,9,11,13,14)], by = c("season" = "season", "week" = "week", "posteam" = "away_team")) %>%
    left_join(schedule[,c(1:3,5,8,9,12,13,14)], by = c("season" = "season", "week" = "week", "posteam" = "home_team")) %>%
    mutate(game_id = if_else(is.na(game_id.x), game_id.y, game_id.x)) %>%
    filter(!is.na(game_id)) %>%
    left_join(df[,c(1,4,5)], by = c("game_id" = "game_id", "posteam" = "posteam")) %>%
    mutate(result = if_else(is.na(result.x), result.y, result.x),
           score = if_else(is.na(away_score), home_score, away_score),
           spread = if_else(is.na(spread_line.x), spread_line.y, spread_line.x),
           moneyline = if_else(is.na(away_moneyline), home_moneyline, away_moneyline),
           total = if_else(is.na(total_line.x), total_line.y, total_line.x),
           spread = if_else(moneyline < 0 & spread < 0, spread, 
                            if_else(moneyline > 0 & spread > 0, spread,
                                    if_else(moneyline > 0 & spread < 0, spread*-1,
                                            if_else(moneyline < 0 & spread > 0, spread*-1, 0)))),
           result = if_else(posteam_type == "away", result, result*-1)) %>%
    select(game_id, season:posteam, posteam_type, off_int:def_plays, result:total)




# data <- schedule %>%
#     filter(game_type == "REG") %>%
#     select(game_id, season, week, away_team, home_team, away_score, home_score, result, total,
#            away_moneyline, home_moneyline, away_spread_odds, home_spread_odds, total_line) %>%
#     mutate(result = result*-1) %>%
#     left_join(df, by = c("season" = "season", "week" = "week",
#                          "away_team" = "posteam")) %>%
#     left_join(df, by = c("season" = "season", "week" = "week",
#                          "home_team" = "posteam")) %>%
#     left_join(weekly, by = c("season" = "season", "week" = "week",
#                              "away_team" = "team")) %>%
#     left_join(weekly, by = c("season" = "season", "week" = "week",
#                              "home_team" = "team"))

# colnames(data) <- gsub("\\.x$", "_away", colnames(data))
# colnames(data) <- gsub("\\.y$", "_home", colnames(data))


u <- paste0("/Users/Jesse/Documents/MyStuff/NFL Analysis/nfl.xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "game stats")
addWorksheet(wb, sheetName = "model stats")
writeData(wb, sheet = "game stats", x = data)
writeData(wb, sheet = "model stats", x = nfl_final)
saveWorkbook(wb, file = u)





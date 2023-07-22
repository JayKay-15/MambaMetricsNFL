library(tidyverse)
library(nflfastR)



nflfastR::load_pbp(2022) %>%
    dplyr::filter(season_type == "REG", complete_pass == 1 | incomplete_pass == 1 | interception == 1, !is.na(down)) %>%
    dplyr::group_by(passer_player_name, posteam) %>%
    dplyr::summarize(
        yards = sum(passing_yards, na.rm = T),
        tds = sum(touchdown == 1 & td_team == posteam),
        ints = sum(interception),
        att = dplyr::n()
    ) %>%
    dplyr::arrange(-yards) %>%
    utils::head(10) %>%
    knitr::kable(digits = 0)


nflfastR::load_pbp(2022) %>%
    dplyr::filter(season_type == "REG") %>%
    nflfastR::calculate_player_stats() %>%
    dplyr::arrange(-passing_yards) %>%
    dplyr::select(player_name, recent_team, completions, attempts, passing_yards, passing_tds, interceptions) %>%
    utils::head(10) %>%
    knitr::kable(digits = 0)


nflfastR::load_pbp(2022) %>%
    dplyr::filter(season_type == "REG") %>%
    nflfastR::calculate_player_stats() %>%
    dplyr::arrange(-rushing_yards) %>%
    dplyr::select(player_name, recent_team, carries, rushing_yards, rushing_tds, rushing_fumbles_lost) %>%
    utils::head(10) %>%
    knitr::kable(digits = 0)


nflfastR::load_pbp(2022) %>%
    dplyr::filter(season_type == "REG", !is.na(down)) %>%
    dplyr::group_by(fantasy_player_name, posteam) %>%
    dplyr::summarize(
        carries = sum(rush_attempt),
        receptions = sum(complete_pass),
        touches = sum(rush_attempt + complete_pass),
        yards = sum(yards_gained),
        tds = sum(touchdown == 1 & td_team == posteam)
    ) %>%
    dplyr::arrange(-yards) %>%
    utils::head(10) %>%
    knitr::kable(digits = 0)

nflfastR::load_pbp(2022) %>%
    dplyr::filter(season_type == "REG") %>%
    nflfastR::calculate_player_stats() %>%
    dplyr::mutate(
        yards = rushing_yards + receiving_yards, 
        touches = carries + receptions, 
        tds = rushing_tds + receiving_tds
    ) %>%
    dplyr::arrange(-yards) %>%
    dplyr::select(player_name, recent_team, carries, receptions, touches, yards, tds) %>%
    utils::head(10) %>%
    knitr::kable(digits = 0)

nflfastR::load_player_stats(seasons = 2022) %>%
    dplyr::filter(season_type == "REG") %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarize(
        player_name = dplyr::first(player_name),
        recent_team = dplyr::first(recent_team),
        yards = sum(rushing_yards + receiving_yards),
        touches = sum(carries + receptions),
        carries = sum(carries),
        receptions = sum(receptions),
        tds = sum(rushing_tds + receiving_tds)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-yards) %>%
    dplyr::select(player_name, recent_team, carries, receptions, touches, yards, tds) %>%
    utils::head(10) %>%
    knitr::kable(digits = 0)


nflfastR::load_pbp(2022) %>%
    dplyr::filter(week <= 17) %>%
    nflfastR::calculate_player_stats() %>%
    dplyr::mutate(
        ppg = fantasy_points_ppr / games
    ) %>%
    dplyr::filter(games > 5) %>%
    # only keep the WRs
    dplyr::inner_join(
        nflfastR::fast_scraper_roster(2022) %>% 
            dplyr::filter(position == "WR") %>% 
            dplyr::select(player_id = gsis_id),
        by = "player_id"
    ) %>%
    dplyr::arrange(-ppg) %>%
    dplyr::select(player_name, recent_team, games, fantasy_points_ppr, ppg) %>%
    utils::head(10) %>%
    knitr::kable(digits = 1)


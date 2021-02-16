install.packages("tidyverse")
install.packages("rvest")
install.packages("janitor")

#Function to obtain advanced, per minute, total players stats from any season
scrape_stats <- function(season = 2020) {
  url <-
    paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_advanced.html")
  stats <- url %>% read_html() %>% html_table() %>% .[[1]]
#Advanced Player Stats
  player_stats_adv <- stats %>%
    remove_empty() %>% #removes any empty columns
    clean_names() %>% #converts columns to snake case -> all lower case, spaces turned into underscores, removes '%' sign
    filter(!player == "Player") %>%
    mutate_at(vars(-c(player, tm, pos)), as.numeric) %>%
    mutate_at(vars(-c(player, tm, pos)), funs(replace(., is.na(.), 0))) %>%
    as_tibble() %>%
    group_by(player) %>%
    slice(1) %>% #remove duplicate rows for players who switched teams (ex: Trevor Ariza)
    ungroup() %>%
    select(-rk)
  
#Total
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html")
  stats_tot <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  #clean
  player_stats_tot <- stats_tot %>% 
    remove_empty() %>%
    clean_names() %>% 
filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)

#Per Minute
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_minute.html")
  stats_pm <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_pm <- stats_pm %>% 
    remove_empty() %>%
    clean_names() %>% 
filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename_at(vars(9:29),funs(paste0(.,"_pm"))) %>% 
    select(-rk)
  
  player_stats <- full_join(player_stats_tot,player_stats_pm,
                            by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>% 
    full_join(player_stats_adv,
              by = c("player", "pos", "age", "tm", "g", "mp"))
  return(player_stats)
}
a<- scrape_stats(season=2019)
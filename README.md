# Premier-League-Matches-Analysis
Project Analyzing last 5 years of Premier League Matches

library(ggplot2)
library(readr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)

# Import Dataset
matches <- read_csv("matches.csv")

# Check Integrity of the data / Data Sanity check
matches %>% 
  group_by(season) %>% 
  count()

matches %>% 
  group_by(season) %>%
  summarise(No_of_teams = n_distinct(team))

# Unusual number of matches and teams in 2023 and 2024 - Data Cleaning is needed

#Clean Data based on date of the match and recalculate Season
matches_clean <- matches %>% 
  mutate(Date_Calc = ymd(date),
         season_calc = case_when(Date_Calc >= "2019-08-09" & Date_Calc <= "2020-07-26" ~ "19-20",
                                 Date_Calc >= "2020-09-12" & Date_Calc <= "2021-05-23" ~ "20-21",
                                 Date_Calc >= "2021-08-13" & Date_Calc <= "2022-05-22" ~ "21-22",
                                 Date_Calc >= "2022-08-05" & Date_Calc <= "2023-05-28" ~ "22-23",
                                 Date_Calc >= "2023-08-11" & Date_Calc <= "2024-05-24" ~ "23-24",
                                 TRUE ~ "-")) %>%
  select(-c(date,season)) %>%
  distinct(Date_Calc,season_calc,team,opponent,.keep_all = TRUE)

matches_clean %>% 
  group_by(season_calc) %>% 
  count() %>%
  arrange(desc(n))

# Figure Out Champion
matches_clean %>% 
  # Calculate points based on the result
  mutate(Pts = case_when(
    result == "W" ~ 3,
    result == "D" ~ 1,
    result == "L" ~ 0
  )) %>% 
  # Group by team and season_calc to calculate total points per team per season
  group_by(team, season_calc) %>% 
  summarise(Pts = sum(Pts), .groups = "drop") %>%
  # Group by season_calc and get the top team by points for each season
  group_by(season_calc) %>%
  slice_max(Pts, n = 1, with_ties = FALSE) %>% 
  arrange(season_calc)  # Arrange by season


# Figure Out Relegation
matches_clean %>% 
  # Calculate points based on the result
  mutate(Pts = case_when(
    result == "W" ~ 3,
    result == "D" ~ 1,
    result == "L" ~ 0
  )) %>% 
  # Group by team and season_calc to calculate total points per team per season
  group_by(team, season_calc) %>% 
  summarise(Pts = sum(Pts), .groups = "drop") %>%
  # Group by season_calc and get the top team by points for each season
  group_by(season_calc) %>%
  slice_min(Pts, n = 3, with_ties = FALSE) %>% 
  arrange(season_calc, desc(Pts))  # Arrange by points in descending order

# Figure out which teams win the most with X referee
matches_clean %>% 
  filter(result == "W") %>%
  group_by(team,referee,result,season_calc) %>% 
  count() %>%
  # Group by season_calc and get the top team by points for each season
  group_by(season_calc) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>% 
  arrange(season_calc, desc(n))

# CMU SURE 2024 Final Project
# Football
# Name: Amelia Yixin Yuan


# I. Data Preprocessing
# 6/20
# install.packages("nflreadr")
# install.packages("nflreadr", repos = c("https://nflverse.r-universe.dev", getOption("repos")))

# or use remotes/devtools
# install.packages("remotes")
# remotes::install_github("nflverse/nflreadr")

library(nflreadr)

# 1. Play-by-play data via nflreadR
pbp_data <- load_pbp()

str(pbp_data)
head(pbp_data)
summary(pbp_data)

colnames(pbp_data)

# 2. 2019-2023 Batted Passes Dataset
# install.packages("readxl")
library(readxl)
getwd()
excel_file_path <- "/Users/amelia/Desktop/Sports Project Yixin/2019-2023 Batted Passes.xlsx"
pass_data <- read_excel(excel_file_path)

str(pass_data)
head(pass_data)
summary(pass_data)

colnames(pass_data)

# 3. integrate
library(dplyr)
# Check if two datasets label "week, home team, away team, and play id" the same as each other
print(unique(pass_data$WEEK))
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
print(unique(pbp_data$week))
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
print(unique(pass_data$HOME))
# [1] "CHI" "IND" "KC"  "LAC" "MIN" "NE"  "NO"  "SEA" "WAS" "ARI" "ATL" "BUF" "CAR" "CIN"
# [15] "DEN" "DET" "JAX" "LAR" "PIT" "TB"  "TEN" "BAL" "GB"  "MIA" "NYJ" "SF"  "CLE" "DAL"
# [29] "NYG" "PHI" "LV"  "HOU" "OAK"
print(unique(pbp_data$home_team))
# [1] "WAS" "NYJ" "ATL" "CLE" "NYG" "KC"  "CHI" "BAL" "IND" "SEA" "DEN" "LAC" "NE"  "PIT"
# [15] "MIN" "NO"  "CIN" "TB"  "HOU" "JAX" "TEN" "BUF" "PHI" "CAR" "ARI" "DAL" "DET" "LA" 
# [29] "MIA" "GB"  "SF"  "LV" 

print(unique(pass_data$AWAY))
# [1] "GB"  "JAX" "DET" "MIA" "TB"  "PHI" "TEN" "LAR" "ARI" "NYG" "LV"  "NO"  "BAL" "WAS"
# [15] "SEA" "KC"  "SF"  "CLE" "CHI" "LAC" "DAL" "IND" "ATL" "DEN" "NE"  "CAR" "BUF" "MIN"
# [29] "CIN" "HOU" "NYJ" "PIT" "OAK"
print(unique(pbp_data$away_team))
# [1] "ARI" "BUF" "CAR" "CIN" "DAL" "DET" "GB"  "HOU" "JAX" "LA"  "LV"  "MIA" "PHI" "SF" 
# [15] "TB"  "TEN" "BAL" "CHI" "CLE" "IND" "KC"  "LAC" "MIN" "NO"  "NYG" "NYJ" "SEA" "WAS"
# [29] "ATL" "DEN" "NE"  "PIT"
print(unique(pass_data[["PLAY ID"]]))
# numbers
print(unique(pbp_data$play_id))


# Findings
# pbp_data: LA
# pass_data: LAR

# Replace the "LA" in pbp_data with "LAR"
pbp_data <- pbp_data |>
  mutate(across(c(home_team, away_team), ~str_replace_all(., "\\bLA\\b", "LAR")))
# Check
print(unique(pbp_data$home_team))
#  [1] "WAS" "NYJ" "ATL" "CLE" "NYG" "KC"  "CHI" "BAL" "IND" "SEA" "DEN" "LAC" "NE"  "PIT"
# [15] "MIN" "NO"  "CIN" "TB"  "HOU" "JAX" "TEN" "BUF" "PHI" "CAR" "ARI" "DAL" "DET" "LAR"
# [29] "MIA" "GB"  "SF"  "LV"
print(unique(pbp_data$away_team))
#  [1] "ARI" "BUF" "CAR" "CIN" "DAL" "DET" "GB"  "HOU" "JAX" "LAR" "LV"  "MIA" "PHI" "SF" 
# [15] "TB"  "TEN" "BAL" "CHI" "CLE" "IND" "KC"  "LAC" "MIN" "NO"  "NYG" "NYJ" "SEA" "WAS"
# [29] "ATL" "DEN" "NE"  "PIT"


library(dplyr)
library(stringr)
pbp_data <- pbp_data %>%
  mutate(across(where(is.character), ~str_replace_all(., "\\bLA\\b", "LAR")))

batted_passes_data <- inner_join(pbp_data, pass_data,
                                      by = c("week" = "WEEK", 
                                             "home_team" = "HOME", 
                                             "away_team" = "AWAY", 
                                             "play_id" = "PLAY ID"))
batted_passes_data_include_complete <- batted_passes_data

## filter out the tipped passes that were caught
colnames(batted_passes_data)
unique(batted_passes_data$complete_pass) # [1] 0 1

batted_passes_data <- batted_passes_data |>
  filter(complete_pass != 1)  
colnames(batted_passes_data)
head(batted_passes_data)

# II. EDA
players_data <- nflreadr::load_players()
colnames(players_data)
# 1. QB and defender height
# get height
head(players_data$gsis_id) # Game Statistics and Information System
head(batted_passes_data$passer_player_id)

unique(players_data$position)

players_data |>
  filter(status == "ACT") # != RET
  
qb_height <- players_data |>
  filter(position == "QB") |>
  select(gsis_id, height)

defender_positions <- c("CB", "DT", "DE", "LB", "MLB", "OLB", "ILB", "FS", "SS", "SAF", "DB", "S", "NT", "DL")

defender_data <- players_data[players_data$position %in% defender_positions, ]
head(defender_data)
defender_height <- defender_data |>
  select(gsis_id, height)
head(defender_height)

# join data

batted_passes_data <- batted_passes_data %>%
  left_join(qb_height, by = c("passer_player_id" = "gsis_id")) %>%
  rename(qb_height = height)  

batted_passes_data <- batted_passes_data %>%
  left_join(defender_height, by = c("passer_player_id" = "gsis_id")) %>%
  rename(defender_height2 = height)

ggplot(batted_passes_data, aes(x = qb_height)) +
  geom_histogram(fill = "midnightblue") +
  labs(title = "Batted Passes by QB Height", x = "Height", y = "Count")

batted_passes_data_include_complete <- batted_passes_data_include_complete |>
  left_join(qb_height, by = c("passer_player_id" = "gsis_id")) %>%
  rename(qb_height = height)  

colnames(batted_passes_data_include_complete)

ggplot(batted_passes_data_include_complete, aes(x = qb_height)) +
  geom_histogram(fill = "gold") +
  labs(title = "QB Height", x = "Height", y = "Count")

ggplot(batted_passes_data, aes(x = defender_height2)) +
  geom_histogram(fill = "midnightblue", bins = 50) +
  labs(title = "Batted Passes by QB Height", x = "Height", y = "Count")

ggplot(batted_passes_data, aes(x = qb_height)) +
  geom_density(fill = "lightblue", alpha = 0.5) 


# 6/27
# put two plot together
# ecds

# individual QB what to throw
# season level

# Numbers for teams across different seasons
library(dplyr)
library(ggplot2)

colnames(batted_passes_data)

batted_passes_data$year <- format(as.Date(batted_passes_data$game_date, format="%Y-%m-%d"), "%Y")

print(unique(batted_passes_data$year))
# 2023；2024

print(unique(batted_passes_data$season_type))
# regular; post

batted_passes_summary <- batted_passes_data |>
  group_by(year, season_type, posteam) |>
  summarise(batted_passes_count = n(), .groups = 'drop')


ggplot(batted_passes_summary, aes(x = year, y = batted_passes_count, fill = season_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~posteam) +
  labs(title = "Batted Passes by Team Across Years and Season Types",
       x = "Year",
       y = "Number of Batted Passes",
       fill = "Season Type") +
  theme_minimal() +
  scale_fill_manual(values = c("REG" = "midnightblue", "POST" = "gold")) 

# check 2023
data_2023 <- batted_passes_data %>% 
  filter(year == "2023")
season_passes_summary_2023 <- data_2023 %>%
  group_by(season_type) %>%
  summarise(total_batted_passes_2023 = n(), .groups = 'drop')
print(season_passes_summary_2023)
# REG                319

# check 2024
data_2024 <- batted_passes_data %>% 
  filter(year == "2024")
season_passes_summary_2024 <- data_2024 %>%
  group_by(season_type) %>%
  summarise(total_batted_passes_2024 = n(), .groups = 'drop')
print(season_passes_summary_2024)
# POST                              17
# REG                               15









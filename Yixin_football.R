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

















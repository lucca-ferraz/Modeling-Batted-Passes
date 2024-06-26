# CMU SURE 2024 Final Project
# Football
# Name: Amelia Yixin Yuan


# I. Data Preprocessing
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
batted_pass_data <- read_excel(file_path)

str(batted_pass_data)
head(batted_pass_data)
summary(batted_pass_data)

colnames(batted_pass_data)

# 3. integrate
library(dplyr)
team_batted_passes_data <- inner_join(pbp_data, batted_pass_data,
                                      by = c("week" = "WEEK", 
                                             "home_team" = "HOME", 
                                             "away_team" = "AWAY", 
                                             "play_id" = "PLAY ID"))

## filter out the tipped passes that were caught
colnames(team_batted_passes_data)
unique(team_batted_passes_data$complete_pass) # [1] 0 1

team_batted_passes_data <- team_batted_passes_data |>
  filter(complete_pass != 1)  

head(team_batted_passes_data)

# II. EDA

































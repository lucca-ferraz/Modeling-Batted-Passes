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
install.packages("nflreadr")
library(nflreadr)

# 1. Play-by-play data via nflreadR
pbp_data <- load_pbp(seasons = c(2019, 2020, 2021, 2022, 2023))
dim(pbp_data)
unique(pbp_data$season)
# [1] 2019 2020 2021 2022 2023
pbp_data_2023 <- load_pbp(2023)

dim(pbp_data_2023)
unique(pbp_data_2023$season)
# [1] 2023
unique(pbp_data$season)
str(pbp_data)
head(pbp_data)
summary(pbp_data)

colnames(pbp_data)

pbp_data$is_batted

# 2. 2019-2023 Batted Passes Dataset
# install.packages("readxl")
library(readxl)
getwd()
excel_file_path <- "/Users/amelia/Desktop/Sports Project Yixin/2019-2023 Batted Passes.xlsx"
pass_data <- read_excel(excel_file_path)

pass_data$player
str(pass_data)
head(pass_data)
summary(pass_data)

colnames(pass_data)

pass_data$is_batted <- 1

all_plays2023 <- nflreadr::load_pbp()
library(tidyverse)
excel_simple <- pass_data |> 
  mutate(across(c("HOME", "AWAY"), ~ str_replace(
    string = .x, pattern = "LAR", replacement = "LA"))) |> 
  select(week = WEEK, home_team = HOME, away_team = AWAY, play_id = `PLAY ID`, is_batted)

# 2023
all_plays2023 <- left_join(all_plays2023, excel_simple)

library(tidyr)
library(dplyr)
all_plays2023$is_batted <- all_plays2023$is_batted |> 
  replace_na(0)
pass_plays2023 <- all_plays2023 |> 
  filter(play_type == "pass")
sum(pass_plays2023$is_batted)

pass_plays2023$is_batted
pass_plays2023$qb_location

sum(pass_plays2023$is_batted)

# 2019 - 2023
pbp_data <- pbp_data |> filter(play_type == "pass")
pbp_data <- left_join(pbp_data, excel_simple)
pbp_data$is_batted <- pbp_data$is_batted |> 
  replace_na(0)
sum(pbp_data$is_batted)

print(unique(pbp_data$home_team))

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
pbp_data_qb_height <- pbp_data |>
  filter(is_batted == 1) |>
  left_join(qb_height, by = c("passer_player_id" = "gsis_id")) %>%
  rename(qb_height = height)  

batted_passes_data <- batted_passes_data %>%
  left_join(qb_height, by = c("passer_player_id" = "gsis_id")) %>%
  rename(qb_height = height)  

# Inspecting the unique IDs in both datasets
unique_ids_passes <- unique(batted_passes_data$passer_player_id)
unique_ids_defenders <- unique(defender_height$gsis_id)

# Checking if there is an intersection
length(intersect(unique_ids_passes, unique_ids_defenders))
# 0

batted_passes_data <- batted_passes_data %>%
  left_join(defender_height, by = c("passer_player_id" = "gsis_id")) %>%
  rename(defender_height2 = height)
batted_passes_data$defender_height2

library(ggplot2)
ggplot(pbp_data_qb_height, aes(x = qb_height)) +
  geom_histogram(fill = "midnightblue") +
  labs(title = "Batted Passes by QB Height", x = "Height", y = "Count")

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


library(dplyr)

# Assuming both data frames are already loaded and have a 'qb_height' column
# Add a new column to each dataframe to mark the type
batted_passes_data$Dataset <- "Only Batted Passes"
batted_passes_data_include_complete$Dataset <- "All Passes"

# Combine the datasets
# Identify missing columns in each dataset
missing_in_first <- setdiff(names(batted_passes_data_include_complete), names(batted_passes_data))
missing_in_second <- setdiff(names(batted_passes_data), names(batted_passes_data_include_complete))

# Add missing columns with NA values to each dataset
for(col in missing_in_first) {
  batted_passes_data[[col]] <- NA
}
for(col in missing_in_second) {
  batted_passes_data_include_complete[[col]] <- NA
}

# Now bind the rows
combined_data <- rbind(batted_passes_data, batted_passes_data_include_complete)

# Check the combined data structure
str(combined_data)

library(ggplot2)

ggplot(combined_data, aes(x = qb_height, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  scale_fill_manual(values = c("gold", "midnightblue")) +
  labs(title = "Comparison of QB Heights Influence (2019-2023)",
       subtitle = "Including vs Excluding Complete Passes",
       x = "QB Height",
       y = "Count") +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold")  # Making the title bold
  )

ggplot(combined_data, aes(x = qb_height)) +
  geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
  facet_wrap(~ Dataset) +
  labs(title = "Comparison of QB Heights",
       subtitle = "Separated by Data Subset",
       x = "QB Height",
       y = "Count") +
  theme_minimal()


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
# "2020" "2021" "2022" "2023" "2024"
print(unique(pass_data$YEAR))
# [1] 2023 2022 2021 2020 2019
print(unique(pbp_data$game_date))

print(unique(batted_passes_data$season_type))
# regular; post

print(unique(batted_passes_data$season))
# [1] 2019 2020 2021 2022 2023

batted_passes_summary <- batted_passes_data |>
  group_by(season, posteam) |>
  summarise(batted_passes_count = n(), .groups = 'drop')


ggplot(batted_passes_summary, aes(x = season, y = batted_passes_count, fill = season)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~posteam) +
  labs(title = "Batted Passes by Team Across Seasons",
       x = "Season",
       y = "Number of Batted Passes",
       fill = "Season") +
  theme_minimal() #+
#  scale_fill_manual(values = c("REG" = "midnightblue", "POST" = "gold")) 

# check 2023
data_2023 <- batted_passes_data %>% 
  filter(year == "2023")
season_passes_summary_2023 <- data_2023 %>%
  group_by(season_type) %>%
  summarise(total_batted_passes_2023 = n(), .groups = 'drop')
print(season_passes_summary_2023)
# REG                338

# check 2024
data_2024 <- batted_passes_data %>% 
  filter(year == "2024")
season_passes_summary_2024 <- data_2024 %>%
  group_by(season_type) %>%
  summarise(total_batted_passes_2024 = n(), .groups = 'drop')
print(season_passes_summary_2024)
# POST                              18
# REG                               15
 
# percentage

# Teams
# Each team each year, percentage of batted passes, line, offensive/defensive
print(unique(pbp_data$is_batted))
colnames(pbp_data)
batted_passes_summary <- pbp_data |>
  filter(is_batted == 1) |>
  group_by(season, posteam) |>
  summarise(batted_passes_count = n(), .groups = 'drop')

total_passes <- pbp_data |>
  group_by(season, posteam) |>
  summarise(total_passes_count = n(), .groups = 'drop')
total_passes

batted_passes_summary <- merge(batted_passes_summary, total_passes, by = c("season", "posteam"))

# Calculate the percentage
batted_passes_summary <- batted_passes_summary %>%
  mutate(percentage_batted_passes = (batted_passes_count / total_passes_count) * 100)

batted_passes_summary

ggplot(batted_passes_summary, aes(x = as.factor(season), y = percentage_batted_passes, group = posteam)) +
  geom_line(aes(color = posteam), size = 1) +
  geom_point(aes(color = posteam), size = 2) +
  facet_wrap(~posteam, ncol = 4) +
  labs(
    title = "Percentage of Batted Passes by Team Across Seasons",
    x = "Season",
    y = "Percentage of Batted Passes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

# Players
# Percentage of Batted Passes by QB Players Across Seasons

# 1. Cleaning players data
players_data |>
  filter(status == "ACT") # != RET

qb_names <- players_data |>
  filter(position == "QB") |>
  select(gsis_id, display_name)

# 2. Join dataset
# join with batted_passes_data_include_complete to get total passes count
total_passes_data_qb_names <- pbp_data |>
  left_join(qb_names, by = c("passer_player_id" = "gsis_id")) |>
  rename(qb_name = display_name)  

print(unique(total_passes_data_qb_names$season))

# join with batted_passes_data to get batted passes count
batted_passes_data <- pbp_data |>
  filter(is_batted == 1)

batted_passes_data_qb_names <- batted_passes_data |>
  left_join(qb_names, by = c("passer_player_id" = "gsis_id")) |>
  rename(qb_name = display_name)  

# 3. count
total_passes_qb <- total_passes_data_qb_names |>
  group_by(season, qb_name) |>
  summarise(total_count_qb = n(), .groups = 'drop')
total_passes_qb

batted_passes_qb <- batted_passes_data_qb_names |>
  group_by(season, qb_name) |>
  summarise(batted_count_qb = n(), .groups = 'drop')
batted_passes_qb

batted_passes_summary_qb <- merge(batted_passes_qb, total_passes_qb,
                                  by = c("season", "qb_name"))
batted_passes_summary_qb

# 4. calculate the percentage
batted_passes_summary_qb <- batted_passes_summary_qb |>
  mutate(percentage_batted_passes_qb = (batted_count_qb / total_count_qb) * 100)

batted_passes_summary_qb

# 5. plot
ggplot(batted_passes_summary_qb, aes(x = as.factor(season), 
                                     y = percentage_batted_passes_qb, 
                                     group = qb_name)) +
  geom_bar(stat = "identity", aes(fill = qb_name), position = "dodge", alpha = 0.7) +  # 绘制柱状图
  
  geom_line(aes(color = qb_name), size = 1) +
  geom_point(aes(color = qb_name), size = 2) +
  facet_wrap(~qb_name, ncol = 10) +
  labs(
    title = "Percentage of Batted Passes by QB Players Across Seasons",
    x = "Season",
    y = "Percentage of Batted Passes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

###########################################################################
# Percentage of Batted Passes by Top 10 QB Players Across Seasons
# calculate the percentage of batted passes of each QB player

overall_batted_passes <- batted_passes_summary_qb %>%
  group_by(qb_name) %>%
  summarise(average_percentage_batted = mean(percentage_batted_passes_qb), .groups = 'drop')

# top 10
top_qbs <- overall_batted_passes %>%
  top_n(10, average_percentage_batted) %>%
  pull(qb_name)

top_qbs

# [1] "Anthony Richardson" "Blaine Gabbert"     "Brett Rypien"      
# [4] "Clayton Tune"       "Drew Lock"          "Gardner Minshew"   
# [7] "Jeff Driskel"       "Marcus Mariota"     "Taysom Hill"       
# [10] "Trevor Siemian"

# filter
top_qb_data <- batted_passes_summary_qb %>%
  filter(qb_name %in% top_qbs)

print(unique(top_qb_data$season))
ggplot(top_qb_data, aes(x = as.factor(season), y = percentage_batted_passes_qb, group = qb_name, color = qb_name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Batted Passes by Top 10 QB Players Across Seasons",
    x = "Season",
    y = "Percentage of Batted Passes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = rainbow(10))  

###########################################################################
# Relationship Between Top QBs' and Their Teams' Percentage of Batted Passes
colnames(batted_passes_summary_qb)
colnames(top_qb_data)
pbp_data_with_names <- pbp_data %>%
  left_join(qb_names, by = c("passer_player_id" = "gsis_id"))
colnames(pbp_data_with_names)
# Extract necessary QB data with team information
qb_batted_passes_summary <- pbp_data_with_names %>%
  select(season, qb_name = display_name, posteam, is_batted) %>%
  group_by(season, qb_name, posteam) %>%
  summarise(batted_passes_qb = sum(is_batted == 1),
            total_passes_qb = n(), .groups = 'drop') %>%
  mutate(percentage_batted_passes_qb = (batted_passes_qb / total_passes_qb) * 100)

# Assuming pbp_data contains the necessary fields
team_batted_passes_summary <- pbp_data %>%
  group_by(season, posteam) %>%
  summarise(total_passes_team = n(),
            batted_passes_team = sum(is_batted == 1), .groups = 'drop') %>%
  mutate(percentage_batted_passes_team = (batted_passes_team / total_passes_team) * 100)

# Join the enhanced QB data with the team data
# Correct Join to Include QB Data
combined_data <- qb_batted_passes_summary %>%
  left_join(team_batted_passes_summary, by = c("season", "posteam"))

# Check the columns in the combined dataset
print(colnames(combined_data))

colnames(pbp_data)
colnames(players_data)
# Check columns in QB data just before the join
print(colnames(qb_batted_passes_summary))


# Identify top 10 QBs based on their average batted passes percentage
top_qbs <- qb_batted_passes_summary %>%
  group_by(qb_name) %>%
  summarise(average_percentage_batted = mean(percentage_batted_passes_qb), .groups = 'drop') %>%
  top_n(10, average_percentage_batted) %>%
  pull(qb_name)
top_qbs
# Filter the combined data for these top QBs
# Filter the combined data for these top QBs
top_qb_team_data <- combined_data %>%
  filter(qb_name %in% top_qbs)

# Check results
print(head(top_qb_team_data))

# Calculate average percentages for further insights
average_stats <- top_qb_team_data %>%
  group_by(qb_name) %>%
  summarise(
    avg_percentage_qb = mean(percentage_batted_passes_qb, na.rm = TRUE),
    avg_percentage_team = mean(percentage_batted_passes_team, na.rm = TRUE)
  )
print(average_stats)
library(ggplot2)

# Plotting to visualize the relationship
ggplot(top_qb_team_data, aes(x = percentage_batted_passes_qb, y = percentage_batted_passes_team, color = qb_name)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Linear regression line
  labs(
    title = "Relationship Between Top QBs' and Their Teams' Percentage of Batted Passes",
    x = "QB's Percentage of Batted Passes",
    y = "Team's Percentage of Batted Passes"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Use a nice color palette

###########################################################################
# Relationship Between Last QBs' and Their Teams' Percentage of Batted Passes
last_qbs <- qb_batted_passes_summary %>%
  group_by(qb_name) %>%
  summarise(average_percentage_batted = mean(percentage_batted_passes_qb), .groups = 'drop') %>%
  slice_min(order_by = average_percentage_batted, n = 10) %>%
  pull(qb_name)
last_qbs
# Filter the combined data for these top QBs
# Filter the combined data for these top QBs
last_qb_team_data <- combined_data %>%
  filter(qb_name %in% last_qbs)

# Check results
print(head(last_qb_team_data))

# Calculate average percentages for further insights
average_stats <- last_qb_team_data %>%
  group_by(qb_name) %>%
  summarise(
    avg_percentage_qb = mean(percentage_batted_passes_qb, na.rm = TRUE),
    avg_percentage_team = mean(percentage_batted_passes_team, na.rm = TRUE)
  )
print(average_stats)
library(ggplot2)

# Plotting to visualize the relationship
ggplot(last_qb_team_data, aes(x = percentage_batted_passes_qb, y = percentage_batted_passes_team, color = qb_name)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Linear regression line
  labs(
    title = "Relationship Between Last QBs' and Their Teams' Percentage of Batted Passes",
    x = "QB's Percentage of Batted Passes",
    y = "Team's Percentage of Batted Passes"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") 

###########################################################################
library(dplyr)
# 10 QB Players with Least Percentage of Batted Passes  Across Seasons
last_qbs <- overall_batted_passes %>%
  slice_min(order_by = average_percentage_batted, n = 10) %>%
  pull(qb_name)

last_qbs_all <- overall_batted_passes %>%
  slice_min(order_by = average_percentage_batted, n = 40) %>%
  pull(qb_name, average_percentage_batted)
last_qbs_all
last_qbs
# [1] NA             "Kirk Cousins" "Joe Flacco"   "Joshua Dobbs" "Jordan Love"  "Tyrod Taylor"
# [7] "Bryce Young"  "Derek Carr"   "Daniel Jones" "Easton Stick"

# filter
last_qb_data <- batted_passes_summary_qb %>%
  filter(qb_name %in% last_qbs)

print(unique(last_qb_data$season))
ggplot(last_qb_data, aes(x = as.factor(season), y = percentage_batted_passes_qb, group = qb_name, color = qb_name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Batted Passes by Last 10 QB Players Across Seasons",
    x = "Season",
    y = "Percentage of Batted Passes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = rainbow(10))  

###########################################################################

### Defensive
library(dplyr)
library(ggplot2)

defender_positions <- c("CB", "DT", "DE", "LB", "MLB", "OLB", "ILB", "FS", "SS", "SAF", "DB", "S", "NT", "DL")
colnames(pbp_data)
# integrate
colnames(players_data)
players_data |>
  filter(status == "ACT") # != RET

players_data |>
  filter(status == "ACT") # != RET

defenders <- players_data |>
  filter(position %in% defender_positions) |>
  select(gsis_id, display_name, position)

print(unique(defenders$display_name))

total_passes_data_df_names <- pbp_data |>
  left_join(defenders, by = c("passer_player_id" = "gsis_id")) |>
  rename(defender_name = display_name)  
total_passes_data_df_names
head(pbp_data$passer_player_id)
head(players_data$gsis_id)
print(unique(total_passes_data_df_names$defender_name))

common_ids <- intersect(players_data$gsis_id, pbp_data$passer_player_id)
num_common_ids <- length(common_ids)
num_common_ids

# join with batted_passes_data to get batted passes count
batted_passes_data <- pbp_data |>
  filter(is_batted == 1)

batted_passes_data_df_names <- batted_passes_data |>
  left_join(defenders, by = c("passer_player_id" = "gsis_id")) |>
  rename(defender_name = display_name)  
print(unique(batted_passes_data_df_names$defender_name))

# 3. count

total_passes_df <- total_passes_data_df_names |>
  group_by(season, defender_name) |>
  summarise(total_count_df = n(), .groups = 'drop')
total_passes_df

colnames(total_passes_data_df_names)
colnames(batted_passes_data_df_names)
batted_passes_df <- batted_passes_data_df_names |>
  group_by(season, defender_name) |>
  summarise(batted_count_df = n(), .groups = 'drop')
batted_passes_df
print(unique(batted_passes_df$defender_name))
colnames(batted_passes_df)
colnames(total_passes_df)
batted_passes_summary_df <- merge(batted_passes_df, total_passes_df,
                                  by = c("season", "defender_name"))
batted_passes_summary_df
print(unique(batted_passes_summary_df$defender_name))

# 4. calculate the percentage
batted_passes_summary_df <- batted_passes_summary_df |>
  mutate(percentage_batted_passes_qb = (batted_count_df / total_count_df) * 100)

batted_passes_summary_df
print(unique(batted_passes_summary_df$defender_name))
##
total_passes_defenders <- pbp_data |>
  group_by(season, defenders) |>
  left_join(defenders, by = c("passer_player_id" = "gsis_id")) |>
  rename(defender_name = display_name)  |>
  summarise(total_count_defenders = n(), .groups = 'drop')
total_passes_defenders

batted_passes_data_defenders_names <- pbp_data |>
  filter(is_batted == 1) |>
  left_join(defenders, by = c("passer_player_id" = "gsis_id")) |>
  rename(defender_name = display_name)  

batted_passes_df <- batted_passes_data_defenders_names |>
  group_by(season, defender_name) |>
  summarise(batted_count_df = n(), .groups = 'drop')
batted_passes_df

batted_passes_summary_df <- merge(batted_passes_df, total_passes_defenders,
                                         by = c("season", "defender_name"))
batted_passes_summary_df

# 4. calculate the percentage
batted_passes_summary_df <- batted_passes_summary_df |>
  mutate(percentage_batted_passes_df = (batted_count_df / total_count_defenders) * 100)

batted_passes_summary_df


combined_summary <- bind_rows(batted_passes_summary_df, batted_passes_summary_qb)
colnames(combined_summary)
library(ggplot2)
ggplot(batted_passes_summary_df, aes(x = as.factor(season), 
                                     y = percentage_batted_passes_qb, 
                                     group = defender_name)) +
  geom_bar(stat = "identity", aes(fill = defender_name), position = "dodge", alpha = 0.7) +  
  
  geom_line(aes(color = defender_name), size = 1) +
  geom_point(aes(color = defender_name), size = 2) +
  facet_wrap(~defender_name, ncol = 10) +
  labs(
    title = "Percentage of Batted Passes by Defensive Players Across Seasons",
    x = "Season",
    y = "Percentage of Batted Passes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 


#################################################################################


#################################################################################

# Modeling

# FTN dataset
ftn_charting <- nflreadr::load_ftn_charting(seasons = c(2022, 2023))
ftn_charting_clean <- ftn_charting |> 
  select(game_id = nflverse_game_id, play_id = nflverse_play_id, 
         starting_hash:n_pass_rushers, -is_qb_sneak)

pass_plays20222023 <- load_pbp(seasons = c(2022, 2023)) |> 
  filter(play_type == "pass")

pass_plays20222023 <- left_join(pass_plays20222023, excel_simple)

merged_ftn <- pass_plays20222023 |> 
  select(play_id:air_yards, qb_hit, passer_player_id, passer_player_name, pass_defense_1_player_id,
         pass_defense_1_player_name, is_batted) |> 
  left_join(ftn_charting_clean) |> 
  select(-qb_kneel, -qb_spike) |> 
  mutate(game_date = as.Date(game_date),
         year = substr(old_game_id, 1, 4),
         season = ifelse(game_date < as.Date("2023-02-13"), 2022, 2023))

merged_ftn$is_batted <- merged_ftn$is_batted |> replace_na(0)

sum(merged_ftn$is_batted)

print(unique(merged_ftn$qb_location))
# [1] "S" "U" "P" NA  "0"

# ftn join with playsers data to get qb_height
players_data <- nflreadr::load_players()
colnames(players_data)

# get height
head(players_data$gsis_id) # Game Statistics and Information System
head(batted_passes_data$passer_player_id)

unique(players_data$position)

players_data |>
  filter(status == "ACT") # != RET

qb_height <- players_data |>
  filter(position == "QB") |>
  select(gsis_id, height)

# join data
merged_ftn <- merged_ftn |>
  filter(is_batted == 1) |>
  left_join(qb_height, by = c("passer_player_id" = "gsis_id")) |>
  rename(qb_height = height) 

colnames(pbp_data)

install.packages("lme4")
library(lme4)
library(dplyr)
print(unique(merged_ftn$season))
merged_ftn2023 <- merged_ftn |> 
  filter(season == 2023) |>
  mutate(passer_name_id = paste(passer_player_name, passer_player_id))

colnames(merged_ftn2023)

merged_ftn2023$qb_location
str(merged_ftn2023$qb_height)
install.packages("Matrix")
update.packages(ask = FALSE)

remove.packages("lme4")
remove.packages("Matrix")
install.packages("Matrix", type = "source")

install.packages("lme4", type="source")
library(lme4)
# Reinstall lme4 and Matrix specifically
install.packages("Matrix", dependencies = TRUE)
install.packages("lme4", dependencies = TRUE)
remove.packages("Matrix")
install.packages("Matrix")
install.packages("lme4")
library(Matrix)
full_model2023 <- glmer(is_batted ~ (1 | passer_name_id) + (1 | defteam) +
                          qb_height +
                          pass_location + qb_location + n_offense_backfield + 
                          is_play_action + is_rpo + is_qb_out_of_pocket +
                          n_pass_rushers,
                        family = binomial, data = merged_ftn2023)

install.packages("installr")
updateR()















































































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
# Each team each year, percentage of batted passes, line, offensive/defensive

total_passes <- batted_passes_data_include_complete %>%
  group_by(season, posteam) %>%
  summarise(total_passes_count = n(), .groups = 'drop')

batted_passes_summary <- merge(batted_passes_summary, total_passes, by = c("season", "posteam"))

# Calculate the percentage
batted_passes_summary <- batted_passes_summary %>%
  mutate(percentage_batted_passes = (batted_passes_count / total_passes_count) * 100)


ggplot(batted_passes_summary, aes(x = as.factor(season), y = percentage_batted_passes, group = posteam)) +
  geom_line(aes(color = posteam), size = 1) +
  geom_point(aes(color = posteam), size = 2) +
  facet_wrap(~posteam) +
  labs(
    title = "Percentage of Batted Passes by Team Across Seasons",
    x = "Season",
    y = "Percentage of Batted Passes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 





















































































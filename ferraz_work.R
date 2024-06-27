# install.packages("nflverse")
library(tidyverse)
library(nflverse)
all_plays2023 <- nflreadr::load_pbp()
library(readxl)
excel_batted <- read_excel("~/Downloads/CMSACamp/2019-2023 Batted Passes.xlsx")
excel_batted$is_batted <- 1
excel_simple <- excel_batted |> 
  select(week = WEEK, home_team = HOME, away_team = AWAY, play_id = `PLAY ID`, is_batted)
excel_batted |> 
  filter(YEAR == 2023) |> 
  count()

all_plays2023 <- left_join(all_plays2023, excel_simple)
all_plays2023$is_batted <- all_plays2023$is_batted |> 
  replace_na(0)
pass_plays2023 <- all_plays2023 |> 
  filter(play_type == "pass")
sum(pass_plays2023$is_batted)

pass_plays2023 |> 
  filter(is_batted == 1) |> 
  group_by(posteam) |> 
  summarise(batted_passes = sum(is_batted)) |> 
  mutate(posteam = fct_reorder(posteam, -batted_passes)) |> 
  ggplot(aes(y = posteam, x = batted_passes)) +
  geom_col(aes(color = posteam, fill = posteam)) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl() +
  ggthemes::theme_clean() +
  labs(x = "Passes Batted", y = "Offensive Team", 
       title = "Batted Passes by Offensive Team in 2023")

pass_plays2023 |> 
  filter(is_batted == 1) |> 
  group_by(defteam) |> 
  summarise(batted_passes = sum(is_batted)) |> 
  mutate(defteam = fct_reorder(defteam, -batted_passes)) |> 
  ggplot(aes(y = defteam, x = batted_passes)) +
  geom_col(aes(color = defteam, fill = defteam)) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl() +
  ggthemes::theme_clean() +
  labs(x = "Passes Batted", y = "Defensive Team", 
       title = "Batted Passes by Defensive Team in 2023")

pass_plays2023 |> 
  filter(is_batted == 1) |> 
  group_by(passer_player_name) |> 
  summarise(batted_passes = sum(is_batted),
            passer_player_id = first(passer_player_id),
            team = first(posteam)) |> 
  arrange(-batted_passes) |> 
  slice_head(n = 10) |> 
  mutate(passer_player_name = fct_reorder(passer_player_name, batted_passes)) |> 
  ggplot(aes(y = passer_player_name, x = batted_passes, fill = team, label = batted_passes)) +
  geom_col() +
  scale_fill_nfl() +
  geom_point() +
  ggthemes::theme_clean() +
  nflplotR::geom_nfl_headshots(aes(player_gsis = passer_player_id), height = 0.2, width = 0.1) +
  labs(title = "Quarterbacks With the Most Batted Passes in 2023", 
       x = "Batted Passes", y = "") +
  geom_label(nudge_x = -1.5, fill = "white", color = "black")

all_players <- nflreadr::load_players()
active_qbs <- all_players |> 
  filter(status == "ACT" & position == "QB")
qbs <- all_players |> 
  filter(position == "QB")

pass_plays20192023 <- load_pbp(seasons = c(2019, 2020, 2021, 2022, 2023)) |> 
  filter(play_type == "pass")
pass_plays20192023 <- left_join(pass_plays20192023, excel_simple)
pass_plays20192023$is_batted <- pass_plays20192023$is_batted |> 
  replace_na(0)
sum(pass_plays20192023$is_batted)

sum(pass_plays2023$is_batted)
avg_bat_pass_pct <- sum(pass_plays20192023$is_batted / length(pass_plays20192023$is_batted))
pass_plays20192023 |> 
  group_by(passer_player_name) |> 
  summarise(batted_passes = sum(is_batted),
            total_passes = n(),
            batted_pass_pct = batted_passes / total_passes,
            player_gsis = first(passer_player_id),
            team_abbr = last(posteam)) |> 
  filter(total_passes > 300) |> 
  left_join(qbs |> select(player_gsis = gsis_id, height)) |> 
  ggplot(aes(x = height, y = batted_pass_pct)) +
  geom_point() +
  geom_hline(yintercept = avg_bat_pass_pct, color = "red", linetype = "dashed", linewidth = 1.5) +
  annotate("text", x = 72, y = avg_bat_pass_pct + 0.002, label = "Avg Batted Pass Percentage", 
           color = "red") +
  ggthemes::theme_clean() +
  labs(title = "Height vs Batted Pass Percentage For Seasons 2019-2023", 
       subtitle = "No clear indicator that shorter QBs have more batted balls",
       y = "Batted Pass %", x = "Height")



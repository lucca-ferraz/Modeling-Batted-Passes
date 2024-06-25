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
  ggthemes::theme_clean()

pass_plays2023 |> 
  filter(is_batted == 1) |> 
  group_by(defteam) |> 
  summarise(batted_passes = sum(is_batted)) |> 
  mutate(defteam = fct_reorder(defteam, -batted_passes)) |> 
  ggplot(aes(y = defteam, x = batted_passes)) +
  geom_col(aes(color = defteam, fill = defteam)) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl() +
  ggthemes::theme_clean()

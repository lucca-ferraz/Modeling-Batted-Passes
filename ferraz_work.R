# Loading in Data ---------------------------------------------------------

# install.packages("nflverse")
library(tidyverse)
library(nflverse)
all_plays2023 <- nflreadr::load_pbp()
library(readxl)
excel_batted <- read_excel("~/Downloads/CMSACamp/2019-2023 Batted Passes.xlsx")
excel_batted$is_batted <- 1

excel_simple <- excel_batted |> 
  mutate(across(c("HOME", "AWAY"), ~ str_replace(
    string = .x, pattern = "LAR", replacement = "LA"))) |> 
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

pass_plays20192023 <- load_pbp(seasons = c(2019, 2020, 2021, 2022, 2023)) |> 
  filter(play_type == "pass")
pass_plays20192023 <- left_join(pass_plays20192023, excel_simple)
pass_plays20192023$is_batted <- pass_plays20192023$is_batted |> 
  replace_na(0)

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

all_players <- nflreadr::load_players()
active_qbs <- all_players |> 
  filter(status == "ACT" & position == "QB")
qbs <- all_players |> 
  filter(position == "QB")
qb_heights <- qbs |> 
  select(passer_player_id = gsis_id, height)

merged_ftn <- merged_ftn |> 
  left_join(qb_heights, by = join_by(passer_player_id)) |> 
  rename(qb_height = height)

merged_ftn$passer_name_id <- paste(merged_ftn$passer_player_name, merged_ftn$passer_player_id)

merged_ftn2023 <- merged_ftn |> 
  filter(season == 2023)

merged_ftn2022 <- merged_ftn |> 
  filter(season == 2022)

pass_plays20192023 <- pass_plays20192023 |> 
  left_join(qb_heights, by = join_by(passer_player_id)) |> 
  rename(qb_height = height)

pass_plays2023 <- pass_plays2023 |> 
  left_join(qb_heights, by = join_by(passer_player_id)) |> 
  rename(qb_height = height)

# EDA Plots ---------------------------------------------------------------

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

# pass_plays2023 |> 
#   filter(is_batted == 1) |> 
#   group_by(passer_player_name) |> 
#   summarise(batted_passes = sum(is_batted),
#             passer_player_id = first(passer_player_id),
#             team = first(posteam)) |> 
#   arrange(-batted_passes) |> 
#   slice_head(n = 10) |> 
#   mutate(passer_player_name = fct_reorder(passer_player_name, batted_passes)) |> 
#   ggplot(aes(y = passer_player_name, x = batted_passes, fill = team, label = batted_passes)) +
#   geom_col() +
#   scale_fill_nfl() +
#   geom_point() +
#   ggthemes::theme_clean() +
#   nflplotR::geom_nfl_headshots(aes(player_gsis = passer_player_id), height = 0.2, width = 0.1) +
#   labs(title = "Quarterbacks With the Most Batted Passes in 2023", 
#        x = "Batted Passes", y = "") +
#   geom_label(nudge_x = -1.5, fill = "white", color = "black")

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

pass_plays20192023 |> 
  group_by(passer) |> 
  summarise(attempts = n(), batted_passes = sum(is_batted), 
            batted_pass_pct = batted_passes / attempts, 
            short_pass_pct = sum(pass_length == "short", na.rm = TRUE) / attempts,
            pass_right_pct = sum(pass_location == "right", na.rm = TRUE) / attempts,
            pass_left_pct = sum(pass_location == "left", na.rm = TRUE) / attempts,
            pass_middle_pct = sum(pass_location == "middle", na.rm = TRUE) / attempts,
            avg_air_yards = sum(air_yards, na.rm = TRUE) / attempts) |> 
  filter(attempts >= 100) |> 
  ggplot(aes(short_pass_pct, batted_pass_pct)) +
  geom_point(alpha = 0.8, color = "dodgerblue4") +
  ggthemes::theme_clean() +
  labs(title = "Does Short Passing Correlate With Batted Passes?",
       subtitle = "Among QBs with at least 100 attempts between 2019 - 2023", 
       x = "% of Short Passes", y = "% of Batted Balls") +
  geom_hline(yintercept = avg_bat_pass_pct, color = "red", linetype = "dashed", linewidth = 1.5) +
  annotate("text", x = 0.68, y = avg_bat_pass_pct + 0.002, label = "Avg Batted Pass Percentage", 
           color = "red")

pass_plays20192023 |> 
  filter(!is.na(pass_location)) |> 
  group_by(pass_location) |> 
  summarise(attempts = n(), batted_passes = sum(is_batted), 
            bat_pct = batted_passes / attempts) |> 
  ggplot() +
  geom_col(aes(x = pass_location, y = bat_pct), fill = "#69b3a2") +
  #geom_col(aes(x = pass_location, y = batted_passes), fill = "red") +
  geom_label(aes(x = pass_location, y = bat_pct, label = round(bat_pct, 4))) +
  labs(x = "Intended Pass Location", y = "Batted Pass Percentage", 
       title = "Percentage of Batted Balls by Intended Pass Location", 
       caption = "Data from 2019-2023 NFL Seasons") +
  ggthemes::theme_clean()

# pass_plays2023 |> 
#   filter(!is.na(pass_location)) |> 
#   group_by(pass_location) |> 
#   summarise(attempts = n(), batted_passes = sum(is_batted), 
#             bat_pct = batted_passes / attempts) |> 
#   ggplot() +
#   geom_col(aes(x = pass_location, y = attempts), fill = "dodgerblue") +
#   geom_col(aes(x = pass_location, y = batted_passes), fill = "red")

pass_plays20192023 |> 
  filter(is_batted == 1 & complete_pass == 0 & !is.na(pass_defense_1_player_name)) |> 
  group_by(gsis_id = pass_defense_1_player_id) |> 
  summarise(batted_passes = n()) |> 
  arrange(-batted_passes) |> 
  left_join(all_players |> 
              select(gsis_id, display_name, position, height)) |> 
  ungroup() |> 
  group_by(position) |> 
  summarise(pos_batted_passes = sum(batted_passes)) |> 
  ggplot(aes(x = position, y = pos_batted_passes)) +
  geom_col() +
  ggthemes::theme_clean()

avg_dl_height <- all_players |> 
  filter(status == "ACT" & position %in% c("DT", "DE", "OLB")) |> 
  summarise(mean(height, na.rm = TRUE)) |> 
  as.numeric()

pass_plays20192023 |> 
  filter(is_batted == 1 & complete_pass == 0 & !is.na(pass_defense_1_player_name)) |> 
  group_by(gsis_id = pass_defense_1_player_id) |> 
  summarise(batted_passes = n()) |> 
  arrange(-batted_passes) |> 
  left_join(all_players |> 
              select(gsis_id, display_name, position, height)) |> 
  ggplot(aes(x = height, y = batted_passes)) +
  geom_point(color = "darkgreen") +
  ggthemes::theme_clean() +
  geom_vline(aes(xintercept = avg_dl_height), color = "red", linetype = "dashed",
             linewidth = 2) +
  annotate("text", x = 75.3, y = 13, label = "Average Height for DT/DE/OLB", 
           color = "red", angle = 90) +
  labs(x = "Height", y = "Batted Passes", 
       title = "Relationship Between Defender Height and Passes Batted")

merged_ftn |> 
  group_by(qb_location, season) |> 
  summarise(plays = n(), batted_balls = sum(is_batted),
            bat_pct = batted_balls / plays) |> 
  filter(qb_location %in% c("P", "S", "U")) |> 
  ggplot(aes(x = qb_location, y = bat_pct)) +
  geom_col(aes(fill = qb_location)) +
  ggthemes::theme_clean() + 
  ggthemes::scale_fill_fivethirtyeight(guide = "none") +
  geom_label(aes(label = round(bat_pct, 3))) +
  labs(x = "QB Location", y = "Batted Pass Percentage", 
       title = "Percentage of Batted Passes by QB Location", 
       subtitle = "Sorted by Season", 
       caption = "Data from FTN Charting via nflreadR") +
  scale_x_discrete(labels = c("Pistol", "Shotgun", "Under Center")) +
  facet_wrap(~ season)

merged_ftn |> 
  group_by(n_pass_rushers) |> 
  filter(!is.na(n_pass_rushers)) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), 
            bat_pct = batted_balls / plays) |> 
  filter(plays > 200) |> 
  ggplot(aes(x = n_pass_rushers, y = bat_pct)) +
  geom_col(aes(fill = bat_pct)) + 
  ggthemes::theme_clean() + 
  ggthemes::scale_fill_continuous_tableau() +
  labs(x = "Number of Pass Rushers", y = "Percent of Passes Batted", 
       title = "More Pass Rushers Leads to More Passes Batted",
       caption = "Data from FTN Charting via nflreadR")

merged_ftn |> 
  filter(!is.na(is_no_huddle)) |> 
  group_by(is_no_huddle) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(is_no_huddle, bat_pct)) +
  geom_col()

avg_bat_pass_pct <- sum(merged_ftn$is_batted / length(merged_ftn$is_batted))

merged_ftn |> 
  filter(!is.na(is_play_action)) |> 
  group_by(is_play_action) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(is_play_action, bat_pct)) +
  geom_col() +
  geom_hline(yintercept = avg_bat_pass_pct, color = "red")

merged_ftn |> 
  filter(!is.na(n_defense_box)) |> 
  group_by(n_defense_box) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  filter(plays > 150) |> 
  ggplot(aes(n_defense_box, bat_pct)) +
  geom_col(aes(fill = plays)) +
  ggthemes::theme_clean() +
  scale_fill_gradient(low = "gold", high = "red") +
  labs(x = "Numbers of Defenders in the Box", y = "% of Passes Batted",
       title = "Number of Box Defenders vs Batted Pass Percentage",
       caption = "data from FTN charting via nflreadR")

merged_ftn |> 
  filter(!is.na(is_motion)) |> 
  group_by(is_motion) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(is_motion, bat_pct)) + 
  geom_col()

merged_ftn |> 
  filter(!is.na(starting_hash) & starting_hash != 0) |> 
  group_by(starting_hash) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(starting_hash, bat_pct)) + 
  geom_col(fill = "darkgreen") +
  ggthemes::theme_clean()

merged_ftn |> 
  filter(!is.na(n_offense_backfield)) |> 
  group_by(n_offense_backfield) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  filter(plays > 150) |> 
  ggplot(aes(n_offense_backfield, bat_pct)) + 
  geom_col(fill = "#69b3a2") +
  geom_label(aes(label = round(bat_pct, 3))) +
  ggthemes::theme_clean() +
  labs(x = "Number of Players in the Offensive Backfield", y = "Batted Pass Percentage",
       title = "More Players in the Backfield Leads to Less Batted Passes",
       caption = "Data from FTN Charting via nflreadR") +
  theme(axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold")) 

merged_ftn |> 
  filter(!is.na(down)) |> 
  group_by(down) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(down, bat_pct)) +
  geom_col()

merged_ftn |> 
  filter(!is.na(n_blitzers)) |> 
  group_by(n_blitzers) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  filter(plays > 100) |> 
  ggplot(aes(n_blitzers, bat_pct)) +
  geom_col()

pass_plays20192023 |> 
  mutate(game_date = as.Date(game_date), season = case_when(
    game_date > as.Date("2023-02-13") ~ 2023,
    game_date > as.Date("2022-02-13") ~ 2022,
    game_date > as.Date("2021-02-07") ~ 2021,
    game_date > as.Date("2020-02-02") ~ 2020,
  )) |> 
  filter(!is.na(season)) |> 
  group_by(passer, season) |> 
  summarise(attempts = n(), batted_passes = sum(is_batted), 
            batted_pass_pct = batted_passes / attempts, 
            short_pass_pct = sum(pass_length == "short", na.rm = TRUE) / attempts,
            pass_right_pct = sum(pass_location == "right", na.rm = TRUE) / attempts,
            pass_left_pct = sum(pass_location == "left", na.rm = TRUE) / attempts,
            pass_middle_pct = sum(pass_location == "middle", na.rm = TRUE) / attempts,
            avg_air_yards = sum(air_yards, na.rm = TRUE) / attempts) |> 
  filter(attempts >= 100) |> 
  ggplot(aes(short_pass_pct, batted_pass_pct)) +
  geom_point(alpha = 0.8, color = "dodgerblue4") +
  ggthemes::theme_clean() +
  labs(title = "Does Short Passing Correlate With Batted Passes?",
       subtitle = "Among QBs with at least 100 attempts between 2019 - 2023", 
       x = "% of Short Passes", y = "% of Batted Balls") +
  geom_hline(yintercept = avg_bat_pass_pct, color = "red", linetype = "dashed", linewidth = 1.5) +
  facet_wrap(~ season)

pass_plays20192023 |> 
  mutate(game_date = as.Date(game_date), season = case_when(
    game_date > as.Date("2023-02-13") ~ 2023,
    game_date > as.Date("2022-02-13") ~ 2022,
    game_date > as.Date("2021-02-07") ~ 2021,
    game_date > as.Date("2020-02-02") ~ 2020,
  )) |> 
  filter(!is.na(season)) |> 
  group_by(passer, season) |> 
  summarise(attempts = n(), batted_passes = sum(is_batted), 
            batted_pass_pct = batted_passes / attempts, 
            short_pass_pct = sum(pass_length == "short", na.rm = TRUE) / attempts,
            pass_right_pct = sum(pass_location == "right", na.rm = TRUE) / attempts,
            pass_left_pct = sum(pass_location == "left", na.rm = TRUE) / attempts,
            pass_middle_pct = sum(pass_location == "middle", na.rm = TRUE) / attempts,
            avg_air_yards = sum(air_yards, na.rm = TRUE) / attempts) |> 
  filter(attempts >= 100) |> 
  ggplot(aes(avg_air_yards, batted_pass_pct)) +
  geom_point(alpha = 0.8, color = "chartreuse4") +
  ggthemes::theme_clean() +
  labs(title = "Does Average Air Yards Correlate With Batted Passes?",
       subtitle = "Among QBs with at least 100 attempts between 2019 - 2023", 
       x = "Average Air Yards", y = "% of Batted Balls") +
  geom_hline(yintercept = avg_bat_pass_pct, color = "red", linetype = "dashed", linewidth = 1.5) +
  facet_wrap(~ season)

merged_ftn |> 
  filter(!is.na(qb_hit)) |> 
  group_by(qb_hit) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays)

merged_ftn |> 
  filter(!is.na(is_qb_out_of_pocket)) |> 
  group_by(is_qb_out_of_pocket) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(is_qb_out_of_pocket, bat_pct)) +
  geom_col()

merged_ftn |> 
  filter(!is.na(is_rpo)) |> 
  group_by(is_rpo) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(is_rpo, bat_pct)) +
  geom_col()

merged_ftn |> 
  filter(!is.na(read_thrown)) |> 
  group_by(read_thrown) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(read_thrown, bat_pct)) +
  geom_col()

# merged_ftn |> 
#   filter(!is.na(is_screen_pass)) |> 
#   group_by(is_screen_pass) |> 
#   summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
#   ggplot(aes(is_screen_pass, bat_pct)) +
#   geom_col()

# pass_plays20192023 |> 
#   mutate(game_date = as.Date(game_date), season = case_when(
#     game_date > as.Date("2023-02-13") ~ 2023,
#     game_date > as.Date("2022-02-13") ~ 2022,
#     game_date > as.Date("2021-02-07") ~ 2021,
#     game_date > as.Date("2020-02-02") ~ 2020,
#   )) |> 
#   filter(!is.na(season)) |> 
#   group_by(season) |> 
#   summarise(passes = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / passes) |> 
#   ggplot(aes(season, bat_pct)) +
#   geom_point() +
#   geom_line()

pass_plays20192023 |> 
  filter(!is.na(offense_formation)) |> 
  group_by(offense_formation) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(offense_formation, bat_pct)) +
  geom_col(fill = "darkgoldenrod4") +
  ggthemes::theme_clean()

# pass_plays20192023 |> 
#   filter(!is.na(defense_man_zone_type)) |> 
#   group_by(defense_man_zone_type) |> 
#   summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
#   ggplot(aes(defense_man_zone_type, bat_pct)) +
#   geom_col()

pass_plays20192023 |> 
  filter(!is.na(defense_coverage_type)) |> 
  group_by(defense_coverage_type) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  ggplot(aes(defense_coverage_type, bat_pct)) +
  geom_col(fill = "dodgerblue4") +
  ggthemes::theme_clean()
pass_plays20192023 |> 
  filter(!is.na(defense_personnel)) |> 
  group_by(defense_personnel) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  filter(plays > 500) |> 
  ggplot(aes(bat_pct, defense_personnel)) +
  geom_col()

pass_plays20192023 |> 
  filter(!is.na(offense_personnel)) |> 
  group_by(offense_personnel) |> 
  summarise(plays = n(), batted_balls = sum(is_batted), bat_pct = batted_balls / plays) |> 
  filter(plays > 500) |> 
  ggplot(aes(bat_pct, offense_personnel)) +
  geom_col()

pass_plays20192023 |> 
  ggplot(aes(time_to_throw, fill = as.factor(is_batted))) +
  geom_density(alpha = 0.4)

# Modeling ----------------------------------------------------------------

library(lme4)
passer_model <- glmer(is_batted ~ (1 | passer_name_id), family = binomial, 
                    data = merged_ftn)
summary(passer_model)
0.03144 / (0.03144 + (pi^2 / 3))

defense_model <- glmer(is_batted ~ (1 | defteam), family = binomial, data = merged_ftn)
summary(defense_model)
0.07701 / (0.07701 + (pi^2 / 3))

passer_and_defense_model <- glmer(is_batted ~ (1 | passer_name_id) + (1 | defteam),
                                  family = binomial, data = merged_ftn)
summary(passer_and_defense_model)

pass_and_d_model23 <- glmer(is_batted ~ (1 | passer_name_id) + (1 | defteam),
                              family = binomial, data = merged_ftn2023)
summary(pass_and_d_model23)

# install.packages("sjPlot")
library(sjPlot)
library(gridExtra)

full_model2023 <- glmer(is_batted ~ (1 | passer_name_id) + (1 | defteam) + qb_height +
                          pass_location + qb_location + n_offense_backfield + 
                          is_play_action + is_rpo + is_qb_out_of_pocket + n_pass_rushers,
                        family = binomial, data = merged_ftn2023)
summary(full_model2023)
logistic_model2023 <- glm(is_batted ~ qb_height + pass_location + qb_location + 
                            n_offense_backfield + is_play_action + is_rpo + 
                            is_qb_out_of_pocket + n_pass_rushers, 
                          family = binomial, data = merged_ftn2023)
height_model2023 <- glm(is_batted ~ qb_height, family = binomial, data = merged_ftn2023)
plot_models(height_model2023, full_model2023, 
            rm.terms = c("pass_locationmiddle", "pass_locationright",
                         "qb_locationP", "qb_locationS", "qb_locationU", 
                         "n_offense_backfield", "is_play_actionTRUE", "is_rpoTRUE",
                         "is_qb_out_of_pocketTRUE", "n_pass_rushers"))
plot_models(logistic_model2023, full_model2023, 
            rm.terms = c("pass_locationmiddle", "pass_locationright",
                         "qb_locationP", "qb_locationS", "qb_locationU", 
                         "n_offense_backfield", "is_play_actionTRUE", "is_rpoTRUE",
                         "is_qb_out_of_pocketTRUE", "n_pass_rushers")) +
  ggthemes::scale_color_fivethirtyeight(labels = c("mixed effects model", "logistic model")) 

plot_models(logistic_model2023, full_model2023) +
  ggthemes::scale_color_fivethirtyeight(labels = c("mixed effects model", "logistic model"))
broom::tidy(logistic_model2023)
tab_model(logistic_model2023, full_model2023)
grid.arrange(log_plot23, full_plot23)
?grid.arrange

full_model2022 <- glmer(is_batted ~ (1 | passer_name_id) + (1 | defteam) + qb_height +
                          pass_location + qb_location + n_offense_backfield + 
                          is_play_action + is_rpo + is_qb_out_of_pocket + n_pass_rushers,
                        family = binomial, data = merged_ftn2022)
summary(full_model2022)
logistic_model2022 <- glm(is_batted ~ qb_height + pass_location + qb_location + 
                            n_offense_backfield + is_play_action + is_rpo + 
                            is_qb_out_of_pocket + n_pass_rushers, 
                          family = binomial, data = merged_ftn2022)
summary(logistic_model2022)

plot_models(logistic_model2022, full_model2022) +
  ggthemes::scale_color_fivethirtyeight(labels = c("mixed effects model", "logistic model")) #+
ggthemes::theme_clean()
tab_model(pass_and_d_model23, full_model2023)

# pass_and_d_model22 <- glmer(is_batted ~ (1 | passer_name_id) + (1 | defteam),
#                             family = binomial, data = merged_ftn2022)
# summary(pass_and_d_model22)
# 
# passer_model2023 <- glmer(is_batted ~ (1 | passer_name_id), family = binomial,
#                           data = merged_ftn2023)
# summary(passer_model2023)
# 0.0275 / (0.0275 + (pi^2 / 3))
# 
# defense_model2023 <- glmer(is_batted ~ (1 | defteam), family = binomial, 
#                            data = merged_ftn2023)
# summary(defense_model2023)
# 0.1022 / (0.1022 + (pi^2 / 3))

tab_model(full_model2023, full_model2022)
plot_models(full_model2023, full_model2022) +
  ggthemes::scale_color_fivethirtyeight(labels = c("2022 Model", "2023 Model")) +
  guides(color = guide_legend(title = "Coefficient Estimates"))

library(broom.mixed)
# install.packages("dotwhisker")
library(dotwhisker)
tidy_model23 <- tidy(full_model2023) |> 
  filter(is.na(group)) |> 
  mutate(model = 2023) 

tidy_model22 <- tidy(full_model2022) |> 
  filter(is.na(group)) |> 
  mutate(model = 2022)

bind_rows(tidy_model23, tidy_model22) |> 
  filter(!is.na(std.error) & !is.na(estimate)) |> 
  relabel_predictors(qb_height = "QB Height", pass_locationmiddle = "Pass Middle",
                     pass_locationright = "Pass Right", qb_locationS = "Shotgun", 
                     qb_locationU = "Under Center", 
                     n_offense_backfield = "Number of Players in Off. Backfield",
                     is_play_actionTRUE = "Play-Action Pass", is_rpoTRUE = "Run-Pass Option",
                     is_qb_out_of_pocketTRUE = "QB Outside of Pocket", 
                     n_pass_rushers = "Number of Pass Rushers",
                     qb_locationP = "Pistol") |> 
  dwplot(ci = 0.68, vline = geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5),
         dot_args = list(size = 3), whisker_args = list(size = .6)) +
    aes(alpha = !(std.error >= abs(estimate))) +
    scale_alpha_manual(breaks = c(FALSE, TRUE), values = c(0.25, 1), 
                       labels = c("Contains zero", "Does not contain zero"),
                       ) +
    ggthemes::scale_color_fivethirtyeight() +
    theme_bw() + 
    labs(color = "Model", alpha = "Interval contains zero") 

# Model Plots -------------------------------------------------------------
tidy_coef22 <- tidy(full_model2022, effects = "ran_vals") |> 
  mutate(season = 2022)

tidy_coef23 <- tidy(full_model2023, effects = "ran_vals") |> 
  mutate(season = 2023)

bind_rows(tidy_coef22, tidy_coef23) |> 
  ggplot(aes(estimate, fill = group)) +
  geom_density() +
  facet_wrap(~ season) +
  labs(title = "Distribution of Random Effects", 
       subtitle = "QB Coefficients vs Defensive Team Coefficients",
       x = "Coefficient", y = "Density",
       fill = "Coefficient Type") +
  ggthemes::theme_clean() +
  ggthemes::scale_fill_tableau(label = c("Defensive Team", "Quarterback"))

ranef(full_model2023) |> 
  as_tibble() |> 
  ggplot(aes(condval, fill = grpvar)) +
  geom_density() +
  labs(title = "Distribution of Random Effects", 
       subtitle = "QB Coefficients vs Defensive Team Coefficients",
       caption = "2023 Model", x = "Coefficient", y = "Density",
       fill = "Coefficient Type") +
  ggthemes::theme_clean() +
  ggthemes::scale_fill_tableau(label = c("Defensive Team", "Quarterback"))

ranef(full_model2023) |> 
  as_tibble() |> 
  filter(grpvar == "passer_name_id") |> 
  arrange(-condval) |> 
  slice_head(n = 10) |> 
  select(grp, condval) |> 
  mutate(gsis_id = word(grp, 2),
         name = word(grp, 1)) |> 
  left_join(pass_plays2023 |> group_by(passer_id) |> summarise(team = last(posteam)), 
            join_by("gsis_id" == "passer_id")) |> 
  ggplot(aes(y = reorder(name, condval), x = condval, fill = team, label = condval)) +
  geom_col() +
  scale_fill_nfl() +
  geom_point() +
  ggthemes::theme_clean() +
  nflplotR::geom_nfl_headshots(aes(player_gsis = gsis_id), height = 0.2, width = 0.1) +
  labs(title = "QBs With the Highest Coefficients in 2023 (More Batted Passes)",
       x = "Coefficient", y = "")

ranef(full_model2023) |> 
  as_tibble() |> 
  filter(grpvar == "passer_name_id") |> 
  arrange(condval) |> 
  slice_head(n = 10) |> 
  select(grp, condval) |> 
  mutate(gsis_id = word(grp, 2),
         name = word(grp, 1)) |> 
  left_join(pass_plays2023 |> group_by(passer_id) |> summarise(team = last(posteam)), 
            join_by("gsis_id" == "passer_id")) |> 
  ggplot(aes(y = reorder(name, condval), x = condval, fill = team, label = condval)) +
  geom_col() +
  scale_fill_nfl() +
  geom_point() +
  ggthemes::theme_clean() +
  nflplotR::geom_nfl_headshots(aes(player_gsis = gsis_id), height = 0.2, width = 0.1) +
  labs(title = "QBs With the Lowest Coefficients in 2023 (Less Batted Passes)",
       x = "Coefficient", y = "")


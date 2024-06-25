# CMU SURE 2024 Final Project
# Football
# Name: Amelia Yixin Yuan


# Data Preprocessing
# 6/21 Friday
# install.packages("nflreadr")
# install.packages("nflreadr", repos = c("https://nflverse.r-universe.dev", getOption("repos")))

# or use remotes/devtools
# install.packages("remotes")
# remotes::install_github("nflverse/nflreadr")

library(nflreadr)

# Play-by-play data via nflreadR
pbp_data <- load_pbp()

str(pbp_data)
head(pbp_data)
summary(pbp_data)






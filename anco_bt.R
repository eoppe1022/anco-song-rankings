
library(tidyverse)
library(BradleyTerryScalable)

svs_data_raw <- readxl::read_xlsx("anco_svs_data.xlsx")

# data manipulation to get in model format
svs_data <- svs_data_raw %>%
  mutate(winner = ifelse(song_1_votes > song_2_votes, song_1, song_2)) %>%
  mutate(winner_votes = ifelse(song_1_votes > song_2_votes, song_1_votes, song_2_votes)) %>%
  mutate(loser = ifelse(song_1_votes > song_2_votes, song_2, song_1)) %>%
  mutate(loser_votes = ifelse(song_1_votes > song_2_votes, song_2_votes, song_1_votes)) %>%
  select(winner, winner_votes, loser, loser_votes)

svs_data_winners <- svs_data %>%
  uncount(winner_votes) %>%
  select(loser, winner)

svs_data_losers <- svs_data %>%
  select(winner = loser, winner_votes = loser_votes, loser = winner, loser_votes = winner_votes) %>%
  uncount(winner_votes) %>%
  select(loser, winner)

svs_data <- svs_data_losers %>%
  bind_rows(svs_data_winners)

# getting data into bradley terry data format
svs_btdata <- svs_data %>%
  mutate(item1wins = 0, item2wins = 1) %>%
  btdata(return_graph = TRUE)

# finally building the model
svs_fit_map <- btfit(svs_btdata, a = 1.1)

svs_fit_map %>%
  btprob(as_df = TRUE)

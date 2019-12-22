# https://technistema.com/posts/introducing-the-schrute-package-the-entire-transcripts-from-the-office/
# install.packages("schrute")
library(schrute)
library(skimr)
library(tidyverse)

# load data into a data frame object
the_office_transcripts <- schrute::theoffice

# inspect data with skimr::skim
skimr::skim(the_office_transcripts)

# tokenize the_office_transcripts
the_office_tokens <- the_office_transcripts %>%
  tidytext::unnest_tokens(word, text)

# count number of tokens per character
char_season_token_count <- the_office_tokens %>%
  group_by(character, season) %>%
  count()

# load list of main characters
character_list <- read_csv("data/character_list.csv") %>%
  separate(character, c("character", "character_lastname"))

# add character info to token count
char_season_token_count <- left_join(char_season_token_count, character_list)

# plot token count for main characters across seasons
char_season_token_count %>%
  filter(type == "Main") %>%
  ggplot(aes(x = season, y = n, color = character)) +
  geom_point() +
  geom_line(aes(group = character))

mean(char_season_token_count$n)

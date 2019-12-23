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


# remove stop words
stop_words <- tidytext::stop_words
clean_the_office_tokens <- the_office_tokens %>%
  dplyr::anti_join(stop_words, by = "word")

# count number of tokens per character
clean_char_season_token_count <- clean_the_office_tokens %>%
  group_by(season, character) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percentage = (n/sum(n))*100)

# add character info to token count
clean_char_season_token_count <- left_join(clean_char_season_token_count, 
                                           character_list)

# count number "of "sorry" tokens per character
sorry_character_count <- the_office_tokens %>%
  filter(word == "sorry") %>%
  group_by(character) %>%
  count()

# count token instances per character
token_character_count <- the_office_tokens %>%
  group_by(character, word) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percentage = n/sum(n))

# add character info to token count
token_character_count <- left_join(token_character_count,
                                   character_list)

# count token instances per character per season
token_character_season_count <- the_office_tokens %>%
  group_by(character, season, word) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percentage = n/sum(n))

# add character info to token count
token_character_season_count <- left_join(token_character_season_count,
                                          character_list)


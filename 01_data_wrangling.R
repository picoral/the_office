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



# plot
char_season_token_count %>%
  ggplot(aes(x = season, y = n, color = character)) +
  geom_point()

mean(char_season_token_count$n)

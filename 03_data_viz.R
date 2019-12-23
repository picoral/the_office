library(RColorBrewer)

display.brewer.all(colorblindFriendly = TRUE)

# plot token count for main characters across seasons
char_season_token_count %>%
  filter(type == "Main") %>%
  ggplot(aes(x = season, y = n, color = character)) +
  geom_point() +
  geom_line(aes(group = character))

# plot token percentage for main characters across seasons
clean_char_season_token_count %>%
  filter(type == "Main") %>%
  ggplot(aes(x = season, y = percentage, color = character)) +
  geom_point() +
  geom_line(aes(group = character)) +
  theme_minimal()

# plot token percentage for main characters across seasons
clean_char_season_token_count %>%
  filter(type == "Main") %>%
  ggplot(aes(x = as.numeric(season), y = percentage, fill = reorder(character, percentage))) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette="Set2") +
  theme_minimal() + 
  coord_flip() + 
  ylab("percentage of talk") +
  scale_x_reverse("season", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  guides(fill = guide_legend(title = "Character", reverse = TRUE))

# plot percentage of talk time for main characters across seasons
talk_time_results %>%
  ggplot(aes(x = reorder(character, Estimate), y = Estimate)) +
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = .2) +
  theme_minimal() +
  xlab("") +
  ylab("Percentage of talk time alloted per season")

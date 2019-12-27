library(RColorBrewer)
library(ggthemes)

#isplay.brewer.all(colorblindFriendly = TRUE)
these_breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
these_labels = paste("Season", these_breaks)

# plot token percentage for main characters across seasons
percentage_bar_chart <- char_season_token_count_v2 %>%
  ggplot(aes(x = as.numeric(season), y = percentage, fill = reorder(character, percentage))) +
  geom_col(position = "stack") +
  geom_text(aes(label = ifelse(percentage > 4, 
                               format(round(percentage, 1)), "")), 
            position = "stack", hjust = 1.1,
            color = "black", size = 3) +
  scale_fill_brewer(palette="Set2") +
  coord_flip() + 
  ylab("percentage of talk") +
  scale_x_reverse("", breaks = these_breaks,
                  labels = these_labels) +
  guides(fill = guide_legend(title = "Character", reverse = TRUE)) +
  theme_economist() +
  ggtitle("From Michael to Others as the main character")

# plot percentage of talk time for main characters across seasons
linear_regression_chart <- talk_time_results %>%
  ggplot(aes(x = reorder(character, Estimate), y = Estimate)) +
  geom_point() +
  coord_flip() +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = .2) +
  theme_economist() +
  xlab("") +
  geom_text(aes(label = paste(format(round(Estimate, 2), nsmall = 2), significant))
            , hjust = -.2, vjust = -.2) +
  ylab("Percentage of talk time alloted per season") +
  ggtitle("Talk Time per Character")

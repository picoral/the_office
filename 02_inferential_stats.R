library(lme4)
library(lmerTest)

# Get percentage of talk time
talk_time <- clean_char_season_token_count %>%
  filter(type == "Main")

# no differences across gender, but differences across characters
summary(aov(percentage ~ gender + Error(character), data = talk_time))
summary(aov(percentage ~ character, data = talk_time))

# linear regression for percentage of talk time predicted by character
talk_time_model <- lm(percentage ~ 0 + character, data = talk_time)

# get coefficients from model
talk_time_coef <- as.data.frame(coef(summary(talk_time_model)))
talk_time_coef$character <- gsub('character', '', rownames(talk_time_coef))

# get confidence intervals
talk_time_cis <- as.data.frame(confint(talk_time_model, method="Wald", level = 0.95))
talk_time_cis$character <- gsub('character', '', rownames(talk_time_cis))

# bind results together
talk_time_results <- full_join(talk_time_coef, talk_time_cis)

# get significant symbols
talk_time_results <- talk_time_results %>%
  mutate(significant = case_when(`Pr(>|t|)` < .001 ~ '***', 
                                 `Pr(>|t|)` < .01 ~ '**', 
                                 `Pr(>|t|)` < .05 ~ '*', 
                                 TRUE ~ ''))

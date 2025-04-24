library(pacman)
p_load(tidyverse, ggplot2, readr, janitor, psych, ggthemes)

theme_set(theme_clean())

emotions <- read_csv("generated_data/cleaned/emotions.csv")

emotions <- emotions[, 3:length(emotions)]
emotions <- clean_names(emotions)

emotions <- emotions %>% 
  mutate(condition = paste(type_of_prompt, temperature, sep = '+'),
         condition = as.factor(condition))

# check summary statistics by condition
describeBy(emotions ~ condition)

# check summary statistics by temperature
describeBy(emotions ~ temperature)

ggplot(emotions, aes(x = anger, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition) +
  labs(title = 'Anger') +
  theme(plot.title = element_text(hjust = .5))

ggplot(emotions, aes(x = joy, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition)

ggplot(emotions, aes(x = trust, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition)

ggplot(emotions, aes(x = surprise, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition)

ggplot(emotions, aes(x = disgust, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition)

ggplot(emotions, aes(x = sadness, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition)

ggplot(emotions, aes(x = fear, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition)  

ggplot(emotions, aes(x = anticipation, group = condition)) +
  geom_histogram(stat = 'density') +
  facet_wrap(~condition)

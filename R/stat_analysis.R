library(pacman)
p_load(tidyverse, ggplot2, readr, janitor, psych, ggthemes, car)

theme_set(theme_clean())

emotions <- read_csv("generated_data/cleaned/emotions.csv")

# remove first two columns
emotions <- emotions[, 3:length(emotions)]

# cleans the names in the dataset
emotions <- clean_names(emotions)

emotions <- emotions %>% 
  mutate(condition = paste(substr(type_of_prompt, 1, 1), temperature, sep = '+'),
         condition = as.factor(condition),
         type_of_prompt = as.factor(type_of_prompt),
         temperature = as.factor(temperature))

# see class imbalances, unfortunately many datapoints having 1.3 
emotions %>% 
  group_by(temperature) %>% 
  summarize(n())
# get the list of emotions in the dataframe
list_emotions <- names(emotions)[5:length(emotions)-1]
# check summary statistics by condition
describeBy(emotions ~ condition)

# check summary statistics by temperature to see if more dispersion in higher temperatures
describeBy(emotions ~ temperature)

emotions %>% 
  group_by(condition) %>% 
  summarize(mean_anger = mean(anger),
            mean_sadness = mean(sadness))

ggplot(emotions, aes(x = anger)) +
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

ggplot(emotions, aes(x = condition, y = anticipation)) +
  geom_boxplot(notch = T)
  # facet_wrap(~condition)

plotting_boxplot <- function(variable, notch = T) {
  ggplot(emotions, aes(x = condition, y = {{ variable }})) +
    geom_boxplot(notch = notch)
}
names(emotions)
plotting_boxplot(joy)
plotting_boxplot(anger)
plotting_boxplot(trust)
plotting_boxplot(surprise)
plotting_boxplot(disgust)
plotting_boxplot(sadness)
plotting_boxplot(fear)
plotting_boxplot(anticipation)

# anova -------------------------------------------------------------------
# assumption checks: normality
check_normality <- function(emotion) {
  # initialize empty vector
  non_normal_conditions <- c()
  # start iterating through the six conditions
  for (condition in unique(emotions$condition)) {
    # conduct shapiro test for that specific condition and emotion
    result <- shapiro.test(emotions[[emotion]][which(emotions$condition == condition)])
    # save p value from test
    p_val <- result$p.value
    # check if test suggests non-normality and if it does use it 
    if (p_val < 0.1) {
      non_normal_conditions <- append(non_normal_conditions, condition)
    }
  }
  return(non_normal_conditions)
}

check_equal_variance <- function(emotion) {
  # initialize empty vector
  non_equal_var_emotions <- c()
  
  results <- leveneTest(as.symbol(emotion)~condition, data = emotions)
  p_val <- results$`Pr(>F)`[1]
  
  if (p_val < 0.1) {
    non_equal_var_emotions <- append(non_equal_var_conditions, emotion)
  }
  return(non_equal_var_emotions)
}

leveneTest(anger ~ condition, data = emotions)
leveneTest(trust ~ condition, data = emotions)
# surprise does not share equal variance across groups
leveneTest(surprise ~ condition, data = emotions)
leveneTest(disgust ~ condition, data = emotions)
# joy does not share equal variance across groups
leveneTest(joy ~ condition, data = emotions)
leveneTest(sadness ~ condition, data = emotions)
# fear does not share equal variance across groups
leveneTest(fear ~ condition, data = emotions)
# anticipation does not share equal variance across groups
leveneTest(anticipation ~ condition, data = emotions)

# get all the emotions whose groups identified by conditions are all normally distributed
fully_normal <- c()
for (emotion in list_emotions) {
  if (length(check_normality(emotion)) == 0) {
    fully_normal <- append(fully_normal, emotion)
  }
}

# get all the emotions that share the same variance across conditions: homogeneity of variance assumption

# from the analysis above we conclude that we cannot use ANOVA due to violations of the assumption of homogeneity of variance

# Run ANOVA and pairwise comparisons (adjusted for multiple comparisons)

Honest_Significant_Difference <- function(emotion) {
    if (emotion %in% fully_normal) {
      mod <- aov(joy ~ condition, data = emotions)
      summ <- summary(mod)
      p_val <- summ[[1]]$`Pr(>F)`[1]
      
      if (p_val < 0.05) {
        return(TukeyHSD(mod))
      }
      
    } else {
      print('Variable is not normally distributed among all groups, ANOVA did not pass assumption checks')
    }
}

# ensures that the margins are adjusted properly
# par(mar = c(bottom, left, top, right))
# par(mar = c(5, 8, 4, 2))  # wider left margin
# plot(Honest_Significant_Difference('surprise'), las = 1)


# Kruskal - Wallis Test (condition)--------------------------------------------------

kruskal.test(anger ~ condition, data = emotions)
# no significant difference in trust levels across conditions
kruskal.test(trust ~ condition, data = emotions)
kruskal.test(surprise ~ condition, data = emotions)
kruskal.test(disgust ~ condition, data = emotions)
kruskal.test(joy ~ condition, data = emotions)
kruskal.test(sadness ~ condition, data = emotions)
kruskal.test(fear ~ condition, data = emotions)
kruskal.test(anticipation ~ condition, data = emotions)

# using false discovery rate to adjust the p-value for multiple comparisons
res <- pairwise.wilcox.test(emotions$anger, emotions$condition,
                     p.adjust.method = "BH")

# can use this to extract only significant p-values
res$p.value[which(res$p.value<0.1)]

pairwise.wilcox.test(emotions$surprise, emotions$condition,
                     p.adjust.method = "BH")
pairwise.wilcox.test(emotions$disgust, emotions$condition,
                     p.adjust.method = "BH")
pairwise.wilcox.test(emotions$joy, emotions$condition,
                     p.adjust.method = "BH")
pairwise.wilcox.test(emotions$sadness, emotions$condition,
                     p.adjust.method = "BH")
pairwise.wilcox.test(emotions$fear, emotions$condition,
                     p.adjust.method = "BH")
pairwise.wilcox.test(emotions$anticipation, emotions$condition,
                     p.adjust.method = "BH")

# TODO: find a nice way to show these differences



# Kruskal - Wallis test (temperature) -------------------------------------


# significant difference in anger based on temperature
kruskal.test(anger ~ temperature, data = emotions)
# significant difference in surprise based on temperature
kruskal.test(surprise ~ temperature, data = emotions)
kruskal.test(disgust ~ temperature, data = emotions)
# significant difference in joy based on temperature
kruskal.test(joy ~ temperature, data = emotions)
# NOT significant difference in sadness based on temperature
kruskal.test(sadness ~ temperature, data = emotions)
kruskal.test(fear ~ temperature, data = emotions)
kruskal.test(anticipation ~ temperature, data = emotions)
kruskal.test(trust ~ temperature, data = emotions)


# Wilcoxon Test (prompt) --------------------------------------------------

# significant difference in anger based on different prompt type
wilcox.test(anger ~ type_of_prompt, data = emotions)
wilcox.test(surprise ~ type_of_prompt, data = emotions)
# significant difference in disgust based on different prompt type
wilcox.test(disgust ~ type_of_prompt, data = emotions)
# significant difference in joy based on different prompt type
wilcox.test(joy ~ type_of_prompt, data = emotions)
# significant difference in sadness based on different prompt type
wilcox.test(sadness ~ type_of_prompt, data = emotions)
# significant difference in fear based on different prompt type
wilcox.test(fear ~ type_of_prompt, data = emotions)
# significant difference in anticipation based on different prompt type
wilcox.test(anticipation ~ type_of_prompt, data = emotions)
# significant difference in sadness based on different prompt type
wilcox.test(trust ~ type_of_prompt, data = emotions)


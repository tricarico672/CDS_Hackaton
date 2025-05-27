library(pacman)
p_load(tidyverse, ggplot2, readr, janitor, psych, ggthemes, car, stringr ,rstatix, patchwork, gridExtra, fmsb)

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
  group_by(temperature, type_of_prompt) %>% 
  summarize(n())
# get the list of emotions in the dataframe
list_emotions <- names(emotions)[5:length(emotions)-1]
# check summary statistics by condition
describeBy(emotions ~ condition)

# check summary statistics by temperature to see if more dispersion in higher temperatures
describeBy(emotions ~ temperature)

# check summary statistics by type of prompt to see if more dispersion in higher temperatures
describeBy(emotions ~ type_of_prompt)
# very balanced with respect to type of prompt (almost a perfect 50/50 split)

emotions %>% 
  group_by(condition) %>% 
  summarize(mean_anger = mean(anger),
            mean_sadness = mean(sadness))

plot_density <- function(emotion) {
  ggplot(emotions, aes(x = {{ emotion }} )) +
    geom_histogram(stat = 'density') +
    facet_wrap(~condition) +
    labs(title = str_to_sentence(as_label(enquo(emotion)))) +
    theme(plot.title = element_text(hjust = .5))
}

plot_density(surprise)
plot_density(fear)
plot_density(anticipation)
plot_density(anger)
plot_density(trust)
plot_density(joy)
plot_density(sadness)
plot_density(disgust)


plotting_boxplot <- function(variable, grouping = condition, notch = F) {
  ggplot(emotions, aes(x = {{grouping}}, y = {{ variable }})) +
    geom_boxplot(notch = notch) +
    theme(axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.ticks = element_line(linewidth = 2),
          axis.text = element_text(size = 15))
}

plotting_boxplot(surprise, grouping = temperature)
plotting_boxplot(fear, grouping = temperature)
plotting_boxplot(trust, grouping = temperature)
plotting_boxplot(joy, grouping = temperature)
plotting_boxplot(sadness, grouping = temperature)
plotting_boxplot(disgust, grouping = temperature)


names(emotions)
# include this
plotting_boxplot(joy, notch = T)
plotting_boxplot(anger)
plotting_boxplot(trust)
plotting_boxplot(surprise)
plotting_boxplot(disgust)
# include this
plotting_boxplot(sadness, notch = T)
plotting_boxplot(fear)
plotting_boxplot(anticipation)

plotting_boxplot(joy, notch = T) + plotting_boxplot(sadness, notch = T)
ggsave(filename = 'images/joy_vs_sadness.png')

grid.arrange(plotting_boxplot(joy, notch = T), plotting_boxplot(sadness, notch = T), ncol = 2)

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

# only one significant difference between p+0.7 and p+1.3
dunn_test(emotions, surprise~condition,
                     p.adjust.method = "BH")
# evidence of some difference in disgust between negative and positive prompt
dunn_test(emotions, disgust~condition,
          p.adjust.method = "BH")
# clear-cut difference in joy between negative and positive prompt
dunn_test(emotions, joy~condition,
          p.adjust.method = "BH")
# clear-cut difference in sadness between negative and positive prompt
dunn_test(emotions, sadness~condition,
          p.adjust.method = "BH")
# clear-cut difference in fear between negative and positive prompt
dunn_test(emotions, fear~condition,
          p.adjust.method = "BH")
# clear-cut difference in anticipation between negative and positive prompt
dunn_test(emotions, anticipation~condition,
          p.adjust.method = "BH")

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


# Consistency with prompt -------------------------------------------------

# check how many texts are coherent with the pre-specified prompt
emotions %>% 
  filter(joy > 1.96) %>% 
  group_by(type_of_prompt) %>% 
  summarize(frequency = n())

emotions %>% 
  filter(sadness > 1.96) %>% 
  group_by(type_of_prompt) %>% 
  summarize(frequency = n())

# it seems apparent that even when instructed to output a text with a sad tone very few texts appear to be able to do so
# indeed most of the texts seem to just output a joyful tone regardless of the type of prompt they have been supplied with

emotions_reduced <- emotions %>% 
  select(type_of_prompt, anger, trust, surprise, disgust, joy, sadness, fear, anticipation) %>% 
  group_by(type_of_prompt) %>% 
  summarise(across(everything(), mean))


# Step 2: Convert to data frame for radar chart
# Drop the 'type_of_prompt' column and make it numeric
radar_data <- as.data.frame(emotions_reduced[,-1])
rownames(radar_data) <- emotions_reduced$type_of_prompt

# Step 3: Add max and min rows (required for radarchart)
radar_data <- rbind(
  rep(4, ncol(radar_data)),    # max values
  rep(-4, ncol(radar_data)),   # min values
  radar_data                   # actual values
)

# Convert only the data rows (excluding first 2 rows) to a numeric matrix
data_matrix <- as.matrix(radar_data[-c(1, 2), ])

# Compute global max and min values
max_val <- ceiling(max(data_matrix, na.rm = TRUE))
min_val <- floor(min(data_matrix, na.rm = TRUE))

radar_data_scaled <- rbind(
  rep(max_val, ncol(data_matrix)),    
  rep(min_val, ncol(data_matrix)),   
  data_matrix
)

# Restore original column names and optionally rownames
colnames(radar_data_scaled) <- colnames(radar_data)
rownames(radar_data_scaled) <- c("Max", "Min", rownames(radar_data)[-c(1, 2)])

# png("radar_chart.png", width = 1500, height = 1500, res = 300)
radarchart(as.data.frame(radar_data_scaled),
           axistype = 0,  # Changed from 1 to 2
           vlcex = 1,   # Added axis label size
           pcol = c("blue", "red"),
           plwd = 2,
           plty = 1,
           title = "Average Emotional Profiles by Prompt Group",
           cglcol = "grey",
           cglty = 1,
           axislabcol = "black",
           caxislabels = seq(min_val, max_val, by = 1),
           calcex = 0.8,
           cglwd = 0.8,
           maxmin = TRUE)
legend("topleft", legend = rownames(radar_data_scaled)[3:nrow(radar_data_scaled)],
       col = c("blue", "red"), lty = 1, lwd = 2)
# dev.off()

# Step 1: Convert to long format
emotions_long <- emotions %>%
  pivot_longer(cols = c(joy, sadness, anger, fear, trust, anticipation, disgust, surprise),
               names_to = "emotion",
               values_to = "z_score")

# Step 2: Aggregate (e.g., compute the mean z-score per group)
emotion_summary <- emotions_long %>%
  group_by(type_of_prompt, temperature, emotion) %>%
  summarise(mean_z = mean(z_score, na.rm = TRUE), .groups = 'drop')

# Step 3: Plot heatmap
ggplot(emotion_summary, aes(x = factor(temperature), y = emotion, fill = mean_z)) +
  geom_tile(color = "white") +
  facet_wrap(~type_of_prompt) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Mean Z-Score") +
  labs(x = "Temperature", y = "Emotion",
       title = "Emotion Heatmap by Prompt Type and Temperature") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_blank(),
        legend.position = "bottom")

heatmp <- ggplot(emotion_summary, aes(x = factor(temperature), y = fct_rev(emotion), fill = mean_z)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_z, 1)), size = 3, color = "black") +  # optional, shows value
  facet_wrap(~type_of_prompt, nrow = 1, strip.position = "top") +
  scale_fill_gradient2(
    low = "#4A90E2", mid = "#F5F5F5", high = "#D0021B", midpoint = 0,
    name = "Mean Z-Score"
  ) +
  labs(
    x = "Temperature", y = NULL,
    title = "Emotion Heatmap by Prompt Type and Temperature"
  ) +
  theme_minimal(base_size = 12, base_family = "Helvetica") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid = element_blank()
  )
heatmp
ggsave("images/heatmap.png", plot = heatmp, width = 1500, height = 1500, units = "px")

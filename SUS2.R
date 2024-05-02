library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the data
data <- read_csv("SUS.csv")

# Rename the participant column and the question columns
colnames(data)[1] <- "Participant"
colnames(data)[-1] <- paste("Q", 1:10, sep="")

# Define a function to convert responses to scores
response_to_score <- function(response) {
  case_when(
    tolower(response) == "strongly disagree" ~ 1,
    tolower(response) == "disagree" ~ 2,
    tolower(response) == "neither agree nor disagree" ~ 3,
    tolower(response) == "agree" ~ 4,
    tolower(response) == "strongly agree" ~ 5
  )
}

# Apply the function across the response columns
data[2:11] <- lapply(data[2:11], function(column) sapply(column, response_to_score))

# Adjust scores for even and odd numbered questions
for (i in 1:10) {
  if (i %% 2 == 0) {
    data[,i+1] <- 5 - data[,i+1]
  } else {
    data[,i+1] <- data[,i+1] - 1
  }
}

# Calculate SUS Score
data$SUS_Score <- rowSums(data[2:11]) * 2.5

# Extracting base participant ID and session
data <- data %>%
  mutate(Base_Participant = gsub("\\(.*\\)", "", Participant),
         Session = ifelse(grepl("\\(", Participant), gsub(".*\\((.*)\\)", "\\1", Participant), "1"))

# Function to compute grade from score
sus_grade <- function(score) {
  if (score >= 91) {
    return("A")
  } else if (score >= 81) {
    return("B")
  } else if (score >= 71) {
    return("C")
  } else if (score >= 61) {
    return("D")
  } else {
    return("F")
  }
}

# Function to compute acceptability from score
sus_acceptability <- function(score) {
  if (score >= 71) {
    return("Acceptable")
  } else if (score >= 63) {
    return("Marginal high")
  } else if (score >= 50) {
    return("Marginal low")
  } else {
    return("Not acceptable")
  }
}

# Function to compute Learnability and Usability
compute_additional_metrics <- function(df) {
  df$Learnability <- (df$Q4 + df$Q10) * (100/(4*2))
  # Assuming that Q1 to Q10 are the first 10 columns after 'Participant'
  score_columns <- grep("Q[1-9]$", names(df)) # Regex to select only Q1 to Q10 columns
  df$Usability <- (rowSums(df[, score_columns]) - 5) * (100/(4*7))
  df
}

# Apply calculations to data
data <- data %>%
  mutate(
    Grade = sapply(SUS_Score, sus_grade),
    Acceptability = sapply(SUS_Score, sus_acceptability)
  ) %>%
  compute_additional_metrics() %>%
  mutate(
    LTR = 1.33 + (0.08 * SUS_Score)
  )






# Separate data by session
data_session1 <- data %>% filter(Session == "1")
data_session2 <- data %>% filter(Session == "2")


# Now let's bind the rows of session 1 and 2 together for the plotting
sus_combined <- bind_rows(data_session1, data_session2)

# Function to plot data for a session
plot_session_data <- function(df, session_number) {
  ggplot(df, aes(x = SUS_Score)) +
    geom_histogram(bins = 10, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of SUS Scores for Session", session_number),
         x = "SUS Score", y = "Count") +
    theme_minimal()
}

# Plot data for each session
plot1 <- plot_session_data(data_session1, 1)
plot2 <- plot_session_data(data_session2, 2)

# Display the plots
print(plot1)
print(plot2)




library(readr)

# Assuming sumi_session1 and sumi_session2 are already prepared and include all necessary statistics

# Save SUMI session 1 data to a CSV file
write_csv(data_session1, "SUS_Session1.csv")

# Save SUMI session 2 data to a CSV file
write_csv(data_session2, "SUS_Session2.csv")





library(ggplot2)
library(dplyr)

# Assuming sus_session1 and sus_session2 are already prepared and include SUS scores for each session

# First, let's add a 'Session' column to each dataframe
sus_session1 <- data_session1 %>% mutate(Session = "Session 1")
sus_session2 <- data_session2 %>% mutate(Session = "Session 2")

# Now let's bind the rows of session 1 and 2 together for the plotting
sus_combined <- bind_rows(sus_session1, sus_session2)

# Boxplot for SUS scores
ggplot(sus_combined, aes(x = Session, y = SUS_Score, fill = Session)) +
  geom_boxplot() +
  labs(title = "Boxplot of SUS Scores by Session",
       x = "Session",
       y = "SUS Score") +
  theme_minimal()

# Display the plot
print(sus_combined)

ggplot(sus_combined, aes(x = SUS_Score)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of SUS Scores", x = "SUS Score", y = "Count") +
  theme_minimal()


ggplot(sus_combined, aes(x = as.factor(Session), y = SUS_Score, fill = as.factor(Session))) +
  geom_boxplot() +
  labs(title = "Boxplot of SUS Scores by Session", x = "Session", y = "SUS Score") +
  theme_minimal()

ggplot(sus_combined, aes(x = SUS_Score, y = LTR)) +
  geom_point(aes(color = as.factor(Session))) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of SUS Score vs. LTR", x = "SUS Score", y = "LTR") +
  theme_minimal()

ggplot(sus_combined, aes(x = as.numeric(Session), y = SUS_Score, group = Base_Participant)) +
  geom_line(aes(color = as.factor(Base_Participant))) +
  geom_point(size = 2, aes(color = as.factor(Base_Participant))) +
  labs(title = "Line Plot of SUS Scores Over Sessions", x = "Session", y = "SUS Score") +
  theme_minimal() +
  theme(legend.position = "none")

library(dplyr)
library(tidyr)
library(dplyr)
library(tidyr)

# Assuming your data frame is named 'merged_data'

# Calculate descriptive statistics for each numerical variable
descriptive_stats <- sus_combined %>%
  summarise(
    Mean_SUS = mean(SUS_Score, na.rm = TRUE),
    SD_SUS = sd(SUS_Score, na.rm = TRUE),
    Min_SUS = min(SUS_Score, na.rm = TRUE),
    Max_SUS = max(SUS_Score, na.rm = TRUE),
    Mean_LTR = mean(LTR, na.rm = TRUE),
    SD_LTR = sd(LTR, na.rm = TRUE),
    Min_LTR = min(LTR, na.rm = TRUE),
    Max_LTR = max(LTR, na.rm = TRUE),
    Mean_Usability = mean(Usability, na.rm = TRUE),
    SD_Usability = sd(Usability, na.rm = TRUE),
    Min_Usability = min(Usability, na.rm = TRUE),
    Max_Usability = max(Usability, na.rm = TRUE),
    Mean_Learnability = mean(Learnability, na.rm = TRUE),
    SD_Learnability = sd(Learnability, na.rm = TRUE),
    Min_Learnability = min(Learnability, na.rm = TRUE),
    Max_Learnability = max(Learnability, na.rm = TRUE)
  ) %>%
  # Transpose the data frame to get statistics as rows and variables as columns
  gather(key = "Stats", value = "Value", everything()) %>%
  separate(Stats, into = c("Statistic", "Variable"), sep = "_") %>%
  spread(key = "Variable", value = "Value")

# View the transposed summary statistics
print(descriptive_stats)





ggplot(sus_combined, aes(x = SUS_Score)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of SUS Scores", x = "SUS Score", y = "Count")



ggplot(sus_combined, aes(x = SUS_Score, y = LTR, color = as.factor(Session))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of SUS Scores vs. LTR by Session",
       x = "SUS Score", y = "LTR",
       color = "Session") +
  theme_minimal()


sus_combined %>%
  group_by(Session) %>%
  summarise(Mean_SUS_Score = mean(SUS_Score, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(Session), y = Mean_SUS_Score, fill = as.factor(Session))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean SUS Scores by Session",
       x = "Session", y = "Mean SUS Score",
       fill = "Session") +
  theme_minimal()


average_sus <- sus_combined %>%
  group_by(Session) %>%
  summarise(Average_SUS = mean(SUS_Score))

ggplot(average_sus, aes(x = as.factor(Session), y = Average_SUS, group = 1)) +
  geom_line() +
  geom_point(size = 4) +
  labs(title = "Trend of Average SUS Scores Across Sessions",
       x = "Session", y = "Average SUS Score") +
  theme_minimal()


# Assuming you already have a dataframe 'descriptive_stats' with the stats calculated

# Reshape the data for plotting
descriptive_stats_long <- descriptive_stats %>%
  pivot_longer(cols = -Statistic, names_to = "Variable", values_to = "Value")

# Plotting descriptive statistics as a bar chart
ggplot(descriptive_stats_long, aes(x = Statistic, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Descriptive Statistics of SUS Scores", x = "Statistic", y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")



library(ggplot2)
library(dplyr)

# Calculate mean SUS Score from your data
mean_sus <- mean(data$SUS_Score, na.rm = TRUE)

# Define the ranges for the acceptability levels and corresponding colors
acceptability_levels <- data.frame(
  Level = c("Not Acceptable", "Marginal Low", "Marginal High", "Acceptable"),
  Min = c(0, 50, 63, 71),
  Max = c(49, 62, 70, 100),
  Color = c("#FFC7CE", "#FFEB9C", "#C6EFCE", "#9C00B3FF")
)

# Define the grades
grades <- data.frame(
  Grade = c("F", "D", "C", "B", "A"),
  Min = c(0, 50, 60, 70, 85),
  Max = c(49, 59, 69, 84, 100),
  LabelX = c(25, 55, 65, 77, 92.5)
)


# Base plot
plot <- ggplot() +
  geom_rect(data = acceptability_levels, aes(xmin = Min, xmax = Max, ymin = 0, ymax = 1, fill = Color), alpha = 0.5) +
  scale_fill_identity() +
  geom_text(data = grades, aes(x = LabelX, y = 0.5, label = Grade), size = 6, color = "black") +
  geom_vline(xintercept = mean_sus, color = "blue", linetype = "dashed") +
  annotate("text", x = mean_sus, y = 1.05, label = paste("Mean SUS Score:", round(mean_sus, 1)), vjust = 1.5, color = "blue") +
  labs(title = "SUS Mean Score Interpretation with Market Comparison",
       x = "SUS Score", y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Print the plot
print(plot)



library(ggplot2)
library(dplyr)

# Calculate mean SUS Score
mean_sus <- mean(sus_combined$SUS_Score, na.rm = TRUE)

# Calculate 95% Confidence Interval for Mean
ci_width <- qt(0.975, df = nrow(sus_combined) - 1) * sd(sus_combined$SUS_Score, na.rm = TRUE) / sqrt(nrow(sus_combined))
ci_lower <- mean_sus - ci_width
ci_upper <- mean_sus + ci_width

# Define the market average
market_average <- 68

# Maximum density for label placement
max_density <- max(density(sus_combined$SUS_Score)$y)

# Create the plot
plot <- ggplot(sus_combined, aes(x = SUS_Score)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +  # Simple fill color
  geom_vline(xintercept = mean_sus, color = "blue", linetype = "dashed", linewidth = 1.5) +
  geom_vline(xintercept = ci_lower, color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = ci_upper, color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = market_average, color = "gray", linetype = "dotted", linewidth = 1.5) +
  geom_text(aes(x = mean_sus, y = max_density, label = "Mean"), vjust = 1.5, hjust = 3, color = "blue", angle = 90) +
  geom_text(aes(x = ci_lower, y = max_density, label = "Lower 95% CI"), vjust = 1.5, hjust = 3, color = "red", angle = 90) +
  geom_text(aes(x = ci_upper, y = max_density, label = "Upper 95% CI"), vjust = 1.5, hjust = 3, color = "red", angle = 90) +
  geom_text(aes(x = market_average, y = max_density, label = "Market Avg"), vjust = 1.5, hjust = 3, color = "gray", angle = 90) +
  labs(title = "Distribution of SUS Scores with Market Average Comparison",
       subtitle = "95% CI and market average SUS score (68)",
       x = "SUS Scores", y = "Density") +
  theme_minimal()

# Display the plot
print(plot)

















library(ggplot2)
library(dplyr)

# Assuming you have two dataframes, sus_session1 and sus_session2, which contain the SUS scores
# and a participant identifier. We'll join them by this identifier.

# Combine the data from both sessions
sus_combined <- full_join(sus_session1, sus_session2, by = "Participant", suffix = c("_Session1", "_Session2")) %>%
  # Pivot longer for ggplot
  pivot_longer(cols = starts_with("SUS_Score"), names_to = "Session", values_to = "SUS_Score") %>%
  # Transform the session column to a factor for plotting
  mutate(Session = factor(Session, levels = c("SUS_Score_Session1", "SUS_Score_Session2")))

# Plot with individual points and lines for each participant
ggplot(sus_combined, aes(x = Session, y = SUS_Score, group = Participant)) +
  geom_line(aes(group = Participant), alpha = 0.3) +  # Draw lines with low opacity
  geom_point(aes(color = Participant)) +  # Use color to distinguish participants
  labs(title = "SUS Scores Across Sessions",
       x = "Session",
       y = "SUS Score",
       color = "Participant") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend to avoid clutter

# Print the plot
ggsave("SUS_Scores_Line_Plot.png", width = 10, height = 6, dpi = 300)





















library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(tibble)
library(stats)

# Assuming data_session1 and data_session2 have been processed as per previous steps...

# Step 1: Join the session data
# Ensure that the data frames are properly sorted by Base_Participant before joining.
comparison_data <- inner_join(
  data_session1 %>% select(Base_Participant, SUS_Score, Grade, Acceptability, Learnability, Usability, LTR),
  data_session2 %>% select(Base_Participant, SUS_Score, Grade, Acceptability, Learnability, Usability, LTR),
  by = "Base_Participant",
  suffix = c("_session1", "_session2")
)

# Step 2: Conduct paired comparison tests
# Use a paired t-test if the data is parametric
paired_t_test_result <- t.test(comparison_data$SUS_Score_session1, comparison_data$SUS_Score_session2, paired = TRUE)
print(paired_t_test_result)

# Use Wilcoxon signed-rank test for non-parametric data
wilcoxon_test_result <- wilcox.test(comparison_data$SUS_Score_session1, comparison_data$SUS_Score_session2, paired = TRUE)
print(wilcoxon_test_result)

# Step 3: Perform correlation analysis
# Pearson correlation for normally distributed data
pearson_correlation <- cor.test(comparison_data$SUS_Score_session1, comparison_data$SUS_Score_session2, method = "pearson")
print(pearson_correlation)

# Spearman's rank correlation for non-normally distributed data
spearman_correlation <- cor.test(comparison_data$SUS_Score_session1, comparison_data$SUS_Score_session2, method = "spearman")
print(spearman_correlation)

# Additional Analysis: Visualizing the results
# Scatter plot to visualize the correlation between sessions
ggplot(comparison_data, aes(x = SUS_Score_session1, y = SUS_Score_session2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Correlation of SUS Scores Between Two Sessions",
       x = "Session 1 SUS Score",
       y = "Session 2 SUS Score") +
  theme_minimal()









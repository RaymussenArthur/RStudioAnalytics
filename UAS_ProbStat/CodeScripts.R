# Load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(stats)
library(e1071)

# Read the data
data_path <- "C:/Rcode/UAS_ProbStat/formatted_data_Customers.xlsx"
data <- read_excel(data_path)

# Rename columns for consistency
colnames(data) <- gsub("Spending Score \\(1-100\\)", "Spending_Score", colnames(data))
colnames(data) <- gsub("Annual Income \\(\\$\\)", "Annual_Income", colnames(data))

# Check the new column names
cat("\nUpdated Column Names:\n")
print(colnames(data))

# Filter data by Profession
data <- data[data$Profession %in% c("Healthcare", "Artist"), ]

# Create Gender_Num variable
data$Gender_Num <- ifelse(data$Gender == "F", 1, 0)



# Chi-Square Test
cat("\nChi-Square Test:\n")
# One-way goodness of fit test (example with Gender)
gender_table <- table(data$Gender)
chi_square_goodness <- chisq.test(gender_table)
print(chi_square_goodness)

# Two-way contingency table test (example with Gender and Profession)
gender_profession_table <- table(data$Gender, data$Profession)
chi_square_independence <- chisq.test(gender_profession_table)
print(chi_square_independence)


# T-Test
cat("\nT-Test:\n")
t_test_result <- t.test(Spending_Score ~ Gender, data = data)
print(t_test_result)

# Uji Hipotesis 2 Sisi
cat("\nUji Hipotesis 2 Sisi:\n")
t_test_two_sided <- t.test(data$Annual_Income, mu = 50000)
print(t_test_two_sided)

# Regresi Linear (Hubungan antara dua variabel numerik)
cat("\nRegresi Linear:\n")
reg_model <- lm(Spending_Score ~ Annual_Income, data = data)
summary(reg_model)

# Scatter plot with regression line
ggplot(data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Regresi Linear Spending Score vs Annual Income")



# Selang Kepercayaan 2 Sisi
ci_two_sided <- t.test(data$Spending_Score, conf.level = 0.95)
cat("\nSelang Kepercayaan 2 Sisi:\n")
print(ci_two_sided)

# Selang Kepercayaan 1 Sisi (Upper dan Lower)
# Upper
ci_upper <- t.test(data$Spending_Score, alternative = "less", conf.level = 0.95)
cat("\nSelang Kepercayaan 1 Sisi (Upper):\n")
print(ci_upper)

# Lower
ci_lower <- t.test(data$Spending_Score, alternative = "greater", conf.level = 0.95)
cat("\nSelang Kepercayaan 1 Sisi (Lower):\n")
print(ci_lower)


# Uji Normalitas (Shapiro-Wilk test dan Q-Q plot)
cat("\nUji Normalitas:\n")
shapiro_test <- shapiro.test(data$Annual_Income)
print(shapiro_test)

# Q-Q plot
ggplot(data, aes(sample = Annual_Income)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot for Annual Income")


# Create distribution table
cat("\nDistribution Table:\n")
distribution_table <- data %>% 
  group_by(Gender, Profession) %>% 
  summarise(Count = n(), .groups = 'drop') %>% 
  mutate(Percentage = round((Count / sum(Count)) * 100, 2))

# Print the distribution table
print(distribution_table)

# Save distribution table as CSV
distribution_table_path <- "C:/Rcode/distribution_table.csv"
write.csv(distribution_table, distribution_table_path, row.names = FALSE)

cat("\nDistribution table saved to:", distribution_table_path, "\n")


# Visualization
## 1. Histogram for Spending_Score
ggplot(data, aes(x = Spending_Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Spending Score", x = "Spending Score", y = "Frequency") +
  theme_minimal()

## 2. Frequency Polygon for Annual_Income
ggplot(data, aes(x = Annual_Income)) +
  geom_freqpoly(binwidth = 5000, color = "red", size = 1) +
  labs(title = "Frequency Polygon of Annual Income", x = "Annual Income ($)", y = "Frequency") +
  theme_minimal()

## 3. Scatter Plot for Annual_Income vs Spending_Score
ggplot(data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point(color = "purple", size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot: Annual Income vs Spending Score", x = "Annual Income ($)", y = "Spending Score") +
  theme_minimal()

# For qualitative data
## 1. Pie Chart for Gender Distribution
gender_count <- data %>% group_by(Gender) %>% summarise(Count = n())
ggplot(gender_count, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Gender Distribution", fill = "Gender") +
  theme_void()

## 2. Bar Chart for Profession Distribution
profession_count <- data %>% group_by(Profession) %>% summarise(Count = n())
ggplot(profession_count, aes(x = Profession, y = Count, fill = Profession)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(title = "Bar Chart of Profession Distribution", x = "Profession", y = "Count") +
  theme_minimal()


calc_stats <- function(x) {
  return(list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    std_dev = sd(x, na.rm = TRUE),
    quartile_1 = quantile(x, 0.25, na.rm = TRUE),
    quartile_3 = quantile(x, 0.75, na.rm = TRUE),
    skewness = skewness(x, na.rm = TRUE),
    kurtosis = kurtosis(x, na.rm = TRUE)
  ))
}

statistics <- list()

# Calculate statistics for Spending Score
statistics$Spending_Score <- calc_stats(data$Spending_Score)

# Calculate statistics for Annual Income
statistics$Annual_Income <- calc_stats(data$Annual_Income)

# Covariance and Correlation
covariance <- cov(data$Spending_Score, data$Annual_Income, use = "complete.obs")
correlation <- cor(data$Spending_Score, data$Annual_Income, use = "complete.obs", method = "pearson")

# Output the results
cat("\nStatistics for Spending Score:\n")
print(statistics$Spending_Score)

cat("\nStatistics for Annual Income:\n")
print(statistics$Annual_Income)

cat("\nCovariance between Spending Score and Annual Income:\n")
print(covariance)

cat("\nPearson Correlation between Spending Score and Annual Income:\n")
print(correlation)

# Output the results
cat("\nStatistics for Spending Score:\n")
print(statistics$Spending_Score)

cat("\nStatistics for Annual Income:\n")
print(statistics$Annual_Income)

cat("\nCovariance between Spending Score and Annual Income:\n")
print(covariance)

cat("\nPearson Correlation between Spending Score and Annual Income:\n")
print(correlation)

# Gender and Profession summaries
gender_summary <- table(data$Gender)
profession_summary <- table(data$Profession)

cat("\nGender Distribution:\n")
print(gender_summary)

cat("\nProfession Distribution:\n")
print(profession_summary)



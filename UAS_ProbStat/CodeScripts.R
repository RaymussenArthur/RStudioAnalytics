# Load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(stats)
library(e1071)

# Load data from excel
data_path <- "C:/Rcode/UAS_ProbStat/formatted_data_Customers.xlsx"
data <- read_excel(data_path)

# Rename column
colnames(data) <- gsub("Spending Score \\(1-100\\)", "Spending_Score", colnames(data))
colnames(data) <- gsub("Annual Income \\(\\$\\)", "Annual_Income", colnames(data))

# Filter data by profession (Artist and Healthcare)
data <- data[data$Profession %in% c("Healthcare", "Artist"), ]

# Add Gender column
data$Gender_Num <- ifelse(data$Gender == "F", 1, 0)

# Data distribution table
cat("\nDistribusi Gender dan Profesi:\n")
print(table(data$Gender))
print(table(data$Profession))

distribution_table <- data %>% 
  group_by(Gender, Profession) %>% 
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 2))
print(distribution_table)

# 3. Statistic descriptif 
calc_stats <- function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    std_dev = sd(x, na.rm = TRUE),
    quartile_1 = quantile(x, 0.25, na.rm = TRUE),
    quartile_3 = quantile(x, 0.75, na.rm = TRUE),
    skewness = skewness(x, na.rm = TRUE),
    kurtosis = kurtosis(x, na.rm = TRUE)
  )
}

stats_spending <- calc_stats(data$Spending_Score)
stats_income <- calc_stats(data$Annual_Income)

cat("\nStatistik Spending Score:\n")
print(stats_spending)
cat("\nStatistik Annual Income:\n")
print(stats_income)

# Covariance and correlation
covariance <- cov(data$Spending_Score, data$Annual_Income)
correlation <- cor(data$Spending_Score, data$Annual_Income)

cat("\nKovariansi:\n")
print(covariance)
cat("\nKorelasi Pearson:\n")
print(correlation)

# Visualization
# Histogram Spending Score
ggplot(data, aes(x = Spending_Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram Spending Score", x = "Spending Score", y = "Frekuensi") +
  theme_minimal()

# Frequency Polygon Annual Income
ggplot(data, aes(x = Annual_Income)) +
  geom_freqpoly(binwidth = 5000, color = "red", size = 1) +
  labs(title = "Poligon Frekuensi Annual Income", x = "Annual Income ($)", y = "Frekuensi") +
  theme_minimal()

# Scatter plot Annual Income and Spending Score
ggplot(data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point(color = "purple", size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot: Annual Income vs Spending Score") +
  theme_minimal()

# Bar Chart Profesi
profession_count <- data %>% group_by(Profession) %>% summarise(Count = n())
ggplot(profession_count, aes(x = Profession, y = Count, fill = Profession)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribusi Profesi") +
  theme_minimal()

# Pie Chart Gender
gender_count <- data %>% group_by(Gender) %>% summarise(Count = n())
ggplot(gender_count, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribusi Gender") +
  theme_void()


# Normality test with Shapiro-Wilk
cat("\nUji Normalitas Shapiro-Wilk Annual Income:\n")
shapiro_test <- shapiro.test(data$Annual_Income)
print(shapiro_test)

ggplot(data, aes(sample = Annual_Income)) +
  stat_qq() + stat_qq_line() +
  ggtitle("Q-Q Plot Annual Income")

# Confidence interval
cat("\nSelang Kepercayaan 95% Spending Score:\n")
ci_two_sided <- t.test(data$Spending_Score)
print(ci_two_sided)

# T-test
# Spending Score by Gender
cat("\nT-Test Spending Score by Gender:\n")
t_test_gender <- t.test(Spending_Score ~ Gender, data = data)
print(t_test_gender)

# Annual Income with 50000
cat("\nUji-T Annual Income vs 50000:\n")
t_test_income <- t.test(data$Annual_Income, mu = 50000)
print(t_test_income)

# Linear regression
cat("\nRegresi Linear Spending Score ~ Annual Income:\n")
reg_model <- lm(Spending_Score ~ Annual_Income, data = data)
print(summary(reg_model))

ggplot(data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regresi Linear Spending Score vs Annual Income")

# Chi-square test
cat("\nChi-Square Goodness of Fit Gender:\n")
chi_gender <- chisq.test(table(data$Gender))
print(chi_gender)

cat("\nChi-Square Independence Gender vs Profession:\n")
chi_independence <- chisq.test(table(data$Gender, data$Profession))
print(chi_independence)

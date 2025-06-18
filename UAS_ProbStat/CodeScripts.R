# Load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(stats)
library(e1071)

# ===============================
# 1. Load dan Preprocessing Data
# ===============================
data_path <- "C:/Rcode/UAS_ProbStat/formatted_data_Customers.xlsx"
data <- read_excel(data_path)

# Rename kolom
colnames(data) <- gsub("Spending Score \\(1-100\\)", "Spending_Score", colnames(data))
colnames(data) <- gsub("Annual Income \\(\\$\\)", "Annual_Income", colnames(data))

# Filter data Artist dan Healthcare
data <- data[data$Profession %in% c("Healthcare", "Artist"), ]

# Tambah kolom Gender Num
data$Gender_Num <- ifelse(data$Gender == "F", 1, 0)

# ===============================
# 2. Distribusi Data Kategorikal
# ===============================
cat("\nDistribusi Gender dan Profesi:\n")
print(table(data$Gender))
print(table(data$Profession))

# Tabel distribusi kombinasi
distribution_table <- data %>% 
  group_by(Gender, Profession) %>% 
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 2))
print(distribution_table)

# ===============================
# 3. Statistik Deskriptif Numerik
# ===============================
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

# ===============================
# 4. Analisis Kovarians & Korelasi
# ===============================
covariance <- cov(data$Spending_Score, data$Annual_Income)
correlation <- cor(data$Spending_Score, data$Annual_Income)

cat("\nKovariansi:\n")
print(covariance)
cat("\nKorelasi Pearson:\n")
print(correlation)

# ===============================
# 5. Visualisasi Data
# ===============================

# Histogram Spending Score
ggplot(data, aes(x = Spending_Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram Spending Score", x = "Spending Score", y = "Frekuensi") +
  theme_minimal()

# Poligon Frekuensi Annual Income
ggplot(data, aes(x = Annual_Income)) +
  geom_freqpoly(binwidth = 5000, color = "red", size = 1) +
  labs(title = "Poligon Frekuensi Annual Income", x = "Annual Income ($)", y = "Frekuensi") +
  theme_minimal()

# Scatter plot Annual Income vs Spending Score
ggplot(data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point(color = "purple", size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot: Annual Income vs Spending Score") +
  theme_minimal()

# Diagram Batang Profesi
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

# ===============================
# 6. Uji Normalitas
# ===============================
cat("\nUji Normalitas Shapiro-Wilk Annual Income:\n")
shapiro_test <- shapiro.test(data$Annual_Income)
print(shapiro_test)

ggplot(data, aes(sample = Annual_Income)) +
  stat_qq() + stat_qq_line() +
  ggtitle("Q-Q Plot Annual Income")

# ===============================
# 7. Inferensi: Selang Kepercayaan
# ===============================
cat("\nSelang Kepercayaan 95% Spending Score:\n")
ci_two_sided <- t.test(data$Spending_Score)
print(ci_two_sided)

# ===============================
# 8. Uji-T Dua Populasi & Satu Populasi
# ===============================
# Spending Score berdasarkan Gender
cat("\nT-Test Spending Score by Gender:\n")
t_test_gender <- t.test(Spending_Score ~ Gender, data = data)
print(t_test_gender)

# Annual Income dibanding 50000
cat("\nUji-T Annual Income vs 50000:\n")
t_test_income <- t.test(data$Annual_Income, mu = 50000)
print(t_test_income)

# ===============================
# 9. Regresi Linear
# ===============================
cat("\nRegresi Linear Spending Score ~ Annual Income:\n")
reg_model <- lm(Spending_Score ~ Annual_Income, data = data)
print(summary(reg_model))

ggplot(data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regresi Linear Spending Score vs Annual Income")

# ===============================
# 10. Uji Chi-Square
# ===============================
cat("\nChi-Square Goodness of Fit Gender:\n")
chi_gender <- chisq.test(table(data$Gender))
print(chi_gender)

cat("\nChi-Square Independence Gender vs Profession:\n")
chi_independence <- chisq.test(table(data$Gender, data$Profession))
print(chi_independence)

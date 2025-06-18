# Customer Spending Pattern Analysis

This is my final project for Probability and Statistic lecture. This is a statistical analysis of customer data to explore the relationship between **Gender**, **Annual Income**, and **Profession** with **Spending Score**. The analysis was performed using **R** and includes descriptive statistics, inferential tests, regression analysis, and visualizations.

---

## 📌 Project Summary

This analysis aims to:

- Analyze the distribution of **gender** and **profession** in customer data.
- Perform descriptive statistics for **Annual Income** and **Spending Score**.
- Calculate the **covariance** and **correlation** between spending and income.
- Visualize data distributions using histograms, frequency polygons, scatter plots, pie charts, and bar charts.
- Test for **normality** using the Shapiro-Wilk test and Q-Q plots.
- Conduct **T-tests** for single and two-population mean comparisons.
- Build a **simple linear regression model**.
- Perform **Chi-Square tests** for goodness of fit and independence.

---

## Data Description

The dataset contains information from **2,000 customers**. After filtering by the two most dominant professions:
- **Artist (31%)**
- **Healthcare (17%)**

A total of **951 customers** were used for analysis.

Key variables:
- `Annual Income ($)`
- `Spending Score (1-100)`
- `Gender` (F/M)
- `Profession` (Artist/Healthcare)

---

## Features & Analysis Performed

- Distribution tables and descriptive statistics
- Data visualizations:
  - Histogram
  - Frequency polygon
  - Scatter plot
  - Pie chart
  - Bar chart
- Covariance and Pearson correlation
- Shapiro-Wilk normality test and Q-Q plot
- Confidence intervals (CI 95%) for spending score
- T-tests:
  - One-sample for Annual Income vs 50,000
  - Two-sample for Spending Score by Gender
- Simple Linear Regression:
  - Spending Score ~ Annual Income
- Chi-Square tests:
  - Goodness of Fit (Gender distribution)
  - Independence test (Gender vs Profession)

---

## Technologies Used

- **R** (version 4.x)
- **RStudio**
- Libraries:
  - `tidyverse`
  - `readxl`
  - `ggplot2`
  - `stats`
  - `e1071`

---

## How to Run

1. **Clone this repository**
   ```bash
  

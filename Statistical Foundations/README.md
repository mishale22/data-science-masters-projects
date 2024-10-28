# 📊 Course: Statistical Foundations of Data Science 

## 🐧 Project: Palmer Penguins Data Analysis

## ⚡️ Overview
This project conducts an exploratory data analysis (EDA) on a dataset of penguins to investigate key physical characteristics and identify patterns related to species, sex, and island habitat. The analysis aims to estimate population-wide probabilities, assess methods for distinguishing penguin sex based on physical traits, and evaluate island-related differences among the species. 

## 🎯 Project Objectives
1. **Exploratory Data Analysis**: Generate graphical and numerical summaries to explore the relationships, patterns, and outliers within the dataset.
2. **Probability Distribution Fitting**: Fit probability distributions to measurement variables (e.g., bill length) to estimate population parameters.
3. **Sexing Analysis**: Identify the best variables to distinguish between male and female penguins. 
4. **Island Analysis**: Assess if island habitat significantly influences penguins' physical traits. 

## 📊 Dataset
The dataset includes 200 penguins sampled from three islands. Each penguin is described by:
- **Categorical Variables**: Species, island, and sex
- **Quantitative Variables**: Bill length, bill depth, flipper length, and body mass

Measurements were taken over a three-year period (2007-2009).

## 📂 Folder Structure

- `notebooks/`: Analysis code, including data visualizations and statistical tests.
- `reports/`: Contains the final project report in PDF format.

## 🛠️ Methods

1. **Exploratory Data Analysis (EDA)**: Statistical summaries and visualizations, such as box plots and scatter plots, to explore variable relationships and patterns.
2. **Distribution Fitting**: Fitting probability distributions to measurement data (e.g., normal distribution for bill length) to model population proportions.
3. **Statistical Testing**: T-tests to assess differences in characteristics by sex and island, and Bartlett tests for variance homogeneity.
4. **Logistic Regression**: Introduced as an approach to estimate penguin sex using measurement variables.

## 🔍 Key Findings

- **Species Differences**: Gentoo penguins tend to have longer bills, flippers, and higher body mass compared to Adelie and Chinstrap penguins.
- **Distribution Insights**: Bill length distributions suggest a multimodal pattern, especially when split by species, indicating varied characteristics among groups.
- **Sex Identification**: Bill depth and body mass are generally higher in male penguins, with statistical support for sex differences in these measurements.
- **Island Effects**: Physical traits like flipper length and body mass vary across islands, highlighting possible environmental or evolutionary influences.


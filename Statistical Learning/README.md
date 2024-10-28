# 📊 Course: Statistical Learning of Data Science

## 🩺 Project: Breast Cancer Data Analysis

## ⚡️Overview
This project analyses a breast cancer dataset to classify tumors as benign or malignant based on various cell characteristics. The goal is to build and evaluate predictive models, including logistic regression, best subset selection, and discriminant analysis, to identify the best classifier for this binary classification problem. Key performance metrics like precision and recall are calculated, with a particular focus on minimising the risk of false positives due to the healthcare context.

## 🎯 Project Objectives
1. **Exploratory Data Analysis**: Examine the dataset’s structure, identify patterns, and assess feature correlations to understand the relationships between predictors.
2. **Logistic Regression Model**: Develop a logistic regression model to classify samples and interpret feature importance.
3. **Subset Selection and Regularization**: Use best subset selection and LASSO to optimise the model, removing irrelevant predictors to improve performance.
4. **Discriminant Analysis**: Apply linear discriminant analysis (LDA) for a comparison with logistic regression models.
5. **Model Comparison**: Use cross-validation to compare model performance based on precision, recall, and test error rates to select the best classifier.

## 📊 Dataset
The breast cancer dataset includes 683 samples, each labeled as benign or malignant. Predictor variables include features of cell characteristics, which are ordinal categorical values between 0 and 10. Larger values indicate greater malignancy likelihood.

## 📂 Folder Structure

- `notebooks/`: Analysis code, including data visualizations and model implementations.
- `reports/`: Final project report in PDF format.

## 🛠️ Methods Used

1. **Exploratory Data Analysis (EDA)**: Visual and statistical summaries, including covariance and correlation matrices.
2. **Logistic Regression**: A logistic regression classifier with transformation and scaling of data for uniformity.
3. **Best Subset Selection**: Identifying optimal predictors using AIC and BIC criteria for logistic regression.
4. **Regularized Logistic Regression (LASSO)**: Applying LASSO to reduce model complexity by penalising less significant predictors.
5. **Discriminant Analysis (LDA)**: Linear discriminant analysis to examine the classifier's performance compared to logistic regression.
6. **Cross-Validation**: Used to validate models and assess accuracy, precision, recall, and F1-score metrics.

## 🔍 Key Findings

- **Class Imbalance**: Benign samples (444) outnumber malignant samples (239), impacting class distribution.
- **Correlation Insights**: Strong correlations exist between several predictors (e.g., Uniformity of Cell Shape and Size), which influence model performance.
- **Subset Selection**: Logistic regression with a subset of six predictors showed the lowest test error and best performance metrics.
- **Model Comparison**: Best subset selection outperformed LASSO and LDA in precision, accuracy, and F1-score, making it the preferred model for this dataset.
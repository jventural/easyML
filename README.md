# easyML: Easy Machine Learning in R

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/jventural/easyML)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-2.0.0-blue)](https://github.com/jventural/easyML)

## Overview

**easyML** is a comprehensive R package that simplifies machine learning workflows for classification and regression. With a single function call (`easy_ml()`), the package automates the entire pipeline: exploratory data analysis, preprocessing, model training, hyperparameter tuning, evaluation, interpretability, and report generation.

Additionally, easyML provides specialized tools for **sample size estimation** via Monte Carlo simulation, and advanced **train/test splitting** strategies (block-based and diversity-based) for rigorous generalization assessment.

## Installation

```r
# Install from GitHub
devtools::install_github("jventural/easyML")
```

## Quick Start

### Complete ML Pipeline

```r
library(easyML)
library(mlbench)
data(PimaIndiansDiabetes)

# One function does everything
result <- easy_ml(
  data = PimaIndiansDiabetes,
  target = "diabetes",
  task = "classification",
  seed = 2026
)

# View results
print(result)
summary(result)
plot(result)

# Make predictions on new data
predictions <- predict(result, new_data)
```

### Sample Size Estimation

```r
# Estimate minimum sample size for classification (AUC >= 0.80)
size_result <- ml_sample_size(
  task = "classification",
  metric = "auc",
  target = 0.80,
  seed = 2026,
  reps = 100
)

# View recommendation
size_result$recommend_n

# Plot learning curve
plot_learning_curve(size_result)
```

## The `easy_ml()` Pipeline

`easy_ml()` runs 8 stages automatically:

| Stage | Description |
|-------|-------------|
| 1. EDA | Dataset structure, target distribution, normality, missing values, outliers, correlations, VIF |
| 2. Preprocessing | Train/test split, imputation, normalization, dummy encoding, class balancing, PCA |
| 3. Feature Engineering | Optional automatic or custom feature creation |
| 4. Modeling | Train multiple models with stratified k-fold cross-validation |
| 5. Tuning | Hyperparameter optimization (random search, grid, Bayesian, racing) |
| 6. Evaluation | Test set metrics, ROC, confusion matrix, calibration, threshold optimization |
| 7. Interpretability | Variable importance and SHAP values |
| 8. Visualization | Automatic generation of all diagnostic plots |

### Parameters

```r
easy_ml(
  data,
  target,
  task = c("auto", "classification", "regression"),
  models = c("rf", "xgboost", "svm", "nnet", "glm", "tree"),
  test_split = 0.20,
  cv_folds = 10,
  tune_best = TRUE,
  tune_method = c("random", "grid", "bayes", "racing"),
  feature_selection = FALSE,
  balance_classes = FALSE,
  balance_method = c("smote", "adasyn", "rose", "up", "down"),
  impute = TRUE,
  impute_method = c("median", "mean", "knn"),
  normalize = TRUE,
  normalize_method = c("zscore", "minmax"),
  use_pca = FALSE,
  run_eda = TRUE,
  run_shap = TRUE,
  optimize_threshold = TRUE,
  calibrate_probs = FALSE,
  check_leakage = TRUE,
  nested_cv = FALSE,
  seed = 2024,
  verbose = TRUE
)
```

### Supported Models

| Model | Code | Engine |
|-------|------|--------|
| Random Forest | `"rf"` | ranger |
| XGBoost | `"xgboost"` | xgboost |
| SVM (RBF) | `"svm"` | kernlab |
| Neural Network | `"nnet"` | nnet |
| Logistic/Linear Regression | `"glm"` | glmnet |
| Decision Tree | `"tree"` | rpart |

### Working with Results

```r
# Print summary
print(result)

# Detailed comparison table
summary(result)

# Visualizations
plot(result)                        # 2x2 panel
plot(result, type = "importance")   # Variable importance
plot(result, type = "roc")          # ROC curve
plot(result, type = "confusion")    # Confusion matrix
plot(result, type = "calibration")  # Calibration plot
plot(result, type = "shap")         # SHAP summary
plot(result, type = "threshold")    # Threshold optimization
plot(result, type = "tuning")       # Hyperparameter tuning

# Save all plots
save_all_plots(result, path = "figures")

# Predictions
predict(result, new_data)
predict(result, new_data, type = "prob")  # Probabilities
```

## Module Functions

Each pipeline stage can also be used independently:

### Exploratory Data Analysis

```r
eda_summary(data, target, task)       # Complete EDA
eda_structure(data)                   # Dataset structure
eda_missing(data)                     # Missing values analysis
eda_outliers(data)                    # Outlier detection (IQR)
eda_correlation(data, target)         # Correlation matrix
eda_vif(data, target)                 # Multicollinearity (VIF)
eda_normality(data, target)           # Shapiro-Wilk normality test
```

### Preprocessing

```r
preprocess_data(data, target, task)   # Full preprocessing pipeline
prep_split(data, target, test_split)  # Train/test split
prep_feature_selection(data, target)  # Boruta feature selection
prep_recipe(data, target, task)       # tidymodels recipe creation
```

### Modeling

```r
train_models(data, target, task, models)  # Train and compare models
setup_cv(data, cv_folds, task)            # Configure cross-validation
define_models(models, task)               # Model specifications
compare_models(cv_results, task)          # Compare and select best
```

### Tuning

```r
tune_best_model(model, recipe, cv, task,
                method = "random",     # "random", "grid", "bayes", "racing"
                grid_size = 20)
```

### Evaluation

```r
evaluate_model(final_fit, test_data, target, task)
eval_metrics(predictions, target, task)
eval_roc(predictions, target)
eval_confusion(predictions, target)
eval_calibration(predictions, target)
```

### Interpretability

```r
interpret_model(final_fit, train_data, target, task)
calculate_shap(final_fit, data, n_shap = 100)
plot_importance(importance_df)
plot_shap_summary(shap_values)
plot_shap_dependence(shap_values, variable)
```

### Advanced Analysis

```r
optimize_threshold(predictions, target)           # Youden, F1, or cost-based
calibrate_probabilities(predictions, method)       # Platt or Isotonic
detect_data_leakage(result)                        # Leakage indicators
run_nested_cv(data, target, task, model)           # Nested CV
advanced_residual_analysis(predictions, target)    # Residual diagnostics
```

## Sample Size Estimation

Estimate the minimum sample size needed for a target ML performance, analogous to G*Power but for machine learning.

```r
result <- ml_sample_size(
  data = my_data,           # Optional: use real data
  formula = outcome ~ .,    # Optional: specify formula
  task = "classification",
  metric = "auc",
  target = 0.80,
  model = "rf",             # "rf", "xgboost", "svm", "glm"
  reps = 100,
  n_outer_splits = 5,       # Multiple partitions for robustness
  bootstrap_ci = TRUE,      # 95% confidence interval for n*
  seed = 2026
)

# Results
result$recommend_n            # Recommended sample size
result$recommend_n_ci         # Bootstrap CI
result$outer_splits_results   # Cross-partition variability
result$curve_diagnostics      # Power-law fit quality

# Visualizations
plot_learning_curve(result)
plot_distribution(result)
```

### Decision Criteria

The recommended n\* is the smallest n satisfying:

```
n* = min{n_k : mean_k >= target AND p_k >= p_min}
```

Where p_k is the probability of success (default >= 0.80).

### Supported Metrics

| Task | Metrics |
|------|---------|
| Classification | `"auc"`, `"accuracy"`, `"f1"` |
| Regression | `"rmse"`, `"mae"`, `"r2"` |

## Train/Test Splitting Strategies

### Block Split

For sequential or temporal data, creates train/test splits using contiguous blocks:

```r
split <- block_split(
  data = my_data,
  target = "outcome",
  n_blocks = 5,
  test_prop = 0.20
)

# Use the split
train <- split$train
test <- split$test
```

### Diversity Split

Creates splits where test data is genuinely different from training data, enabling fairer generalization assessment:

```r
split <- diversity_split(
  data = my_data,
  target = "outcome",
  test_prop = 0.20,
  method = "cluster"   # "cluster", "dissimilarity", "boundary", "hybrid"
)
```

## Report Generation

Export results and generate scientific reports with AI:

```r
# Method 1: Export and launch interactive Shiny app
result <- easy_ml_capture(data, target = "outcome", task = "classification")
export_verbose_json(result, "analysis.json")
launch_report_generator()

# Method 2: Generate report directly from R
generate_report_with_ai(
  json_path = "analysis.json",
  api_key = "sk-proj-...",
  output_path = "Report.docx",
  language = "es",
  model = "gpt-4.1-mini"
)

# Method 3: One-step export (TXT + JSON)
easy_ml_export(data, target = "outcome", task = "classification")
```

## Citation

If you use this package, please cite:

```
Ventura-Leon, J. (2026). easyML: Easy Machine Learning Pipeline for
Classification and Regression in R. R package version 2.0.0.
https://github.com/jventural/easyML
```

## Author

**Jose Ventura-Leon**
- Email: jventuraleon@gmail.com
- ORCID: [0000-0003-2996-4244](https://orcid.org/0000-0003-2996-4244)

## License

MIT License

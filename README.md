# easyML: Easy Machine Learning in R

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/jventural/easyML)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**easyML** provides tools for estimating the minimum sample size needed to achieve a target performance in Machine Learning models. Unlike traditional power analysis tools (like G*Power), this package is designed specifically for ML applications where analytical solutions are not available.

The package implements:

- **Monte Carlo simulation** for generating learning curves
- **Power-law fitting** for performance extrapolation
- **Multiple decision criteria** (mean performance, probability of success, stability)
- **Robustness protocols** for small samples
- **Bootstrap confidence intervals** for uncertainty quantification

## Installation

```r
# Install from GitHub
devtools::install_github("jventural/easyML")
```
## Quick Start

```r
library(easyML)

# Estimate sample size for classification (AUC >= 0.80)
result <- ml_sample_size(
  task = "classification",
  metric = "auc",
  target = 0.80,
  reps = 100
)

# View recommendation
result$recommend_n

# Plot learning curve
plot_learning_curve(result)
```

## Main Functions

| Function | Description |
|----------|-------------|
| `ml_sample_size()` | Main function for sample size estimation via Monte Carlo |
| `estimate_n()` | Extrapolate sample size for a specific target |
| `estimate_n_plateau()` | Find sample size where performance plateaus |
| `plot_learning_curve()` | Visualize learning curves |

## Key Features

### 1. Multiple Test Partitions (for small samples)

When N < 1000, use multiple partitions to assess variability:

```r
result <- ml_sample_size(
  data = my_small_data,
  formula = outcome ~ .,
  n_outer_splits = 5,  # 5 independent partitions
  ...
)

# Results include median, range, and CV of n*
result$outer_splits_results
```

### 2. Bootstrap Confidence Intervals

```r
result <- ml_sample_size(
  ...,
  bootstrap_ci = TRUE,
  n_boot = 1000
)

# 95% CI for n*
result$recommend_n_ci
```

### 3. Fit Diagnostics

```r
# Check power-law fit quality before extrapolating
result$curve_diagnostics
# $pseudo_r2: 0.97
# $fit_quality: "good"
```

### 4. Guidance for Choosing R (repetitions)

The SE of the success proportion p_k follows:

```
SE(p_k) ≈ sqrt(p_k * (1 - p_k) / R)
```

| R | SE at p=0.80 | Use Case |
|---|--------------|----------|
| 50 | 0.057 | Exploratory |
| 100 | 0.040 | Standard |
| 200 | 0.028 | Publication |

## Example with Real Data

```r
library(mlbench)
data(PimaIndiansDiabetes)

result <- ml_sample_size(
  data = PimaIndiansDiabetes,
  formula = diabetes ~ .,
  task = "classification",
  metric = "auc",
  target = 0.75,
  positive = "pos",
  reps = 100,
  n_outer_splits = 5,
  bootstrap_ci = TRUE
)

print(result)
```

## Decision Criteria

The recommended sample size n* is the smallest n satisfying:

1. **Mean performance** ≥ target
2. **Probability of success** p_k ≥ 0.80
3. **Stability** (optional): SD ≤ threshold

Formally:

```
n* = min{n_k : mean_k ≥ target AND p_k ≥ p_min}
```

## Supported Models

- Random Forest (`model = "rf"`)
- XGBoost (`model = "xgboost"`)
- SVM (`model = "svm"`)
- GLM (`model = "glm"`)

## Supported Metrics

**Classification:** `"auc"`, `"accuracy"`, `"f1"`

**Regression:** `"rmse"`, `"mae"`, `"r2"`

## Citation

If you use this package, please cite:

```
Ventura-Leon, J. (2026). Sample Size Estimation for Machine Learning via
Monte Carlo Simulation: Learning Curves and Power Laws.
```

## Author

**Jose Ventura-Leon**
- Email: jventuraleon@gmail.com
- ORCID: [0000-0003-2996-4244](https://orcid.org/0000-0003-2996-4244)

## License

MIT License

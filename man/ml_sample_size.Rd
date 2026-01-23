\name{ml_sample_size}
\alias{ml_sample_size}
\title{Sample Size Estimation for Machine Learning via Monte Carlo Simulation}
\description{
Estimates the minimum sample size needed to achieve a target performance
in Machine Learning models using Monte Carlo simulation and learning curves.
Implements the methodology described in Ventura-Leon (2026) with support for
multiple test partitions, bootstrap confidence intervals, and power-law
curve fitting with diagnostics.
}
\usage{
ml_sample_size(
  data = NULL,
  formula = NULL,
  task = c("classification", "regression"),
  model = c("rf"),
  metric = NULL,
  n_grid = c(50, 100, 150, 200, 300, 400, 600, 800, 1000, 1500, 2000),
  reps = 50,
  target = NULL,
  target_dir = c("higher_better", "lower_better"),
  prob_min = 0.80,
  sd_max = NULL,
  n_test = 5000,

  n_outer_splits = 1,
  bootstrap_ci = FALSE,
  n_boot = 1000,
  sampling = c("stratified", "none", "up", "down"),
  positive = "1",
  threshold = 0.5,
  sim_params = list(),
  model_params = list(),
  seed = 123,
  verbose = TRUE
)
}
\arguments{
\item{data}{Data frame with real data. If NULL, synthetic data is simulated.}

\item{formula}{Model formula (e.g., \code{outcome ~ .}). Required when using real data.}

\item{task}{Task type: \code{"classification"} or \code{"regression"}.}

\item{model}{Model(s) to evaluate. Can be a single string or vector:
\code{"rf"} (Random Forest), \code{"xgboost"}, \code{"svm"}, \code{"glm"}.
When multiple models are specified, results are compared.}

\item{metric}{Evaluation metric. For classification: \code{"auc"}, \code{"accuracy"},
\code{"f1"}. For regression: \code{"rmse"}, \code{"mae"}, \code{"r2"}.
Defaults to \code{"auc"} for classification and \code{"rmse"} for regression.}

\item{n_grid}{Vector of training sample sizes to evaluate. Default covers
50 to 2000 with increasing gaps.}

\item{reps}{Number of Monte Carlo repetitions per sample size. Recommendations:
\itemize{
  \item R = 50: Exploratory analysis (SE \eqn{\approx} 0.057 at p = 0.80)
  \item R = 100: Standard analysis (SE \eqn{\approx} 0.040)
  \item R \eqn{\geq} 200: Publication quality (SE \eqn{\approx} 0.028)
}
Default is 50.}

\item{target}{Target metric value (e.g., AUC \eqn{\geq} 0.80).}

\item{target_dir}{Target direction: \code{"higher_better"} (default for AUC, accuracy)
or \code{"lower_better"} (for RMSE, MAE). Auto-detected based on metric.}

\item{prob_min}{Minimum proportion of repetitions that must meet the target
(the \eqn{p_k} criterion). Default is 0.80, meaning at least 80\% of
repetitions must achieve the target for a sample size to be recommended.}

\item{sd_max}{Optional maximum standard deviation for the stability criterion.
If specified, only sample sizes with SD \eqn{\leq} \code{sd_max} are considered.}

\item{n_test}{Size of the fixed test set. Default is 5000 or 20\% of data
if smaller. Minimum recommended: 500 for stable AUC estimation.
}

\item{n_outer_splits}{Number of independent test partitions for robustness
assessment. When N < 1000, using 5-10 partitions is recommended to quantify
variability in \eqn{n^*} estimates. Default is 1 (single partition).}

\item{bootstrap_ci}{Logical. If TRUE, compute bootstrap 95\% confidence
interval for \eqn{n^*}. Default is FALSE.}

\item{n_boot}{Number of bootstrap resamples when \code{bootstrap_ci = TRUE}.
Default is 1000.}

\item{sampling}{Sampling strategy for classification:
\itemize{
  \item \code{"stratified"}: Preserve class proportions (default)
  \item \code{"none"}: Simple random sampling
  \item \code{"up"}: Oversample minority class
  \item \code{"down"}: Undersample majority class
}}

\item{positive}{Label of the positive class for classification. Default is \code{"1"}.}

\item{threshold}{Classification threshold for computing accuracy/F1. Default is 0.5.}

\item{sim_params}{List of simulation parameters when \code{data = NULL}:
\itemize{
  \item \code{N_pop}: Population size to simulate (default: 30000)
  \item \code{p}: Number of predictors (default: 20)
  \item \code{prevalence}: Positive class prevalence (default: 0.20)
  \item \code{signal}: Signal strength controlling max achievable AUC:
    \itemize{
      \item signal = 1.5 \eqn{\rightarrow} AUC \eqn{\approx} 0.70-0.75
      \item signal = 2.5 \eqn{\rightarrow} AUC \eqn{\approx} 0.85-0.90 (default)
      \item signal = 3.5 \eqn{\rightarrow} AUC \eqn{\approx} 0.92-0.95
    }
  \item \code{noise_sd}: Noise SD for regression (default: 1.0)
}}

\item{model_params}{List of model hyperparameters:
\itemize{
  \item \code{num.trees}: Number of trees for RF (default: 500)
  \item \code{mtry}: Variables per split (default: sqrt(p))
  \item \code{min.node.size}: Minimum node size (default: 5)
  \item \code{max.depth}: Maximum tree depth (default: 0 = unlimited)
}}

\item{seed}{Random seed for reproducibility.}

\item{verbose}{Logical. Show progress messages. Default is TRUE.}
}
\value{
Object of class \code{ml_sample_size} containing:

\item{raw}{Data frame with results from each Monte Carlo repetition.
Columns: model, n, rep, metric_value, (partition if n_outer_splits > 1).}

\item{summary}{Aggregated summary by sample size with columns:
\itemize{
  \item \code{model}: Model name

  \item \code{n}: Sample size
  \item \code{mean}: Mean metric value
  \item \code{sd}: Standard deviation
  \item \code{se}: Standard error of the mean
  \item \code{p_meet}: Proportion meeting target (\eqn{p_k})
  \item \code{se_p_meet}: Standard error of \eqn{p_k}, computed as
    \eqn{\sqrt{p_k(1-p_k)/R}}
}}

\item{recommend_n}{Recommended sample size \eqn{n^*}. For multiple models,
a named list. \eqn{n^*} is the smallest n where all criteria are met.}

\item{recommend_n_ci}{Bootstrap 95\% CI for \eqn{n^*} (if \code{bootstrap_ci = TRUE}).
Contains: ci_lower, ci_upper, ci_median, n_valid.}

\item{curve_fit}{Fitted power-law model(s). The model has form:
\itemize{
  \item Higher-is-better: \eqn{m(n) = a - b \cdot n^{-c}}
  \item Lower-is-better: \eqn{m(n) = a + b \cdot n^{-c}}
}
where \eqn{a} is the asymptote, \eqn{b} is the improvement magnitude,
and \eqn{c} is the convergence rate.}

\item{curve_diagnostics}{Fit quality diagnostics:
\itemize{
  \item \code{pseudo_r2}: Proportion of variance explained
  \item \code{rmse_fit}: Root mean squared error of fit
  \item \code{fit_quality}: "good" (R\eqn{^2 \geq} 0.95), "acceptable"
    (R\eqn{^2 \geq} 0.80), or "poor"
}}

\item{outer_splits_results}{When \code{n_outer_splits > 1}, contains:
\itemize{
  \item \code{median}: Median \eqn{n^*} across partitions
  \item \code{mean}: Mean \eqn{n^*}
  \item \code{sd}: Standard deviation
  \item \code{cv}: Coefficient of variation
  \item \code{range}: [min, max]
  \item \code{all_values}: Vector of all \eqn{n^*} estimates
}}

\item{settings}{List of all settings used in the analysis.}
}
\details{
\strong{Decision Criteria:}

The recommended sample size \eqn{n^*} is the smallest \eqn{n_k} satisfying:
\enumerate{
  \item Mean performance \eqn{\geq} target (or \eqn{\leq} for error metrics)
  \item Probability of success \eqn{p_k \geq} \code{prob_min}
  \item (Optional) Stability: SD \eqn{\leq} \code{sd_max}
}

Formally: \eqn{n^* = \min\{n_k : \bar{m}_k \geq m_{target} \land p_k \geq p_{min}\}}

\strong{Standard Error of \eqn{p_k}:}

The SE is computed using the binomial approximation:
\deqn{SE(p_k) \approx \sqrt{\frac{p_k(1-p_k)}{R}}}

This guides the choice of R (repetitions):
\itemize{
  \item R = 50: SE \eqn{\approx} 0.057 (exploratory)
  \item R = 100: SE \eqn{\approx} 0.040 (standard)
  \item R \eqn{\geq} 200: SE \eqn{\approx} 0.028 (publication)
}

\strong{Multiple Partitions (Robustness Protocol):}

When \code{n_outer_splits > 1}, the procedure runs S independent analyses
with different test set partitions. This is recommended when N < 1000 to
assess variability in \eqn{n^*}. Results include the median, range, and CV
of \eqn{n^*} across partitions. A CV > 50\% indicates high uncertainty.

\strong{Bootstrap Confidence Intervals:}

When \code{bootstrap_ci = TRUE}, a bootstrap procedure generates B = \code{n_boot}
resamples of the Monte Carlo results, recomputes \eqn{n^*} for each, and
reports the 2.5th and 97.5th percentiles as the 95\% CI.

\strong{Power-Law Fitting:}
The learning curve is modeled as \eqn{m(n) = a - b \cdot n^{-c}} (or
\eqn{+ b \cdot n^{-c}} for error metrics). Before extrapolating, check:
\enumerate{
  \item Pseudo-R\eqn{^2 > 0.95}
  \item Extrapolation does not exceed 2x the largest observed n
  \item The estimated asymptote is plausible
}
}
\note{
\strong{Important:} The recommended \eqn{n^*} assumes fixed hyperparameters.
When hyperparameter tuning is planned, \eqn{n^*} may need to be increased
as tuning can introduce additional variability at small sample sizes.
}
\references{
Ventura-Leon, J. (2026). Sample Size Estimation for Machine Learning via
Monte Carlo Simulation: Learning Curves and Power Laws.

Figueroa, R. L., et al. (2012). Predicting sample size required for
classification performance. BMC Medical Informatics and Decision Making, 12(1), 8.
}
\examples{
\dontrun{
# ==============================================
# Example 1: Basic usage with simulated data
# ==============================================
result <- ml_sample_size(
  task = "classification",
  metric = "auc",
  target = 0.80,
  seed = 2026,
  n_grid = c(100, 200, 300, 400, 600, 800, 1000),
  reps = 100,
  sim_params = list(p = 20, prevalence = 0.20, signal = 2.5)
)

# View recommendation
result$recommend_n
# [1] 600

# View results
print(result)

# Check SE of p_k
result$summary[, c("n", "mean", "p_meet", "se_p_meet")]

# Check fit quality before extrapolating
result$curve_diagnostics$fit_quality
result$curve_diagnostics$pseudo_r2

# Plot learning curve
plot_learning_curve(result)

# ==============================================
# Example 2: Small dataset with robustness protocol
# ==============================================
# When N < 1000, use multiple partitions
data(PimaIndiansDiabetes, package = "mlbench")

result <- ml_sample_size(
  data = PimaIndiansDiabetes,
  formula = diabetes ~ .,
  task = "classification",
  metric = "auc",
  target = 0.75,
  positive = "pos",
  seed = 2026,
  reps = 100,
  n_outer_splits = 5,    # 5 independent partitions
  verbose = TRUE
)

# View aggregated results across partitions
result$outer_splits_results
# $median: 50
# $range: [50, 150]
# $cv: 0.559

# ==============================================
# Example 3: Compare multiple models
# ==============================================
result <- ml_sample_size(
  task = "classification",
  model = c("rf", "xgboost", "glm"),
  metric = "auc",
  target = 0.80,
  seed = 2026,
  reps = 50,
  sim_params = list(signal = 2.5)
)

# Results by model (values will vary)
result$recommend_n
# $rf: 600       # Complex models need more data
# $xgboost: 600  # Similar to RF
# $glm: 200      # Simpler models need less data

# Fit diagnostics by model
result$curve_diagnostics

# ==============================================
# Example 4: Extrapolation with caution
# ==============================================
# Estimate n for a higher target
estimate_n(result, target_metric = 0.85)

# Estimate plateau (within 1\% of asymptote)
estimate_n_plateau(result, delta = 0.01)

# Use relative delta (1\% of target metric)
estimate_n_plateau(result, delta = 0.01, relative = TRUE)

# ==============================================
# Example 5: Regression task
# ==============================================
# Sample size estimation for regression with RMSE target
result_reg <- ml_sample_size(
  task = "regression",
  metric = "rmse",
  target = 1.0,
  target_dir = "lower_better",
  seed = 2026,
  n_grid = c(100, 200, 300, 400, 600, 800, 1000),
  reps = 100,
  sim_params = list(
    p = 15,
    noise_sd = 1.0
  )
)

# View recommendation
result_reg$recommend_n

# The method works analogously for regression:
# - Metrics: "rmse", "mae", "r2"
# - target_dir: "lower_better" for RMSE/MAE, "higher_better" for R2
# - Power-law model: m(n) = a + b * n^(-c) for error metrics

# Plot learning curve
plot_learning_curve(result_reg)
}
}
\seealso{
\code{\link{estimate_n}} for extrapolating to specific targets,
\code{\link{estimate_n_plateau}} for finding the plateau,
\code{\link{plot_learning_curve}} for visualization,
\code{\link{print.ml_sample_size}} for formatted output.
}
\keyword{machine learning}
\keyword{sample size}
\keyword{Monte Carlo}
\keyword{learning curves}

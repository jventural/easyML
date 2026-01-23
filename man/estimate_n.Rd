\name{estimate_n}
\alias{estimate_n}
\title{Estimate Sample Size for a Target Metric Using Power-Law Extrapolation}
\description{
Uses the fitted power-law model from \code{\link{ml_sample_size}} to estimate
the sample size required to achieve a specified target metric value.
Includes warnings for unreliable extrapolations.
}
\usage{
estimate_n(object, target_metric, model = NULL, warn_extrapolation = TRUE)
}
\arguments{
\item{object}{Object of class \code{ml_sample_size} containing the fitted
power-law curve.}

\item{target_metric}{Target metric value to achieve (e.g., AUC = 0.85).}

\item{model}{For multi-model results, which model's curve to use.
Default is the first model. Ignored for single-model results.}

\item{warn_extrapolation}{Logical. If TRUE (default), warnings are issued when:
\itemize{
  \item The target exceeds the estimated asymptote
  \item The required n exceeds 2x the largest observed sample size
  \item The curve fit quality is poor (pseudo-R\eqn{^2} < 0.80)
}}
}
\value{
Integer. The estimated sample size required to achieve the target metric.
Returns \code{Inf} if the target exceeds the estimated asymptote.
}
\details{
The function inverts the power-law equation to solve for n:

For higher-is-better metrics (AUC, accuracy, R\eqn{^2}):
\deqn{n_{required} = \left(\frac{b}{a - m_{target}}\right)^{1/c}}

For lower-is-better metrics (RMSE, MAE):
\deqn{n_{required} = \left(\frac{b}{m_{target} - a}\right)^{1/c}}

where \eqn{a} is the asymptote, \eqn{b} is the improvement magnitude,
and \eqn{c} is the convergence rate from the fitted power-law model.

\strong{Extrapolation Warnings:}

The function issues warnings in the following situations:
\enumerate{
  \item \strong{Target exceeds asymptote:} The target metric is unachievable
    regardless of sample size. Returns \code{Inf}.
  \item \strong{Large extrapolation:} The estimated n exceeds 2x the largest
    observed sample size. The power-law may not hold at such extreme values.
  \item \strong{Poor fit quality:} The pseudo-R\eqn{^2} is below 0.80,
    indicating the power-law model does not fit the data well.
}

When warnings are issued, consider:
\itemize{
  \item Expanding \code{n_grid} to include larger sample sizes
  \item Increasing \code{reps} for more stable curve estimation
  \item Using only interpolation within the observed range
}
}
\examples{
\dontrun{
# Fit learning curve
result <- ml_sample_size(
  task = "classification",
  metric = "auc",
  target = 0.80,
  n_grid = c(100, 200, 400, 600, 800, 1000),
  reps = 100
)

# Estimate n for a higher target
estimate_n(result, target_metric = 0.85)
# [1] 1250

# Check fit quality first
result$curve_diagnostics$fit_quality
# [1] "good"

# For multi-model results
result_multi <- ml_sample_size(
  model = c("rf", "glm"),
  ...
)

# Specify which model
estimate_n(result_multi, target_metric = 0.85, model = "rf")
estimate_n(result_multi, target_metric = 0.85, model = "glm")

# Suppress warnings for known extrapolation
estimate_n(result, target_metric = 0.90, warn_extrapolation = FALSE)
}
}
\seealso{
\code{\link{ml_sample_size}} for fitting the learning curve,
\code{\link{estimate_n_plateau}} for finding the plateau sample size.
}
\keyword{extrapolation}
\keyword{sample size}

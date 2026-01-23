\name{estimate_n_plateau}
\alias{estimate_n_plateau}
\title{Estimate Sample Size to Reach the Performance Plateau}
\description{
Computes the sample size at which predictive performance approaches
the asymptote within a specified distance (delta). This identifies
the point of diminishing returns where additional data yields only
marginal improvements.
}
\usage{
estimate_n_plateau(object, delta = 0.01, relative = FALSE,
                   model = NULL, warn_extrapolation = TRUE)
}
\arguments{
\item{object}{Object of class \code{ml_sample_size} containing the fitted
power-law curve.}

\item{delta}{Distance to the asymptote to consider as the plateau.
Default is 0.01 (absolute). See \code{relative} parameter.}

\item{relative}{Logical. If TRUE, \code{delta} is interpreted as a proportion
of the target metric rather than an absolute value. Default is FALSE.

For example, with \code{target = 0.80} and \code{delta = 0.01}:
\itemize{
  \item \code{relative = FALSE}: plateau at asymptote - 0.01
  \item \code{relative = TRUE}: plateau at asymptote - 0.008 (1\% of 0.80)
}

Relative delta is useful when comparing across metrics with different scales.}

\item{model}{For multi-model results, which model's curve to use.
Default is the first model.}

\item{warn_extrapolation}{Logical. If TRUE (default), warnings are issued when:
\itemize{
  \item The estimated n exceeds 2x the largest observed sample size
  \item The curve fit quality is poor (pseudo-R\eqn{^2} < 0.80)
}}
}
\value{
Integer. The estimated sample size at which performance reaches the plateau
(within \code{delta} of the asymptote).
}
\details{
The plateau is defined operationally as the sample size where performance
is within \code{delta} of the asymptotic maximum (or minimum for error metrics).

The formula used is:
\deqn{n_{plateau} = \left(\frac{b}{\delta}\right)^{1/c}}

where \eqn{b} is the improvement magnitude and \eqn{c} is the convergence
rate from the fitted power-law model.

\strong{Choosing Delta:}

\itemize{
  \item \strong{Absolute delta} (default): Use \code{delta = 0.01} for AUC
    to define plateau as within 0.01 of the asymptote.
  \item \strong{Relative delta}: Use \code{relative = TRUE} with \code{delta = 0.01}
    to define plateau as within 1\% of the target metric value. This is more
    stringent for lower targets.
}

Example: With target AUC = 0.80 and asymptote = 0.88:
\itemize{
  \item Absolute \code{delta = 0.01}: plateau at AUC = 0.87

  \item Relative \code{delta = 0.01}: plateau at AUC = 0.872 (within 0.008 = 1\% of 0.80)
}

\strong{Interpretation:}

The plateau sample size represents the point of diminishing returns.
Beyond this point, collecting additional data provides minimal improvement
in predictive performance. This information helps researchers make
cost-benefit decisions about data collection.
}
\note{
The plateau estimate can be very large when the convergence rate \eqn{c}
is small (slow convergence) or when \code{delta} is small (stringent definition).
Always verify the estimate is practically meaningful for your context.
}
\examples{
\dontrun{
# Fit learning curve
result <- ml_sample_size(
  task = "classification",
  metric = "auc",
  target = 0.80,
  n_grid = c(100, 200, 400, 600, 800, 1000, 2000),
  reps = 100
)

# Estimate plateau with absolute delta
estimate_n_plateau(result, delta = 0.01)
# [1] 8500

# Estimate plateau with relative delta (1\% of target)
estimate_n_plateau(result, delta = 0.01, relative = TRUE)
# Using relative delta: 0.01 x 0.8 = 0.008
# [1] 12000

# More lenient plateau definition
estimate_n_plateau(result, delta = 0.02)
# [1] 3200

# For multi-model results
result_multi <- ml_sample_size(
  model = c("rf", "glm"),
  ...
)

# Compare plateau across models
estimate_n_plateau(result_multi, delta = 0.01, model = "rf")
estimate_n_plateau(result_multi, delta = 0.01, model = "glm")
}
}
\seealso{
\code{\link{ml_sample_size}} for fitting the learning curve,
\code{\link{estimate_n}} for estimating n at a specific target.
}
\keyword{plateau}
\keyword{sample size}
\keyword{diminishing returns}

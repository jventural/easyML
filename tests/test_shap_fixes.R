# =============================================================================
# Test script for the v2.4.1 fixes (2026-09-22)
#   1. supervised_ml() with a single model no longer fails in compare_models()
#   2. predictors removed by preprocessing are excluded from SHAP
#      (reported in $removed_vars instead of showing SHAP = 0)
#   3. collinearity among the predictors kept is reported ($collinear_vars)
#   4. the SHAP sample is stratified by class in classification
#   5. the optimal threshold is chosen on out-of-fold CV predictions and
#      evaluated on the test set (it was chosen AND evaluated on the test set)
# Found while running the RENACYT data (prevalence 4 %, VIF in the hundreds).
# =============================================================================

library(easyML)

set.seed(42)
n <- 1500
x1 <- rnorm(n)
datos <- data.frame(
  x1 = x1,
  x2 = x1 + rnorm(n, sd = 0.05),     # casi redundante con x1 -> VIF alto
  x3 = x1 + rnorm(n, sd = 0.05),
  x4 = rnorm(n),
  x5 = rnorm(n)
)
lp <- -3.5 + 1.2 * x1 + 0.8 * datos$x4
datos$y <- factor(ifelse(runif(n) < plogis(lp), "Yes", "No"), levels = c("No", "Yes"))
cat("Prevalencia de Yes:", round(mean(datos$y == "Yes"), 3), "\n")

ok <- function(cond, msg) cat(if (isTRUE(cond)) "  [OK] " else "  [FALLA] ", msg, "\n")

cat("\n=== TEST 1-2-4: un solo modelo, preprocesamiento por defecto ===\n")
r1 <- supervised_ml(datos, "y", task = "classification", models = "glm",
                    tune_best = FALSE, cv_folds = 5, seed = 1, verbose = FALSE)
s1 <- r1$interpretation$shap
ok(r1$best_model == "glm", "un solo modelo termina sin error")
ok(length(s1$removed_vars) > 0, paste("detecta variables eliminadas:",
                                      paste(s1$removed_vars, collapse = ", ")))
ok(!any(s1$removed_vars %in% s1$importance$Variable),
   "las variables eliminadas no aparecen en la importancia SHAP")
ok(identical(colnames(s1$shap_values), colnames(s1$test_sample)),
   "shap_values y test_sample tienen las mismas columnas")
ok(grepl("estratificado", s1$sample_design), "muestra SHAP estratificada")

cat("\n=== TEST 3: conservando variables colineales ===\n")
r2 <- supervised_ml(datos, "y", task = "classification", models = c("glm", "tree"),
                    tune_best = FALSE, cv_folds = 5, seed = 1, verbose = FALSE,
                    remove_high_cor = FALSE, remove_high_vif = FALSE)
s2 <- r2$interpretation$shap
ok(length(s2$removed_vars) == 0, "sin eliminacion no hay variables excluidas")
ok(all(c("x1", "x2", "x3") %in% s2$collinear_vars$Variable),
   "reporta x1, x2 y x3 como colineales")
ok(!("x5" %in% s2$collinear_vars$Variable), "no marca x5, que es independiente")

cat("\n=== TEST 5: umbral elegido fuera del conjunto de prueba ===\n")
t2 <- r2$threshold_optimization
ok(identical(t2$source, "cv_oof"), "el umbral se elige con predicciones fuera de pliegue")
ok(!is.null(t2$test_metrics) && t2$test_metrics$threshold == t2$optimal_threshold,
   "las metricas de prueba usan ese mismo umbral")

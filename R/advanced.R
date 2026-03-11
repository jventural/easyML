# =============================================================================
# Funciones Avanzadas para easyML
# =============================================================================
# Incluye: Optimización de Threshold, Calibración de Probabilidades,
#          Nested CV, Análisis de Residuos Avanzado, Detección de Data Leakage
# =============================================================================


# =============================================================================
# 1. OPTIMIZACION DE THRESHOLD (Clasificación)
# =============================================================================

#' @title Optimizar Threshold de Clasificación
#'
#' @description
#' Encuentra el threshold óptimo para clasificación binaria basado en
#' diferentes criterios: Youden's J, F1, costo personalizado, etc.
#'
#' @param predictions Data frame con predicciones (debe incluir probabilidades)
#' @param target Nombre de la variable objetivo
#' @param method Método de optimización: "youden", "f1", "cost", "balanced"
#' @param cost_fp Costo de falso positivo (para method = "cost")
#' @param cost_fn Costo de falso negativo (para method = "cost")
#' @param verbose Mostrar resultados
#'
#' @return Lista con threshold óptimo y métricas
#' @export
optimize_threshold <- function(predictions,
                               target,
                               method = "youden",
                               cost_fp = 1,
                               cost_fn = 1,
                               verbose = TRUE) {

  # Detectar clase positiva y event_level
  event_info <- .detect_event_level(predictions[[target]])

  if (!event_info$prob_col %in% names(predictions)) {
    stop("No se encontraron probabilidades en las predicciones")
  }

  probs <- predictions[[event_info$prob_col]]
  truth <- as.numeric(predictions[[target]] == event_info$positive_class)

  method_descriptions <- c(
    youden = "Maximiza Youden's J (sensitivity + specificity - 1)",
    f1 = "Maximiza F1-Score (balance precision-recall)",
    balanced = "Maximiza Balanced Accuracy",
    cost = "Minimiza costo total de errores"
  )

  # Evaluar diferentes thresholds
  thresholds <- seq(0.01, 0.99, by = 0.01)

  results <- lapply(thresholds, function(thresh) {
    pred_class <- ifelse(probs >= thresh, 1, 0)

    tp <- sum(pred_class == 1 & truth == 1)
    tn <- sum(pred_class == 0 & truth == 0)
    fp <- sum(pred_class == 1 & truth == 0)
    fn <- sum(pred_class == 0 & truth == 1)

    sensitivity <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    specificity <- ifelse((tn + fp) > 0, tn / (tn + fp), 0)
    precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall <- sensitivity
    f1 <- ifelse((precision + recall) > 0,
                 2 * precision * recall / (precision + recall), 0)

    # Youden's J statistic
    youden_j <- sensitivity + specificity - 1

    # Costo total
    total_cost <- cost_fp * fp + cost_fn * fn

    # Balanced accuracy
    balanced_acc <- (sensitivity + specificity) / 2

    data.frame(
      threshold = thresh,
      sensitivity = sensitivity,
      specificity = specificity,
      precision = precision,
      f1 = f1,
      youden_j = youden_j,
      balanced_acc = balanced_acc,
      total_cost = total_cost,
      tp = tp, tn = tn, fp = fp, fn = fn
    )
  })

  results_df <- do.call(rbind, results)

  # Seleccionar threshold óptimo según método
  optimal_idx <- switch(method,
    "youden" = which.max(results_df$youden_j),
    "f1" = which.max(results_df$f1),
    "cost" = which.min(results_df$total_cost),
    "balanced" = which.max(results_df$balanced_acc),
    which.max(results_df$youden_j)
  )

  optimal_threshold <- results_df$threshold[optimal_idx]
  optimal_metrics <- results_df[optimal_idx, ]

  if (verbose) {
    cat("    Metodo:", method, "-", method_descriptions[method], "\n\n")

    cat("    Resultado:\n")
    cat("      - Threshold por defecto: 0.50\n")
    cat("      - Threshold optimo encontrado:", round(optimal_threshold, 2), "\n\n")

    cat("    Metricas con threshold optimo (", round(optimal_threshold, 2), "):\n", sep = "")
    cat("      - Sensibilidad:", round(optimal_metrics$sensitivity * 100, 1), "% de positivos detectados\n")
    cat("      - Especificidad:", round(optimal_metrics$specificity * 100, 1), "% de negativos correctos\n")
    cat("      - Precision:", round(optimal_metrics$precision * 100, 1), "% de predicciones positivas son correctas\n")
    cat("      - F1-Score:", round(optimal_metrics$f1, 4), "\n")

    # Comparar con threshold 0.5
    default_idx <- which(results_df$threshold == 0.5)
    if (length(default_idx) > 0) {
      default_metrics <- results_df[default_idx, ]
      cat("\n    Comparacion:\n")
      cat("      - F1 con threshold 0.5:", round(default_metrics$f1, 4), "\n")
      cat("      - F1 con threshold optimo:", round(optimal_metrics$f1, 4), "\n")
      improvement <- (optimal_metrics$f1 - default_metrics$f1) / default_metrics$f1 * 100
      if (abs(improvement) > 0.1) {
        if (improvement > 0) {
          cat("      - Mejora:", round(improvement, 1), "%\n")
        } else {
          cat("      - Nota: El threshold por defecto ya era optimo para F1\n")
        }
      } else {
        cat("      - Diferencia minima entre ambos thresholds\n")
      }
    }

    cat("\n    Uso: Puedes aplicar este threshold con apply_threshold(predictions, target,",
        round(optimal_threshold, 2), ")\n")
  }

  list(
    optimal_threshold = optimal_threshold,
    optimal_metrics = optimal_metrics,
    all_results = results_df,
    method = method
  )
}


#' @title Aplicar Threshold Optimizado
#' @export
apply_threshold <- function(predictions, target, threshold) {

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])

  probs <- predictions[[event_info$prob_col]]
  new_class <- ifelse(probs >= threshold,
                      event_info$positive_class,
                      event_info$negative_class)
  predictions$.pred_class_optimized <- factor(new_class, levels = event_info$levels)

  predictions
}


#' @title Grafico de Optimización de Threshold
#' @export
plot_threshold_optimization <- function(threshold_result) {

  results <- threshold_result$all_results
  optimal <- threshold_result$optimal_threshold

  # Preparar datos para gráfico
  plot_data <- tidyr::pivot_longer(
    results[, c("threshold", "sensitivity", "specificity", "f1", "precision")],
    cols = -threshold,
    names_to = "metric",
    values_to = "value"
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = threshold, y = value, color = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_vline(xintercept = optimal, linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray50") +
    ggplot2::annotate("text", x = optimal + 0.05, y = 0.95,
                      label = paste("Optimo:", round(optimal, 2)),
                      color = "red", hjust = 0) +
    ggplot2::labs(
      title = "Optimizacion de Threshold",
      subtitle = paste("Metodo:", threshold_result$method),
      x = "Threshold",
      y = "Valor",
      color = "Metrica"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}


# =============================================================================
# 2. CALIBRACION DE PROBABILIDADES
# =============================================================================

#' @title Calibrar Probabilidades
#'
#' @description
#' Calibra las probabilidades predichas usando Platt Scaling (logístico)
#' o Isotonic Regression.
#'
#' @param predictions Data frame con predicciones
#' @param target Nombre de la variable objetivo
#' @param method Método: "platt" (sigmoid) o "isotonic"
#' @param verbose Mostrar resultados
#'
#' @return Lista con modelo de calibración y probabilidades calibradas
#' @export
calibrate_probabilities <- function(predictions,
                                    target,
                                    method = "platt",
                                    verbose = TRUE) {

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])

  probs <- predictions[[event_info$prob_col]]
  truth <- as.numeric(predictions[[target]] == event_info$positive_class)

  if (verbose) {
    cat("    Configuracion de clases:\n")
    cat("      - Clase positiva:", event_info$positive_class, "\n")
    cat("      - event_level:", event_info$event_level, "\n\n")
  }

  if (method == "platt") {
    # Platt Scaling (Regresión logística)
    cal_data <- data.frame(prob = probs, truth = truth)
    cal_model <- glm(truth ~ prob, data = cal_data, family = binomial())

    calibrated_probs <- predict(cal_model, type = "response")

  } else if (method == "isotonic") {
    # Isotonic Regression
    cal_model <- isoreg(probs, truth)

    # Función para predecir con isotonic
    calibrated_probs <- approx(
      x = cal_model$x[cal_model$ord],
      y = cal_model$yf,
      xout = probs,
      rule = 2
    )$y
  }

  # Calcular métricas de calibración
  # Brier Score
  brier_original <- mean((probs - truth)^2)
  brier_calibrated <- mean((calibrated_probs - truth)^2)

  # Expected Calibration Error (ECE)
  ece_original <- calculate_ece(probs, truth)
  ece_calibrated <- calculate_ece(calibrated_probs, truth)

  if (verbose) {
    cat("    Metodo de calibracion:", method, "\n\n")
    cat("    Brier Score:\n")
    cat("      - Original:", round(brier_original, 4), "\n")
    cat("      - Calibrado:", round(brier_calibrated, 4), "\n")

    improvement_brier <- (brier_original - brier_calibrated) / brier_original * 100
    if (improvement_brier > 0) {
      cat("      - Mejora:", round(improvement_brier, 1), "%\n")
    }

    cat("\n    Expected Calibration Error (ECE):\n")
    cat("      - Original:", round(ece_original, 4), "\n")
    cat("      - Calibrado:", round(ece_calibrated, 4), "\n")
  }

  # Agregar probabilidades calibradas al dataframe
  predictions$.pred_calibrated <- calibrated_probs

  list(
    calibration_model = cal_model,
    predictions = predictions,
    target = target,
    method = method,
    brier_original = brier_original,
    brier_calibrated = brier_calibrated,
    ece_original = ece_original,
    ece_calibrated = ece_calibrated
  )
}


#' @title Calcular Expected Calibration Error
#' @noRd
calculate_ece <- function(probs, truth, n_bins = 10) {
  bins <- cut(probs, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)

  ece <- 0
  n_total <- length(probs)

  for (bin in levels(bins)) {
    idx <- which(bins == bin)
    if (length(idx) > 0) {
      avg_prob <- mean(probs[idx])
      avg_truth <- mean(truth[idx])
      weight <- length(idx) / n_total
      ece <- ece + weight * abs(avg_prob - avg_truth)
    }
  }

  ece
}


#' @title Grafico de Calibración
#' @export
plot_calibration_curve <- function(calibration_result, n_bins = 10) {

  predictions <- calibration_result$predictions

  # Usar target guardado por calibrate_probabilities
  target_col <- calibration_result$target
  if (is.null(target_col)) {
    # Fallback: buscar factor que no sea .pred_class
    factor_cols <- names(predictions)[sapply(predictions, is.factor)]
    factor_cols <- factor_cols[!factor_cols %in% c(".pred_class", ".pred_class_optimized")]
    target_col <- factor_cols[1]
  }

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target_col]])

  probs_orig <- predictions[[event_info$prob_col]]
  probs_cal <- predictions$.pred_calibrated
  truth <- as.numeric(predictions[[target_col]] == event_info$positive_class)

  # Crear bins
  bins <- cut(probs_orig, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)

  # Calcular promedios por bin
  cal_data <- data.frame(
    bin = bins,
    prob_orig = probs_orig,
    prob_cal = probs_cal,
    truth = truth
  )

  cal_summary <- cal_data |>
    dplyr::group_by(bin) |>
    dplyr::summarise(
      mean_pred_orig = mean(prob_orig),
      mean_pred_cal = mean(prob_cal),
      mean_obs = mean(truth),
      n = dplyr::n(),
      .groups = "drop"
    )

  ggplot2::ggplot(cal_summary) +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(ggplot2::aes(x = mean_pred_orig, y = mean_obs),
                        color = "red", size = 3, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(x = mean_pred_orig, y = mean_obs),
                       color = "red", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = mean_pred_cal, y = mean_obs),
                        color = "blue", size = 3, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(x = mean_pred_cal, y = mean_obs),
                       color = "blue", alpha = 0.5) +
    ggplot2::labs(
      title = "Curva de Calibracion",
      subtitle = "Rojo: Original | Azul: Calibrado",
      x = "Probabilidad Predicha",
      y = "Proporcion Observada"
    ) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


# =============================================================================
# 3. NESTED CROSS-VALIDATION
# =============================================================================

#' @title Nested Cross-Validation
#'
#' @description
#' Implementa nested CV para obtener estimaciones menos sesgadas del
#' rendimiento del modelo con tuning de hiperparámetros.
#'
#' @param data Datos de entrenamiento
#' @param target Variable objetivo
#' @param task Tipo de tarea
#' @param recipe Receta de preprocesamiento
#' @param model_name Nombre del modelo ("rf", "xgboost")
#' @param outer_folds Número de folds externos
#' @param inner_folds Número de folds internos
#' @param grid_size Tamaño del grid para tuning
#' @param seed Semilla
#' @param verbose Mostrar progreso
#'
#' @return Lista con resultados de nested CV
#' @export
run_nested_cv <- function(data,
                      target,
                      task,
                      recipe,
                      model_name = "rf",
                      outer_folds = 5,
                      inner_folds = 5,
                      grid_size = 10,
                      seed = 2024,
                      verbose = TRUE) {

  set.seed(seed)

  if (!model_name %in% c("rf", "xgboost")) {
    stop("Nested CV solo disponible para 'rf' y 'xgboost'")
  }

  if (verbose) {
    cat("    Configuracion:\n")
    cat("      - Folds externos:", outer_folds, "\n")
    cat("      - Folds internos:", inner_folds, "\n")
    cat("      - Grid size:", grid_size, "\n")
    cat("      - Modelo:", model_name, "\n\n")
  }

  # Crear folds externos
  if (task == "classification") {
    outer_cv <- rsample::vfold_cv(data, v = outer_folds,
                                   strata = !!rlang::sym(target))
  } else {
    outer_cv <- rsample::vfold_cv(data, v = outer_folds)
  }

  # Definir métricas
  if (task == "classification") {
    metrics_set <- yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
    select_metric <- "roc_auc"
  } else {
    metrics_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
    select_metric <- "rmse"
  }

  # Resultados por fold externo
  outer_results <- list()

  for (i in 1:outer_folds) {
    if (verbose) {
      cat("    Fold externo", i, "/", outer_folds, "...")
    }

    # Datos de este fold
    train_outer <- rsample::analysis(outer_cv$splits[[i]])
    test_outer <- rsample::assessment(outer_cv$splits[[i]])

    # Crear folds internos
    if (task == "classification") {
      inner_cv <- rsample::vfold_cv(train_outer, v = inner_folds,
                                     strata = !!rlang::sym(target))
    } else {
      inner_cv <- rsample::vfold_cv(train_outer, v = inner_folds)
    }

    # Crear especificación con tune
    if (model_name == "rf") {
      tune_spec <- parsnip::rand_forest(
        mtry = tune::tune(),
        min_n = tune::tune(),
        trees = 500
      ) |>
        parsnip::set_engine("ranger") |>
        parsnip::set_mode(task)
    } else {
      tune_spec <- parsnip::boost_tree(
        mtry = tune::tune(),
        min_n = tune::tune(),
        tree_depth = tune::tune(),
        learn_rate = tune::tune(),
        trees = 500
      ) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode(task)
    }

    # Workflow
    wf <- workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(tune_spec)

    # Tuning en folds internos
    tune_results <- suppressMessages(
      tune::tune_grid(
        wf,
        resamples = inner_cv,
        grid = grid_size,
        metrics = metrics_set,
        control = tune::control_grid(verbose = FALSE)
      )
    )

    # Mejor configuración
    best_params <- tune::select_best(tune_results, metric = select_metric)
    final_wf <- tune::finalize_workflow(wf, best_params)

    # Entrenar en train_outer y evaluar en test_outer
    final_fit <- parsnip::fit(final_wf, data = train_outer)

    # Predicciones en test externo
    if (task == "classification") {
      preds <- stats::predict(final_fit, new_data = test_outer)
      probs <- stats::predict(final_fit, new_data = test_outer, type = "prob")
      test_preds <- dplyr::bind_cols(test_outer, preds, probs)

      # Detectar clase positiva y event_level
      event_info <- .detect_event_level(test_outer[[target]])

      # Calcular métricas con event_level correcto
      if (event_info$type == "binary") {
        auc <- yardstick::roc_auc(test_preds,
                                   truth = !!rlang::sym(target),
                                   !!rlang::sym(event_info$prob_col),
                                   event_level = event_info$event_level)$.estimate
      } else {
        prob_cols_syms <- rlang::syms(event_info$prob_cols)
        auc <- yardstick::roc_auc(test_preds,
                                   truth = !!rlang::sym(target),
                                   !!!prob_cols_syms)$.estimate
      }
      acc <- yardstick::accuracy(test_preds,
                                  truth = !!rlang::sym(target),
                                  estimate = .pred_class)$.estimate

      outer_results[[i]] <- data.frame(
        fold = i,
        roc_auc = auc,
        accuracy = acc
      )

      if (verbose) cat(" AUC =", round(auc, 3), "\n")

    } else {
      preds <- stats::predict(final_fit, new_data = test_outer)
      test_preds <- dplyr::bind_cols(test_outer, preds)

      rmse_val <- yardstick::rmse(test_preds,
                                   truth = !!rlang::sym(target),
                                   estimate = .pred)$.estimate
      rsq_val <- yardstick::rsq(test_preds,
                                 truth = !!rlang::sym(target),
                                 estimate = .pred)$.estimate

      outer_results[[i]] <- data.frame(
        fold = i,
        rmse = rmse_val,
        rsq = rsq_val
      )

      if (verbose) cat(" RMSE =", round(rmse_val, 3), "\n")
    }
  }

  results_df <- do.call(rbind, outer_results)

  # Resumen
  if (verbose) {
    cat("\n    Resultados de Nested CV:\n")
    if (task == "classification") {
      cat("      - ROC-AUC: ", round(mean(results_df$roc_auc), 4),
          " (SD:", round(sd(results_df$roc_auc), 4), ")\n")
      cat("      - Accuracy:", round(mean(results_df$accuracy), 4),
          " (SD:", round(sd(results_df$accuracy), 4), ")\n")
    } else {
      cat("      - RMSE:", round(mean(results_df$rmse), 4),
          " (SD:", round(sd(results_df$rmse), 4), ")\n")
      cat("      - R-squared:", round(mean(results_df$rsq), 4),
          " (SD:", round(sd(results_df$rsq), 4), ")\n")
    }
  }

  list(
    results = results_df,
    summary = if (task == "classification") {
      list(
        roc_auc_mean = mean(results_df$roc_auc),
        roc_auc_sd = sd(results_df$roc_auc),
        accuracy_mean = mean(results_df$accuracy),
        accuracy_sd = sd(results_df$accuracy)
      )
    } else {
      list(
        rmse_mean = mean(results_df$rmse),
        rmse_sd = sd(results_df$rmse),
        rsq_mean = mean(results_df$rsq),
        rsq_sd = sd(results_df$rsq)
      )
    },
    task = task,
    model = model_name,
    config = list(outer_folds = outer_folds, inner_folds = inner_folds)
  )
}


# =============================================================================
# 4. ANALISIS DE RESIDUOS AVANZADO (Regresión)
# =============================================================================

#' @title Análisis de Residuos Avanzado
#'
#' @description
#' Realiza un análisis completo de residuos incluyendo:
#' - Test de homocedasticidad (Breusch-Pagan)
#' - Test de normalidad (Shapiro-Wilk)
#' - Detección de puntos influyentes (Cook's Distance simulado)
#' - Gráficos diagnósticos
#'
#' @param predictions Data frame con predicciones
#' @param target Variable objetivo
#' @param verbose Mostrar resultados
#'
#' @return Lista con resultados del análisis
#' @export
advanced_residual_analysis <- function(predictions, target, verbose = TRUE) {

  obs <- predictions[[target]]
  pred <- predictions$.pred
  residuals <- obs - pred
  n <- length(residuals)

  results <- list()

  # 1. Estadísticas básicas de residuos
  results$basic_stats <- list(
    mean = mean(residuals),
    sd = sd(residuals),
    median = median(residuals),
    min = min(residuals),
    max = max(residuals),
    skewness = (mean(residuals) - median(residuals)) / sd(residuals)
  )

  # 2. Test de normalidad (Shapiro-Wilk)
  if (n <= 5000) {
    shapiro_result <- shapiro.test(residuals)
  } else {
    shapiro_result <- shapiro.test(sample(residuals, 5000))
  }
  results$normality_test <- shapiro_result

  # 3. Test de homocedasticidad (simplificado - basado en correlación)
  # Correlación entre residuos absolutos y predicciones
  cor_test <- cor.test(abs(residuals), pred)
  results$homoscedasticity <- list(
    correlation = cor_test$estimate,
    p_value = cor_test$p.value,
    heteroscedastic = cor_test$p.value < 0.05
  )

  # 4. Detección de outliers en residuos (usando IQR)
  q1 <- quantile(residuals, 0.25)
  q3 <- quantile(residuals, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 3 * iqr
  upper <- q3 + 3 * iqr
  outlier_idx <- which(residuals < lower | residuals > upper)

  results$outliers <- list(
    n_outliers = length(outlier_idx),
    pct_outliers = length(outlier_idx) / n * 100,
    indices = outlier_idx
  )

  # 5. Puntos influyentes (basado en residuos estandarizados + leverage aproximado)
  std_residuals <- (residuals - mean(residuals)) / sd(residuals)
  influential_idx <- which(abs(std_residuals) > 3)

  results$influential_points <- list(
    n_influential = length(influential_idx),
    indices = influential_idx
  )

  # 6. Autocorrelación de residuos (Durbin-Watson aproximado)
  dw_statistic <- sum(diff(residuals)^2) / sum(residuals^2)
  results$autocorrelation <- list(
    durbin_watson = dw_statistic,
    interpretation = if (dw_statistic < 1.5) "Autocorrelación positiva" else
                     if (dw_statistic > 2.5) "Autocorrelación negativa" else
                     "Sin autocorrelación significativa"
  )

  if (verbose) {
    cat("    Estadisticas de Residuos:\n")
    cat("      - Media:", round(results$basic_stats$mean, 4), "\n")
    cat("      - DE:", round(results$basic_stats$sd, 4), "\n")
    cat("      - Rango: [", round(results$basic_stats$min, 2), ",",
        round(results$basic_stats$max, 2), "]\n\n")

    cat("    Test de Normalidad (Shapiro-Wilk):\n")
    cat("      - W =", round(shapiro_result$statistic, 4), "\n")
    cat("      - p-value =", format.pval(shapiro_result$p.value), "\n")
    if (shapiro_result$p.value < 0.05) {
      cat("      - [!] Residuos NO normales\n\n")
    } else {
      cat("      - [OK] Residuos aproximadamente normales\n\n")
    }

    cat("    Test de Homocedasticidad:\n")
    cat("      - Correlacion |residuos| vs pred:", round(cor_test$estimate, 4), "\n")
    if (results$homoscedasticity$heteroscedastic) {
      cat("      - [!] Posible heterocedasticidad (p <", format.pval(cor_test$p.value), ")\n\n")
    } else {
      cat("      - [OK] Homocedasticidad asumida\n\n")
    }

    cat("    Outliers en Residuos:\n")
    cat("      -", results$outliers$n_outliers, "outliers detectados",
        "(", round(results$outliers$pct_outliers, 1), "%)\n\n")

    cat("    Puntos Influyentes:\n")
    cat("      -", results$influential_points$n_influential,
        "puntos con residuos estandarizados > 3\n\n")

    cat("    Autocorrelacion (Durbin-Watson):\n")
    cat("      - DW =", round(dw_statistic, 4), "\n")
    cat("      -", results$autocorrelation$interpretation, "\n")
  }

  results$residuals <- residuals
  results$predicted <- pred
  results$observed <- obs

  class(results) <- c("easyml_residuals", "list")
  results
}


#' @title Gráficos de Diagnóstico de Residuos
#' @export
plot_residual_diagnostics <- function(residual_analysis) {

  residuals <- residual_analysis$residuals
  predicted <- residual_analysis$predicted
  observed <- residual_analysis$observed

  plot_data <- data.frame(
    residuals = residuals,
    predicted = predicted,
    observed = observed,
    std_residuals = (residuals - mean(residuals)) / sd(residuals)
  )

  # 1. Residuos vs Predicciones
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = predicted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "orange") +
    ggplot2::labs(title = "Residuos vs Predicciones", x = "Predicciones", y = "Residuos") +
    ggplot2::theme_minimal()

  # 2. Q-Q Plot
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = std_residuals)) +
    ggplot2::stat_qq(alpha = 0.5, color = "#e74c3c") +
    ggplot2::stat_qq_line(color = "black", linetype = "dashed") +
    ggplot2::labs(title = "Q-Q Plot de Residuos", x = "Cuantiles Teoricos", y = "Cuantiles Observados") +
    ggplot2::theme_minimal()

  # 3. Histograma de residuos
  p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 30, fill = "#2ecc71", color = "white", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Distribucion de Residuos", x = "Residuos", y = "Frecuencia") +
    ggplot2::theme_minimal()

  # 4. Scale-Location
  p4 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = predicted, y = sqrt(abs(std_residuals)))) +
    ggplot2::geom_point(alpha = 0.5, color = "#9b59b6") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
    ggplot2::labs(title = "Scale-Location", x = "Predicciones", y = "sqrt(|Residuos Std|)") +
    ggplot2::theme_minimal()

  # Combinar con patchwork si está disponible
  if (requireNamespace("patchwork", quietly = TRUE)) {
    (p1 + p2) / (p3 + p4)
  } else {
    list(residuals_vs_fitted = p1, qq_plot = p2,
         histogram = p3, scale_location = p4)
  }
}


# =============================================================================
# 5. DETECCION DE DATA LEAKAGE (Post-Modelo)
# =============================================================================

#' @title Detectar Posible Data Leakage (Verificaciones Post-Modelo)
#'
#' @description
#' Analiza los resultados del modelo entrenado para detectar
#' indicadores de data leakage que solo se pueden verificar despues
#' del entrenamiento. Las verificaciones pre-modelo (correlacion con
#' target, posibles IDs) se realizan en eda_leakage() durante el EDA.
#'
#' Verificaciones realizadas:
#' - AUC/R-squared > 0.99 (rendimiento sospechosamente alto)
#' - Variable con importancia > 80% del total (variable dominante)
#'
#' @param model_results Resultados del modelo (requerido)
#' @param importance Importancia de variables (requerido)
#' @param verbose Mostrar resultados
#'
#' @return Lista con alertas de posible leakage
#' @export
detect_data_leakage <- function(model_results = NULL,
                                 importance = NULL,
                                 verbose = TRUE) {

  warnings <- list()
  warning_count <- 0

  if (verbose) {
    cat("    (Verificaciones post-modelo - las verificaciones pre-modelo se\n")
    cat("     realizaron en EDA seccion 1.2.8)\n\n")
    cat("    Verificando:\n")
    cat("      - AUC/R-squared > 0.99 (rendimiento sospechoso)\n")
    cat("      - Variable con importancia > 80% (variable dominante)\n\n")
  }

  # 1. Rendimiento sospechosamente alto (AUC > 0.99 o R-squared > 0.99)
  if (!is.null(model_results) && !is.null(model_results$test_metrics)) {
    metrics <- model_results$test_metrics

    # Verificar AUC para clasificacion
    if ("roc_auc" %in% metrics$.metric) {
      auc <- metrics$.estimate[metrics$.metric == "roc_auc"]
      if (auc > 0.99) {
        warnings$high_performance <- list(
          type = "RENDIMIENTO MUY ALTO",
          metric = "AUC",
          value = auc,
          message = "AUC > 0.99 puede indicar data leakage o target trivial"
        )
        warning_count <- warning_count + 1
      }
    }

    # Verificar R-squared para regresion
    if ("rsq" %in% metrics$.metric) {
      rsq <- metrics$.estimate[metrics$.metric == "rsq"]
      if (rsq > 0.99) {
        warnings$high_performance <- list(
          type = "RENDIMIENTO MUY ALTO",
          metric = "R-squared",
          value = rsq,
          message = "R-squared > 0.99 puede indicar data leakage"
        )
        warning_count <- warning_count + 1
      }
    }
  }

  # 2. Variable con importancia dominante (> 80% del total)
  if (!is.null(importance) && nrow(importance) > 1) {
    total_importance <- sum(importance$Importance)
    top_importance <- importance$Importance[1]
    top_var <- importance$Variable[1]
    pct_top <- (top_importance / total_importance) * 100

    if (pct_top > 80) {
      warnings$dominant_variable <- list(
        type = "VARIABLE DOMINANTE",
        variable = top_var,
        importance_pct = round(pct_top, 1),
        message = paste0("Una variable explica ", round(pct_top, 1),
                         "% de la importancia total - posible leakage")
      )
      warning_count <- warning_count + 1
    }
  }

  # Mostrar resultados
  if (verbose) {
    if (warning_count == 0) {
      cat("    [ok] No se detectaron indicadores post-modelo de data leakage\n")
      cat("         - Rendimiento dentro de rangos normales\n")
      cat("         - Importancia distribuida entre variables\n")
    } else {
      cat("    [!] Se detectaron", warning_count, "posibles indicadores de leakage:\n\n")

      if (!is.null(warnings$high_performance)) {
        w <- warnings$high_performance
        cat("      RENDIMIENTO SOSPECHOSAMENTE ALTO:\n")
        cat("        -", w$metric, "=", round(w$value, 4), "\n")
        cat("        - [!]", w$message, "\n\n")
      }

      if (!is.null(warnings$dominant_variable)) {
        w <- warnings$dominant_variable
        cat("      VARIABLE DOMINANTE:\n")
        cat("        - Variable:", w$variable, "\n")
        cat("        - Importancia:", w$importance_pct, "% del total\n")
        cat("        - [!]", w$message, "\n")
        cat("        - [!] Verifique que esta variable no contiene\n")
        cat("              informacion del futuro o del target\n\n")
      }
    }
  }

  list(
    warnings = warnings,
    n_warnings = warning_count,
    leakage_suspected = warning_count > 0
  )
}


# =============================================================================
# 6. FEATURE SELECTION DENTRO DEL CV (Wrapper)
# =============================================================================

#' @title Feature Selection con Validación Cruzada
#'
#' @description
#' Realiza feature selection dentro del loop de CV para evitar overfitting.
#' Usa recursive feature elimination (RFE) simplificado.
#'
#' @param data Datos de entrenamiento
#' @param target Variable objetivo
#' @param task Tipo de tarea
#' @param cv_folds Número de folds
#' @param min_features Mínimo de features a mantener
#' @param seed Semilla
#' @param verbose Mostrar progreso
#'
#' @return Lista con features seleccionadas
#' @export
cv_feature_selection <- function(data,
                                  target,
                                  task,
                                  cv_folds = 5,
                                  min_features = 5,
                                  seed = 2024,
                                  verbose = TRUE) {

  set.seed(seed)

  if (verbose) {
    cat("    Feature Selection con CV (evita overfitting):\n")
    cat("      - Folds:", cv_folds, "\n")
    cat("      - Minimo features:", min_features, "\n\n")
  }

  # Crear folds
  if (task == "classification") {
    folds <- rsample::vfold_cv(data, v = cv_folds, strata = !!rlang::sym(target))
  } else {
    folds <- rsample::vfold_cv(data, v = cv_folds)
  }

  # Variables predictoras
  predictors <- setdiff(names(data), target)
  n_predictors <- length(predictors)

  # Conteo de selección por variable
  selection_count <- setNames(rep(0, n_predictors), predictors)

  for (i in 1:cv_folds) {
    if (verbose) cat("      Fold", i, "/", cv_folds, "...")

    train_fold <- rsample::analysis(folds$splits[[i]])

    # Feature selection en este fold usando RF importance
    tryCatch({
      # Preparar datos
      train_clean <- train_fold |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
        na.omit()

      # Entrenar RF rápido para importancia
      if (task == "classification") {
        rf_model <- ranger::ranger(
          formula = as.formula(paste(target, "~ .")),
          data = train_clean,
          num.trees = 100,
          importance = "impurity",
          seed = seed
        )
      } else {
        rf_model <- ranger::ranger(
          formula = as.formula(paste(target, "~ .")),
          data = train_clean,
          num.trees = 100,
          importance = "impurity",
          seed = seed
        )
      }

      # Obtener importancia
      imp <- rf_model$variable.importance
      imp_sorted <- sort(imp, decreasing = TRUE)

      # Seleccionar top features (al menos min_features)
      n_select <- max(min_features, ceiling(length(imp) * 0.5))
      selected_vars <- names(imp_sorted)[1:min(n_select, length(imp_sorted))]

      # Incrementar conteo
      for (v in selected_vars) {
        if (v %in% names(selection_count)) {
          selection_count[v] <- selection_count[v] + 1
        }
      }

      if (verbose) cat(" OK\n")

    }, error = function(e) {
      if (verbose) cat(" Error\n")
    })
  }

  # Features seleccionadas en mayoría de folds
  threshold <- ceiling(cv_folds / 2)
  final_selected <- names(selection_count)[selection_count >= threshold]

  if (verbose) {
    cat("\n    Features seleccionadas (en >=", threshold, "folds):\n")
    cat("      -", length(final_selected), "de", n_predictors, "variables\n")
    if (length(final_selected) <= 10) {
      cat("      - Variables:", paste(final_selected, collapse = ", "), "\n")
    } else {
      cat("      - Top 10:", paste(head(final_selected, 10), collapse = ", "), "...\n")
    }
  }

  list(
    selected_features = final_selected,
    selection_counts = sort(selection_count, decreasing = TRUE),
    threshold = threshold,
    n_folds = cv_folds
  )
}

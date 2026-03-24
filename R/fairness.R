# =============================================================================
# Analisis de Equidad (Fairness) para easyML
# Solo clasificacion binaria
# =============================================================================

#' @title Analisis de Equidad (Fairness) del Modelo
#'
#' @description
#' Evalua la equidad del modelo de clasificacion comparando metricas de
#' rendimiento entre subgrupos de una variable protegida. Calcula Demographic
#' Parity, Equalized Odds, Predictive Parity, Calibration por grupo y
#' Disparate Impact Ratio.
#'
#' @param predictions Data frame con predicciones (columnas: target, .pred_class, y probabilidades).
#' @param target Nombre de la variable objetivo.
#' @param protected_var Nombre de la variable protegida (e.g., "sex", "race").
#' @param fairness_threshold Umbral para la regla 4/5 de Disparate Impact (default: 0.8).
#' @param verbose Mostrar resultados en consola (default: TRUE).
#'
#' @return Lista con metricas de fairness por subgrupo y metricas globales.
#'
#' @examples
#' \dontrun{
#' result <- easy_ml(data, target = "outcome",
#'                   protected_var = "sex", run_fairness = TRUE)
#' result$fairness_analysis
#' }
#'
#' @export
analyze_fairness <- function(predictions, target, protected_var,
                             fairness_threshold = 0.8, verbose = TRUE) {

  # --- Validaciones ---
  if (!protected_var %in% names(predictions)) {
    stop("Variable protegida '", protected_var, "' no encontrada en predictions")
  }
  if (!target %in% names(predictions)) {
    stop("Variable objetivo '", target, "' no encontrada en predictions")
  }
  if (!".pred_class" %in% names(predictions)) {
    stop("Columna '.pred_class' no encontrada. Se requiere clasificacion binaria.")
  }

  target_vec <- predictions[[target]]
  if (!is.factor(target_vec) || length(levels(target_vec)) != 2) {
    stop("Fairness requiere clasificacion binaria (variable objetivo con 2 niveles)")
  }

  protected <- predictions[[protected_var]]
  groups <- unique(protected)

  if (length(groups) < 2) {
    stop("La variable protegida debe tener al menos 2 grupos")
  }

  # Detectar clase positiva
  event_info <- .detect_event_level(target_vec)
  pos_class <- event_info$positive_class
  neg_class <- event_info$negative_class
  prob_col <- event_info$prob_col

  pred_class <- predictions$.pred_class
  truth <- target_vec

  has_probs <- prob_col %in% names(predictions)

  if (verbose) {
    cat("  Variable protegida: ", protected_var, "\n", sep = "")
    cat("  Grupos: ", paste(groups, collapse = ", "), "\n", sep = "")
    cat("  Clase positiva: ", pos_class, "\n", sep = "")
    cat("  n total: ", nrow(predictions), "\n\n", sep = "")
  }

  # --- Metricas por subgrupo ---
  group_metrics_list <- lapply(groups, function(g) {
    idx <- protected == g
    n_g <- sum(idx)
    truth_g <- truth[idx]
    pred_g <- pred_class[idx]

    tp <- sum(pred_g == pos_class & truth_g == pos_class)
    fp <- sum(pred_g == pos_class & truth_g == neg_class)
    tn <- sum(pred_g == neg_class & truth_g == neg_class)
    fn <- sum(pred_g == neg_class & truth_g == pos_class)

    prevalence <- (tp + fn) / n_g
    positive_rate <- (tp + fp) / n_g
    tpr <- if ((tp + fn) > 0) tp / (tp + fn) else NA
    fpr <- if ((fp + tn) > 0) fp / (fp + tn) else NA
    ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA
    npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA
    accuracy <- (tp + tn) / n_g

    mean_prob <- NA
    mean_prob_pos <- NA
    mean_prob_neg <- NA
    if (has_probs) {
      probs_g <- predictions[[prob_col]][idx]
      mean_prob <- mean(probs_g, na.rm = TRUE)
      if (sum(truth_g == pos_class) > 0) {
        mean_prob_pos <- mean(probs_g[truth_g == pos_class], na.rm = TRUE)
      }
      if (sum(truth_g == neg_class) > 0) {
        mean_prob_neg <- mean(probs_g[truth_g == neg_class], na.rm = TRUE)
      }
    }

    data.frame(
      group = as.character(g),
      n = n_g,
      prevalence = round(prevalence, 3),
      positive_rate = round(positive_rate, 3),
      tpr = round(tpr, 3),
      fpr = round(fpr, 3),
      ppv = round(ppv, 3),
      npv = round(npv, 3),
      accuracy = round(accuracy, 3),
      mean_prob = round(mean_prob, 3),
      mean_prob_pos = round(mean_prob_pos, 3),
      stringsAsFactors = FALSE
    )
  })

  group_metrics <- do.call(rbind, group_metrics_list)

  # Advertencia para grupos pequenos
  small_groups <- group_metrics$group[group_metrics$n < 10]
  if (length(small_groups) > 0 && verbose) {
    cat("  ADVERTENCIA: Grupos con n < 10: ", paste(small_groups, collapse = ", "),
        "\n  Las metricas pueden ser poco fiables.\n\n", sep = "")
  }

  # --- Metricas globales de fairness ---

  # 1. Demographic Parity Difference
  dp_diff <- max(group_metrics$positive_rate, na.rm = TRUE) -
    min(group_metrics$positive_rate, na.rm = TRUE)

  # 2. Equalized Odds Difference (TPR y FPR)
  eo_tpr_diff <- max(group_metrics$tpr, na.rm = TRUE) -
    min(group_metrics$tpr, na.rm = TRUE)
  eo_fpr_diff <- max(group_metrics$fpr, na.rm = TRUE) -
    min(group_metrics$fpr, na.rm = TRUE)

  # 3. Predictive Parity Difference
  pp_diff <- max(group_metrics$ppv, na.rm = TRUE) -
    min(group_metrics$ppv, na.rm = TRUE)

  # 4. Calibration Difference
  cal_diff <- NA
  if (has_probs) {
    cal_diff <- max(group_metrics$mean_prob_pos, na.rm = TRUE) -
      min(group_metrics$mean_prob_pos, na.rm = TRUE)
  }

  # 5. Disparate Impact Ratio (regla 4/5)
  min_pr <- min(group_metrics$positive_rate, na.rm = TRUE)
  max_pr <- max(group_metrics$positive_rate, na.rm = TRUE)
  di_ratio <- if (max_pr > 0) min_pr / max_pr else NA
  passes_45 <- !is.na(di_ratio) && di_ratio >= fairness_threshold

  # --- Verbose ---
  if (verbose) {
    cat("  METRICAS POR SUBGRUPO:\n")
    cat("  ", sprintf("%-12s %5s %6s %6s %6s %6s %6s %6s\n",
                      "Grupo", "n", "P(+)", "TPR", "FPR", "PPV", "Acc", "P.prob"))
    cat("  ", paste(rep("-", 60), collapse = ""), "\n", sep = "")
    for (i in seq_len(nrow(group_metrics))) {
      g <- group_metrics[i, ]
      cat("  ", sprintf("%-12s %5d %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f\n",
                        g$group, g$n, g$positive_rate,
                        g$tpr, g$fpr, g$ppv, g$accuracy, g$mean_prob))
    }

    cat("\n  METRICAS GLOBALES DE FAIRNESS:\n")
    cat("  ", paste(rep("-", 50), collapse = ""), "\n", sep = "")

    .print_fairness_metric("Demographic Parity Diff", dp_diff, 0.1)
    .print_fairness_metric("Equalized Odds (TPR Diff)", eo_tpr_diff, 0.1)
    .print_fairness_metric("Equalized Odds (FPR Diff)", eo_fpr_diff, 0.1)
    .print_fairness_metric("Predictive Parity Diff", pp_diff, 0.1)
    if (!is.na(cal_diff)) {
      .print_fairness_metric("Calibration Diff", cal_diff, 0.1)
    }

    cat("\n  DISPARATE IMPACT:\n")
    cat("    Ratio: ", round(di_ratio, 3), "\n", sep = "")
    cat("    Regla 4/5 (umbral ", fairness_threshold, "): ",
        ifelse(passes_45, "CUMPLE", "NO CUMPLE"), "\n", sep = "")

    if (!passes_45) {
      cat("\n  ADVERTENCIA: El modelo muestra disparate impact.\n")
      cat("  El grupo con menor tasa de predicciones positivas recibe\n")
      cat("  menos del ", round(fairness_threshold * 100), "% de la tasa del grupo favorecido.\n", sep = "")
    }

    cat("\n  INTERPRETACION:\n")
    overall_fair <- dp_diff < 0.1 && eo_tpr_diff < 0.1 && passes_45
    if (overall_fair) {
      cat("    El modelo muestra equidad aceptable entre subgrupos.\n")
      cat("    Todas las diferencias estan dentro de umbrales recomendados.\n")
    } else {
      if (dp_diff >= 0.1) {
        cat("    - Demographic Parity: tasas de prediccion positiva difieren\n")
        cat("      significativamente entre grupos.\n")
      }
      if (eo_tpr_diff >= 0.1) {
        cat("    - Equalized Odds: la sensibilidad difiere entre grupos.\n")
        cat("      El modelo detecta mejor los positivos en unos grupos que otros.\n")
      }
      if (eo_fpr_diff >= 0.1) {
        cat("    - Equalized Odds: la tasa de falsos positivos difiere.\n")
        cat("      Algunos grupos reciben mas falsos positivos.\n")
      }
      if (!passes_45) {
        cat("    - Disparate Impact: considerar re-calibracion o post-procesamiento.\n")
      }
    }
  }

  # --- Resultado ---
  list(
    group_metrics = group_metrics,
    demographic_parity_diff = round(dp_diff, 4),
    equalized_odds_tpr_diff = round(eo_tpr_diff, 4),
    equalized_odds_fpr_diff = round(eo_fpr_diff, 4),
    predictive_parity_diff = round(pp_diff, 4),
    calibration_diff = if (!is.na(cal_diff)) round(cal_diff, 4) else NA,
    disparate_impact_ratio = round(di_ratio, 4),
    fairness_threshold = fairness_threshold,
    passes_four_fifths = passes_45,
    protected_var = protected_var,
    is_fair = dp_diff < 0.1 && eo_tpr_diff < 0.1 && passes_45
  )
}


# --- Helper para imprimir metrica de fairness ---
#' @noRd
.print_fairness_metric <- function(name, value, threshold) {
  status <- if (value < threshold) "[ok]" else "[!] "
  cat(sprintf("    %s %-30s %6.3f %s\n", status, name, value,
              ifelse(value >= threshold, paste0("(> ", threshold, ")"), "")))
}


#' @title Correccion de Fairness por Threshold por Grupo
#' @param predictions Data frame con predicciones
#' @param target Nombre de la variable objetivo
#' @param protected_var Nombre de la variable protegida
#' @param verbose Mostrar resultados
#' @return Lista con thresholds por grupo y metricas antes/despues
#' @export
correct_fairness_threshold <- function(predictions, target, protected_var,
                                        verbose = TRUE) {
  # Detect positive class
  event_info <- .detect_event_level(predictions[[target]])
  pos_class <- event_info$positive_class
  neg_class <- event_info$negative_class
  prob_col <- event_info$prob_col

  probs <- predictions[[prob_col]]
  truth <- predictions[[target]]
  groups <- predictions[[protected_var]]
  unique_groups <- unique(groups)

  # Find per-group threshold that equalizes TPR
  # First, compute target TPR = mean TPR across groups at Youden threshold
  group_youden <- sapply(unique_groups, function(g) {
    idx <- groups == g
    p <- probs[idx]
    t <- truth[idx]
    best_j <- -Inf
    best_th <- 0.5
    for (th in seq(0.05, 0.95, by = 0.01)) {
      pred <- ifelse(p >= th, pos_class, neg_class)
      pred <- factor(pred, levels = levels(truth))
      tp <- sum(pred == pos_class & t == pos_class)
      fn <- sum(pred == neg_class & t == pos_class)
      fp <- sum(pred == pos_class & t == neg_class)
      tn <- sum(pred == neg_class & t == neg_class)
      tpr <- if ((tp + fn) > 0) tp / (tp + fn) else 0
      fpr <- if ((fp + tn) > 0) fp / (fp + tn) else 0
      j <- tpr - fpr
      if (j > best_j) { best_j <- j; best_th <- th }
    }
    best_th
  })

  # Compute TPR at each group's Youden threshold
  group_tpr_before <- sapply(seq_along(unique_groups), function(i) {
    g <- unique_groups[i]
    idx <- groups == g
    pred <- ifelse(probs[idx] >= 0.5, pos_class, neg_class)
    pred <- factor(pred, levels = levels(truth))
    t <- truth[idx]
    tp <- sum(pred == pos_class & t == pos_class)
    fn <- sum(pred == neg_class & t == pos_class)
    if ((tp + fn) > 0) tp / (tp + fn) else 0
  })

  # Target TPR = mean of TPRs at Youden
  target_tpr_vals <- sapply(seq_along(unique_groups), function(i) {
    g <- unique_groups[i]
    idx <- groups == g
    th <- group_youden[i]
    pred <- ifelse(probs[idx] >= th, pos_class, neg_class)
    t <- truth[idx]
    tp <- sum(pred == pos_class & t == pos_class)
    fn <- sum(pred == neg_class & t == pos_class)
    if ((tp + fn) > 0) tp / (tp + fn) else 0
  })
  target_tpr <- mean(target_tpr_vals)

  # For each group, find threshold closest to target TPR
  group_thresholds <- sapply(unique_groups, function(g) {
    idx <- groups == g
    p <- probs[idx]
    t <- truth[idx]
    best_diff <- Inf
    best_th <- 0.5
    for (th in seq(0.05, 0.95, by = 0.01)) {
      pred <- ifelse(p >= th, pos_class, neg_class)
      tp <- sum(pred == pos_class & t == pos_class)
      fn <- sum(pred == neg_class & t == pos_class)
      tpr <- if ((tp + fn) > 0) tp / (tp + fn) else 0
      diff <- abs(tpr - target_tpr)
      if (diff < best_diff) { best_diff <- diff; best_th <- th }
    }
    best_th
  })

  # Compute TPR after correction
  group_tpr_after <- sapply(seq_along(unique_groups), function(i) {
    g <- unique_groups[i]
    idx <- groups == g
    pred <- ifelse(probs[idx] >= group_thresholds[i], pos_class, neg_class)
    t <- truth[idx]
    tp <- sum(pred == pos_class & t == pos_class)
    fn <- sum(pred == neg_class & t == pos_class)
    if ((tp + fn) > 0) tp / (tp + fn) else 0
  })

  # Build corrected predictions
  corrected <- predictions
  new_pred <- character(nrow(predictions))
  for (i in seq_along(unique_groups)) {
    idx <- groups == unique_groups[i]
    new_pred[idx] <- ifelse(probs[idx] >= group_thresholds[i], pos_class, neg_class)
  }
  corrected$.pred_class <- factor(new_pred, levels = levels(truth))

  # Build result
  gt_df <- data.frame(
    group = unique_groups,
    threshold = round(as.numeric(group_thresholds), 2),
    tpr_before = round(group_tpr_before, 3),
    tpr_after = round(group_tpr_after, 3),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cat("\n    CORRECCION POSTPROCESAMIENTO:\n")
    cat("    Thresholds por Grupo (Equalizando TPR)\n\n")
    cat("    Grupo        Threshold  TPR antes  TPR despues\n")
    cat("    ", paste(rep("-", 50), collapse = ""), "\n", sep = "")
    for (i in seq_len(nrow(gt_df))) {
      cat(sprintf("    %-12s %9.2f  %9.3f  %11.3f\n",
                  gt_df$group[i], gt_df$threshold[i],
                  gt_df$tpr_before[i], gt_df$tpr_after[i]))
    }
    tpr_diff_before <- max(group_tpr_before) - min(group_tpr_before)
    tpr_diff_after <- max(group_tpr_after) - min(group_tpr_after)
    cat("\n    TPR Difference: ", round(tpr_diff_before, 3),
        " -> ", round(tpr_diff_after, 3), "\n", sep = "")
    if (tpr_diff_after < tpr_diff_before) {
      cat("    [ok] Equidad mejorada\n")
    }
    cat("\n    Referencia: Hardt, M., Price, E., & Srebro, N. (2016).\n")
    cat("    Equality of opportunity in supervised learning. NeurIPS.\n")
  }

  list(
    method = "postprocesamiento",
    group_thresholds = gt_df,
    target_tpr = round(target_tpr, 3),
    corrected_predictions = corrected
  )
}


#' @title Resampleo Estratificado por Equidad
#' @param train_data Data frame de entrenamiento
#' @param target Nombre de la variable objetivo
#' @param protected_var Nombre de la variable protegida
#' @param seed Semilla
#' @param verbose Mostrar resultados
#' @return Data frame rebalanceado
#' @export
resample_fairness <- function(train_data, target, protected_var,
                               seed = 2024, verbose = TRUE) {
  set.seed(seed)

  # Create stratification variable
  strat <- paste(train_data[[target]], train_data[[protected_var]], sep = "_")
  counts <- table(strat)
  target_n <- max(counts)

  if (verbose) {
    cat("\n    CORRECCION PREPROCESAMIENTO:\n")
    cat("    Resampleo estratificado por equidad\n\n")
    cat("    Variable protegida:", protected_var, "\n")
    cat("    Combinaciones target x", protected_var, ":\n")
  }

  resampled_parts <- list()
  for (combo in names(counts)) {
    idx <- which(strat == combo)
    current_n <- length(idx)

    if (current_n < target_n) {
      # Upsample with replacement
      extra_idx <- sample(idx, target_n - current_n, replace = TRUE)
      resampled_parts[[combo]] <- train_data[c(idx, extra_idx), , drop = FALSE]
    } else {
      resampled_parts[[combo]] <- train_data[idx, , drop = FALSE]
    }

    if (verbose) {
      new_n <- nrow(resampled_parts[[combo]])
      added <- new_n - current_n
      cat(sprintf("      %-20s : %4d -> %4d (+%d)\n",
                  combo, current_n, new_n, added))
    }
  }

  result <- do.call(rbind, resampled_parts)
  rownames(result) <- NULL

  if (verbose) {
    cat("\n    Total train antes:", nrow(train_data),
        "| despues:", nrow(result), "\n")
    cat("    Metodo: upsampling estratificado (con reemplazo)\n")
  }

  result
}

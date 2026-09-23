# =============================================================================
# Funciones de Interpretabilidad (SHAP) para easyML
# =============================================================================

#' @title Analisis de Interpretabilidad
#'
#' @description
#' Calcula importancia de variables y valores SHAP para interpretabilidad.
#'
#' @param evaluation_result Resultado de evaluate_model()
#' @param train_data Datos de entrenamiento
#' @param test_data Datos de test
#' @param target Variable objetivo
#' @param n_shap Numero de observaciones para SHAP
#' @param verbose Mostrar progreso
#'
#' @return Lista con resultados de interpretabilidad
#' @export
interpret_model <- function(evaluation_result,
                            train_data,
                            test_data,
                            target,
                            n_shap = 100,
                            verbose = TRUE) {

  if (verbose) {
    .print_section(6, "Interpretabilidad del Modelo")
  }

  results <- list()
  final_fit <- evaluation_result$final_fit

  # 6.1 Importancia de Variables
  if (verbose) .print_subsection(6, 1, "Importancia de Variables")
  importance <- get_importance(final_fit, verbose)
  results$importance <- importance
  if (verbose) .print_reference("importance")

  # 6.2 Valores SHAP
  if (verbose) .print_subsection(6, 2, "Valores SHAP")

  shap_result <- tryCatch({
    calculate_shap(
      final_fit = final_fit,
      train_data = train_data,
      test_data = test_data,
      target = target,
      n_samples = n_shap,
      verbose = verbose
    )
  }, error = function(e) {
    if (verbose) {
      cat("    [!] No se pudo calcular SHAP:", conditionMessage(e), "\n")
    }
    NULL
  })

  results$shap <- shap_result
  if (verbose) .print_reference("shap")

  class(results) <- c("supervisedml_interpret", "list")
  return(results)
}


#' @title Obtener Importancia de Variables
#' @export
get_importance <- function(final_fit, verbose = TRUE) {

  importance <- tryCatch({
    imp <- vip::vi(final_fit)
    imp_df <- data.frame(
      Variable = imp$Variable,
      Importance = imp$Importance
    )
    imp_df <- imp_df[order(-imp_df$Importance), ]
    imp_df$Rank <- 1:nrow(imp_df)
    imp_df
  }, error = function(e) {
    if (verbose) {
      cat("    [!] No se pudo extraer importancia:", conditionMessage(e), "\n")
    }
    data.frame(Variable = character(), Importance = numeric(), Rank = integer())
  })

  if (verbose && nrow(importance) > 0) {
    cat("    Top 10 variables mas importantes:\n\n")
    top10 <- utils::head(importance, 10)
    for (i in 1:nrow(top10)) {
      cat("    ", i, ". ", top10$Variable[i], " (",
          round(top10$Importance[i], 3), ")\n", sep = "")
    }
  }

  importance
}


#' @title Calcular Valores SHAP
#' @export
calculate_shap <- function(final_fit,
                           train_data,
                           test_data,
                           target,
                           n_samples = 100,
                           verbose = TRUE) {

  if (!requireNamespace("fastshap", quietly = TRUE)) {
    if (verbose) cat("    [!] Paquete fastshap no instalado\n")
    return(NULL)
  }

  # Preparar datos
  predictors <- setdiff(names(train_data), target)

  # Variables que el modelo realmente usa. La receta puede eliminar
  # predictores (alta correlacion, VIF, varianza cero); fastshap les asigna
  # SHAP = 0 y en el grafico parecen "sin importancia" cuando el modelo
  # nunca las vio. Se detectan aqui para quitarlas del resultado.
  used_predictors <- .shap_used_predictors(final_fit, train_data, predictors)
  removed_vars <- setdiff(predictors, used_predictors)

  # Submuestra para SHAP. En clasificacion se estratifica con el mismo
  # numero de casos por clase: con clases desbalanceadas, una muestra
  # aleatoria de 100 deja muy pocos casos de la clase minoritaria
  # (p. ej., 4 con una prevalencia del 4 %) y la importancia es inestable.
  target_test <- test_data[[target]]
  n_test <- min(n_samples, nrow(test_data))
  if (!is.numeric(target_test)) {
    idx <- .shap_stratified_index(target_test, n_test)
    sample_design <- "estratificado (mismo numero de casos por clase)"
  } else {
    idx <- sample(nrow(test_data), n_test)
    sample_design <- "aleatorio"
  }
  test_sample <- test_data[idx, ]
  n_test <- length(idx)

  if (verbose) {
    cat("    Calculando SHAP para", n_test, "observaciones (muestreo",
        sample_design, ")...\n")
    if (!is.numeric(target_test)) {
      tab <- table(test_sample[[target]])
      cat("    Casos por clase:", paste(names(tab), tab, sep = " = ", collapse = ", "), "\n")
    }
  }

  # Usar el workflow completo (no el modelo extraido) para que la receta
  # se aplique automaticamente. Asi fastshap puede permutar los datos
  # crudos (con factores) y el workflow los transforma internamente.

  # Detectar tipo de tarea
  target_vec <- train_data[[target]]
  is_regression <- is.numeric(target_vec)
  n_classes <- if (is_regression) 0 else length(levels(factor(target_vec)))
  is_multiclass <- !is_regression && n_classes >= 3

  X_shap <- as.data.frame(test_sample[, predictors])

  # Calcular SHAP
  shap_values <- tryCatch({
    if (is_regression) {
      # Regresion: prediccion numerica directa
      if (verbose) cat("    Modo: regresion (prediccion numerica)\n")
      fastshap::explain(
        object = final_fit,
        X = X_shap,
        pred_wrapper = function(model, newdata) {
          newdata <- as.data.frame(newdata)
          preds <- stats::predict(model, new_data = newdata)
          as.numeric(preds$.pred)
        },
        nsim = 50
      )
    } else if (is_multiclass) {
      # Multiclass: calcular SHAP por clase y promediar |SHAP|
      if (verbose) cat("    Calculando SHAP por clase (", n_classes, " clases)...\n", sep = "")
      all_shap <- lapply(seq_len(n_classes), function(k) {
        shap_k <- fastshap::explain(
          object = final_fit,
          X = X_shap,
          pred_wrapper = function(model, newdata) {
            newdata <- as.data.frame(newdata)
            probs <- stats::predict(model, new_data = newdata, type = "prob")
            probs[[k]]
          },
          nsim = 50
        )
        as.matrix(as.data.frame(shap_k))
      })
      # Promedio de |SHAP| entre clases -> importancia global
      shap_agg <- Reduce("+", lapply(all_shap, abs)) / length(all_shap)
      as.data.frame(shap_agg)
    } else {
      # Binary: probabilidad de clase positiva
      fastshap::explain(
        object = final_fit,
        X = X_shap,
        pred_wrapper = function(model, newdata) {
          newdata <- as.data.frame(newdata)
          probs <- stats::predict(model, new_data = newdata, type = "prob")
          probs[[2]]
        },
        nsim = 50
      )
    }
  }, error = function(e) {
    if (verbose) cat("    Error en fastshap:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(shap_values)) {
    return(NULL)
  }

  # Quitar las variables que el modelo no usa (ver arriba)
  shap_values <- as.data.frame(shap_values)
  keep <- intersect(colnames(shap_values), used_predictors)
  shap_values <- shap_values[, keep, drop = FALSE]

  # Calcular importancia media por variable
  shap_importance <- data.frame(
    Variable = colnames(shap_values),
    Mean_Abs_SHAP = colMeans(abs(shap_values))
  )
  shap_importance <- shap_importance[order(-shap_importance$Mean_Abs_SHAP), ]
  rownames(shap_importance) <- NULL

  # Colinealidad entre las variables usadas: con VIF altos, el modelo reparte
  # el efecto entre variables casi redundantes de forma arbitraria y el SHAP
  # de cada una no es interpretable por separado.
  collinear_vars <- .shap_collinear(train_data, keep, threshold = 5)

  if (verbose) {
    cat("\n    Importancia SHAP (top 10):\n\n")
    top10 <- utils::head(shap_importance, 10)
    for (i in 1:nrow(top10)) {
      cat("    ", i, ". ", top10$Variable[i], " (",
          round(top10$Mean_Abs_SHAP[i], 4), ")\n", sep = "")
    }
    if (length(removed_vars) > 0) {
      cat("\n    [i] Variables eliminadas en el preprocesamiento (el modelo no las usa,\n",
          "        se excluyen del SHAP):", paste(removed_vars, collapse = ", "), "\n")
    }
    if (nrow(collinear_vars) > 0) {
      cat("\n    [!] Colinealidad entre predictores usados (VIF > 5):\n")
      for (i in seq_len(nrow(collinear_vars))) {
        cat("        -", collinear_vars$Variable[i], "(VIF =",
            round(collinear_vars$VIF[i], 1), ")\n")
      }
      cat("        El reparto del SHAP entre estas variables depende de cual elige\n",
          "       el modelo y NO debe interpretarse variable por variable. Para saber\n",
          "       cuanto discrimina cada una por si sola, use una medida univariada\n",
          "       (p. ej., AUC de cada predictor).\n")
    }
  }

  # Guardar test_sample solo con predictores y row-names limpios
  # para que shapviz no tenga discrepancia de filas
  test_sample_clean <- as.data.frame(test_sample[, keep, drop = FALSE])
  rownames(test_sample_clean) <- NULL
  rownames(shap_values) <- NULL

  list(
    shap_values = shap_values,
    importance = shap_importance,
    test_sample = test_sample_clean,
    removed_vars = removed_vars,
    collinear_vars = collinear_vars,
    sample_design = sample_design
  )
}


# Predictores que el workflow ajustado realmente usa: los que sobreviven a la
# receta preparada. Una variable original cuenta como usada si aparece tal
# cual o como prefijo de una columna dummy (var_nivel). Si no se puede
# determinar, se devuelven todos (comportamiento anterior).
.shap_used_predictors <- function(final_fit, train_data, predictors) {
  tryCatch({
    rec <- workflows::extract_recipe(final_fit, estimated = TRUE)
    baked <- names(recipes::bake(rec, new_data = utils::head(train_data, 5)))
    used <- predictors[vapply(predictors, function(p) {
      p %in% baked || any(startsWith(baked, paste0(p, "_")))
    }, logical(1))]
    if (length(used) == 0) predictors else used
  }, error = function(e) predictors)
}


# Indices de una muestra con el mismo numero de casos por clase (hasta donde
# alcance cada clase); lo que falte se completa al azar con el resto.
.shap_stratified_index <- function(y, n) {
  y <- factor(y)
  por_clase <- floor(n / nlevels(y))
  idx <- unlist(lapply(levels(y), function(k) {
    pool <- which(y == k)
    if (length(pool) <= por_clase) pool else sample(pool, por_clase)
  }), use.names = FALSE)
  faltan <- n - length(idx)
  if (faltan > 0) {
    resto <- setdiff(seq_along(y), idx)
    idx <- c(idx, if (length(resto) <= faltan) resto else sample(resto, faltan))
  }
  idx
}


# VIF de las variables numericas usadas: diagonal de la inversa de la matriz
# de correlaciones. Devuelve las que superan el umbral.
.shap_collinear <- function(data, vars, threshold = 5) {
  vacio <- data.frame(Variable = character(), VIF = numeric())
  num <- vars[vapply(vars, function(v) is.numeric(data[[v]]), logical(1))]
  if (length(num) < 2) return(vacio)
  tryCatch({
    R <- stats::cor(data[, num, drop = FALSE], use = "pairwise.complete.obs")
    # si R es singular (variables exactamente redundantes), pseudoinversa por
    # SVD, sin depender de MASS
    inv <- tryCatch(solve(R), error = function(e) {
      s <- svd(R)
      d <- ifelse(s$d > max(dim(R)) * max(s$d) * .Machine$double.eps, 1 / s$d, 0)
      s$v %*% (d * t(s$u))
    })
    vif <- diag(inv)
    out <- data.frame(Variable = num, VIF = as.numeric(vif))
    out <- out[out$VIF > threshold, , drop = FALSE]
    out[order(-out$VIF), , drop = FALSE]
  }, error = function(e) vacio)
}


#' @title Grafico de Importancia de Variables
#' @export
plot_importance <- function(interpret_result, top_n = 15) {

  importance <- interpret_result$importance

  if (is.null(importance) || nrow(importance) == 0) {
    message("No hay datos de importancia disponibles")
    return(NULL)
  }

  top_vars <- utils::head(importance, top_n)
  top_vars$Variable <- factor(top_vars$Variable,
                               levels = rev(top_vars$Variable))

  ggplot2::ggplot(top_vars, ggplot2::aes(x = Importance, y = Variable)) +
    ggplot2::geom_col(fill = "#3498db", alpha = 0.8) +
    ggplot2::labs(
      title = "Importancia de Variables",
      x = "Importancia",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


#' @title Grafico SHAP Summary (Beeswarm)
#' @export
plot_shap_summary <- function(interpret_result, top_n = 15) {

  if (is.null(interpret_result$shap) ||
      is.null(interpret_result$shap$shap_values)) {
    message("No hay valores SHAP disponibles")
    return(NULL)
  }

  shap_values <- interpret_result$shap$shap_values
  test_sample <- interpret_result$shap$test_sample

  # Usar shapviz para beeswarm plot
  if (requireNamespace("shapviz", quietly = TRUE)) {
    X_data <- as.data.frame(test_sample)
    sv <- shapviz::shapviz(as.matrix(shap_values), X = X_data)
    p <- shapviz::sv_importance(sv, kind = "beeswarm", show_numbers = TRUE,
                                max_display = top_n,
                                viridis_args = list(option = "D")) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "SHAP Values (Impact on Model Output)",
                    x = "", y = "SHAP value", color = "Feature Value")
    return(p)
  }

  # Fallback si shapviz no esta instalado: boxplot manual
  importance <- interpret_result$shap$importance
  top_vars <- utils::head(importance$Variable, top_n)
  shap_df <- as.data.frame(shap_values[, top_vars, drop = FALSE])
  shap_long <- tidyr::pivot_longer(
    shap_df,
    cols = dplyr::everything(),
    names_to = "Variable",
    values_to = "SHAP"
  )
  shap_long$Variable <- factor(shap_long$Variable, levels = rev(top_vars))

  ggplot2::ggplot(shap_long, ggplot2::aes(x = SHAP, y = Variable)) +
    ggplot2::geom_boxplot(fill = "#e74c3c", alpha = 0.6, outlier.size = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = "Distribucion de Valores SHAP",
      x = "Valor SHAP",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


#' @title Grafico SHAP Bar (Importancia)
#' @export
plot_shap_bar <- function(interpret_result, top_n = 15) {

  if (is.null(interpret_result$shap) ||
      is.null(interpret_result$shap$shap_values)) {
    message("No hay valores SHAP disponibles")
    return(NULL)
  }

  shap_values <- interpret_result$shap$shap_values
  test_sample <- interpret_result$shap$test_sample

  # Usar shapviz para bar plot
  if (requireNamespace("shapviz", quietly = TRUE)) {
    X_data <- as.data.frame(test_sample)
    sv <- shapviz::shapviz(as.matrix(shap_values), X = X_data)
    p <- shapviz::sv_importance(sv, kind = "bar", max_display = top_n) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "SHAP Feature Importance",
                    x = "", y = "mean(|SHAP value|)")
    return(p)
  }

  # Fallback: barras manuales con mean(|SHAP|)
  importance <- interpret_result$shap$importance
  top_vars <- utils::head(importance, top_n)
  top_vars$Variable <- factor(top_vars$Variable,
                               levels = rev(top_vars$Variable))

  ggplot2::ggplot(top_vars,
                  ggplot2::aes(x = Mean_Abs_SHAP, y = Variable)) +
    ggplot2::geom_col(fill = "#33D1FF", color = "gray32", alpha = 0.8) +
    ggplot2::labs(
      title = "SHAP Feature Importance",
      x = "mean(|SHAP value|)",
      y = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


#' @title Grafico SHAP para una variable
#' @export
plot_shap_dependence <- function(interpret_result, variable, test_data) {

  if (is.null(interpret_result$shap)) {
    message("No hay valores SHAP disponibles")
    return(NULL)
  }

  shap_values <- interpret_result$shap$shap_values
  test_sample <- interpret_result$shap$test_sample

  if (!variable %in% colnames(shap_values)) {
    message("Variable no encontrada en SHAP values")
    return(NULL)
  }

  plot_df <- data.frame(
    Feature_Value = test_sample[[variable]],
    SHAP_Value = shap_values[, variable]
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Feature_Value, y = SHAP_Value)) +
    ggplot2::geom_point(alpha = 0.6, color = "#9b59b6") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#2c3e50") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = paste("Dependencia SHAP:", variable),
      x = variable,
      y = "Valor SHAP"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}

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

  class(results) <- c("easyml_interpret", "list")
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

  # Submuestra para SHAP
  n_test <- min(n_samples, nrow(test_data))
  test_sample <- test_data[sample(nrow(test_data), n_test), ]

  if (verbose) {
    cat("    Calculando SHAP para", n_test, "observaciones...\n")
  }

  # Usar el workflow completo (no el modelo extraido) para que la receta
  # se aplique automaticamente. Asi fastshap puede permutar los datos
  # crudos (con factores) y el workflow los transforma internamente.

  # Detectar si es multiclass
  target_vec <- train_data[[target]]
  n_classes <- length(levels(factor(target_vec)))
  is_multiclass <- n_classes >= 3

  # Funcion de prediccion para el workflow
  predict_fn <- function(workflow, newdata) {
    newdata <- as.data.frame(newdata)
    preds <- stats::predict(workflow, new_data = newdata)
    if (".pred_class" %in% names(preds)) {
      # Clasificacion - usar probabilidades
      probs <- stats::predict(workflow, new_data = newdata, type = "prob")
      if (ncol(probs) == 2) {
        return(probs[[2]])  # Binary: probabilidad de clase positiva
      } else {
        return(as.matrix(probs))  # Multiclass: matrix de probabilidades
      }
    } else {
      return(preds$.pred)
    }
  }

  # Calcular SHAP
  shap_values <- tryCatch({
    fastshap::explain(
      object = final_fit,
      X = as.data.frame(test_sample[, predictors]),
      pred_wrapper = function(model, newdata) {
        predict_fn(model, newdata)
      },
      nsim = 50
    )
  }, error = function(e) {
    if (verbose) cat("    Error en fastshap:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(shap_values)) {
    return(NULL)
  }

  # Multiclass: fastshap retorna una lista de data.frames (uno por clase).
  # Agregar promediando |SHAP| entre clases para importancia global.
  if (is.list(shap_values) && !is.data.frame(shap_values)) {
    shap_matrices <- lapply(shap_values, function(sv) as.matrix(as.data.frame(sv)))
    # Promedio de |SHAP| entre clases → importancia global por variable
    shap_values <- Reduce("+", lapply(shap_matrices, abs)) / length(shap_matrices)
    shap_values <- as.data.frame(shap_values)
    if (verbose) {
      cat("    SHAP multiclase: importancia promediada entre", length(shap_matrices), "clases\n")
    }
  }

  # Calcular importancia media por variable
  shap_importance <- data.frame(
    Variable = colnames(shap_values),
    Mean_Abs_SHAP = colMeans(abs(shap_values))
  )
  shap_importance <- shap_importance[order(-shap_importance$Mean_Abs_SHAP), ]

  if (verbose) {
    cat("\n    Importancia SHAP (top 10):\n\n")
    top10 <- utils::head(shap_importance, 10)
    for (i in 1:nrow(top10)) {
      cat("    ", i, ". ", top10$Variable[i], " (",
          round(top10$Mean_Abs_SHAP[i], 4), ")\n", sep = "")
    }
  }

  list(
    shap_values = shap_values,
    importance = shap_importance,
    test_sample = test_sample
  )
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
    predictors <- colnames(shap_values)
    X_data <- as.data.frame(test_sample[, predictors, drop = FALSE])
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
    predictors <- colnames(shap_values)
    X_data <- as.data.frame(test_sample[, predictors, drop = FALSE])
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

# =============================================================================
# Funciones de visualizacion para easyML
# =============================================================================

#' @title Generar Todos los Graficos
#'
#' @description
#' Genera todos los graficos relevantes con metadatos para reportes cientificos.
#' Los graficos se almacenan con numeracion (Figura 1, Figura 2, etc.) y
#' nombres descriptivos para facilitar su referencia en reportes.
#'
#' @param x Objeto easyml
#' @param verbose Mostrar progreso
#'
#' @return Lista con graficos y catalogo de figuras
#' @export
generate_all_plots <- function(x, verbose = TRUE) {

  if (verbose) cat("\n    Generando graficos...\n")

  plots <- list()
  figures_catalog <- list()
  task <- x$task
  fig_num <- 0

  # 1. Importancia de Variables
  if (!is.null(x$importance) && nrow(x$importance) > 0) {
    fig_num <- fig_num + 1
    plots$importance <- .plot_importance(x, top_n = 15)
    figures_catalog$importance <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Importancia de Variables",
      description = paste0("Importancia de variables del modelo ", x$best_model),
      filename = paste0("Figura_", fig_num, "_Importancia_Variables.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Importancia de variables\n")
  }

  # 2. Comparacion de Modelos
  if (!is.null(x$cv_summary)) {
    fig_num <- fig_num + 1
    plots$model_comparison <- .plot_metrics(x)
    metric_name <- ifelse(task == "classification", "ROC-AUC", "RMSE")
    figures_catalog$model_comparison <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Comparacion de Modelos",
      description = paste0("Rendimiento de modelos usando ", metric_name, " en CV"),
      filename = paste0("Figura_", fig_num, "_Comparacion_Modelos.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Comparacion de modelos\n")
  }

  # 3. Graficos especificos por tarea
  if (task == "classification") {
    # Curva ROC
    fig_num <- fig_num + 1
    plots$roc_curve <- suppressWarnings(.plot_roc(x))
    auc_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "roc_auc"]
    figures_catalog$roc_curve <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Curva ROC",
      description = paste0("Curva ROC del modelo. AUC = ", round(auc_val, 3)),
      filename = paste0("Figura_", fig_num, "_Curva_ROC.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Curva ROC\n")

    # Matriz de Confusion
    fig_num <- fig_num + 1
    plots$confusion_matrix <- suppressWarnings(.plot_confusion(x))
    figures_catalog$confusion_matrix <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Matriz de Confusion",
      description = "Matriz de confusion en conjunto de test",
      filename = paste0("Figura_", fig_num, "_Matriz_Confusion.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Matriz de confusion\n")

    # Calibracion
    if (!is.null(x$evaluation$calibration)) {
      fig_num <- fig_num + 1
      plots$calibration <- .plot_calibration_deciles(x$evaluation$calibration)
      figures_catalog$calibration <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Calibracion del Modelo",
        description = "Calibracion por deciles de probabilidad",
        filename = paste0("Figura_", fig_num, "_Calibracion.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Calibracion por deciles\n")
    }

    # Optimizacion de Threshold
    if (!is.null(x$threshold_optimization)) {
      fig_num <- fig_num + 1
      plots$threshold_optimization <- plot_threshold_optimization(x$threshold_optimization)
      opt_thresh <- x$threshold_optimization$optimal_threshold
      figures_catalog$threshold_optimization <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Optimizacion de Threshold",
        description = paste0("Threshold optimo = ", round(opt_thresh, 3)),
        filename = paste0("Figura_", fig_num, "_Threshold.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Optimizacion de threshold\n")
    }

    # Curva de Calibracion (post-calibracion)
    if (!is.null(x$calibration)) {
      fig_num <- fig_num + 1
      plots$calibration_curve <- plot_calibration_curve(x$calibration)
      figures_catalog$calibration_curve <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Curva de Calibracion",
        description = "Curva de calibracion post-ajuste",
        filename = paste0("Figura_", fig_num, "_Calibracion_Curva.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Curva de calibracion\n")
    }

  } else {
    # Regresion
    fig_num <- fig_num + 1
    plots$pred_vs_obs <- .plot_pred_vs_obs(x)
    r2_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "rsq"]
    figures_catalog$pred_vs_obs <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Predicciones vs Observaciones",
      description = paste0("R2 = ", round(r2_val, 3)),
      filename = paste0("Figura_", fig_num, "_Pred_vs_Obs.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Predicciones vs Observaciones\n")

    fig_num <- fig_num + 1
    plots$residuals <- .plot_residuals(x)
    figures_catalog$residuals <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Analisis de Residuos",
      description = "Residuos vs valores predichos",
      filename = paste0("Figura_", fig_num, "_Residuos.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Analisis de residuos\n")

    if (!is.null(x$residual_analysis)) {
      fig_num <- fig_num + 1
      plots$residual_diagnostics <- plot_residual_diagnostics(x$residual_analysis)
      figures_catalog$residual_diagnostics <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Diagnosticos de Residuos",
        description = "Panel de diagnosticos avanzados",
        filename = paste0("Figura_", fig_num, "_Residuos_Diag.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Diagnosticos de residuos\n")
    }
  }

  # 4. Tuning (si aplica)
  if (!is.null(x$tuning) && x$tuning$tuned && !is.null(x$tuning$tune_results)) {
    fig_num <- fig_num + 1
    plots$tuning <- plot_tuning(x$tuning)
    figures_catalog$tuning <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Resultados de Tuning",
      description = "Optimizacion de hiperparametros",
      filename = paste0("Figura_", fig_num, "_Tuning.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Resultados de tuning\n")
  }

  # 5. SHAP (si aplica)
  if (!is.null(x$interpretation$shap)) {
    fig_num <- fig_num + 1
    interpret_obj <- list(
      importance = x$importance,
      shap = x$interpretation$shap
    )
    class(interpret_obj) <- c("easyml_interpret", "list")
    plots$shap_summary <- plot_shap_summary(interpret_obj, top_n = 15)
    figures_catalog$shap_summary <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "SHAP Summary Plot",
      description = "Contribucion de variables a predicciones individuales",
      filename = paste0("Figura_", fig_num, "_SHAP.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": SHAP summary\n")
  }

  if (verbose) {
    cat("      Total:", length(plots), "graficos generados\n")
  }

  # Retornar estructura con plots y catalogo
  list(
    plots = plots,
    figures_catalog = figures_catalog,
    total_figures = fig_num
  )
}


#' @title Graficar resultados de easyML
#'
#' @description
#' Genera visualizaciones de los resultados del analisis ML.
#'
#' @param x Objeto easyml
#' @param type Tipo de grafico: "panel", "all", "importance", "metrics",
#'             "confusion", "roc", "residuals", "pred_vs_obs", "tuning",
#'             "threshold", "calibration"
#' @param top_n Numero de variables a mostrar en importancia (default: 15)
#' @param ... Argumentos adicionales
#'
#' @return Objeto ggplot o lista de graficos
#'
#' @examples
#' \dontrun{
#' result <- easy_ml(data, target = "outcome")
#' plot(result)                    # Panel combinado
#' plot(result, type = "all")      # Lista con todos los graficos
#' plot(result, type = "importance")
#' plot(result, type = "roc")
#' }
#'
#' @export
plot.easyml <- function(x, type = "panel", top_n = 15, ...) {

  # Obtener plots - ahora pueden estar en estructura nueva o antigua
  plots_data <- x$plots
  if (is.null(plots_data) || length(plots_data) == 0) {
    plots_data <- generate_all_plots(x, verbose = FALSE)
  }

  # Extraer plots si estan en estructura nueva
  if (!is.null(plots_data$plots)) {
    actual_plots <- plots_data$plots
  } else {
    actual_plots <- plots_data
  }

  # Panel combinado
  if (type == "panel") {
    return(plot_all.easyml(x))
  }

  # Todos los graficos
  if (type == "all") {
    return(actual_plots)
  }

  # Graficos individuales por nombre
  plot_map <- list(
    importance = "importance",
    metrics = "model_comparison",
    comparison = "model_comparison",
    roc = "roc_curve",
    confusion = "confusion_matrix",
    calibration = "calibration",
    calibration_curve = "calibration_curve",
    pred_vs_obs = "pred_vs_obs",
    residuals = "residuals",
    residual_diagnostics = "residual_diagnostics",
    tuning = "tuning",
    threshold = "threshold_optimization",
    shap = "shap_summary"
  )

  if (type %in% names(plot_map)) {
    plot_name <- plot_map[[type]]
    if (!is.null(actual_plots[[plot_name]])) {
      return(actual_plots[[plot_name]])
    }
  }

  # Fallback a funciones originales para compatibilidad
  if (type == "importance") {
    return(.plot_importance(x, top_n))
  }

  if (type == "metrics") {
    return(.plot_metrics(x))
  }

  if (type == "confusion" && x$task == "classification") {
    return(.plot_confusion(x))
  }

  if (type == "roc" && x$task == "classification") {
    return(.plot_roc(x))
  }

  if (type == "residuals" && x$task == "regression") {
    return(.plot_residuals(x))
  }

  if (type == "pred_vs_obs" && x$task == "regression") {
    return(.plot_pred_vs_obs(x))
  }

  message("Tipo de grafico no disponible. Opciones: panel, all, importance, metrics, roc, confusion, calibration, pred_vs_obs, residuals, tuning, threshold, shap")
  return(NULL)
}


#' @noRd
.plot_importance <- function(x, top_n = 15) {

  imp <- utils::head(x$importance, top_n)

  p <- ggplot2::ggplot(imp,
                       ggplot2::aes(x = stats::reorder(Variable, Importance),
                                    y = Importance)) +
    ggplot2::geom_col(fill = "#3498db", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Importancia de Variables",
      subtitle = paste("Modelo:", x$best_model),
      x = "",
      y = "Importancia"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text = ggplot2::element_text(size = 10)
    )

  return(p)
}


#' @noRd
.plot_metrics <- function(x) {

  cv_data <- x$cv_summary

  if (x$task == "classification") {
    metric_col <- "roc_auc"
    metric_label <- "ROC-AUC"
  } else {
    metric_col <- "rmse"
    metric_label <- "RMSE"
  }

  p <- ggplot2::ggplot(cv_data,
                       ggplot2::aes(x = stats::reorder(model, -.data[[metric_col]]),
                                    y = .data[[metric_col]])) +
    ggplot2::geom_col(fill = "#2ecc71", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data[[metric_col]], 3)),
                       vjust = -0.5, size = 4) +
    ggplot2::labs(
      title = "Comparacion de Modelos (CV)",
      x = "Modelo",
      y = metric_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}


#' @noRd
.plot_confusion <- function(x) {

  target <- x$target
  preds <- x$predictions

  conf_mat <- yardstick::conf_mat(preds,
                                  truth = !!rlang::sym(target),
                                  estimate = .pred_class)

  p <- ggplot2::autoplot(conf_mat, type = "heatmap") +
    ggplot2::labs(
      title = "Matriz de Confusion",
      subtitle = paste("Modelo:", x$best_model)
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' @noRd
.plot_roc <- function(x) {

  target <- x$target
  preds <- x$predictions

  event_info <- .detect_event_level(preds[[target]])

  roc_data <- yardstick::roc_curve(preds,
                                   truth = !!rlang::sym(target),
                                   !!rlang::sym(event_info$prob_col),
                                   event_level = event_info$event_level)

  auc_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "roc_auc"]

  p <- suppressWarnings(
    ggplot2::autoplot(roc_data) +
      ggplot2::labs(
        title = "Curva ROC",
        subtitle = paste("AUC =", round(auc_val, 3), "| Clase positiva:", event_info$positive_class)
      ) +
      ggplot2::theme_minimal()
  )

  return(p)
}


#' @noRd
.plot_residuals <- function(x) {

  preds <- x$predictions
  target <- x$target

  preds$residuals <- preds[[target]] - preds$.pred

  p <- ggplot2::ggplot(preds, ggplot2::aes(x = .pred, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#e74c3c") +
    ggplot2::labs(
      title = "Analisis de Residuos",
      subtitle = paste("Modelo:", x$best_model),
      x = "Valores Predichos",
      y = "Residuos"
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' @noRd
.plot_pred_vs_obs <- function(x) {

  preds <- x$predictions
  target <- x$target

  r2_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "rsq"]

  p <- ggplot2::ggplot(preds, ggplot2::aes(x = .data[[target]], y = .pred)) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "red", linewidth = 1) +
    ggplot2::labs(
      title = "Predicciones vs Observaciones",
      subtitle = paste("R2 =", round(r2_val, 3)),
      x = "Valores Observados",
      y = "Valores Predichos"
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' @title Graficar todos los resultados
#'
#' @description
#' Genera un panel con todos los graficos relevantes.
#'
#' @param x Objeto easyml
#' @param ... Argumentos adicionales
#'
#' @return Panel de graficos
#' @export
plot_all <- function(x, ...) {
  UseMethod("plot_all")
}


#' @export
plot_all.easyml <- function(x, ...) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Instalar paquete patchwork: install.packages(patchwork)")
  }

  p1 <- .plot_importance(x, top_n = 10)
  p2 <- .plot_metrics(x)

  if (x$task == "classification") {
    p3 <- .plot_confusion(x)
    p4 <- .plot_roc(x)
  } else {
    p3 <- .plot_pred_vs_obs(x)
    p4 <- .plot_residuals(x)
  }

  combined <- (p1 | p2) / (p3 | p4) +
    patchwork::plot_annotation(
      title = paste("Resumen de", ifelse(x$task == "classification", "Clasificacion", "Regresion")),
      subtitle = paste("Mejor modelo:", .get_model_label(x$best_model)),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11)
      )
    )

  return(combined)
}


#' @title Grafico de Calibracion por Deciles
#' @noRd
.plot_calibration_deciles <- function(calibration_data) {

  if (is.null(calibration_data)) return(NULL)

  calibration_data <- calibration_data[!is.na(calibration_data$mean_pred), ]

  ggplot2::ggplot(calibration_data, ggplot2::aes(x = mean_pred, y = mean_obs)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(ggplot2::aes(size = n), color = "#9b59b6", alpha = 0.7) +
    ggplot2::geom_line(color = "#9b59b6", alpha = 0.5) +
    ggplot2::labs(
      title = "Calibracion del Modelo",
      subtitle = "Probabilidad predicha vs proporcion observada",
      x = "Probabilidad Predicha Promedio",
      y = "Proporcion Observada",
      size = "N obs"
    ) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}


#' @title Guardar Todos los Graficos
#'
#' @description
#' Guarda todos los graficos de un objeto easyml en archivos PNG con nombres
#' descriptivos (Figura_1_Importancia_Variables.png, etc.).
#'
#' @param x Objeto easyml
#' @param path Directorio donde guardar
#' @param prefix Prefijo para los nombres de archivo (opcional)
#' @param width Ancho en pulgadas
#' @param height Alto en pulgadas
#' @param dpi Resolucion
#'
#' @return Lista con catalogo de figuras guardadas (invisible)
#' @export
save_all_plots <- function(x, path = ".", prefix = NULL, width = 8, height = 6, dpi = 300) {

  plots_data <- x$plots
  if (is.null(plots_data) || length(plots_data) == 0) {
    plots_data <- generate_all_plots(x, verbose = FALSE)
  }

  if (!is.null(plots_data$plots)) {
    actual_plots <- plots_data$plots
    figures_catalog <- plots_data$figures_catalog
  } else {
    actual_plots <- plots_data
    figures_catalog <- NULL
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  saved_files <- list()

  for (name in names(actual_plots)) {
    p <- actual_plots[[name]]
    if (!is.null(p) && (inherits(p, "ggplot") || inherits(p, "patchwork"))) {

      if (!is.null(figures_catalog) && !is.null(figures_catalog[[name]])) {
        filename_base <- figures_catalog[[name]]$filename
      } else {
        filename_base <- paste0("plot_", name, ".png")
      }

      if (!is.null(prefix)) {
        filename_base <- paste0(prefix, "_", filename_base)
      }

      filename <- file.path(path, filename_base)

      tryCatch({
        ggplot2::ggsave(filename, p, width = width, height = height, dpi = dpi)
        saved_files[[name]] <- list(
          filename = basename(filename),
          full_path = normalizePath(filename, mustWork = FALSE),
          catalog = figures_catalog[[name]]
        )
      }, error = function(e) {
        message("No se pudo guardar: ", name)
      })
    }
  }

  tryCatch({
    panel <- plot_all.easyml(x)
    if (!is.null(panel)) {
      panel_name <- ifelse(is.null(prefix),
                           "Figura_0_Panel_Resumen.png",
                           paste0(prefix, "_Figura_0_Panel_Resumen.png"))
      filename <- file.path(path, panel_name)
      ggplot2::ggsave(filename, panel, width = 12, height = 10, dpi = dpi)
      saved_files$panel <- list(
        filename = basename(filename),
        full_path = normalizePath(filename, mustWork = FALSE),
        catalog = list(number = 0, title = "Panel Resumen", type = "figura")
      )
    }
  }, error = function(e) {
    message("No se pudo guardar panel combinado")
  })

  cat("\n============================================================\n")
  cat(" CATALOGO DE FIGURAS GUARDADAS\n")
  cat("============================================================\n")
  cat("Directorio:", normalizePath(path), "\n\n")

  for (name in names(saved_files)) {
    info <- saved_files[[name]]
    if (!is.null(info$catalog)) {
      fig_id <- if (!is.null(info$catalog$id)) info$catalog$id else "Figura"
      fig_title <- if (!is.null(info$catalog$title)) info$catalog$title else name
      cat(sprintf("  %s - %s\n", fig_id, fig_title))
      cat(sprintf("    Archivo: %s\n", info$filename))
      if (!is.null(info$catalog$description)) {
        cat(sprintf("    Descripcion: %s\n", info$catalog$description))
      }
      cat("\n")
    }
  }

  cat("Total archivos:", length(saved_files), "\n")
  cat("============================================================\n")

  invisible(saved_files)
}


#' @title Obtener Catalogo de Figuras
#'
#' @description
#' Devuelve el catalogo de figuras generadas por easy_ml().
#'
#' @param x Objeto easyml
#'
#' @return Data frame con el catalogo de figuras
#' @export
get_figures_catalog <- function(x) {

  plots_data <- x$plots
  if (is.null(plots_data)) {
    message("No hay graficos generados.")
    return(NULL)
  }

  if (!is.null(plots_data$figures_catalog)) {
    catalog <- plots_data$figures_catalog
  } else {
    message("No hay catalogo de figuras.")
    return(NULL)
  }

  catalog_df <- do.call(rbind, lapply(names(catalog), function(name) {
    item <- catalog[[name]]
    data.frame(
      key = name,
      number = item$number,
      id = item$id,
      title = item$title,
      description = item$description,
      filename = item$filename,
      stringsAsFactors = FALSE
    )
  }))

  catalog_df <- catalog_df[order(catalog_df$number), ]
  rownames(catalog_df) <- NULL

  catalog_df
}


#' @title Imprimir Catalogo de Figuras
#'
#' @description
#' Imprime el catalogo de figuras para el usuario.
#'
#' @param x Objeto easyml
#'
#' @return NULL (invisible)
#' @export
print_figures_catalog <- function(x) {

  catalog <- get_figures_catalog(x)

  if (is.null(catalog)) return(invisible(NULL))

  cat("\n============================================================\n")
  cat(" CATALOGO DE FIGURAS\n")
  cat("============================================================\n\n")

  for (i in seq_len(nrow(catalog))) {
    row <- catalog[i, ]
    cat(sprintf("%s: %s\n", row$id, row$title))
    cat(sprintf("  Descripcion: %s\n", row$description))
    cat(sprintf("  Archivo: %s\n", row$filename))
    cat(sprintf("  Acceso: resultado$figures$%s\n\n", row$key))
  }

  cat("============================================================\n")
  cat("Uso en reportes:\n")
  cat("  [Insertar Figura 1 - Importancia de Variables]\n")
  cat("  [Insertar Figura 2 - Comparacion de Modelos]\n")
  cat("============================================================\n")

  invisible(NULL)
}


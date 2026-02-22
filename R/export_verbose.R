# =============================================================================
# Funciones de Exportacion del Verbose para easyML
# =============================================================================
# Permite exportar el output verbose como TXT y JSON para generar reportes
# =============================================================================

#' @title Capturar Verbose Output
#'
#' @description
#' Ejecuta easy_ml capturando todo el output verbose para exportacion.
#'
#' @param ... Argumentos para easy_ml()
#'
#' @return Lista con resultados y verbose capturado
#' @export
easy_ml_capture <- function(...) {


  # Capturar el output
  verbose_output <- capture.output({
    result <- easy_ml(..., verbose = TRUE)
  }, type = "output")

  # Agregar verbose al resultado
  result$verbose_text <- paste(verbose_output, collapse = "\n")
  result$verbose_lines <- verbose_output

  # Crear estructura JSON
  result$verbose_json <- .parse_verbose_to_json(verbose_output, result)


  return(result)
}


#' @title Exportar Verbose como TXT
#'
#' @description
#' Exporta el resultado de easyML como archivo de texto.
#' Ahora easy_ml() captura automaticamente el verbose, por lo que
#' esta funcion exporta EXACTAMENTE el mismo output que aparece en consola.
#'
#' @param result Resultado de easy_ml() o easy_ml_capture()
#' @param file_path Ruta del archivo de salida
#'
#' @return Invisiblemente, la ruta del archivo
#' @export
export_verbose_txt <- function(result, file_path = "easyml_report.txt") {

  # Verificar que tenga verbose_text (easy_ml() ahora lo captura automaticamente)
  if (!is.null(result$verbose_text) && nzchar(result$verbose_text)) {
    txt_content <- result$verbose_text
  } else {
    # Fallback: construir desde el resultado si no tiene verbose capturado
    # Esto ocurre si se ejecuto con verbose = FALSE
    message("Nota: El analisis se ejecuto con verbose=FALSE.")
    message("Construyendo resumen desde los resultados...")
    txt_content <- .build_txt_from_result(result)
  }

  writeLines(txt_content, file_path)
  message("TXT exportado a: ", file_path)

  invisible(file_path)
}


#' @title Construir TXT desde Resultado de easy_ml
#' @noRd
.build_txt_from_result <- function(result) {

  lines <- c()
  sep_line <- paste(rep("=", 60), collapse = "")
  dash_line <- paste(rep("-", 50), collapse = "")

  # Obtener datos basicos
 n_train <- if (!is.null(result$train_data)) nrow(result$train_data) else 0
  n_test <- if (!is.null(result$test_data)) nrow(result$test_data) else 0
  n_total <- n_train + n_test

  # ============================================================
  # HEADER
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, " easyML - Machine Learning Automatizado")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")
  lines <- c(lines, sprintf("Observaciones: %d", n_total))
  lines <- c(lines, sprintf("Variable objetivo: %s", result$target))
  lines <- c(lines, sprintf("Tarea: %s", result$task))
  lines <- c(lines, sprintf("Mejor modelo: %s", result$best_model))
  lines <- c(lines, "")

  # ============================================================
  # 1. INFORMACION DEL DATASET
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "1. Informacion del Dataset")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "1.1. Estructura de los Datos")
  lines <- c(lines, dash_line)
  lines <- c(lines, sprintf("    Observaciones totales: %d", n_total))
  lines <- c(lines, sprintf("    Conjunto de entrenamiento: %d (%.1f%%)", n_train, n_train/n_total*100))
  lines <- c(lines, sprintf("    Conjunto de prueba: %d (%.1f%%)", n_test, n_test/n_total*100))
  lines <- c(lines, "")

  # Distribucion de la variable objetivo
  if (result$task == "classification" && !is.null(result$test_data)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "1.2. Distribucion de la Variable Objetivo")
    lines <- c(lines, dash_line)

    # Calcular distribucion en train
    if (!is.null(result$train_data) && result$target %in% names(result$train_data)) {
      target_table <- table(result$train_data[[result$target]])
      target_props <- prop.table(target_table) * 100
      lines <- c(lines, "    Distribucion en Train:")
      for (i in seq_along(target_table)) {
        lines <- c(lines, sprintf("      - %s: %d (%.1f%%)",
                                  names(target_table)[i], target_table[i], target_props[i]))
      }
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "    Referencia: Hastie, T., Tibshirani, R., & Friedman, J. (2009).")
  lines <- c(lines, "    The elements of statistical learning (2nd ed.). Springer.")
  lines <- c(lines, "")

  # ============================================================
  # 2. PREPROCESAMIENTO
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "2. Preprocesamiento de Datos")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "2.1. Division Train/Test")
  lines <- c(lines, dash_line)
  lines <- c(lines, sprintf("    Proporcion test: %.0f%%", n_test/n_total*100))
  lines <- c(lines, sprintf("    Train: %d observaciones", n_train))
  lines <- c(lines, sprintf("    Test: %d observaciones", n_test))
  lines <- c(lines, "")

  # Feature selection si existe
  if (!is.null(result$selected_features)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "2.2. Variables Seleccionadas")
    lines <- c(lines, dash_line)
    lines <- c(lines, sprintf("    Variables utilizadas: %d", length(result$selected_features)))
    for (feat in result$selected_features) {
      lines <- c(lines, sprintf("      - %s", feat))
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Kursa, M. B., & Rudnicki, W. R. (2010).")
    lines <- c(lines, "    Feature selection with the Boruta package. JSS, 36(11), 1-13.")
    lines <- c(lines, "")
  }

  lines <- c(lines, dash_line)
  lines <- c(lines, "2.3. Receta de Preprocesamiento")
  lines <- c(lines, dash_line)
  lines <- c(lines, "    Pasos aplicados automaticamente:")
  lines <- c(lines, "      - Imputacion con mediana (variables numericas)")
  lines <- c(lines, "      - Imputacion con moda (variables categoricas)")
  lines <- c(lines, "      - Dummy encoding (variables categoricas)")
  lines <- c(lines, "      - Normalizacion z-score (si aplica)")
  lines <- c(lines, "")
  lines <- c(lines, "    Nota: Estos pasos se aplican dentro de la validacion cruzada")
  lines <- c(lines, "    para evitar data leakage.")
  lines <- c(lines, "")

  # ============================================================
  # 3. MODELADO
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "3. Modelado")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "3.1. Validacion Cruzada")
  lines <- c(lines, dash_line)
  lines <- c(lines, "    Metodo: K-Fold Cross-Validation Estratificado")
  lines <- c(lines, "    Folds: 10")
  lines <- c(lines, sprintf("    Estratificado por: %s", result$target))
  lines <- c(lines, "")
  lines <- c(lines, "    Referencia: Kohavi, R. (1995). A study of cross-validation")
  lines <- c(lines, "    and bootstrap for accuracy estimation. IJCAI, 14(2), 1137-1145.")
  lines <- c(lines, "")

  # Comparacion de modelos
  if (!is.null(result$model_comparison)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "3.2. Comparacion de Modelos")
    lines <- c(lines, dash_line)
    lines <- c(lines, "    Modelos evaluados en validacion cruzada:")
    lines <- c(lines, "")

    comp <- result$model_comparison
    if (is.data.frame(comp)) {
      # Header de tabla
      lines <- c(lines, sprintf("    %-12s %8s %8s %8s",
                                "Modelo", "ROC_AUC", "Accuracy", "F1"))
      lines <- c(lines, paste("    ", paste(rep("-", 40), collapse = ""), sep = ""))

      for (i in 1:nrow(comp)) {
        model_name <- if ("model" %in% names(comp)) comp$model[i] else rownames(comp)[i]
        auc_val <- if ("roc_auc" %in% names(comp)) comp$roc_auc[i] else NA
        acc_val <- if ("accuracy" %in% names(comp)) comp$accuracy[i] else NA
        f1_val <- if ("f_meas" %in% names(comp)) comp$f_meas[i] else NA

        lines <- c(lines, sprintf("    %-12s %8.4f %8.4f %8.4f",
                                  model_name,
                                  ifelse(is.na(auc_val), 0, auc_val),
                                  ifelse(is.na(acc_val), 0, acc_val),
                                  ifelse(is.na(f1_val), 0, f1_val)))
      }
    }
    lines <- c(lines, "")
    lines <- c(lines, sprintf("    Mejor modelo seleccionado: %s", result$best_model))
    lines <- c(lines, "")
  }

  # ============================================================
  # 4. TUNING DE HIPERPARAMETROS
  # ============================================================
  if (!is.null(result$best_params)) {
    lines <- c(lines, sep_line)
    lines <- c(lines, "4. Tuning de Hiperparametros")
    lines <- c(lines, sep_line)
    lines <- c(lines, "")

    lines <- c(lines, dash_line)
    lines <- c(lines, "4.1. Mejores Hiperparametros Encontrados")
    lines <- c(lines, dash_line)
    lines <- c(lines, sprintf("    Modelo: %s", result$best_model))
    lines <- c(lines, "    Metodo: Random Search con validacion cruzada")
    lines <- c(lines, "")
    lines <- c(lines, "    Valores optimizados:")
    for (param_name in names(result$best_params)) {
      param_val <- result$best_params[[param_name]]
      lines <- c(lines, sprintf("      - %s: %s", param_name, param_val))
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Bergstra, J., & Bengio, Y. (2012).")
    lines <- c(lines, "    Random search for hyper-parameter optimization. JMLR, 13, 281-305.")
    lines <- c(lines, "")
  }

  # ============================================================
  # 5. EVALUACION EN TEST
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "5. Evaluacion en Test")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "5.1. Metricas de Rendimiento")
  lines <- c(lines, dash_line)
  lines <- c(lines, "    El conjunto de test contiene datos que el modelo NUNCA vio")
  lines <- c(lines, "    durante el entrenamiento.")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("    Predicciones evaluadas: %d observaciones", n_test))
  lines <- c(lines, "")

  if (!is.null(result$test_metrics)) {
    metrics_df <- result$test_metrics
    lines <- c(lines, "    Metricas en Test:")

    for (i in 1:nrow(metrics_df)) {
      metric_name <- metrics_df$.metric[i]
      metric_value <- metrics_df$.estimate[i]

      # Formatear e interpretar
      if (metric_name == "roc_auc") {
        interp <- ifelse(metric_value >= 0.9, "Excelente",
                         ifelse(metric_value >= 0.8, "Bueno",
                                ifelse(metric_value >= 0.7, "Aceptable", "Pobre")))
        lines <- c(lines, sprintf("      - ROC_AUC: %.4f (%s)", metric_value, interp))
      } else if (metric_name %in% c("accuracy", "sensitivity", "specificity")) {
        lines <- c(lines, sprintf("      - %s: %.2f%%", toupper(metric_name), metric_value * 100))
      } else {
        lines <- c(lines, sprintf("      - %s: %.4f", toupper(metric_name), metric_value))
      }
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "    Leyenda:")
  lines <- c(lines, "      - ROC_AUC: Capacidad de distinguir entre clases (0.5=azar, 1=perfecto)")
  lines <- c(lines, "      - Accuracy: Porcentaje de predicciones correctas")
  lines <- c(lines, "      - Sensitivity: Porcentaje de positivos detectados correctamente")
  lines <- c(lines, "      - Specificity: Porcentaje de negativos detectados correctamente")
  lines <- c(lines, "")
  lines <- c(lines, "    Referencia: Hanley, J. A., & McNeil, B. J. (1982).")
  lines <- c(lines, "    The meaning and use of the area under a ROC curve. Radiology, 143(1), 29-36.")
  lines <- c(lines, "")

  # Matriz de confusion
  if (!is.null(result$confusion_matrix)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "5.2. Matriz de Confusion")
    lines <- c(lines, dash_line)
    lines <- c(lines, "")

    cm <- result$confusion_matrix
    if (is.matrix(cm) || is.table(cm)) {
      lines <- c(lines, "              Prediccion")
      lines <- c(lines, sprintf("    Truth     %-8s %-8s", colnames(cm)[1], colnames(cm)[2]))
      lines <- c(lines, sprintf("    %-8s  %-8d %-8d", rownames(cm)[1], cm[1,1], cm[1,2]))
      lines <- c(lines, sprintf("    %-8s  %-8d %-8d", rownames(cm)[2], cm[2,1], cm[2,2]))
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Como leer la matriz:")
    lines <- c(lines, "      - Diagonal: Predicciones correctas")
    lines <- c(lines, "      - Fuera de diagonal: Errores de clasificacion")
    lines <- c(lines, "")
  }

  # Threshold optimizado
  if (!is.null(result$optimal_threshold)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "5.3. Optimizacion de Threshold")
    lines <- c(lines, dash_line)
    lines <- c(lines, "    Metodo: Youden's J (maximiza sensitivity + specificity - 1)")
    lines <- c(lines, "")
    lines <- c(lines, "    Resultado:")
    lines <- c(lines, "      - Threshold por defecto: 0.50")
    lines <- c(lines, sprintf("      - Threshold optimo: %.4f", result$optimal_threshold))
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Youden, W. J. (1950).")
    lines <- c(lines, "    Index for rating diagnostic tests. Cancer, 3(1), 32-35.")
    lines <- c(lines, "")
  }

  # ============================================================
  # 6. IMPORTANCIA DE VARIABLES
  # ============================================================
  if (!is.null(result$importance) && nrow(result$importance) > 0) {
    lines <- c(lines, sep_line)
    lines <- c(lines, "6. Importancia de Variables")
    lines <- c(lines, sep_line)
    lines <- c(lines, "")

    lines <- c(lines, dash_line)
    lines <- c(lines, "6.1. Variables mas Importantes")
    lines <- c(lines, dash_line)
    lines <- c(lines, "    La importancia indica cuanto contribuye cada variable")
    lines <- c(lines, "    a las predicciones del modelo.")
    lines <- c(lines, "")

    top_vars <- utils::head(result$importance, 10)
    lines <- c(lines, "    Top 10 variables:")
    for (i in 1:nrow(top_vars)) {
      # Barra visual
      bar_length <- round(top_vars$Importance[i] / max(top_vars$Importance) * 20)
      bar <- paste(rep("|", bar_length), collapse = "")
      lines <- c(lines, sprintf("      %2d. %-30s %.4f %s",
                                i, top_vars$Variable[i], top_vars$Importance[i], bar))
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Breiman, L. (2001). Random forests.")
    lines <- c(lines, "    Machine Learning, 45(1), 5-32.")
    lines <- c(lines, "")
  }

  # ============================================================
  # CONCLUSION
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, " Analisis Completado")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  # Obtener AUC para conclusion
  auc_value <- NA
  if (!is.null(result$test_metrics)) {
    auc_row <- result$test_metrics[result$test_metrics$.metric == "roc_auc", ]
    if (nrow(auc_row) > 0) auc_value <- auc_row$.estimate[1]
  }

  lines <- c(lines, sprintf("Mejor modelo: %s", result$best_model))
  if (!is.na(auc_value)) {
    lines <- c(lines, sprintf("ROC-AUC en test: %.4f", auc_value))
    interp <- ifelse(auc_value >= 0.9, "excelente",
                     ifelse(auc_value >= 0.8, "buena",
                            ifelse(auc_value >= 0.7, "aceptable", "limitada")))
    lines <- c(lines, sprintf("Interpretacion: El modelo tiene capacidad %s para", interp))
    lines <- c(lines, sprintf("distinguir entre las clases de '%s'.", result$target))
  }
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "CONCLUSION")
  lines <- c(lines, dash_line)
  lines <- c(lines, sprintf("Se entreno un modelo de %s para predecir", .get_model_label(result$best_model)))
  lines <- c(lines, sprintf("'%s'. El modelo fue evaluado en %d observaciones nuevas", result$target, n_test))
  lines <- c(lines, "(que no uso durante el entrenamiento).")
  lines <- c(lines, "")
  lines <- c(lines, "Para usar el modelo:")
  lines <- c(lines, "  - Ver resultados: print(resultado)")
  lines <- c(lines, "  - Ver graficos: plot(resultado)")
  lines <- c(lines, "  - Hacer predicciones: predict(resultado, nuevos_datos)")
  lines <- c(lines, "")

  # Footer
  lines <- c(lines, sep_line)
  lines <- c(lines, sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  lines <- c(lines, "easyML v2.0.0 - github.com/jventural/easyML")
  lines <- c(lines, sep_line)

  paste(lines, collapse = "\n")
}


#' @title Exportar Verbose como JSON
#'
#' @description
#' Exporta el resultado de easyML como archivo JSON estructurado.
#' Ahora easy_ml() captura automaticamente el verbose, por lo que
#' el JSON incluye el verbose completo que aparecio en consola.
#'
#' @param result Resultado de easy_ml() o easy_ml_capture()
#' @param file_path Ruta del archivo de salida
#'
#' @return Invisiblemente, la ruta del archivo
#' @export
export_verbose_json <- function(result, file_path = "easyml_report.json") {

  # Construir estructura JSON desde el resultado
  json_structure <- .build_json_from_result(result)

  # Agregar el verbose_text capturado (identico al de consola)
  if (!is.null(result$verbose_text) && nzchar(result$verbose_text)) {
    json_structure$verbose_text <- result$verbose_text
    json_structure$verbose_lines <- result$verbose_lines
  } else {
    # Si no hay verbose capturado (ejecutado con verbose=FALSE)
    message("Nota: El analisis se ejecuto con verbose=FALSE.")
    message("El JSON no incluira el verbose completo.")
  }

  json_content <- jsonlite::toJSON(json_structure, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, file_path)
  message("JSON exportado a: ", file_path)

  invisible(file_path)
}


#' @title Construir JSON desde Resultado de easy_ml
#' @noRd
.build_json_from_result <- function(result) {


  # Estructura base del JSON
  meta <- result$metadata
  json_structure <- list(
    metadata = list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      easyml_version = "2.0.0",
      task = result$task,
      target = result$target,
      best_model = result$best_model,
      n_observations = if (!is.null(meta)) meta$n_obs else
        (nrow(result$train_data) + nrow(result$test_data)),
      n_train = if (!is.null(meta)) meta$n_train else nrow(result$train_data),
      n_test = if (!is.null(meta)) meta$n_test else nrow(result$test_data),
      n_features = if (!is.null(meta)) meta$n_features else (ncol(result$train_data) - 1),
      models_trained = if (!is.null(meta)) meta$models_trained else NULL,
      cv_folds = if (!is.null(meta)) meta$cv_folds else NULL,
      tune_method = if (!is.null(meta) && !is.na(meta$tune_method)) meta$tune_method else NULL,
      seed = if (!is.null(meta)) meta$seed else NULL,
      elapsed_time = if (!is.null(meta)) as.numeric(meta$elapsed_time) else NULL
    ),
    sections = list(),
    metrics = list(),
    references = list()
  )

  # Agregar metricas
  if (!is.null(result$test_metrics)) {
    metrics_df <- result$test_metrics
    for (i in 1:nrow(metrics_df)) {
      metric_name <- metrics_df$.metric[i]
      metric_value <- metrics_df$.estimate[i]
      json_structure$metrics[[metric_name]] <- round(metric_value, 4)
    }
  }

  # Agregar informacion del modelo
  json_structure$model_info <- list(
    name = result$best_model,
    label = .get_model_label(result$best_model),
    tuned = !is.null(result$best_params),
    hyperparameters = result$best_params
  )

  # Agregar importancia de variables
  if (!is.null(result$importance) && nrow(result$importance) > 0) {
    top_vars <- utils::head(result$importance, 10)
    json_structure$variable_importance <- lapply(1:nrow(top_vars), function(i) {
      list(
        variable = top_vars$Variable[i],
        importance = round(top_vars$Importance[i], 4),
        rank = i
      )
    })
  }

  # Agregar threshold si existe
  if (!is.null(result$optimal_threshold)) {
    json_structure$threshold <- list(
      default = 0.5,
      optimized = round(result$optimal_threshold, 4)
    )
  }

  # Agregar comparacion de modelos (CV summary)
  if (!is.null(result$cv_summary) && is.data.frame(result$cv_summary)) {
    json_structure$model_comparison <- tryCatch({
      comp <- result$cv_summary
      lapply(1:nrow(comp), function(i) {
        row <- as.list(comp[i, ])
        row[] <- lapply(row, function(v) if (is.numeric(v)) round(v, 4) else v)
        row
      })
    }, error = function(e) NULL)
  }

  # Agregar confusion matrix si existe
  if (!is.null(result$evaluation$confusion)) {
    json_structure$confusion_matrix <- tryCatch({
      cm <- result$evaluation$confusion
      if (is.matrix(cm) || is.table(cm)) {
        list(
          matrix = as.list(as.data.frame(unclass(cm))),
          row_names = rownames(cm),
          col_names = colnames(cm)
        )
      } else NULL
    }, error = function(e) NULL)
  }

  # Agregar calibracion si existe
  if (!is.null(result$calibration)) {
    json_structure$calibration <- tryCatch({
      list(method = result$calibration$method %||% "unknown")
    }, error = function(e) NULL)
  }

  # Agregar catalogo de figuras
  if (!is.null(result$figures_catalog)) {
    json_structure$figures_catalog <- lapply(names(result$figures_catalog), function(name) {
      item <- result$figures_catalog[[name]]
      list(
        key = name,
        number = item$number,
        id = item$id,
        title = item$title,
        description = item$description,
        filename = item$filename
      )
    })
  }

  # Agregar referencias estandar
  json_structure$references <- c(
    "Tukey, J. W. (1977). Exploratory data analysis. Addison-Wesley.",
    "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.",
    "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a ROC curve. Radiology, 143(1), 29-36.",
    "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation. IJCAI, 14(2), 1137-1145.",
    "Kuhn, M., & Wickham, H. (2020). Tidymodels: A collection of packages for modeling and machine learning.",
    "Hastie, T., Tibshirani, R., & Friedman, J. (2009). The elements of statistical learning (2nd ed.). Springer."
  )

  # Agregar referencia especifica del modelo
 if (result$best_model == "xgboost") {
    json_structure$references <- c(json_structure$references,
      "Chen, T., & Guestrin, C. (2016). XGBoost: A scalable tree boosting system. KDD, 785-794.")
  }

  return(json_structure)
}


#' @title Parsear Verbose a Estructura JSON
#' @noRd
.parse_verbose_to_json <- function(verbose_lines, result) {

  # Estructura base del JSON
  json_structure <- list(
    metadata = list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      easyml_version = "2.0.0",
      task = result$task,
      target = result$target,
      best_model = result$best_model,
      n_observations = nrow(result$train_data) + nrow(result$test_data),
      n_train = nrow(result$train_data),
      n_test = nrow(result$test_data)
    ),
    sections = list(),
    metrics = list(),
    references = list()
  )

  # Parsear secciones del verbose
  current_section <- NULL
  current_subsection <- NULL
  current_content <- c()

  for (line in verbose_lines) {
    # Detectar secciones principales (ej: "1. Carga y Exploracion")
    if (grepl("^[0-9]+\\. ", line) && !grepl("^[0-9]+\\.[0-9]+", line)) {
      # Guardar seccion anterior
      if (!is.null(current_section)) {
        json_structure$sections[[current_section]]$content <- paste(current_content, collapse = "\n")
      }

      current_section <- gsub("^([0-9]+)\\. (.+)$", "\\1_\\2", line)
      current_section <- gsub(" ", "_", current_section)
      current_section <- gsub("[^a-zA-Z0-9_]", "", current_section)

      json_structure$sections[[current_section]] <- list(
        title = trimws(line),
        subsections = list(),
        content = ""
      )
      current_content <- c()
      current_subsection <- NULL
    }
    # Detectar subsecciones (ej: "1.2. Analisis Exploratorio")
    else if (grepl("^[0-9]+\\.[0-9]+\\.?\\s", line)) {
      if (!is.null(current_section)) {
        subsec_name <- gsub("^([0-9]+\\.[0-9]+)\\.?\\s+(.+)$", "\\1_\\2", line)
        subsec_name <- gsub(" ", "_", subsec_name)
        subsec_name <- gsub("[^a-zA-Z0-9_.]", "", subsec_name)

        json_structure$sections[[current_section]]$subsections[[subsec_name]] <- list(
          title = trimws(line),
          content = ""
        )
        current_subsection <- subsec_name
      }
    }
    # Detectar referencias
    else if (grepl("^\\s*Referencia:", line)) {
      ref_text <- trimws(gsub("^\\s*Referencia:\\s*", "", line))
      json_structure$references <- c(json_structure$references, ref_text)
    }
    # Contenido regular
    else {
      current_content <- c(current_content, line)
    }
  }

  # Guardar ultima seccion
  if (!is.null(current_section) && length(current_content) > 0) {
    json_structure$sections[[current_section]]$content <- paste(current_content, collapse = "\n")
  }

  # Agregar metricas
  if (!is.null(result$test_metrics)) {
    metrics_df <- result$test_metrics
    for (i in 1:nrow(metrics_df)) {
      metric_name <- metrics_df$.metric[i]
      metric_value <- metrics_df$.estimate[i]
      json_structure$metrics[[metric_name]] <- round(metric_value, 4)
    }
  }

  # Agregar informacion del modelo
  json_structure$model_info <- list(
    name = result$best_model,
    label = .get_model_label(result$best_model),
    tuned = !is.null(result$best_params),
    hyperparameters = result$best_params
  )

  # Agregar importancia de variables
  if (!is.null(result$importance) && nrow(result$importance) > 0) {
    top_vars <- head(result$importance, 10)
    json_structure$variable_importance <- lapply(1:nrow(top_vars), function(i) {
      list(
        variable = top_vars$Variable[i],
        importance = round(top_vars$Importance[i], 4),
        rank = i
      )
    })
  }

  # Agregar threshold si existe
  if (!is.null(result$optimal_threshold)) {
    json_structure$threshold <- list(
      default = 0.5,
      optimized = round(result$optimal_threshold, 4)
    )
  }

  # Eliminar referencias duplicadas
  json_structure$references <- unique(unlist(json_structure$references))

  return(json_structure)
}


#' @title Generar Reporte desde JSON
#'
#' @description
#' Genera un reporte en formato Rmarkdown/Word desde el JSON exportado.
#'
#' @param json_path Ruta al archivo JSON
#' @param output_path Ruta del archivo de salida
#' @param format Formato de salida: "docx", "html", "pdf"
#' @param template Plantilla a usar: "scientific", "brief", "full"
#'
#' @return Invisiblemente, la ruta del archivo generado
#' @export
generate_report_from_json <- function(json_path,
                                       output_path = "easyml_report.docx",
                                       format = c("docx", "html", "pdf"),
                                       template = c("scientific", "brief", "full")) {

  format <- match.arg(format)
  template <- match.arg(template)

  # Leer JSON
  json_data <- jsonlite::fromJSON(json_path)

  # Generar contenido Rmd
  rmd_content <- .generate_rmd_content(json_data, template)

  # Escribir archivo temporal Rmd
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, temp_rmd)

  # Renderizar
  output_format <- switch(format,
    "docx" = "word_document",
    "html" = "html_document",
    "pdf" = "pdf_document"
  )

  rmarkdown::render(
    input = temp_rmd,
    output_format = output_format,
    output_file = basename(output_path),
    output_dir = dirname(output_path),
    quiet = TRUE
  )

  # Limpiar
  unlink(temp_rmd)

  message("Reporte generado: ", output_path)
  invisible(output_path)
}


#' @title Generar Contenido Rmd
#' @noRd
.generate_rmd_content <- function(json_data, template) {

  # Header YAML
  yaml_header <- paste0(
    "---\n",
    "title: \"Reporte de Analisis de Machine Learning\"\n",
    "date: \"", json_data$metadata$generated_at, "\"\n",
    "output:\n",
    "  word_document:\n",
    "    reference_docx: null\n",
    "---\n\n"
  )

  # Contenido segun template
  if (template == "scientific") {
    content <- .generate_scientific_report(json_data)
  } else if (template == "brief") {
    content <- .generate_brief_report(json_data)
  } else {
    content <- .generate_full_report(json_data)
  }

  paste0(yaml_header, content)
}


#' @title Generar Reporte Cientifico
#' @noRd
.generate_scientific_report <- function(json_data) {

  meta <- json_data$metadata
  metrics <- json_data$metrics
  model <- json_data$model_info
  refs <- json_data$references

  # Seccion Metodo
  content <- "# Metodo\n\n"

  content <- paste0(content, "## Participantes\n\n")
  content <- paste0(content,
    "La muestra estuvo conformada por ", meta$n_observations, " observaciones, ",
    "las cuales fueron divididas en un conjunto de entrenamiento (", meta$n_train,
    " observaciones, ", round(meta$n_train/meta$n_observations*100, 1), "%) ",
    "y un conjunto de prueba (", meta$n_test, " observaciones, ",
    round(meta$n_test/meta$n_observations*100, 1), "%) ",
    "para la validacion del modelo.\n\n"
  )

  content <- paste0(content, "## Analisis de Datos\n\n")
  content <- paste0(content,
    "El analisis estadistico se realizo en R utilizando el paquete easyML. ",
    "Se empleo un enfoque de aprendizaje automatico para ",
    ifelse(meta$task == "classification", "clasificacion", "regresion"),
    " con el objetivo de predecir la variable '", meta$target, "'.\n\n"
  )

  # Modelo utilizado
  content <- paste0(content,
    "Entre los modelos evaluados, ", model$label, " emergio como la opcion preferida ",
    "debido a su rendimiento superior en las metricas clave. "
  )

  if (model$tuned && !is.null(model$hyperparameters)) {
    content <- paste0(content,
      "Se realizo un ajuste de hiperparametros para optimizar el rendimiento del modelo. ",
      "Los hiperparametros optimizados fueron: "
    )
    params_text <- paste(names(model$hyperparameters), "=",
                          unlist(model$hyperparameters), collapse = ", ")
    content <- paste0(content, params_text, ".\n\n")
  } else {
    content <- paste0(content, "\n\n")
  }

  # Validacion cruzada
  content <- paste0(content,
    "Para mitigar el sobreajuste, se empleo validacion cruzada de 10 folds durante ",
    "el entrenamiento, asegurando que el rendimiento del modelo se evaluara en ",
    "multiples subconjuntos de datos (Kohavi, 1995). Este metodo divide los datos en ",
    "10 partes iguales, donde en cada iteracion se usa una parte para validacion ",
    "y las nueve restantes para entrenamiento.\n\n"
  )

  # Metricas
  content <- paste0(content, "## Metricas de Evaluacion\n\n")

  if (meta$task == "classification") {
    content <- paste0(content,
      "Los modelos se evaluaron utilizando metricas criticas para datos de clasificacion ",
      "(Hanley & McNeil, 1982). Las metricas obtenidas en el conjunto de prueba fueron:\n\n"
    )

    if (!is.null(metrics$roc_auc)) {
      content <- paste0(content, "- **AUC (Area Under the Curve)**: ",
                        round(metrics$roc_auc, 4), "\n")
    }
    if (!is.null(metrics$accuracy)) {
      content <- paste0(content, "- **Accuracy**: ",
                        round(metrics$accuracy * 100, 2), "%\n")
    }
    if (!is.null(metrics$sensitivity)) {
      content <- paste0(content, "- **Sensibilidad**: ",
                        round(metrics$sensitivity * 100, 2), "%\n")
    }
    if (!is.null(metrics$specificity)) {
      content <- paste0(content, "- **Especificidad**: ",
                        round(metrics$specificity * 100, 2), "%\n")
    }
    if (!is.null(metrics$f_meas)) {
      content <- paste0(content, "- **F1-Score**: ",
                        round(metrics$f_meas, 4), "\n")
    }
  } else {
    content <- paste0(content,
      "Los modelos se evaluaron utilizando metricas estandar para regresion. ",
      "Las metricas obtenidas en el conjunto de prueba fueron:\n\n"
    )

    if (!is.null(metrics$rmse)) {
      content <- paste0(content, "- **RMSE**: ", round(metrics$rmse, 4), "\n")
    }
    if (!is.null(metrics$rsq)) {
      content <- paste0(content, "- **R-squared**: ", round(metrics$rsq, 4), "\n")
    }
    if (!is.null(metrics$mae)) {
      content <- paste0(content, "- **MAE**: ", round(metrics$mae, 4), "\n")
    }
  }

  content <- paste0(content, "\n")

  # Importancia de variables
  if (!is.null(json_data$variable_importance) && length(json_data$variable_importance) > 0) {
    content <- paste0(content, "## Importancia de Variables\n\n")
    content <- paste0(content,
      "Para comprender el impacto de cada variable en la prediccion, se calculo ",
      "la importancia de variables del modelo (Breiman, 2001). Las variables mas ",
      "influyentes fueron:\n\n"
    )

    content <- paste0(content, "| Rango | Variable | Importancia |\n")
    content <- paste0(content, "|-------|----------|-------------|\n")

    for (var_info in json_data$variable_importance) {
      content <- paste0(content, "| ", var_info$rank, " | ",
                        var_info$variable, " | ", var_info$importance, " |\n")
    }
    content <- paste0(content, "\n")
  }

  # Threshold optimizado
  if (!is.null(json_data$threshold)) {
    content <- paste0(content, "## Optimizacion de Umbral\n\n")
    content <- paste0(content,
      "Se realizo una optimizacion del umbral de clasificacion para mejorar ",
      "el rendimiento del modelo. El umbral optimo encontrado fue ",
      json_data$threshold$optimized, " (comparado con el umbral por defecto de ",
      json_data$threshold$default, ").\n\n"
    )
  }

  # Referencias
  content <- paste0(content, "# Referencias\n\n")

  # Referencias fijas del framework
  fixed_refs <- c(
    "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.",
    "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a ROC curve. Radiology, 143(1), 29-36.",
    "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation and model selection. IJCAI, 14(2), 1137-1145.",
    "Kuhn, M., & Wickham, H. (2020). Tidymodels: a collection of packages for modeling and machine learning using tidyverse principles."
  )

  all_refs <- unique(c(fixed_refs, refs))
  all_refs <- sort(all_refs)

  for (ref in all_refs) {
    if (nchar(ref) > 5) {
      content <- paste0(content, ref, "\n\n")
    }
  }

  return(content)
}


#' @title Generar Reporte Breve
#' @noRd
.generate_brief_report <- function(json_data) {

  meta <- json_data$metadata
  metrics <- json_data$metrics
  model <- json_data$model_info

  content <- "# Resumen del Analisis\n\n"

  content <- paste0(content,
    "Se realizo un analisis de ", meta$task, " para predecir '", meta$target, "' ",
    "utilizando ", meta$n_observations, " observaciones.\n\n"
  )

  content <- paste0(content, "## Modelo Seleccionado\n\n")
  content <- paste0(content, "- **Modelo**: ", model$label, "\n")
  content <- paste0(content, "- **Datos de entrenamiento**: ", meta$n_train, "\n")
  content <- paste0(content, "- **Datos de prueba**: ", meta$n_test, "\n\n")

  content <- paste0(content, "## Metricas\n\n")
  for (metric_name in names(metrics)) {
    content <- paste0(content, "- **", metric_name, "**: ",
                      round(metrics[[metric_name]], 4), "\n")
  }

  return(content)
}


#' @title Generar Reporte Completo
#' @noRd
.generate_full_report <- function(json_data) {
  # Combina el reporte cientifico con detalles adicionales
  content <- .generate_scientific_report(json_data)

  # Agregar verbose completo si existe
  if (!is.null(json_data$verbose_text)) {
    content <- paste0(content, "\n\n# Anexo: Output Completo\n\n")
    content <- paste0(content, "```\n", json_data$verbose_text, "\n```\n")
  }

  return(content)
}


#' @title Lanzar Generador de Reportes Shiny
#'
#' @description
#' Abre la aplicacion Shiny para generar reportes cientificos
#' a partir de archivos JSON exportados de easyML.
#'
#' @param launch.browser Abrir en navegador (default: TRUE)
#'
#' @export
launch_report_generator <- function(launch.browser = TRUE) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install with: install.packages('shiny')",
         call. = FALSE)
  }

  app_dir <- system.file("shiny/report_generator", package = "easyML")

  if (app_dir == "") {
    # Si no esta instalado como paquete, buscar en directorio local
    app_dir <- file.path(getwd(), "inst/shiny/report_generator")
  }

  if (!dir.exists(app_dir)) {
    stop("No se encontro la aplicacion Shiny. ",
         "Asegurese de que el paquete easyML este instalado correctamente.")
  }

  shiny::runApp(app_dir, launch.browser = launch.browser)
}


#' @title Exportar Analisis Completo
#'
#' @description
#' Funcion de conveniencia que ejecuta easy_ml, captura el verbose,
#' y exporta tanto TXT como JSON.
#'
#' @param ... Argumentos para easy_ml()
#' @param output_dir Directorio de salida
#' @param prefix Prefijo para los archivos
#'
#' @return El resultado de easy_ml con verbose capturado
#' @export
easy_ml_export <- function(..., output_dir = ".", prefix = "easyml") {

  # Ejecutar con captura
  result <- easy_ml_capture(...)

  # Crear nombres de archivo
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  txt_file <- file.path(output_dir, paste0(prefix, "_", timestamp, ".txt"))
  json_file <- file.path(output_dir, paste0(prefix, "_", timestamp, ".json"))


  # Exportar
  export_verbose_txt(result, txt_file)
  export_verbose_json(result, json_file)

  message("\nArchivos exportados:")
  message("  TXT:  ", txt_file)
  message("  JSON: ", json_file)
  message("\nPara generar un reporte, ejecute:")
  message("  launch_report_generator()")

  invisible(result)
}


#' @title Generar Reporte Cientifico con IA
#'
#' @description
#' Genera un reporte cientifico utilizando ChatGPT directamente desde R,
#' sin necesidad de abrir la aplicacion Shiny.
#'
#' @param json_path Ruta al archivo JSON exportado con export_verbose_json()
#' @param api_key Tu API Key de OpenAI
#' @param output_path Ruta del archivo de salida (default: "easyML_Report.docx")
#' @param title Titulo del reporte
#' @param author Nombre del autor (opcional)
#' @param language Idioma del reporte: "es" (espanol) o "en" (ingles)
#' @param model Modelo de OpenAI a usar (default: "gpt-4.1-mini")
#'
#' @return Invisiblemente, la ruta del archivo generado
#'
#' @examples
#' \dontrun{
#' # Primero ejecutar analisis y exportar JSON
#' resultado <- easy_ml_capture(data, target = "mi_variable", task = "classification")
#' export_verbose_json(resultado, "mi_analisis.json")
#'
#' # Generar reporte con IA
#' generate_report_with_ai(
#'   json_path = "mi_analisis.json",
#'   api_key = "sk-proj-...",
#'   output_path = "Mi_Reporte.docx",
#'   title = "Analisis de Machine Learning",
#'   author = "Dr. Juan Perez",
#'   language = "es"
#' )
#' }
#'
#' @export
generate_report_with_ai <- function(json_path,
                                     api_key,
                                     output_path = "easyML_Report.docx",
                                     title = "Analisis de Machine Learning",
                                     author = NULL,
                                     language = c("es", "en"),
                                     model = "gpt-4.1-mini") {

  # Validar argumentos
  language <- match.arg(language)

  if (!file.exists(json_path)) {
    stop("No se encontro el archivo JSON: ", json_path)
  }

  if (!nzchar(api_key)) {
    stop("Debe proporcionar una API Key de OpenAI valida.")
  }

  # Verificar paquetes requeridos
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Se requiere el paquete 'httr2'. Instale con: install.packages('httr2')")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Se requiere el paquete 'officer'. Instale con: install.packages('officer')")
  }

  message("Leyendo archivo JSON...")
  json_data <- jsonlite::fromJSON(json_path)

  meta <- json_data$metadata
  metrics <- json_data$metrics
  model_info <- json_data$model_info
  var_imp <- json_data$variable_importance

  # Construir prompt del sistema
  message("Preparando contexto para ChatGPT...")

  system_prompt <- .build_system_prompt(language)

  # Construir mensaje del usuario
  user_message <- .build_user_message(meta, metrics, model_info, var_imp, json_data, language)

  # Llamar a la API de OpenAI
  message("Generando reporte con ", model, "...")

  clean_api_key <- trimws(api_key)

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", clean_api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0.4,
      max_tokens = 16000,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_message)
      )
    )) |>
    httr2::req_timeout(300)

  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop("Error de conexion con OpenAI: ", e$message)
  })

  # Verificar respuesta
  status <- httr2::resp_status(resp)
  if (status == 401) {
    stop("API Key invalida (Error 401). Verifica que la key sea correcta y tenga creditos.")
  } else if (status != 200) {
    body_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    stop("Error HTTP ", status, ": ", body_text)
  }

  # Extraer contenido
  js <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (!is.null(js$error)) {
    stop("Error de OpenAI: ", js$error$message)
  }

  report_text <- ""
  if (!is.null(js$choices) && length(js$choices) > 0) {
    if (!is.null(js$choices[[1]]$message$content)) {
      report_text <- js$choices[[1]]$message$content
    }
  }

  if (!nzchar(report_text)) {
    stop("La API devolvio una respuesta vacia.")
  }

  message("Reporte generado. Creando documento Word...")

  # Crear documento Word con tablas reales
  doc <- .create_word_document(
    report_text = report_text,
    title = title,
    author = author,
    json_data = json_data
  )

  # Guardar documento
  print(doc, target = output_path)

  message("\nReporte generado exitosamente!")
  message("Archivo: ", output_path)

  invisible(output_path)
}


#' @title Crear Documento Word con Tablas Reales
#' @noRd
.create_word_document <- function(report_text, title, author, json_data) {

  # Crear documento

  doc <- officer::read_docx()

  # Titulo
  doc <- officer::body_add_par(doc, title, style = "heading 1")

  if (!is.null(author) && nzchar(author)) {
    doc <- officer::body_add_par(doc, author, style = "Normal")
  }

  doc <- officer::body_add_par(doc, format(Sys.Date(), "%Y-%m-%d"), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # Procesar Markdown linea por linea
  md_lines <- unlist(strsplit(report_text, "\n", fixed = TRUE))
  i <- 1

  while (i <= length(md_lines)) {
    ln <- md_lines[i]

    # Lineas vacias
    if (nchar(trimws(ln)) == 0) {
      i <- i + 1
      next
    }

    # Separadores
    if (grepl("^---+$", ln)) {
      i <- i + 1
      next
    }

    # Headers nivel 1
    if (grepl("^#\\s+", ln) && !grepl("^##", ln)) {
      texto <- gsub("^#\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- officer::body_add_par(doc, texto, style = "heading 1")
      i <- i + 1
      next
    }

    # Headers nivel 2
    if (grepl("^##\\s+", ln) && !grepl("^###", ln)) {
      texto <- gsub("^##\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- officer::body_add_par(doc, texto, style = "heading 2")
      i <- i + 1
      next
    }

    # Headers nivel 3
    if (grepl("^###\\s+", ln)) {
      texto <- gsub("^###\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- officer::body_add_par(doc, texto, style = "heading 3")
      i <- i + 1
      next
    }

    # Placeholder de Tabla 1 - Insertar tabla de metricas real
    if (grepl("\\[Insertar Tabla 1", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 1", ln, ignore.case = TRUE)) {
      doc <- .add_metrics_table(doc, json_data)
      i <- i + 1
      next
    }

    # Placeholder de Tabla 2 - Insertar tabla de importancia de variables
    if (grepl("\\[Insertar Tabla 2", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 2", ln, ignore.case = TRUE)) {
      doc <- .add_importance_table(doc, json_data)
      i <- i + 1
      next
    }

    # Otros placeholders de figuras/tablas
    if (grepl("^\\[Insertar", ln, ignore.case = TRUE) ||
        grepl("^\\[Insert", ln, ignore.case = TRUE)) {
      # Agregar placeholder en italica
      doc <- officer::body_add_par(doc, ln, style = "Normal")
      i <- i + 1
      next
    }

    # Titulo de Tabla o Figura (en negrita)
    if (grepl("^Tabla\\s+\\d+", ln) || grepl("^Table\\s+\\d+", ln) ||
        grepl("^Figura\\s+\\d+", ln) || grepl("^Figure\\s+\\d+", ln)) {
      texto <- gsub("\\*\\*", "", ln)
      texto <- gsub("\\*", "", texto)
      # Agregar como parrafo con formato especial
      doc <- officer::body_add_par(doc, texto, style = "Normal")
      i <- i + 1
      next
    }

    # Detectar tabla markdown (linea con |)
    if (grepl("^\\|", ln) && grepl("\\|$", ln)) {
      # Recopilar todas las lineas de la tabla
      table_lines <- c()
      while (i <= length(md_lines) && grepl("^\\|", md_lines[i])) {
        table_lines <- c(table_lines, md_lines[i])
        i <- i + 1
      }
      # Parsear y agregar tabla
      doc <- .add_markdown_table(doc, table_lines)
      next
    }

    # Parrafos normales
    texto <- ln
    texto <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", texto)
    texto <- gsub("\\*([^*]+)\\*", "\\1", texto)
    texto <- gsub("_([^_]+)_", "\\1", texto)
    doc <- officer::body_add_par(doc, texto, style = "Normal")
    i <- i + 1
  }

  return(doc)
}


#' @title Agregar Tabla de Metricas al Word
#' @noRd
.add_metrics_table <- function(doc, json_data) {

  metrics <- json_data$metrics
  if (is.null(metrics)) {
    doc <- officer::body_add_par(doc, "[Metricas no disponibles]", style = "Normal")
    return(doc)
  }

  # Crear data frame con metricas
  metric_names <- names(metrics)
  metric_values <- unlist(metrics)

  # Formatear nombres y valores
  formatted_names <- sapply(metric_names, function(m) {
    switch(m,
      "roc_auc" = "ROC-AUC",
      "accuracy" = "Accuracy",
      "sensitivity" = "Sensitivity",
      "specificity" = "Specificity",
      "f_meas" = "F1-Score",
      "pr_auc" = "PR-AUC",
      "bal_accuracy" = "Balanced Accuracy",
      "rmse" = "RMSE",
      "rsq" = "R-squared",
      "mae" = "MAE",
      toupper(m)
    )
  })

  formatted_values <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]
    val <- metric_values[i]
    if (m %in% c("accuracy", "sensitivity", "specificity", "bal_accuracy")) {
      sprintf("%.2f%%", val * 100)
    } else if (m %in% c("rmse", "mae")) {
      sprintf("%.4f", val)
    } else {
      sprintf("%.4f", val)
    }
  })

  # Agregar interpretacion
  interpretations <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]
    val <- metric_values[i]
    if (m == "roc_auc") {
      if (val >= 0.9) "Excelente"
      else if (val >= 0.8) "Bueno"
      else if (val >= 0.7) "Aceptable"
      else "Bajo"
    } else if (m %in% c("accuracy", "sensitivity", "specificity")) {
      if (val >= 0.9) "Excelente"
      else if (val >= 0.8) "Bueno"
      else if (val >= 0.7) "Aceptable"
      else "Bajo"
    } else {
      ""
    }
  })

  df <- data.frame(
    Metrica = formatted_names,
    Valor = formatted_values,
    Interpretacion = interpretations,
    stringsAsFactors = FALSE
  )

  # Crear tabla con flextable
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(ft,
      Metrica = "Metrica",
      Valor = "Valor",
      Interpretacion = "Interpretacion"
    )
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft)
    ft <- flextable::align(ft, align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::fontsize(ft, size = 10, part = "all")

    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    # Fallback sin flextable
    doc <- officer::body_add_par(doc, "", style = "Normal")
    for (j in 1:nrow(df)) {
      line <- sprintf("%s: %s %s", df$Metrica[j], df$Valor[j],
                      ifelse(nzchar(df$Interpretacion[j]), paste0("(", df$Interpretacion[j], ")"), ""))
      doc <- officer::body_add_par(doc, line, style = "Normal")
    }
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' @title Agregar Tabla de Importancia de Variables al Word
#' @noRd
.add_importance_table <- function(doc, json_data) {

  var_imp <- json_data$variable_importance
  if (is.null(var_imp)) {
    doc <- officer::body_add_par(doc, "[Importancia de variables no disponible]", style = "Normal")
    return(doc)
  }

  # Convertir a data frame si es lista
  if (is.list(var_imp) && !is.data.frame(var_imp)) {
    df <- do.call(rbind, lapply(var_imp, function(v) {
      data.frame(
        Rango = v$rank,
        Variable = v$variable,
        Importancia = v$importance,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    df <- data.frame(
      Rango = var_imp$rank,
      Variable = var_imp$variable,
      Importancia = var_imp$importance,
      stringsAsFactors = FALSE
    )
  }

  # Formatear importancia
  df$Importancia <- sprintf("%.4f", df$Importancia)

  # Limitar a top 10
  df <- utils::head(df, 10)

  # Crear tabla con flextable
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(ft,
      Rango = "Rango",
      Variable = "Variable",
      Importancia = "Importancia"
    )
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft)
    ft <- flextable::align(ft, j = 1, align = "center", part = "all")
    ft <- flextable::align(ft, j = 2, align = "left", part = "all")
    ft <- flextable::align(ft, j = 3, align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::fontsize(ft, size = 10, part = "all")

    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    # Fallback sin flextable
    doc <- officer::body_add_par(doc, "", style = "Normal")
    for (j in 1:nrow(df)) {
      line <- sprintf("%d. %s: %s", df$Rango[j], df$Variable[j], df$Importancia[j])
      doc <- officer::body_add_par(doc, line, style = "Normal")
    }
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' @title Agregar Tabla Markdown al Word
#' @noRd
.add_markdown_table <- function(doc, table_lines) {

  if (length(table_lines) < 2) {
    return(doc)
  }

  # Parsear lineas de tabla markdown
  parse_row <- function(line) {
    cells <- strsplit(line, "\\|")[[1]]
    cells <- cells[nzchar(trimws(cells))]
    trimws(cells)
  }

  # Primera linea es header
  header <- parse_row(table_lines[1])

  # Segunda linea es separador (ignorar)
  # Resto son datos
  data_rows <- list()
  for (k in 3:length(table_lines)) {
    if (k <= length(table_lines)) {
      row <- parse_row(table_lines[k])
      if (length(row) > 0) {
        data_rows[[length(data_rows) + 1]] <- row
      }
    }
  }

  if (length(data_rows) == 0) {
    return(doc)
  }

  # Crear data frame
  df <- do.call(rbind, lapply(data_rows, function(r) {
    # Asegurar que tenga el mismo numero de columnas que header
    if (length(r) < length(header)) {
      r <- c(r, rep("", length(header) - length(r)))
    } else if (length(r) > length(header)) {
      r <- r[1:length(header)]
    }
    r
  }))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- header

  # Crear tabla con flextable
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft)
    ft <- flextable::align(ft, align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::fontsize(ft, size = 10, part = "all")

    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    # Fallback
    for (j in 1:nrow(df)) {
      line <- paste(df[j, ], collapse = " | ")
      doc <- officer::body_add_par(doc, line, style = "Normal")
    }
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' @title Construir Prompt del Sistema
#' @noRd


#' @title Construir Prompt del Sistema
#' @noRd
.build_system_prompt <- function(language) {
  if (language == "es") {
    paste(
      "Eres un experto en redaccion cientifica especializado en Machine Learning y analisis predictivo.",
      "Tu tarea es generar un reporte academico en ESTRUCTURA DE ARTICULO CIENTIFICO.",
      "",
      "=============================================================================",
      "REGLA CRITICA: SEPARACION ENTRE METODO Y RESULTADOS",
      "=============================================================================",
      "",
      "METODO = describe QUE SE HIZO y COMO (procedimiento, plan)",
      "RESULTADOS = presenta LOS NUMEROS Y HALLAZGOS OBTENIDOS",
      "",
      "En METODO/Analisis de Datos NUNCA debes incluir:",
      "- Valores especificos de metricas (ej: AUC = 0.82)",
      "- Cual modelo fue seleccionado como mejor",
      "- Valores de hiperparametros optimizados (ej: mtry = 4)",
      "- Resultados de la validacion cruzada",
      "",
      "Esos valores van SOLO en la seccion RESULTADOS.",
      "",
      "=============================================================================",
      "ESTRUCTURA OBLIGATORIA DEL REPORTE",
      "=============================================================================",
      "",
      "# METODO",
      "",
      "## Participantes",
      "Describe detalladamente:",
      "- Tamano de la muestra (N total)",
      "- Division train/test (n y porcentajes)",
      "- Caracteristicas del dataset (numero y tipo de variables)",
      "- Distribucion de la variable objetivo",
      "- Valores faltantes y outliers detectados",
      "- Criterios de inclusion/exclusion si aplican",
      "",
      "## Analisis de Datos",
      "IMPORTANTE: Esta seccion describe SOLO el procedimiento metodologico.",
      "Redacta en tiempo pasado describiendo QUE se hizo, NO los resultados.",
      "",
      "Debe incluir:",
      "- Software y paquetes utilizados (R, easyML, tidymodels)",
      "- Pasos de preprocesamiento aplicados:",
      "  * Imputacion (mediana para numericas, moda para categoricas)",
      "  * Normalizacion z-score",
      "  * Dummy encoding para variables categoricas",
      "  * SMOTE para balance de clases (si se uso)",
      "- Seleccion de variables (metodo Boruta si se uso)",
      "- Estrategia de validacion cruzada (K-fold estratificado, numero de folds)",
      "- Modelos que FUERON evaluados (no cual gano, eso va en resultados)",
      "- Metricas de evaluacion utilizadas (ROC-AUC, accuracy, etc.)",
      "- Procedimiento de optimizacion de hiperparametros (Random Search, grid)",
      "- Metodo de optimizacion de threshold (indice de Youden)",
      "- Procedimiento de deteccion de data leakage",
      "",
      "EJEMPLO CORRECTO: Se evaluaron tres modelos (Random Forest, XGBoost y regresion logistica)",
      "mediante validacion cruzada de 10 folds estratificados.",
      "",
      "EJEMPLO INCORRECTO: Random Forest fue el mejor modelo con AUC de 0.80.",
      "(Esto es un RESULTADO, no metodologia)",
      "",
      "# RESULTADOS",
      "",
      "Esta seccion presenta TODOS los hallazgos numericos.",
      "Aqui SI van los valores especificos, metricas y comparaciones.",
      "",
      "## Seleccion del Modelo",
      "- Presenta los resultados de la comparacion de modelos en CV",
      "- Indica cual modelo fue seleccionado y por que",
      "- Reporta las metricas de cada modelo evaluado",
      "[Insertar Figura - Comparacion de Modelos]",
      "",
      "## Optimizacion de Hiperparametros",
      "- Valores optimos encontrados para cada hiperparametro",
      "- Mejora obtenida respecto a valores por defecto",
      "",
      "## Rendimiento del Modelo Final",
      "- TODAS las metricas del conjunto de prueba",
      "- Interpretacion de cada metrica",
      "[Insertar Tabla 1 - Metricas de rendimiento]",
      "",
      "## Capacidad Discriminativa",
      "- Valor de AUC e interpretacion",
      "- Descripcion de la curva ROC",
      "[Insertar Figura - Curva ROC]",
      "",
      "## Matriz de Confusion",
      "- Verdaderos positivos/negativos",
      "- Falsos positivos/negativos",
      "- Implicaciones practicas de los errores",
      "[Insertar Figura - Matriz de Confusion]",
      "",
      "## Calibracion del Modelo",
      "- Comparacion entre probabilidades predichas y observadas",
      "- Interpretacion de la calidad de calibracion",
      "[Insertar Figura - Calibracion]",
      "",
      "## Optimizacion del Umbral",
      "- Threshold optimo vs default",
      "- Cambios en sensibilidad/especificidad",
      "[Insertar Figura - Threshold]",
      "",
      "## Importancia de Variables",
      "- Ranking de variables mas importantes",
      "- Interpretacion teorica de cada variable relevante",
      "[Insertar Tabla 2 - Importancia de Variables]",
      "[Insertar Figura - Importancia de Variables]",
      "",
      "# DISCUSION",
      "",
      "IMPORTANTE: Seccion BREVE con PUNTOS CLAVE solamente.",
      "El usuario desarrollara estos puntos en su propia discusion.",
      "",
      "Formato obligatorio:",
      "",
      "Hallazgos principales:",
      "1. [Punto conciso sobre rendimiento]",
      "2. [Punto sobre variables importantes]",
      "3. [Punto sobre capacidad predictiva]",
      "",
      "Limitaciones:",
      "1. [Limitacion metodologica]",
      "2. [Limitacion de datos]",
      "",
      "Direcciones futuras:",
      "1. [Sugerencia de investigacion]",
      "2. [Mejora metodologica posible]",
      "",
      "# REFERENCIAS",
      "- Incluye TODAS las referencias mencionadas en el verbose",
      "- Agrega referencias adicionales relevantes",
      "- Formato APA 7ma edicion",
      "- Minimo 10 referencias",
      "",
      "=============================================================================",
      "ESTILO DE ESCRITURA",
      "=============================================================================",
      "- Parrafos extensos y narrativos en Metodo y Resultados (150+ palabras)",
      "- NO uses listas con bullets en Metodo y Resultados",
      "- Discusion: SOLO puntos numerados, NO parrafos largos",
      "- Citas integradas en el texto (Autor, ano)",
      "- Estadisticos en formato APA: AUC = .82, 95% CI [.75, .89]",
      "",
      "IMPORTANTE: Usa los nombres EXACTOS del catalogo de figuras proporcionado.",
      "",
      sep = "\n"
    )
  } else {
    paste(
      "You are a scientific writing expert specialized in Machine Learning.",
      "Generate an academic report in SCIENTIFIC ARTICLE STRUCTURE.",
      "",
      "CRITICAL RULE: METHOD describes WHAT was done (procedure).",
      "RESULTS presents THE NUMBERS AND FINDINGS obtained.",
      "",
      "In METHOD/Data Analysis NEVER include specific metric values,",
      "which model was selected, or optimized hyperparameter values.",
      "Those go ONLY in RESULTS.",
      "",
      "# METHOD",
      "## Participants - Sample description, train/test split",
      "## Data Analysis - Procedure only, NO results",
      "",
      "# RESULTS",
      "## Model Selection - Which model won and why",
      "## Hyperparameter Optimization - Optimal values found",
      "## Final Model Performance - All test metrics",
      "## ROC Curve - AUC interpretation",
      "## Confusion Matrix - Error analysis",
      "## Calibration - Probability reliability",
      "## Threshold Optimization - Optimal cutoff",
      "## Variable Importance - Top predictors",
      "",
      "# DISCUSSION - Brief key points only",
      "# REFERENCES - APA 7th edition",
      "",
      sep = "\n"
    )
  }
}




#' @title Construir Mensaje del Usuario
#' @noRd
.build_user_message <- function(meta, metrics, model_info, var_imp, json_data, language) {

  # Formatear metricas
  metrics_text <- paste(sapply(names(metrics), function(m) {
    val <- metrics[[m]]
    if (m %in% c("accuracy", "sensitivity", "specificity")) {
      sprintf("%s: %.2f%%", toupper(m), val * 100)
    } else {
      sprintf("%s: %.4f", toupper(m), val)
    }
  }), collapse = "\n")

  # Formatear importancia de variables
  var_text <- if (!is.null(var_imp) && (is.data.frame(var_imp) || length(var_imp) > 0)) {
    if (is.data.frame(var_imp)) {
      paste(sapply(1:min(10, nrow(var_imp)), function(i) {
        sprintf("%d. %s (%.4f)", var_imp$rank[i], var_imp$variable[i], var_imp$importance[i])
      }), collapse = "\n")
    } else {
      paste(sapply(1:min(10, length(var_imp)), function(i) {
        v <- var_imp[[i]]
        sprintf("%d. %s (%.4f)", v$rank, v$variable, v$importance)
      }), collapse = "\n")
    }
  } else {
    "No disponible"
  }

  # Hiperparametros
  hyperparam_text <- if (model_info$tuned && !is.null(model_info$hyperparameters)) {
    paste(names(model_info$hyperparameters), "=", unlist(model_info$hyperparameters), collapse = ", ")
  } else {
    "No se realizo tuning"
  }

  # Obtener verbose_text completo si existe
  verbose_section <- ""
  if (!is.null(json_data$verbose_text) && nzchar(json_data$verbose_text)) {
    verbose_section <- paste(
      "",
      "## OUTPUT COMPLETO DEL ANALISIS (VERBOSE)",
      "A continuacion se presenta el output completo del analisis de Machine Learning.",
      "Usa TODA esta informacion para escribir un reporte MUY DETALLADO y COMPLETO.",
      "Incluye todos los detalles tecnicos, interpretaciones y referencias mencionadas.",
      "",
      "```",
      json_data$verbose_text,
      "```",
      "",
      sep = "\n"
    )
  }

  paste(
    "# DATOS COMPLETOS PARA REPORTE ACADEMICO DE MACHINE LEARNING",
    "",
    "## RESUMEN DEL ESTUDIO",
    sprintf("Tipo de tarea: %s", ifelse(meta$task == "classification", "Clasificacion binaria", "Regresion")),
    sprintf("Variable objetivo: %s", meta$target),
    sprintf("Total de observaciones: N = %d", meta$n_observations),
    sprintf("Conjunto de entrenamiento: n = %d (%.1f%%)", meta$n_train, meta$n_train/meta$n_observations*100),
    sprintf("Conjunto de prueba: n = %d (%.1f%%)", meta$n_test, meta$n_test/meta$n_observations*100),
    "",
    "## MODELO SELECCIONADO",
    sprintf("Algoritmo: %s", model_info$label),
    sprintf("Tuning: %s", ifelse(model_info$tuned, "Si", "No")),
    sprintf("Hiperparametros: %s", hyperparam_text),
    "",
    "## METRICAS DE RENDIMIENTO (en test set)",
    metrics_text,
    "",
    if (!is.null(json_data$threshold)) {
      sprintf("Threshold optimizado: %.4f (vs default 0.50)", json_data$threshold$optimized)
    } else {
      ""
    },
    "",
    "## IMPORTANCIA DE VARIABLES (Top 10)",
    var_text,
    "",
    "## CATALOGO DE FIGURAS DISPONIBLES",
    "Cuando menciones figuras en el reporte, usa estos nombres exactos:",
    if (!is.null(json_data$figures_catalog) && length(json_data$figures_catalog) > 0) {
      # Manejar tanto data.frame (desde JSON) como lista (desde resultado)
      if (is.data.frame(json_data$figures_catalog)) {
        paste(apply(json_data$figures_catalog, 1, function(row) {
          sprintf("  - %s: %s (%s)", row["id"], row["title"], row["filename"])
        }), collapse = "\n")
      } else {
        paste(sapply(json_data$figures_catalog, function(fig) {
          sprintf("  - %s: %s (%s)", fig$id, fig$title, fig$filename)
        }), collapse = "\n")
      }
    } else {
      "  (No hay catalogo de figuras disponible)"
    },
    "",
    "IMPORTANTE: Cuando indiques donde insertar una figura, usa el formato:",
    "  [Insertar Figura_N - Titulo] donde N es el numero de la figura",
    "  Ejemplo: [Insertar Figura_1 - Importancia de Variables]",
    "  El usuario vera los archivos guardados con esos nombres.",
    verbose_section,
    "",
    sprintf("IDIOMA DEL REPORTE: %s", ifelse(language == "es", "ESPANOL", "ENGLISH")),
    "",
    "INSTRUCCIONES FINALES:",
    "- Usa TODA la informacion del verbose para escribir el reporte",
    "- Incluye detalles sobre preprocesamiento, validacion cruzada, metricas",
    "- Menciona las interpretaciones de cada metrica que aparecen en el verbose",
    "- Incluye las referencias bibliograficas mencionadas en el analisis",
    "- El reporte debe ser MUY COMPLETO y DETALLADO (minimo 2500 palabras)",
    "",
    "GENERA EL REPORTE AHORA.",
    sep = "\n"
  )
}


#' @title Lanzar App Integrada easyML
#'
#' @description
#' Abre la aplicacion Shiny integrada que combina Pipeline ML,
#' Estimacion de Tamano Muestral y Generador de Reportes con IA.
#'
#' @param launch.browser Abrir en navegador (default: TRUE)
#'
#' @export
launch_easyml_app <- function(launch.browser = TRUE) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install with: install.packages('shiny')",
         call. = FALSE)
  }

  app_dir <- system.file("shiny/easyml_app", package = "easyML")

  if (app_dir == "") {
    app_dir <- file.path(getwd(), "inst/shiny/easyml_app")
  }

  if (!dir.exists(app_dir)) {
    stop("easyML integrated app not found. ",
         "Make sure the easyML package is installed correctly.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = launch.browser)
}



# =============================================================================
# Smart Diagnostics Engine (internal)
# =============================================================================

# @noRd
.build_diagnostics <- function(result) {
  findings <- list()
  task <- result$task %||% "unknown"
  target <- result$target %||% "unknown"
  meta <- result$metadata

  add <- function(id, issue, severity, details, suggested_code = "") {
    findings[[length(findings) + 1]] <<- list(
      id = id, issue = issue, severity = severity,
      details = details, suggested_code = suggested_code
    )
  }

  # Rule 1: Perfect scores
  if (!is.null(result$test_metrics) && is.data.frame(result$test_metrics)) {
    tm <- result$test_metrics
    if (task == "classification") {
      auc_val <- tm$.estimate[tm$.metric == "roc_auc"]
      if (length(auc_val) > 0 && !is.na(auc_val[1]) && auc_val[1] >= 0.995) {
        add("perfect_scores",
            sprintf("Perfect or near-perfect scores (AUC = %.3f)", auc_val[1]),
            "critical",
            "AUC this high almost always indicates data leakage, a duplicate/derived feature, or a trivially separable dataset.",
            sprintf(paste0(
              "# Check for data leakage - look for IDs or target-derived columns:\n",
              "str(your_data)\n",
              "# Try excluding suspicious variables:\n",
              "result2 <- easy_ml(your_data, \"%s\", \"%s\", exclude_vars = c(\"suspicious_var\"))"),
              target, task))
      }
    } else {
      rsq_val <- tm$.estimate[tm$.metric == "rsq"]
      if (length(rsq_val) > 0 && !is.na(rsq_val[1]) && rsq_val[1] >= 0.995) {
        add("perfect_scores",
            sprintf("Perfect or near-perfect scores (R-sq = %.4f)", rsq_val[1]),
            "critical",
            "R-squared this high almost always indicates data leakage or a derived feature that trivially predicts the target.",
            sprintf(paste0(
              "# Check for columns that are transformations of '%s':\n",
              "str(your_data)\n",
              "# Try excluding suspicious variables:\n",
              "result2 <- easy_ml(your_data, \"%s\", \"%s\", exclude_vars = c(\"suspicious_var\"))"),
              target, target, task))
      }
    }
  }

  # Rule 2: Overfitting (train-test gap)
  if (!is.null(result$evaluation$overfitting$comparison)) {
    comp <- result$evaluation$overfitting$comparison
    select_metric <- meta$select_metric %||%
      (if (task == "classification") "roc_auc" else "rmse")
    row_idx <- which(comp$metric == select_metric)
    if (length(row_idx) > 0) {
      gap <- comp$gap[row_idx[1]]
      if (!is.na(gap) && gap > 0.15) {
        sev <- if (gap > 0.30) "critical" else "warning"
        add("overfitting",
            sprintf("Overfitting detected (train-test gap = %.3f on %s)", gap, select_metric),
            sev,
            sprintf("Training %s is substantially higher than test. The model memorizes training data instead of learning general patterns.", select_metric),
            sprintf(paste0(
              "# Reduce overfitting with tuning and more CV folds:\n",
              "result2 <- easy_ml(your_data, \"%s\", \"%s\", tune = TRUE, cv_folds = 10)\n",
              "# Or try simpler models:\n",
              "result3 <- easy_ml(your_data, \"%s\", \"%s\", models = c(\"glm\", \"tree\"))"),
              target, task, target, task))
      }
    }
  }

  # Rule 3: All models identical
  if (!is.null(result$cv_summary) && is.data.frame(result$cv_summary) &&
      nrow(result$cv_summary) > 1) {
    cv <- result$cv_summary
    select_metric <- meta$select_metric %||%
      (if (task == "classification") "roc_auc" else "rmse")
    if (select_metric %in% names(cv)) {
      vals <- cv[[select_metric]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 1 && (max(vals) - min(vals)) < 0.01) {
        add("identical_models",
            sprintf("All models show identical performance (%s range = %.4f)",
                    select_metric, max(vals) - min(vals)),
            "warning",
            "When all models converge to the same score, the task is likely trivially easy (ceiling effect) or data leakage gives every algorithm the same perfect signal.",
            "# If scores are near-perfect, investigate data leakage (see above)\n# If scores are modest, try feature engineering to strengthen the signal")
      }
    }
  }

  # Rule 4: Class imbalance (classification only)
  if (task == "classification" && !is.null(result$train_data) &&
      target %in% names(result$train_data)) {
    y <- result$train_data[[target]]
    if (!is.null(y)) {
      tab <- table(y)
      props <- prop.table(tab)
      minority_pct <- min(props) * 100
      if (minority_pct < 20) {
        sev <- if (minority_pct < 10) "critical" else "warning"
        minority_class <- names(which.min(tab))
        add("class_imbalance",
            sprintf("Class imbalance (minority class '%s' = %.1f%%)",
                    minority_class, minority_pct),
            sev,
            "Imbalanced classes can bias models toward the majority class. Accuracy alone is misleading.",
            sprintf(paste0(
              "# Use balanced metrics:\n",
              "result2 <- easy_ml(your_data, \"%s\", \"classification\",\n",
              "                   select_metric = \"bal_accuracy\")"),
              target))
      }
    }
  }

  # Rule 5: Small dataset
  if (!is.null(meta)) {
    n <- meta$n_obs %||% 0L
    p <- meta$n_features %||% 1L
    if (n > 0 && n < 50) {
      add("small_n",
          sprintf("Very small dataset (N = %d)", n),
          "warning",
          "With fewer than 50 observations, ML models are prone to overfitting and unstable estimates.",
          sprintf(paste0(
            "# Use more CV folds for stability:\n",
            "result2 <- easy_ml(your_data, \"%s\", \"%s\", cv_folds = %d)\n",
            "# Prefer simpler models:\n",
            "result3 <- easy_ml(your_data, \"%s\", \"%s\", models = c(\"glm\", \"tree\"))"),
            target, task, min(n, 10), target, task))
    } else if (n > 0 && p > 0 && (n / p) < 10) {
      add("low_ratio",
          sprintf("Low observations-to-features ratio (N/p = %.1f)", n / p),
          "info",
          "Fewer than 10 observations per feature increases overfitting risk for complex models.",
          sprintf(paste0(
            "# Consider dimensionality reduction:\n",
            "result2 <- easy_ml(your_data, \"%s\", \"%s\", use_pca = TRUE)"),
            target, task))
    }
  }

  # Rule 6: Dominant variable
  if (!is.null(result$importance) && is.data.frame(result$importance) &&
      nrow(result$importance) > 1) {
    imp <- result$importance
    total_imp <- sum(imp$Importance, na.rm = TRUE)
    if (total_imp > 0) {
      top_pct <- (imp$Importance[1] / total_imp) * 100
      if (top_pct > 80) {
        add("dominant_variable",
            sprintf("Dominant variable: '%s' accounts for %.1f%% of total importance",
                    imp$Variable[1], top_pct),
            "warning",
            "When one variable dominates, the model may be fragile. If that variable is noisy or unavailable, performance collapses.",
            sprintf(paste0(
              "# Try removing the dominant variable:\n",
              "result2 <- easy_ml(your_data, \"%s\", \"%s\",\n",
              "                   exclude_vars = c(\"%s\"))"),
              target, task, imp$Variable[1]))
      }
    }
  }

  # Rule 7: No tuning
  if (!is.null(meta) && isFALSE(meta$tuned)) {
    add("no_tuning",
        "Models were not hyperparameter-tuned",
        "info",
        "Default hyperparameters may not be optimal. Tuning often improves performance.",
        sprintf(paste0(
          "# Enable tuning:\n",
          "result2 <- easy_ml(your_data, \"%s\", \"%s\",\n",
          "                   tune = TRUE, tune_method = \"bayes\")"),
          target, task))
  }

  findings
}


# =============================================================================
# AI Assistant: explain_with_ai()
# =============================================================================

#' @title Interpret ML Results with AI
#'
#' @description
#' Sends the results from \code{easy_ml()} to an LLM and returns a clear
#' interpretation of the analysis. Uses Groq (Llama 3.3 70B, free) by default
#' with a built-in API key, or OpenAI models if you provide your own key.
#' Runs a diagnostic engine first to detect common issues (data leakage,
#' overfitting, class imbalance, etc.) and feeds structured findings to the AI.
#'
#' @param result An object of class \code{easyml} returned by \code{easy_ml()}.
#' @param api_key API key for the provider. If \code{NULL} (default), uses the
#'   built-in Groq key (shared, limited to ~1000 requests/day across all users).
#'   For OpenAI, you must provide your own key. You can also set the environment
#'   variable \code{EASYML_API_KEY} to override the built-in key.
#' @param language Language for the interpretation: \code{"es"} (Spanish,
#'   default) or \code{"en"} (English).
#' @param model Model identifier. If \code{NULL} (default), auto-selects:
#'   \code{"llama-3.3-70b-versatile"} for Groq, \code{"gpt-4.1-mini"} for
#'   OpenAI. Can be overridden with any model ID supported by the provider.
#' @param provider API provider: \code{"groq"} (free, default) or
#'   \code{"openai"} (requires your own API key).
#' @param max_tokens Maximum tokens for the response (default 2000).
#'
#' @return Invisibly, a list with components \code{success} (logical),
#'   \code{text} (character), and \code{diagnostics} (list of findings from the
#'   built-in diagnostic engine). The interpretation is also printed to the
#'   console.
#'
#' @examples
#' \dontrun{
#' result <- easy_ml(iris_binary, "Species", "classification")
#'
#' # Default: free Groq (no key needed)
#' explain_with_ai(result)
#' explain_with_ai(result, language = "en")
#'
#' # With OpenAI (requires your own key)
#' explain_with_ai(result, provider = "openai", api_key = "sk-...")
#'
#' # Custom model
#' explain_with_ai(result, provider = "openai", api_key = "sk-...",
#'                 model = "gpt-4.1")
#' }
#'
#' @export
explain_with_ai <- function(result,
                            api_key = NULL,
                            language = c("es", "en"),
                            model = NULL,
                            provider = c("groq", "openai"),
                            max_tokens = 2000L) {

  # --- Validate inputs ---
  language <- match.arg(language)
  provider <- match.arg(provider)

  if (!inherits(result, "easyml")) {
    stop("'result' must be an object returned by easy_ml().", call. = FALSE)
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Install with: install.packages('httr')",
         call. = FALSE)
  }

  # --- Auto-select model based on provider ---
  if (is.null(model)) {
    model <- switch(provider,
      groq   = "llama-3.3-70b-versatile",
      openai = "gpt-4.1-mini"
    )
  }

  # --- API key (cascade: param -> env var -> built-in for Groq) ---
  .builtin_groq_key <- intToUtf8(c(
    103L, 115L, 107L, 95L, 75L, 87L, 81L, 81L, 122L, 107L, 111L, 53L,
    103L, 109L, 65L, 72L, 73L, 111L, 80L, 90L, 118L, 77L, 101L, 108L,
    87L, 71L, 100L, 121L, 98L, 51L, 70L, 89L, 71L, 105L, 115L, 80L,
    106L, 80L, 122L, 103L, 72L, 78L, 50L, 70L, 79L, 74L, 97L, 110L,
    66L, 73L, 51L, 119L, 82L, 103L, 85L, 104L
  ))
  using_builtin <- FALSE

  if (is.null(api_key)) {
    api_key <- Sys.getenv("EASYML_API_KEY", unset = "")
    if (!nzchar(api_key)) {
      if (provider == "groq") {
        api_key <- .builtin_groq_key
        using_builtin <- TRUE
      } else {
        stop(paste0(
          "OpenAI API key required. Provide it via:\n",
          "  1) explain_with_ai(result, api_key = 'sk-...', provider = 'openai'), or\n",
          "  2) Sys.setenv(EASYML_API_KEY = 'sk-...')\n",
          "Get your key at: https://platform.openai.com/api-keys"
        ), call. = FALSE)
      }
    }
  }

  # --- API URL ---
  base_url <- switch(provider,
    groq   = "https://api.groq.com/openai/v1/chat/completions",
    openai = "https://api.openai.com/v1/chat/completions"
  )

  # --- Run diagnostics ---
  diag_findings <- .build_diagnostics(result)

  # --- Extract structured data ---
  task       <- result$task %||% "unknown"
  target     <- result$target %||% "unknown"
  best_model <- result$best_model %||% "unknown"
  meta       <- result$metadata

  # Dataset info
  meta_parts <- c()
  if (!is.null(meta$n_obs))      meta_parts <- c(meta_parts, sprintf("N = %d", meta$n_obs))
  if (!is.null(meta$n_train))    meta_parts <- c(meta_parts, sprintf("Train = %d", meta$n_train))
  if (!is.null(meta$n_test))     meta_parts <- c(meta_parts, sprintf("Test = %d", meta$n_test))
  if (!is.null(meta$n_features)) meta_parts <- c(meta_parts, sprintf("Features = %d", meta$n_features))
  if (!is.null(meta$cv_folds))   meta_parts <- c(meta_parts, sprintf("CV folds = %d", meta$cv_folds))
  if (!is.null(meta$tuned))      meta_parts <- c(meta_parts, sprintf("Tuned = %s", meta$tuned))
  meta_text <- paste(meta_parts, collapse = ", ")

  # Test metrics
  metrics_text <- ""
  if (!is.null(result$test_metrics) && is.data.frame(result$test_metrics)) {
    tm <- result$test_metrics
    metrics_text <- paste(sapply(seq_len(nrow(tm)), function(i) {
      sprintf("  %s: %.4f", tm$.metric[i], tm$.estimate[i])
    }), collapse = "\n")
  }

  # Train vs Test comparison
  comparison_text <- ""
  if (!is.null(result$evaluation$overfitting$comparison)) {
    comp <- result$evaluation$overfitting$comparison
    comparison_text <- paste(sapply(seq_len(nrow(comp)), function(i) {
      sprintf("  %s: train=%.4f, test=%.4f, gap=%.4f",
              comp$metric[i], comp$train[i], comp$test[i], comp$gap[i])
    }), collapse = "\n")
  }

  # CV model comparison
  cv_text <- ""
  if (!is.null(result$cv_summary) && is.data.frame(result$cv_summary)) {
    cv <- result$cv_summary
    select_metric <- meta$select_metric %||%
      (if (task == "classification") "roc_auc" else "rmse")
    if (select_metric %in% names(cv) && "model" %in% names(cv)) {
      cv_text <- paste(sapply(seq_len(nrow(cv)), function(i) {
        sprintf("  %s: %s=%.4f", cv$model[i], select_metric, cv[[select_metric]][i])
      }), collapse = "\n")
    }
  }

  # Variable importance (top 10)
  importance_text <- ""
  if (!is.null(result$importance) && is.data.frame(result$importance) &&
      nrow(result$importance) > 0) {
    top_imp <- utils::head(result$importance, 10)
    importance_text <- paste(sapply(seq_len(nrow(top_imp)), function(i) {
      sprintf("  %d. %s (%.4f)", i, top_imp$Variable[i], top_imp$Importance[i])
    }), collapse = "\n")
  }

  # Format diagnostic findings for user message
  diag_text <- ""
  if (length(diag_findings) > 0) {
    diag_text <- paste(sapply(diag_findings, function(f) {
      sprintf("[%s] %s: %s\n  Detail: %s",
              toupper(f$severity), f$id, f$issue, f$details)
    }), collapse = "\n")
  }

  # --- Console header + diagnostics ---
  cat("\n")
  cat(strrep("=", 60), "\n")
  if (language == "es") {
    cat("  Asistente IA - Interpretacion de Resultados\n")
  } else {
    cat("  AI Assistant - Results Interpretation\n")
  }
  cat(strrep("=", 60), "\n")

  if (length(diag_findings) > 0) {
    cat("\n")
    if (language == "es") {
      cat("[DIAGNOSTICOS DETECTADOS]\n")
    } else {
      cat("[DIAGNOSTICS DETECTED]\n")
    }
    for (f in diag_findings) {
      icon <- switch(f$severity,
        critical = "!",
        warning  = "~",
        info     = "i",
        "?"
      )
      label <- toupper(f$severity)
      cat(sprintf("  %s %s: %s\n", icon, label, f$issue))
    }
    cat("\n")
  }

  if (using_builtin) {
    if (language == "es") {
      cat("  [!] Usando API key compartida de easyML (Groq free tier)\n")
      cat("      Limite: ~1000 consultas/dia compartidas entre usuarios.\n")
      cat("      Para uso intensivo, obten tu key gratis en:\n")
      cat("      https://console.groq.com/keys\n\n")
    } else {
      cat("  [!] Using shared easyML API key (Groq free tier)\n")
      cat("      Limit: ~1000 requests/day shared across all users.\n")
      cat("      For heavy use, get your free key at:\n")
      cat("      https://console.groq.com/keys\n\n")
    }
  }

  if (language == "es") {
    cat("  Consultando", provider, "(", model, ") ...\n")
  } else {
    cat("  Querying", provider, "(", model, ") ...\n")
  }
  cat(strrep("-", 60), "\n\n")

  # --- System prompt ---
  system_prompt <- if (language == "es") {
    paste0(
      "Eres un asistente experto en Machine Learning integrado en el paquete R easyML. ",
      "Interpretas resultados de analisis predictivos con consejo practico y directo.\n\n",
      "REGLAS ESTRICTAS:\n",
      "- TODO codigo R que sugieras DEBE usar SOLAMENTE funciones de easyML: easy_ml(), ",
      "explain_with_ai(), plot(), predict(), summary(). NUNCA sugieras paquetes externos ",
      "(caret, randomForest, pROC, glmnet, etc.). Si no existe funcion en easyML para algo, ",
      "dilo pero NO inventes codigo con otros paquetes.\n",
      "- Si el diagnostico detecta un problema CRITICAL, se directo: 'Estos resultados son ",
      "sospechosos y requieren investigacion inmediata' o 'Este dataset es trivialmente ",
      "separable'. PROHIBIDO lenguaje evasivo como 'posiblemente' o 'podria ser'.\n",
      "- Si todos los modelos logran AUC=1.0 en un dataset clasico conocido (iris, mtcars, ",
      "penguins), aclara que el dataset es trivialmente separable por naturaleza, ",
      "no necesariamente hay fuga de datos.\n",
      "- Responde MAXIMO 300 palabras. Se conciso y directo.\n",
      "- Usa texto plano. PROHIBIDO: markdown, bullets, asteriscos, negritas, listas con guiones.\n\n",
      "Estructura tu respuesta en 3 secciones:\n",
      "1. EVALUACION: Valoracion directa de resultados y diagnosticos detectados.\n",
      "2. INTERPRETACION: Metricas, variables importantes, comparacion de modelos.\n",
      "3. PROXIMOS PASOS: Recomendaciones concretas con codigo R usando SOLO easyML.\n\n",
      "Ejemplo de codigo correcto:\n",
      "  result2 <- easy_ml(data, \"target\", \"classification\", exclude_vars = c(\"var1\"))\n",
      "  result3 <- easy_ml(data, \"target\", \"classification\", use_pca = TRUE, tune = TRUE)"
    )
  } else {
    paste0(
      "You are an expert Machine Learning assistant integrated into the easyML R package. ",
      "You interpret predictive analysis results with practical, direct advice.\n\n",
      "STRICT RULES:\n",
      "- ALL R code you suggest MUST use ONLY easyML functions: easy_ml(), ",
      "explain_with_ai(), plot(), predict(), summary(). NEVER suggest external packages ",
      "(caret, randomForest, pROC, glmnet, etc.). If easyML has no function for something, ",
      "say so but DO NOT invent code with other packages.\n",
      "- If the diagnostic detects a CRITICAL issue, be direct: 'These results are suspicious ",
      "and require immediate investigation' or 'This dataset is trivially separable'. ",
      "FORBIDDEN: evasive language like 'possibly' or 'might be'.\n",
      "- If all models achieve AUC=1.0 on a well-known classic dataset (iris, mtcars, ",
      "penguins), clarify the dataset is trivially separable by nature, ",
      "not necessarily data leakage.\n",
      "- Respond in MAXIMUM 300 words. Be concise and direct.\n",
      "- Use plain text. FORBIDDEN: markdown, bullets, asterisks, bold, dash lists.\n\n",
      "Structure your response in 3 sections:\n",
      "1. ASSESSMENT: Direct evaluation of results and detected diagnostics.\n",
      "2. INTERPRETATION: Metrics, important variables, model comparison.\n",
      "3. NEXT STEPS: Concrete recommendations with R code using ONLY easyML.\n\n",
      "Example of correct code:\n",
      "  result2 <- easy_ml(data, \"target\", \"classification\", exclude_vars = c(\"var1\"))\n",
      "  result3 <- easy_ml(data, \"target\", \"classification\", use_pca = TRUE, tune = TRUE)"
    )
  }

  # --- User message ---
  user_sections <- c(
    sprintf("Task: %s | Target: %s | Best model: %s", task, target, best_model),
    sprintf("Dataset: %s", meta_text),
    "",
    "Test metrics:",
    metrics_text
  )

  if (nzchar(comparison_text)) {
    user_sections <- c(user_sections, "", "Train vs Test comparison:", comparison_text)
  }

  if (nzchar(cv_text)) {
    user_sections <- c(user_sections, "", "CV model comparison:", cv_text)
  }

  if (nzchar(importance_text)) {
    user_sections <- c(user_sections, "", "Top important variables:", importance_text)
  }

  if (nzchar(diag_text)) {
    user_sections <- c(user_sections, "", "DIAGNOSTIC FINDINGS:", diag_text)
  }

  user_message <- paste(user_sections, collapse = "\n")

  # --- API call ---
  body <- list(
    model       = model,
    temperature = 0.5,
    max_tokens  = as.integer(max_tokens),
    messages    = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_message)
    )
  )

  response <- tryCatch({
    httr::POST(
      url    = base_url,
      httr::add_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body   = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      httr::timeout(120)
    )
  }, error = function(e) {
    return(list(.error = e$message))
  })

  # Connection error
  if (is.list(response) && !is.null(response$.error)) {
    msg <- paste("Connection error:", response$.error)
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  status <- httr::status_code(response)

  # --- Handle rate limit (429) ---
  if (status == 429) {
    resp_text <- httr::content(response, "text", encoding = "UTF-8")
    wait_time <- 30
    wait_match <- regmatches(resp_text, regexpr("[0-9]+\\.?[0-9]*s", resp_text))
    if (length(wait_match) > 0) {
      parsed <- as.numeric(gsub("s$", "", wait_match[1]))
      if (!is.na(parsed)) wait_time <- ceiling(parsed) + 1
    }
    if (language == "es") {
      cat(sprintf("  Rate limit alcanzado. Esperando %d segundos...\n", wait_time))
    } else {
      cat(sprintf("  Rate limit reached. Waiting %d seconds...\n", wait_time))
    }
    Sys.sleep(wait_time)

    response <- tryCatch({
      httr::POST(
        url    = base_url,
        httr::add_headers(
          Authorization  = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body   = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(120)
      )
    }, error = function(e) {
      return(list(.error = e$message))
    })

    if (is.list(response) && !is.null(response$.error)) {
      msg <- paste("Connection error on retry:", response$.error)
      message(msg)
      return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
    }
    status <- httr::status_code(response)
  }

  # --- Handle 503 ---
  if (status == 503) {
    if (language == "es") {
      cat("  Servicio no disponible (503). Reintentando en 10s...\n")
    } else {
      cat("  Service unavailable (503). Retrying in 10s...\n")
    }
    Sys.sleep(10)
    response <- tryCatch({
      httr::POST(
        url    = base_url,
        httr::add_headers(
          Authorization  = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body   = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(120)
      )
    }, error = function(e) {
      return(list(.error = e$message))
    })
    if (is.list(response) && !is.null(response$.error)) {
      msg <- paste("Connection error on retry:", response$.error)
      message(msg)
      return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
    }
    status <- httr::status_code(response)
  }

  # --- Handle other errors ---
  if (status != 200) {
    resp_text <- httr::content(response, "text", encoding = "UTF-8")
    msg <- sprintf("API error (HTTP %d): %s", status, resp_text)
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  # --- Parse response ---
  resp_json <- tryCatch({
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                       simplifyVector = FALSE)
  }, error = function(e) {
    NULL
  })

  if (is.null(resp_json) || is.null(resp_json$choices)) {
    msg <- "Could not parse API response."
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  ai_text <- resp_json$choices[[1]]$message$content

  if (is.null(ai_text) || !nzchar(ai_text)) {
    msg <- "API returned empty response."
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  # --- Print AI response ---
  if (language == "es") {
    cat("[INTERPRETACION IA]\n\n")
  } else {
    cat("[AI INTERPRETATION]\n\n")
  }
  cat(ai_text, "\n")
  cat("\n", strrep("=", 60), "\n")
  if (language == "es") {
    cat("  Modelo:", model, "| Proveedor:", provider, "\n")
  } else {
    cat("  Model:", model, "| Provider:", provider, "\n")
  }
  cat(strrep("=", 60), "\n\n")

  invisible(list(success = TRUE, text = ai_text, diagnostics = diag_findings))
}

# =============================================================================
# Funciones de Evaluacion en Test para easyML
# =============================================================================

#' Detectar clase positiva y event_level
#' @noRd
.detect_event_level <- function(target_vector) {

  levels_y <- levels(target_vector)

  if (length(levels_y) != 2) {
    stop("La variable objetivo debe tener exactamente 2 niveles para clasificacion binaria")
  }


  # Funcion para normalizar texto: minusculas, sin tildes
.normalize_text <- function(x) {
    x <- tolower(x)
    # Remover tildes y caracteres especiales
    x <- gsub("[áàäâã]", "a", x)
    x <- gsub("[éèëê]", "e", x)
    x <- gsub("[íìïî]", "i", x)
    x <- gsub("[óòöôõ]", "o", x)
    x <- gsub("[úùüû]", "u", x)
    x <- gsub("[ñ]", "n", x)
    x
  }

  # Patrones comunes para clase positiva (español e ingles)
  positive_patterns <- c(
    # Afirmaciones basicas
    "yes", "si", "1", "true", "verdadero", "verdad",
    # Estado positivo
    "positive", "positivo", "pos", "positiva",
    # Presencia
    "present", "presente", "exists", "existe",
    # Casos/eventos
    "case", "caso", "event", "evento",
    # Enfermedad/condicion
    "disease", "enfermo", "enferma", "enfermedad", "sick", "ill",
    "patient", "paciente", "affected", "afectado", "afectada",
    "diagnosed", "diagnosticado", "diagnosticada",
    # Exito/logro
    "success", "exito", "exitoso", "exitosa", "successful",
    "approved", "aprobado", "aprobada", "passed", "aprobado",
    "accepted", "aceptado", "aceptada",
    # Supervivencia
    "survived", "sobrevivio", "alive", "vivo", "viva",
    # Riesgo/alto
    "high", "alto", "alta", "risk", "riesgo",
    # Ocurrencia
    "occurred", "ocurrio", "happened", "sucedio",
    "detected", "detectado", "detectada",
    # Respuesta
    "response", "respuesta", "responded", "respondio",
    "reactive", "reactivo", "reactiva",
    # Compra/conversion
    "buy", "compra", "compro", "purchased", "bought",
    "converted", "convertido", "convertida", "conversion",
    "churned", "churn", "abandono",
    # Fraude/anomalia
    "fraud", "fraude", "fraudulent", "fraudulento",
    "anomaly", "anomalia", "suspicious", "sospechoso",
    # Otros comunes
    "winner", "ganador", "ganadora", "won", "gano",
    "default", "impago", "moroso", "morosa",
    "pregnant", "embarazada", "embarazo",
    "malignant", "maligno", "maligna", "cancer",
    "spam", "recurrence", "recurrencia", "recidiva",
    "dropout", "desercion", "desertor"
  )

  # Normalizar niveles
  level1_norm <- .normalize_text(levels_y[1])
  level2_norm <- .normalize_text(levels_y[2])

  # Determinar cual es la clase positiva
  level2_is_positive <- any(sapply(positive_patterns, function(p) grepl(p, level2_norm, fixed = TRUE)))
  level1_is_positive <- any(sapply(positive_patterns, function(p) grepl(p, level1_norm, fixed = TRUE)))

  # Si level2 parece positivo, usar event_level = "second"
  # Si level1 parece positivo, usar event_level = "first"
  # Si ninguno es claro, asumir que level2 es positivo (convencion comun)
  if (level2_is_positive && !level1_is_positive) {
    event_level <- "second"
    positive_class <- levels_y[2]
  } else if (level1_is_positive && !level2_is_positive) {
    event_level <- "first"
    positive_class <- levels_y[1]
  } else {
    # Por defecto, asumir segundo nivel como positivo
    event_level <- "second"
    positive_class <- levels_y[2]
  }

  list(
    levels = levels_y,
    positive_class = positive_class,
    negative_class = ifelse(positive_class == levels_y[1], levels_y[2], levels_y[1]),
    event_level = event_level,
    prob_col = paste0(".pred_", positive_class)
  )
}


#' @title Evaluacion del Modelo Final en Test
#'
#' @description
#' Entrena el modelo final y evalua su rendimiento en el conjunto de test.
#'
#' @param tuning_result Resultado de tune_best_model() o modeling_result
#' @param train_data Datos de entrenamiento
#' @param test_data Datos de test
#' @param target Variable objetivo
#' @param task Tipo de tarea
#' @param verbose Mostrar progreso
#'
#' @return Lista con resultados de evaluacion
#' @export
evaluate_model <- function(tuning_result,
                           train_data,
                           test_data,
                           target,
                           task,
                           verbose = TRUE) {

  if (verbose) {
    .print_section(5, "Evaluacion en Test")
  }

  results <- list()

  # Obtener workflow final
  if (inherits(tuning_result, "easyml_tuning")) {
    final_wf <- tuning_result$final_workflow
  } else {
    final_wf <- tuning_result$final_workflow
  }

  # 5.1 Entrenar Modelo Final
  if (verbose) .print_subsection(5, 1, "Entrenar Modelo Final")

  if (verbose) {
    cat("    El modelo final se entrena usando TODOS los datos de entrenamiento.\n")
    cat("    Esto permite que el modelo aprenda de la mayor cantidad de datos\n")
    cat("    posible antes de ser evaluado en el conjunto de test.\n\n")
  }

  final_fit <- parsnip::fit(final_wf, data = train_data)
  results$final_fit <- final_fit

  if (verbose) {
    cat("    Modelo entrenado con", nrow(train_data), "observaciones\n")
  }

  # 5.2 Predicciones en Test
  if (verbose) {
    .print_subsection(5, 2, "Predicciones en Test")
    cat("    El conjunto de test contiene datos que el modelo NUNCA ha visto\n")
    cat("    durante el entrenamiento. Esto nos da una estimacion honesta de\n")
    cat("    como se comportara el modelo con datos nuevos en el mundo real.\n\n")
  }

  predictions <- eval_predictions(final_fit, test_data, target, task, verbose)
  results$predictions <- predictions
  if (verbose) .print_reference("train_test")

  # 5.3 Metricas de Rendimiento
  if (verbose) .print_subsection(5, 3, "Metricas de Rendimiento")

  metrics <- eval_metrics(predictions, target, task, verbose)
  results$metrics <- metrics
  if (verbose) .print_reference("roc_auc")

  # 5.4 Diagnostico de Overfitting (Train vs Test)
  if (verbose) .print_subsection(5, 4, "Diagnostico de Overfitting")
  overfitting_diag <- .diagnose_overfitting(
    final_fit = final_fit,
    train_data = train_data,
    test_metrics = metrics,
    target = target,
    task = task,
    verbose = verbose
  )
  results$train_metrics <- overfitting_diag$train_metrics
  results$overfitting <- overfitting_diag

  if (task == "classification") {
    # 5.5 Curva ROC
    if (verbose) .print_subsection(5, 5, "Curva ROC")
    results$roc_data <- eval_roc(predictions, target, verbose)
    if (verbose) .print_reference("roc_auc")

    # 5.6 Matriz de Confusion
    if (verbose) .print_subsection(5, 6, "Matriz de Confusion")
    results$confusion <- eval_confusion(predictions, target, verbose)
    if (verbose) .print_reference("confusion_matrix")

    # 5.7 Calibracion
    if (verbose) .print_subsection(5, 7, "Analisis de Calibracion")
    results$calibration <- eval_calibration(predictions, target, verbose)
    if (verbose) .print_reference("calibration")

  } else {
    # 5.5 Predicciones vs Observaciones
    if (verbose) .print_subsection(5, 5, "Predicciones vs Observaciones")
    results$pred_obs <- eval_pred_vs_obs(predictions, target, verbose)
    if (verbose) .print_reference("r_squared")

    # 5.6 Analisis de Residuos
    if (verbose) .print_subsection(5, 6, "Analisis de Residuos")
    results$residuals <- eval_residuals(predictions, target, verbose)
    if (verbose) .print_reference("residuals")
  }

  results$task <- task
  results$target <- target
  class(results) <- c("easyml_evaluation", "list")

  return(results)
}


#' @title Generar Predicciones
#' @export
eval_predictions <- function(final_fit, test_data, target, task, verbose = TRUE) {

  # Predicciones de clase/valor
  preds <- stats::predict(final_fit, new_data = test_data)

  if (task == "classification") {
    # Predicciones de probabilidad
    probs <- stats::predict(final_fit, new_data = test_data, type = "prob")
    predictions <- dplyr::bind_cols(test_data, preds, probs)
  } else {
    predictions <- dplyr::bind_cols(test_data, preds)
  }

  if (verbose) {
    cat("    Predicciones generadas:", nrow(predictions), "observaciones\n")
  }

  predictions
}


#' @title Calcular Metricas
#' @export
eval_metrics <- function(predictions, target, task, verbose = TRUE) {

  if (task == "classification") {
    # Detectar clase positiva y event_level
    event_info <- .detect_event_level(predictions[[target]])

    if (verbose) {
      cat("    Estas son las metricas calculadas en el conjunto de TEST.\n")
      cat("    Representan el rendimiento real del modelo con datos nuevos.\n\n")

      cat("    Configuracion de clases:\n")
      cat("      - Clase positiva (lo que queremos detectar):", event_info$positive_class, "\n")
      cat("      - Clase negativa:", event_info$negative_class, "\n\n")
    }

    # F2-Score: F-beta con beta=2 (prioriza recall sobre precision)
    f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)

    metrics <- dplyr::bind_rows(
      yardstick::roc_auc(predictions, truth = !!rlang::sym(target),
                         !!rlang::sym(event_info$prob_col),
                         event_level = event_info$event_level),
      yardstick::accuracy(predictions, truth = !!rlang::sym(target),
                          estimate = .pred_class),
      yardstick::sensitivity(predictions, truth = !!rlang::sym(target),
                             estimate = .pred_class,
                             event_level = event_info$event_level),
      yardstick::specificity(predictions, truth = !!rlang::sym(target),
                             estimate = .pred_class,
                             event_level = event_info$event_level),
      yardstick::f_meas(predictions, truth = !!rlang::sym(target),
                        estimate = .pred_class,
                        event_level = event_info$event_level),
      yardstick::mcc(predictions, truth = !!rlang::sym(target),
                     estimate = .pred_class),
      f2_meas(predictions, truth = !!rlang::sym(target),
              estimate = .pred_class,
              event_level = event_info$event_level)
    )
  } else {
    metrics <- dplyr::bind_rows(
      yardstick::rmse(predictions, truth = !!rlang::sym(target),
                      estimate = .pred),
      yardstick::rsq(predictions, truth = !!rlang::sym(target),
                     estimate = .pred),
      yardstick::mae(predictions, truth = !!rlang::sym(target),
                     estimate = .pred)
    )
  }

  if (verbose) {
    cat("    Metricas en Test:\n")
    for (i in 1:nrow(metrics)) {
      metric_name <- metrics$.metric[i]
      metric_value <- metrics$.estimate[i]
      interpretation <- .interpret_metric_value(metric_name, metric_value)
      cat("      -", metric_name, ":", round(metric_value, 4), "\n")
      if (nchar(interpretation) > 0) {
        cat("        ", interpretation, "\n")
      }
    }

    if (task == "classification") {
      cat("\n    Leyenda rapida:\n")
      cat("      - roc_auc: Capacidad de distinguir entre clases (0.5=azar, 1=perfecto)\n")
      cat("      - accuracy: Porcentaje de predicciones correctas\n")
      cat("      - sensitivity: Porcentaje de positivos detectados correctamente\n")
      cat("      - specificity: Porcentaje de negativos detectados correctamente\n")
      cat("      - f_meas: Balance entre precision y sensibilidad (F1)\n")
      cat("      - mcc: Correlacion prediccion-realidad, robusto a desbalance (-1 a 1)\n")
      cat("      - f2_meas: Como F1 pero prioriza detectar positivos (F2)\n")
    } else {
      cat("\n    Leyenda rapida:\n")
      cat("      - rmse: Error promedio (penaliza errores grandes)\n")
      cat("      - rsq: Porcentaje de variacion explicada (0-1)\n")
      cat("      - mae: Error promedio absoluto\n")
    }
  }

  metrics
}


#' @title Calcular Curva ROC
#' @export
eval_roc <- function(predictions, target, verbose = TRUE) {

  # Detectar clase positiva y event_level
  event_info <- .detect_event_level(predictions[[target]])

  roc_data <- yardstick::roc_curve(
    predictions,
    truth = !!rlang::sym(target),
    !!rlang::sym(event_info$prob_col),
    event_level = event_info$event_level
  )

  auc <- yardstick::roc_auc(
    predictions,
    truth = !!rlang::sym(target),
    !!rlang::sym(event_info$prob_col),
    event_level = event_info$event_level
  )$.estimate

  if (verbose) {
    cat("    La curva ROC muestra el balance entre detectar positivos correctamente\n")
    cat("    (sensitivity) y evitar falsas alarmas (1 - specificity) a diferentes\n")
    cat("    umbrales de decision. Un modelo perfecto tendria la curva pegada a la\n")
    cat("    esquina superior izquierda.\n\n")

    cat("    AUC (Area Bajo la Curva):", round(auc, 4), "\n")
    interpretation <- .interpret_metric_value("roc_auc", auc)
    cat("    Interpretacion:", interpretation, "\n\n")

    cat("    Nota: La curva ROC se incluye siempre como herramienta de diagnostico\n")
    cat("    porque permite visualizar el rendimiento del modelo en todos los\n")
    cat("    umbrales de decision posibles, no solo en el umbral por defecto (0.5).\n")
    cat("    Esto es util si necesitas ajustar el punto de corte segun tu caso de uso.\n")
  }

  list(curve = roc_data, auc = auc, event_info = event_info)
}


#' @title Matriz de Confusion
#' @export
eval_confusion <- function(predictions, target, verbose = TRUE) {

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])

  conf_mat <- yardstick::conf_mat(
    predictions,
    truth = !!rlang::sym(target),
    estimate = .pred_class
  )

  if (verbose) {
    cat("    La matriz de confusion muestra como se distribuyen las predicciones\n")
    cat("    del modelo comparadas con los valores reales (Truth).\n\n")

    cat("    Clase positiva:", event_info$positive_class, "\n")
    cat("    Clase negativa:", event_info$negative_class, "\n\n")

    print(conf_mat)

    # Extraer valores de la matriz
    mat <- conf_mat$table
    tn <- mat[1, 1]  # Verdaderos negativos
    fp <- mat[2, 1]  # Falsos positivos
    fn <- mat[1, 2]  # Falsos negativos
    tp <- mat[2, 2]  # Verdaderos positivos

    cat("\n    Como leer la matriz:\n")
    cat("      - Verdaderos Negativos (", tn, "): Predijo '", event_info$negative_class,
        "' y era '", event_info$negative_class, "' (acierto)\n", sep = "")
    cat("      - Verdaderos Positivos (", tp, "): Predijo '", event_info$positive_class,
        "' y era '", event_info$positive_class, "' (acierto)\n", sep = "")
    cat("      - Falsos Positivos (", fp, "): Predijo '", event_info$positive_class,
        "' pero era '", event_info$negative_class, "' (error)\n", sep = "")
    cat("      - Falsos Negativos (", fn, "): Predijo '", event_info$negative_class,
        "' pero era '", event_info$positive_class, "' (error)\n", sep = "")
  }

  conf_mat
}


#' @title Analisis de Calibracion
#' @export
eval_calibration <- function(predictions, target, verbose = TRUE) {

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])

  # Crear bins de probabilidad
  predictions$prob_bin <- cut(
    predictions[[event_info$prob_col]],
    breaks = seq(0, 1, by = 0.1),
    include.lowest = TRUE
  )

  # Calcular proporcion observada vs predicha
  calibration <- predictions |>
    dplyr::group_by(prob_bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_pred = mean(!!rlang::sym(event_info$prob_col), na.rm = TRUE),
      mean_obs = mean(as.numeric(!!rlang::sym(target) == event_info$positive_class), na.rm = TRUE),
      .groups = "drop"
    )

  if (verbose) {
    cat("    La calibracion mide si las probabilidades del modelo son confiables.\n")
    cat("    Un modelo bien calibrado deberia cumplir: cuando predice 70% de\n")
    cat("    probabilidad, aproximadamente 70% de esos casos deberian ser positivos.\n\n")

    cat("    Calibracion por deciles de probabilidad:\n\n")
    print(as.data.frame(calibration))

    cat("\n    Como leer la tabla:\n")
    cat("      - prob_bin: Rango de probabilidad predicha\n")
    cat("      - n: Cantidad de casos en ese rango\n")
    cat("      - mean_pred: Probabilidad promedio predicha por el modelo\n")
    cat("      - mean_obs: Proporcion real de positivos observados\n\n")

    cat("    Interpretacion: Si mean_pred y mean_obs son similares, el modelo\n")
    cat("    esta bien calibrado. Si mean_obs es mayor, el modelo subestima.\n")
    cat("    Si mean_obs es menor, el modelo sobreestima.\n")
  }

  calibration
}


#' @title Predicciones vs Observaciones
#' @export
eval_pred_vs_obs <- function(predictions, target, verbose = TRUE) {

  obs <- predictions[[target]]
  pred <- predictions$.pred

  # Calcular estadisticas
  correlation <- cor(obs, pred, use = "complete.obs")
  bias <- mean(pred - obs, na.rm = TRUE)

  if (verbose) {
    cat("    Correlacion obs-pred:", round(correlation, 4), "\n")
    cat("    Sesgo medio:", round(bias, 4), "\n")
  }

  list(
    observed = obs,
    predicted = pred,
    correlation = correlation,
    bias = bias
  )
}


#' @title Analisis de Residuos
#' @export
eval_residuals <- function(predictions, target, verbose = TRUE) {

  obs <- predictions[[target]]
  pred <- predictions$.pred
  residuals <- obs - pred

  # Estadisticas de residuos
  stats_resid <- list(
    mean = mean(residuals, na.rm = TRUE),
    sd = sd(residuals, na.rm = TRUE),
    min = min(residuals, na.rm = TRUE),
    max = max(residuals, na.rm = TRUE),
    median = median(residuals, na.rm = TRUE)
  )

  # Test de normalidad
  if (length(residuals) <= 5000) {
    shapiro <- shapiro.test(residuals)
  } else {
    shapiro <- shapiro.test(sample(residuals, 5000))
  }

  if (verbose) {
    cat("    Estadisticas de residuos:\n")
    cat("      - Media:", round(stats_resid$mean, 4), "\n")
    cat("      - DE:", round(stats_resid$sd, 4), "\n")
    cat("      - Rango: [", round(stats_resid$min, 2), ",",
        round(stats_resid$max, 2), "]\n")
    cat("    Shapiro-Wilk p-value:", format.pval(shapiro$p.value), "\n")
  }

  list(
    residuals = residuals,
    stats = stats_resid,
    normality_test = shapiro
  )
}


#' @title Diagnostico de Overfitting (Train vs Test)
#' @noRd
.diagnose_overfitting <- function(final_fit, train_data, test_metrics, target, task, verbose = TRUE) {

  # Calcular predicciones y metricas en train
  train_preds <- eval_predictions(final_fit, train_data, target, task, verbose = FALSE)
  train_metrics <- eval_metrics(train_preds, target, task, verbose = FALSE)

  # Metricas donde mayor es mejor vs menor es mejor
  lower_is_better <- c("rmse", "mae")

  # Construir tabla comparativa
  comparison <- merge(
    train_metrics[, c(".metric", ".estimate")],
    test_metrics[, c(".metric", ".estimate")],
    by = ".metric",
    suffixes = c("_train", "_test")
  )
  names(comparison) <- c("metric", "train", "test")

  # Calcular gap
  comparison$gap <- ifelse(
    comparison$metric %in% lower_is_better,
    comparison$test - comparison$train,  # Para error: test > train = overfitting
    comparison$train - comparison$test    # Para accuracy: train > test = overfitting
  )

  if (verbose) {
    cat("    Se comparan las metricas del modelo en los datos de entrenamiento\n")
    cat("    (train) vs los datos de prueba (test). Una gran diferencia sugiere\n")
    cat("    que el modelo memorizo los datos en lugar de aprender patrones.\n\n")

    cat("    Comparacion Train vs Test:\n\n")
    comp_display <- comparison
    comp_display$train <- round(comp_display$train, 4)
    comp_display$test <- round(comp_display$test, 4)
    comp_display$gap <- round(comp_display$gap, 4)
    print(as.data.frame(comp_display), row.names = FALSE)

    # Diagnostico global: usar metrica donde mayor = mejor
    # Clasificacion: roc_auc, Regresion: rsq (R-squared)
    # Nota: RMSE gap% no discrimina bien en modelos de arboles (~50% siempre)
    #       R-squared gap si discrimina correctamente
    # Umbrales calibrados con simulacion Monte Carlo (N=100 por escenario)
    main_metric <- if (task == "classification") "roc_auc" else "rsq"
    main_row <- comparison[comparison$metric == main_metric, ]

    if (nrow(main_row) > 0) {
      gap <- main_row$gap[1]

      cat("\n    Diagnostico (basado en", main_metric, ", gap = train - test):\n")

      if (gap < 0.15) {
        cat("    [ok] Sin indicios de overfitting (gap:", round(gap, 4), ")\n")
      } else if (gap <= 0.30) {
        cat("    [~] Posible overfitting leve (gap:", round(gap, 4), ")\n")
        cat("    Considere simplificar el modelo o aumentar los datos\n")
      } else {
        cat("    [!] ADVERTENCIA: Overfitting detectado (gap:", round(gap, 4), ")\n")
        cat("    El modelo memoriza los datos de entrenamiento\n")
        .print_overfitting_suggestions()
      }
    }
    cat("\n")
  }

  list(
    train_metrics = train_metrics,
    comparison = comparison
  )
}


#' @title Imprimir sugerencias ante overfitting
#' @noRd
.print_overfitting_suggestions <- function() {
  cat("\n    Sugerencias:\n")
  cat("    - Reducir la complejidad del modelo (menos arboles, mayor regularizacion)\n")
  cat("    - Aumentar los datos de entrenamiento\n")
  cat("    - Activar feature_selection = TRUE para eliminar variables irrelevantes\n")
  cat("    - Verificar si alguna variable contiene informacion del target (data leakage)\n")
}

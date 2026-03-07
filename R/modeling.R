# =============================================================================
# Funciones de Modelado para easyML
# =============================================================================

#' @title Pipeline de Modelado con Validacion Cruzada
#'
#' @description
#' Entrena multiples modelos usando validacion cruzada y compara resultados.
#'
#' @param preprocess_result Resultado de preprocess_data()
#' @param models Vector con modelos a entrenar
#' @param cv_folds Numero de folds para CV
#' @param select_metric Metrica para seleccionar mejor modelo
#' @param seed Semilla
#' @param verbose Mostrar progreso
#'
#' @return Lista con resultados de modelado
#' @export
train_models <- function(preprocess_result,
                         models = c("rf", "xgboost", "glm"),
                         cv_folds = 10,
                         select_metric = NULL,
                         seed = 2024,
                         verbose = TRUE) {

  set.seed(seed)

  task <- preprocess_result$task
  target <- preprocess_result$target
  train_data <- preprocess_result$train_data
  recipe <- preprocess_result$recipe

  # Establecer metrica por defecto si no se especifica
  if (is.null(select_metric)) {
    select_metric <- if (task == "classification") "roc_auc" else "rmse"
  }

  if (verbose) {
    .print_section(3, "Modelado")
  }

  results <- list()

  # 3.1 Configurar CV
  if (verbose) .print_subsection(3, 1, "Configurar Validacion Cruzada")
  cv_result <- setup_cv(train_data, target, task, cv_folds, seed, verbose)
  results$cv_folds <- cv_result
  if (verbose) .print_reference("cv")

  # 3.2 Definir Modelos
  if (verbose) .print_subsection(3, 2, "Definir Modelos")
  model_specs <- define_models(models, task, verbose)
  results$model_specs <- model_specs

  # 3.3 Entrenar Modelos
  if (verbose) .print_subsection(3, 3, "Entrenar y Evaluar Modelos")
  cv_results <- fit_models_cv(
    model_specs = model_specs,
    recipe = recipe,
    cv_folds = cv_result,
    task = task,
    verbose = verbose
  )
  results$cv_results <- cv_results

  # 3.4 Comparar Modelos
  if (verbose) .print_subsection(3, 4, "Comparar Modelos")
  comparison <- compare_models(cv_results, task, select_metric, verbose)
  if (verbose) .print_reference("roc_auc")
  results$comparison <- comparison
  results$best_model <- comparison$best_model

  results$task <- task
  results$target <- target
  results$recipe <- recipe
  results$train_data <- train_data
  class(results) <- c("easyml_modeling", "list")

  return(results)
}


#' @title Configurar Validacion Cruzada
#' @export
setup_cv <- function(train_data, target, task, cv_folds = 10, seed = 2024, verbose = TRUE) {

  set.seed(seed)

  if (task == "classification") {
    folds <- rsample::vfold_cv(
      train_data,
      v = cv_folds,
      strata = !!rlang::sym(target)
    )
    cv_method <- "stratified"
  } else {
    folds <- rsample::vfold_cv(train_data, v = cv_folds)
    cv_method <- "simple"
  }

  if (verbose) {
    cat("    La validacion cruzada divide los datos en K partes (folds) para\n")
    cat("    evaluar el modelo de forma robusta. El modelo se entrena K veces,\n")
    cat("    cada vez usando K-1 folds para entrenar y 1 fold para validar.\n\n")

    if (cv_method == "stratified") {
      cat("    Metodo: K-Fold Cross-Validation Estratificado\n")
      cat("      - Folds:", cv_folds, "\n")
      cat("      - Estratificado por:", target, "\n")
      cat("      - Observaciones por fold: ~", round(nrow(train_data) / cv_folds), "\n")
      cat("      (Mantiene la proporcion de clases en cada fold)\n")
    } else {
      cat("    Metodo: K-Fold Cross-Validation Simple\n")
      cat("      - Folds:", cv_folds, "\n")
      cat("      - Observaciones por fold: ~", round(nrow(train_data) / cv_folds), "\n")
      cat("      (Division aleatoria sin estratificacion)\n")
    }
  }

  folds
}


#' @title Definir Especificaciones de Modelos
#' @export
define_models <- function(models, task, verbose = TRUE) {

  model_specs <- list()

  # Descripciones de cada modelo
  model_descriptions <- list(
    rf = "Ensemble de arboles de decision con muestreo aleatorio",
    xgboost = "Gradient boosting optimizado, entrena arboles secuencialmente",
    glm = if (task == "classification") "Modelo lineal para probabilidades (regresion logistica)" else "Modelo lineal clasico para predecir valores continuos",
    svm = "Busca el hiperplano optimo para separar clases o predecir valores",
    nnet = "Modelo inspirado en neuronas, captura relaciones no lineales",
    tree = "Arbol de decision simple, facil de interpretar",
    nb = "Clasificador probabilistico basado en el teorema de Bayes"
  )

  for (model_name in models) {
    spec <- .get_model_spec(model_name, task)
    model_specs[[model_name]] <- spec

    if (verbose) {
      cat("    -", .get_model_label(model_name), "\n")
      if (model_name %in% names(model_descriptions)) {
        cat("       ", model_descriptions[[model_name]], "\n")
      }
    }
  }

  model_specs
}


#' @title Entrenar Modelos con CV
#' @export
fit_models_cv <- function(model_specs, recipe, cv_folds, task, verbose = TRUE) {

  # Definir metricas
  if (task == "classification") {
    # F2-Score: F-beta con beta=2 (prioriza recall sobre precision)
    f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)

    metrics_set <- yardstick::metric_set(
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::sensitivity,
      yardstick::specificity,
      yardstick::f_meas,
      yardstick::bal_accuracy,
      yardstick::pr_auc,
      yardstick::mcc,
      f2_meas
    )
  } else {
    metrics_set <- yardstick::metric_set(
      yardstick::rmse,
      yardstick::rsq,
      yardstick::mae
    )
  }

  cv_results <- list()

  for (model_name in names(model_specs)) {
    if (verbose) {
      cat("    Entrenando:", .get_model_label(model_name), "...")
    }

    # Crear workflow
    wf <- workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(model_specs[[model_name]])

    # Entrenar con CV (suprimir mensajes informativos)
    cv_result <- suppressMessages(
      tune::fit_resamples(
        wf,
        resamples = cv_folds,
        metrics = metrics_set,
        control = tune::control_resamples(save_pred = TRUE)
      )
    )

    cv_results[[model_name]] <- cv_result

    if (verbose) {
      metrics <- tune::collect_metrics(cv_result)
      if (task == "classification") {
        auc <- metrics$mean[metrics$.metric == "roc_auc"]
        cat(" AUC =", round(auc, 3), "\n")
      } else {
        rmse <- metrics$mean[metrics$.metric == "rmse"]
        cat(" RMSE =", round(rmse, 3), "\n")
      }
    }
  }

  cv_results
}


#' @title Comparar Resultados de Modelos
#' @export
compare_models <- function(cv_results, task, select_metric = NULL, verbose = TRUE) {

  # Establecer metrica por defecto
  if (is.null(select_metric)) {
    select_metric <- if (task == "classification") "roc_auc" else "rmse"
  }

  # Etiquetas legibles para las metricas
  metric_labels <- c(
    roc_auc = "ROC-AUC",
    f_meas = "F1-Score",
    f2_meas = "F2-Score",
    accuracy = "Accuracy",
    sensitivity = "Sensitivity",
    specificity = "Specificity",
    bal_accuracy = "Balanced Accuracy",
    pr_auc = "PR-AUC",
    mcc = "MCC",
    rmse = "RMSE",
    rsq = "R-squared",
    mae = "MAE"
  )

  # Descripciones de cuando usar cada metrica
  metric_descriptions <- c(
    roc_auc = "Buena para desbalance moderado, evalua discriminacion global",
    f_meas = "Balance entre precision y recall, buena para desbalance",
    f2_meas = "Como F1 pero prioriza recall (detectar positivos), ideal cuando perder un positivo es costoso",
    accuracy = "Simple pero sensible al desbalance de clases",
    sensitivity = "Prioriza detectar todos los positivos (recall)",
    specificity = "Prioriza evitar falsos positivos",
    bal_accuracy = "Promedio de sensitivity y specificity",
    pr_auc = "Ideal para desbalance severo (clases muy desiguales)",
    mcc = "Correlacion entre prediccion y realidad, robusto a desbalance (-1 a 1)",
    rmse = "Penaliza errores grandes, en unidades originales",
    rsq = "Proporcion de varianza explicada (0-1)",
    mae = "Error promedio absoluto, robusto a outliers"
  )

  # Metricas que se maximizan vs minimizan
  maximize_metrics <- c("roc_auc", "f_meas", "f2_meas", "accuracy", "sensitivity",
                        "specificity", "bal_accuracy", "pr_auc", "mcc", "rsq")

  # Recopilar metricas de todos los modelos
  all_metrics <- lapply(names(cv_results), function(model_name) {
    metrics <- tune::collect_metrics(cv_results[[model_name]])
    metrics$model <- model_name
    metrics
  })

  metrics_df <- do.call(rbind, all_metrics)

  # Crear resumen
  summary_df <- metrics_df |>
    dplyr::select(model, .metric, mean) |>
    tidyr::pivot_wider(names_from = .metric, values_from = mean)

  # Ordenar segun la metrica seleccionada
  if (select_metric %in% maximize_metrics) {
    summary_df <- summary_df |>
      dplyr::arrange(dplyr::desc(!!rlang::sym(select_metric)))
  } else {
    summary_df <- summary_df |>
      dplyr::arrange(!!rlang::sym(select_metric))
  }

  best_model <- summary_df$model[1]
  best_metric_value <- summary_df[[select_metric]][1]

  if (verbose) {
    cat("    Metrica de seleccion:", metric_labels[select_metric], "\n\n")
    cat("    Resumen de modelos:\n\n")
    print(as.data.frame(summary_df))

    # Mostrar leyenda de metricas
    cat("\n    Leyenda de metricas:\n")
    if (task == "classification") {
      cat("      - accuracy:     Proporcion de predicciones correctas\n")
      cat("      - bal_accuracy: Promedio de sensitivity y specificity\n")
      cat("      - f_meas:       F1-Score, balance entre precision y recall\n")
      cat("      - f2_meas:      F2-Score, como F1 pero prioriza recall\n")
      cat("      - mcc:          Matthews Correlation Coefficient (-1 a 1)\n")
      cat("      - pr_auc:       Area bajo curva Precision-Recall\n")
      cat("      - roc_auc:      Area bajo curva ROC\n")
      cat("      - sensitivity:  Tasa de verdaderos positivos (recall)\n")
      cat("      - specificity:  Tasa de verdaderos negativos\n")
    } else {
      cat("      - rmse: Error cuadratico medio (penaliza errores grandes)\n")
      cat("      - rsq:  R-cuadrado, proporcion de varianza explicada\n")
      cat("      - mae:  Error absoluto medio (robusto a outliers)\n")
    }

    cat("\n    Mejor modelo seleccionado por:", metric_labels[select_metric], "\n")
    cat("    Mejor modelo:", .get_model_label(best_model), "\n")
    cat("   ", metric_labels[select_metric], ":", round(best_metric_value, 4), "\n")

    # Interpretacion del valor de la metrica
    interpretation <- .interpret_metric_value(select_metric, best_metric_value)
    cat("    Interpretacion:", interpretation, "\n\n")

    # Diagnostico de estabilidad entre folds (varianza CV)
    best_row <- metrics_df[metrics_df$model == best_model & metrics_df$.metric == select_metric, ]
    if (nrow(best_row) > 0 && !is.na(best_row$std_err[1]) && best_row$mean[1] != 0) {
      n_folds <- best_row$n[1]
      std_err_val <- best_row$std_err[1]
      mean_val <- best_row$mean[1]
      cv_pct <- abs(std_err_val * sqrt(n_folds) / mean_val) * 100

      cat("    Estabilidad entre folds (CV%): ", round(cv_pct, 1), "%\n", sep = "")
      if (cv_pct < 5) {
        cat("    [ok] Modelo estable entre folds\n\n")
      } else if (cv_pct <= 15) {
        cat("    [~] Variabilidad moderada entre folds -- el modelo podria ser inestable\n\n")
      } else {
        cat("    [!] ADVERTENCIA: Alta variabilidad entre folds -- rendimiento inestable\n")
        cat("    Considere: aumentar los datos de entrenamiento o simplificar el modelo\n\n")
      }
    }

    cat("    Nota: El", metric_labels[select_metric], "es", metric_descriptions[select_metric], "\n")
  }

  list(
    summary = as.data.frame(summary_df),
    all_metrics = metrics_df,
    best_model = best_model,
    best_metric = best_metric_value,
    select_metric = select_metric
  )
}


#' @title Obtener etiqueta legible del modelo
#' @noRd
.get_model_label <- function(model_name) {
  labels <- c(
    rf = "Random Forest",
    xgboost = "XGBoost",
    svm = "SVM (RBF)",
    nnet = "Red Neuronal",
    glm = "Regresion Logistica/Lineal",
    tree = "Arbol de Decision",
    nb = "Naive Bayes"
  )
  if (model_name %in% names(labels)) {
    return(labels[[model_name]])
  }
  return(model_name)
}


#' @title Interpretar valor de metrica
#' @description Proporciona interpretaciones claras y accesibles para usuarios no expertos
#' @noRd
.interpret_metric_value <- function(metric, value) {

  # ROC-AUC y PR-AUC: miden que tan bien el modelo distingue entre clases
  if (metric %in% c("roc_auc", "pr_auc")) {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.90) return("Excelente: el modelo distingue muy bien entre las clases")
    if (value >= 0.80) return("Bueno: el modelo distingue bien entre las clases")
    if (value >= 0.70) return("Aceptable: el modelo distingue moderadamente entre las clases")
    if (value >= 0.60) return("Debil: el modelo tiene dificultad para distinguir las clases")
    return("Insuficiente: el modelo no logra distinguir las clases correctamente")
  }

  # Accuracy y Balanced Accuracy: proporcion de aciertos
  if (metric %in% c("accuracy", "bal_accuracy")) {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: ", pct, "% de acierto es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: el modelo acierta en ", pct, "% de los casos"))
    if (value >= 0.80) return(paste0("Bueno: el modelo acierta en ", pct, "% de los casos"))
    if (value >= 0.70) return(paste0("Aceptable: el modelo acierta en ", pct, "% de los casos"))
    if (value >= 0.60) return(paste0("Moderado: el modelo acierta en ", pct, "% de los casos"))
    return(paste0("Bajo: el modelo solo acierta en ", pct, "% de los casos"))
  }

  # MCC: correlacion entre prediccion y realidad (-1 a 1)
  if (metric == "mcc") {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.70) return("Excelente: alta correlacion entre predicciones y valores reales")
    if (value >= 0.50) return("Bueno: buena correlacion entre predicciones y valores reales")
    if (value >= 0.30) return("Aceptable: correlacion moderada entre predicciones y valores reales")
    if (value >= 0.10) return("Debil: baja correlacion entre predicciones y valores reales")
    if (value >= 0) return("Muy debil: el modelo apenas supera al azar")
    return("Negativo: el modelo predice peor que el azar (predicciones inversas)")
  }

  # F2-Score: como F1 pero prioriza recall (detectar positivos)
  if (metric == "f2_meas") {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.90) return("Excelente: detecta casi todos los positivos con buena precision")
    if (value >= 0.80) return("Bueno: buena deteccion de positivos con precision razonable")
    if (value >= 0.70) return("Aceptable: detecta la mayoria de positivos pero con algunos errores")
    if (value >= 0.60) return("Moderado: pierde algunos positivos o tiene errores frecuentes")
    return("Bajo: pierde muchos positivos o comete demasiados errores")
  }

  # F1-Score: equilibrio entre encontrar todos los positivos y no equivocarse
  if (metric == "f_meas") {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.90) return("Excelente: encuentra los positivos sin casi equivocarse")
    if (value >= 0.80) return("Bueno: buen equilibrio entre encontrar positivos y precision")
    if (value >= 0.70) return("Aceptable: equilibrio moderado entre encontrar positivos y precision")
    if (value >= 0.60) return("Moderado: puede perder positivos o tener algunos errores")
    return("Bajo: pierde muchos positivos o comete muchos errores")
  }

  # Sensitivity: capacidad de detectar los casos positivos
  if (metric == "sensitivity") {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: ", pct, "% de sensitivity es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: detecta el ", pct, "% de los casos positivos"))
    if (value >= 0.80) return(paste0("Bueno: detecta el ", pct, "% de los casos positivos"))
    if (value >= 0.70) return(paste0("Aceptable: detecta el ", pct, "% de los casos positivos"))
    if (value >= 0.60) return(paste0("Moderado: detecta el ", pct, "% de los casos positivos"))
    return(paste0("Bajo: solo detecta el ", pct, "% de los casos positivos"))
  }

  # Specificity: capacidad de detectar los casos negativos
  if (metric == "specificity") {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: ", pct, "% de specificity es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: identifica correctamente el ", pct, "% de los negativos"))
    if (value >= 0.80) return(paste0("Bueno: identifica correctamente el ", pct, "% de los negativos"))
    if (value >= 0.70) return(paste0("Aceptable: identifica correctamente el ", pct, "% de los negativos"))
    if (value >= 0.60) return(paste0("Moderado: identifica correctamente el ", pct, "% de los negativos"))
    return(paste0("Bajo: solo identifica correctamente el ", pct, "% de los negativos"))
  }

  # R-squared: cuanto de la variabilidad explica el modelo
  if (metric == "rsq") {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: R-squared de ", pct, "% es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: el modelo explica el ", pct, "% de la variacion en los datos"))
    if (value >= 0.75) return(paste0("Bueno: el modelo explica el ", pct, "% de la variacion en los datos"))
    if (value >= 0.50) return(paste0("Moderado: el modelo explica el ", pct, "% de la variacion en los datos"))
    if (value >= 0.25) return(paste0("Debil: el modelo solo explica el ", pct, "% de la variacion"))
    return(paste0("Muy debil: el modelo apenas explica el ", pct, "% de la variacion"))
  }

  # RMSE y MAE: error promedio en las predicciones
  if (metric %in% c("rmse", "mae")) {
    if (value < 1e-6) return("[!] ADVERTENCIA: Error practicamente cero es sospechoso (posible overfitting o data leakage). Revise los datos.")
    return(paste0("Error promedio de ", round(value, 2), " unidades (comparar con el rango de la variable)"))
  }

  return("")
}

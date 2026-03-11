# =============================================================================
# Funciones de Tuning de Hiperparametros para easyML
# =============================================================================

#' @title Tuning de Hiperparametros
#'
#' @description
#' Realiza tuning de hiperparametros para el mejor modelo.
#'
#' @param modeling_result Resultado de train_models()
#' @param method Metodo de busqueda: "grid" (Grid Search), "random" (Random Search),
#'   "bayes" (Optimizacion Bayesiana), "racing" (Racing ANOVA)
#' @param grid_size Numero de combinaciones a probar (para grid/random)
#' @param iter Numero de iteraciones (para bayes, default 30)
#' @param seed Semilla
#' @param verbose Mostrar progreso
#'
#' @return Lista con resultados del tuning
#' @export
tune_best_model <- function(modeling_result,
                            method = "random",
                            grid_size = 20,
                            iter = 30,
                            select_metric = NULL,
                            seed = 2024,
                            verbose = TRUE) {

  set.seed(seed)

  best_model <- modeling_result$best_model
  task <- modeling_result$task
  recipe <- modeling_result$recipe
  cv_folds <- modeling_result$cv_folds

  if (verbose) {
    .print_section(4, "Tuning de Hiperparametros")
  }

  results <- list()
  results$original_model <- best_model

  # 4.1 Identificar Mejor Modelo
  if (verbose) .print_subsection(4, 1, "Identificar Mejor Modelo")

  if (!best_model %in% c("rf", "xgboost")) {
    if (verbose) {
      cat("    Mejor modelo:", .get_model_label(best_model), "\n")
      cat("    [!] Tuning solo disponible para RF y XGBoost\n")
      cat("    Omitiendo tuning...\n")
    }

    results$tuned <- FALSE
    results$best_params <- NULL
    results$final_workflow <- .create_final_workflow(best_model, task, recipe)
    return(results)
  }

  # Validar metodo

  method <- match.arg(method, c("grid", "random", "bayes", "racing"))

  method_labels <- c(
    grid = "Grid Search (busqueda exhaustiva en cuadricula)",
    random = "Random Search (busqueda aleatoria)",
    bayes = "Bayesian Optimization (optimizacion bayesiana)",
    racing = "Racing ANOVA (descarte temprano de malas combinaciones)"
  )

  if (verbose) {
    cat("    Los hiperparametros son configuraciones del modelo que afectan\n")
    cat("    como aprende de los datos. A diferencia de los parametros normales\n")
    cat("    (que el modelo aprende automaticamente), los hiperparametros deben\n")
    cat("    ser definidos antes del entrenamiento. El 'tuning' busca los mejores\n")
    cat("    valores probando diferentes combinaciones.\n\n")

    cat("    Modelo seleccionado:", .get_model_label(best_model), "\n")
    cat("    Metodo de busqueda:", method_labels[method], "\n")
    if (method %in% c("grid", "random")) {
      cat("    Combinaciones a evaluar:", grid_size, "\n")
    } else if (method == "bayes") {
      cat("    Iteraciones maximas:", iter, "\n")
    } else if (method == "racing") {
      cat("    Combinaciones iniciales:", grid_size, "\n")
    }
    cat("\n    Hiperparametros a tunear:\n")
    params_info <- .get_tunable_params_info(best_model)
    for (p in names(params_info)) {
      cat("      -", p, "\n")
      cat("        ", params_info[[p]], "\n")
    }
  }

  # 4.2 Realizar Tuning
  if (verbose) {
    .print_subsection(4, 2, "Busqueda de Hiperparametros")
    cat("    Ahora se probaran diferentes combinaciones de hiperparametros.\n")
    cat("    Cada combinacion se evalua usando validacion cruzada para estimar\n")
    cat("    su rendimiento. Al final, se selecciona la combinacion que obtuvo\n")
    cat("    el mejor resultado promedio.\n\n")
  }

  tune_result <- .perform_tuning(
    model_name = best_model,
    recipe = recipe,
    cv_folds = cv_folds,
    task = task,
    method = method,
    grid_size = grid_size,
    iter = iter,
    select_metric = select_metric,
    verbose = verbose
  )

  results$tune_results <- tune_result$tune_results
  results$best_params <- tune_result$best_params
  results$final_workflow <- tune_result$workflow
  results$tuned <- TRUE

  if (verbose) {
    cat("\n    Mejores hiperparametros encontrados:\n")
    params_interpretation <- .interpret_best_params(best_model, tune_result$best_params)
    for (param in names(tune_result$best_params)) {
      cat("      -", param, ":", tune_result$best_params[[param]], "\n")
      if (param %in% names(params_interpretation)) {
        cat("        ", params_interpretation[[param]], "\n")
      }
    }
    cat("\n    Estos valores seran utilizados para entrenar el modelo final\n")
    cat("    que se evaluara en el conjunto de test (Seccion 5. Evaluacion).\n")
  }

  # 4.3 Resultados del Tuning
  if (verbose) .print_subsection(4, 3, "Resultados del Tuning")

  if (verbose) {
    cat("    El tuning evaluo multiples combinaciones de hiperparametros usando\n")
    cat("    validacion cruzada. La mejor combinacion es la que obtuvo el mejor\n")
    cat("    rendimiento promedio en todos los folds.\n\n")

    # Mostrar mejora (dinamico segun select_metric)
    metrics_tune <- tune::collect_metrics(tune_result$tune_results)

    metric_labels_tune <- c(
      roc_auc = "ROC-AUC", f_meas = "F1-Score", f2_meas = "F2-Score",
      accuracy = "Accuracy", mcc = "MCC", kap = "Cohen's Kappa",
      rmse = "RMSE", rsq = "R-squared", mae = "MAE"
    )
    maximize_metrics <- c("roc_auc", "f_meas", "f2_meas", "accuracy", "mcc", "kap", "rsq")

    metric_vals <- metrics_tune$mean[metrics_tune$.metric == select_metric]
    if (length(metric_vals) > 0) {
      best_val <- if (select_metric %in% maximize_metrics) max(metric_vals) else min(metric_vals)
      label <- metric_labels_tune[select_metric]
      cat("    Mejor", label, "en CV:", round(best_val, 4), "\n")

      interpretation <- .interpret_metric_value(select_metric, best_val)
      cat("    Interpretacion:", interpretation, "\n\n")
    }

    cat("    Siguiente paso: El modelo final sera entrenado con estos hiperparametros\n")
    cat("    optimizados y se evaluara en el conjunto de test (datos que el modelo\n")
    cat("    nunca ha visto) para medir su rendimiento real.\n")

    # Imprimir referencia segun metodo al final
    if (method == "bayes") {
      .print_reference("bayes_opt")
    } else if (method == "racing") {
      .print_reference("racing")
    } else if (method == "grid") {
      .print_reference("grid_search")
    } else {
      .print_reference("random_search")
    }
  }

  results$task <- task
  results$method <- method
  class(results) <- c("easyml_tuning", "list")

  return(results)
}


#' @title Obtener parametros tuneables (solo nombres)
#' @noRd
.get_tunable_params <- function(model_name) {
  if (model_name == "rf") {
    return(c("mtry", "min_n"))
  } else if (model_name == "xgboost") {
    return(c("mtry", "min_n", "tree_depth", "learn_rate"))
  }
  return(character())
}

#' @title Obtener parametros tuneables con descripciones detalladas
#' @noRd
.get_tunable_params_info <- function(model_name) {
  if (model_name == "rf") {
    return(list(
      "mtry (variables por division)" =
        "Cuantas variables considera en cada division del arbol. Mas variables = decisiones mas informadas pero puede sobreajustar.",
      "min_n (observaciones minimas)" =
        "Minimo de casos requeridos para dividir un nodo. Valores altos = modelo mas simple, valores bajos = mas detalle."
    ))
  } else if (model_name == "xgboost") {
    return(list(
      "mtry (variables por division)" =
        "Cuantas variables considera en cada division. Similar a Random Forest.",
      "min_n (observaciones minimas)" =
        "Minimo de casos para dividir. Controla la complejidad del modelo.",
      "tree_depth (profundidad del arbol)" =
        "Niveles maximos de cada arbol. Mas profundo = captura patrones complejos pero puede sobreajustar.",
      "learn_rate (tasa de aprendizaje)" =
        "Velocidad de aprendizaje. Valores pequenos = aprendizaje lento pero estable, valores grandes = rapido pero inestable."
    ))
  }
  return(list())
}


#' @title Interpretar los valores de los mejores hiperparametros encontrados
#' @noRd
.interpret_best_params <- function(model_name, params) {
  interpretations <- list()

  # mtry: variables por division

  if ("mtry" %in% names(params)) {
    mtry_val <- params$mtry
    if (mtry_val <= 3) {
      interpretations$mtry <- paste0("Considera pocas variables (", mtry_val, ") por decision = modelo mas diverso y robusto")
    } else if (mtry_val <= 7) {
      interpretations$mtry <- paste0("Considera un numero moderado de variables (", mtry_val, ") = buen balance")
    } else {
      interpretations$mtry <- paste0("Considera muchas variables (", mtry_val, ") por decision = decisiones muy informadas")
    }
  }

  # min_n: observaciones minimas
  if ("min_n" %in% names(params)) {
    min_n_val <- params$min_n
    if (min_n_val <= 5) {
      interpretations$min_n <- paste0("Permite nodos pequenos (", min_n_val, " obs) = modelo detallado, puede captar patrones finos")
    } else if (min_n_val <= 15) {
      interpretations$min_n <- paste0("Requiere nodos medianos (", min_n_val, " obs) = buen balance entre detalle y generalizacion")
    } else {
      interpretations$min_n <- paste0("Requiere nodos grandes (", min_n_val, " obs) = modelo mas simple y generalizable")
    }
  }

  # tree_depth: profundidad del arbol (XGBoost)
  if ("tree_depth" %in% names(params)) {
    depth_val <- params$tree_depth
    if (depth_val <= 3) {
      interpretations$tree_depth <- paste0("Arboles poco profundos (", depth_val, " niveles) = modelo simple, menos riesgo de sobreajuste")
    } else if (depth_val <= 6) {
      interpretations$tree_depth <- paste0("Profundidad moderada (", depth_val, " niveles) = buen balance complejidad-generalizacion")
    } else {
      interpretations$tree_depth <- paste0("Arboles profundos (", depth_val, " niveles) = captura patrones complejos")
    }
  }

  # learn_rate: tasa de aprendizaje (XGBoost)
  if ("learn_rate" %in% names(params)) {
    lr_val <- params$learn_rate
    if (lr_val <= 0.05) {
      interpretations$learn_rate <- paste0("Aprendizaje lento (", round(lr_val, 4), ") = conservador, necesita mas arboles pero mas estable")
    } else if (lr_val <= 0.2) {
      interpretations$learn_rate <- paste0("Aprendizaje moderado (", round(lr_val, 4), ") = buen balance velocidad-estabilidad")
    } else {
      interpretations$learn_rate <- paste0("Aprendizaje rapido (", round(lr_val, 4), ") = agresivo, puede converger rapido pero riesgo de inestabilidad")
    }
  }

  return(interpretations)
}


#' @title Realizar tuning
#' @noRd
.perform_tuning <- function(model_name, recipe, cv_folds, task, method, grid_size, iter, select_metric = NULL, verbose) {

  # Establecer metrica de seleccion
  if (is.null(select_metric)) {
    select_metric <- if (task == "classification") "mcc" else "rsq"
  }
  # rank_mean no es una metrica de yardstick; usar mcc/rsq para tuning
  if (select_metric == "rank_mean") {
    select_metric <- if (task == "classification") "mcc" else "rsq"
  }

  # Definir metricas
  if (task == "classification") {
    f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)
    metrics_set <- yardstick::metric_set(
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::mcc,
      yardstick::kap,
      f2_meas
    )
  } else {
    metrics_set <- yardstick::metric_set(
      yardstick::rmse,
      yardstick::rsq
    )
  }

  # Crear especificacion con tune()
  if (model_name == "rf") {
    tune_spec <- parsnip::rand_forest(
      mtry = tune::tune(),
      min_n = tune::tune(),
      trees = 500
    ) |>
      parsnip::set_engine("ranger", importance = "impurity") |>
      parsnip::set_mode(task)

  } else if (model_name == "xgboost") {
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

  # Ejecutar busqueda segun metodo
  if (method == "grid") {
    # Grid Search: cuadricula regular
    if (verbose) {
      cat("    Generando cuadricula regular de", grid_size, "puntos...\n")
    }

    # Crear grid regular
    grid <- dials::grid_regular(
      hardhat::extract_parameter_set_dials(wf),
      levels = ceiling(grid_size^(1/length(.get_tunable_params(model_name))))
    )

    if (verbose) {
      cat("    Evaluando", nrow(grid), "combinaciones...\n")
    }

    tune_results <- suppressMessages(
      tune::tune_grid(
        wf,
        resamples = cv_folds,
        grid = grid,
        metrics = metrics_set,
        control = tune::control_grid(verbose = FALSE)
      )
    )

  } else if (method == "random") {
    # Random Search: puntos aleatorios en el espacio
    if (verbose) {
      cat("    Evaluando", grid_size, "combinaciones aleatorias...\n")
    }

    tune_results <- suppressMessages(
      tune::tune_grid(
        wf,
        resamples = cv_folds,
        grid = grid_size,
        metrics = metrics_set,
        control = tune::control_grid(verbose = FALSE)
      )
    )

  } else if (method == "bayes") {
    # Bayesian Optimization
    if (verbose) {
      cat("    Iniciando optimizacion bayesiana...\n")
      cat("    Evaluaciones iniciales: 5, Iteraciones: ", iter, "\n")
    }

    tune_results <- suppressMessages(
      tune::tune_bayes(
        wf,
        resamples = cv_folds,
        initial = 5,
        iter = iter,
        metrics = metrics_set,
        control = tune::control_bayes(verbose = FALSE, no_improve = 10)
      )
    )

  } else if (method == "racing") {
    # Racing ANOVA: descarta malas combinaciones temprano
    if (verbose) {
      cat("    Iniciando Racing ANOVA con", grid_size, "candidatos iniciales...\n")
    }

    # Requiere finetune package
    if (!requireNamespace("finetune", quietly = TRUE)) {
      warning("Paquete 'finetune' no disponible. Usando Random Search.")
      tune_results <- suppressMessages(
        tune::tune_grid(
          wf,
          resamples = cv_folds,
          grid = grid_size,
          metrics = metrics_set,
          control = tune::control_grid(verbose = FALSE)
        )
      )
    } else {
      tune_results <- suppressMessages(
        finetune::tune_race_anova(
          wf,
          resamples = cv_folds,
          grid = grid_size,
          metrics = metrics_set,
          control = finetune::control_race(verbose = FALSE)
        )
      )
    }
  }

  # Seleccionar mejores parametros
  best_params <- tune::select_best(tune_results, metric = select_metric)

  # Finalizar workflow
  final_wf <- tune::finalize_workflow(wf, best_params)

  # Convertir parametros a lista limpia
  params_list <- as.list(best_params)
  params_list$.config <- NULL

  list(
    tune_results = tune_results,
    best_params = params_list,
    workflow = final_wf
  )
}


#' @title Crear workflow final sin tuning
#' @noRd
.create_final_workflow <- function(model_name, task, recipe) {
  model_spec <- .get_model_spec(model_name, task)

  workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model_spec)
}


#' @title Grafico de resultados de tuning
#' @export
plot_tuning <- function(tune_result, metric = NULL) {

  if (!tune_result$tuned || is.null(tune_result$tune_results)) {
    message("No hay resultados de tuning para graficar")
    return(NULL)
  }

  tune::autoplot(tune_result$tune_results)
}

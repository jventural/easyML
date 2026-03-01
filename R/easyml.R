
# =============================================================================
# easyML: Funcion Principal para Machine Learning Automatizado
# =============================================================================

#' @title easyML: Machine Learning Automatizado para Clasificacion y Regresion
#'
#' @description
#' Funcion principal que ejecuta un pipeline completo de Machine Learning.
#' Automatiza EDA, preprocesamiento, entrenamiento de multiples modelos con
#' validacion cruzada, tuning de hiperparametros, evaluacion e interpretabilidad.
#'
#' @param data Data frame con los datos.
#' @param target Nombre de la variable objetivo (como string).
#' @param task Tipo de tarea: "auto", "classification", "regression".
#' @param models Vector con modelos a entrenar.
#' @param test_split Proporcion de datos para test (default: 0.20).
#' @param cv_folds Numero de folds para validacion cruzada (default: 10).
#' @param select_metric Metrica para seleccionar el mejor modelo. Para clasificacion:
#'   "roc_auc" (default), "f_meas" (F1), "accuracy", "sensitivity", "specificity",
#'   "bal_accuracy" (Balanced Accuracy), "pr_auc" (Precision-Recall AUC).
#'   Para regresion: "rmse" (default), "rsq", "mae".
#' @param tune_best Realizar tuning del mejor modelo (default: TRUE).
#' @param tune_method Metodo de busqueda de hiperparametros: "random" (Random Search),
#'   "grid" (Grid Search), "bayes" (Optimizacion Bayesiana), "racing" (Racing ANOVA).
#' @param tune_grid Numero de combinaciones para tuning (default: 20).
#' @param tune_iter Numero de iteraciones para Bayesian Optimization (default: 30).
#' @param feature_engineering Aplicar feature engineering automatico: detecta ceros
#'   sospechosos, log-transform para variables sesgadas, indicadores de missing,
#'   e interacciones entre top variables (default: FALSE).
#' @param custom_features Lista con nombre para crear variables de dominio. Cada
#'   elemento: nombre = nueva variable, valor = expresion R como string.
#'   Ejemplo: list(familia = "SibSp + Parch", fare_pp = "Fare / (SibSp + Parch + 1)")
#' @param feature_selection Usar seleccion de variables con CV (default: FALSE).
#' @param balance_classes Para clasificacion, balancear clases (default: FALSE).
#' @param balance_method Metodo de balanceo: "smote", "adasyn", "rose", "up", "down" (default: "smote").
#' @param impute Imputar valores faltantes (default: TRUE).
#' @param impute_method Metodo de imputacion: "knn", "median", "mean" (default: "knn").
#' @param normalize Normalizar variables numericas (default: TRUE).
#' @param normalize_method Metodo de normalizacion: "zscore" (estandar), "minmax" (rango [0,1]) (default: "zscore").
#' @param use_pca Aplicar PCA despues de normalizacion (default: FALSE).
#' @param pca_threshold Proporcion de varianza a conservar con PCA (default: 0.95).
#' @param treat_outliers Tratar outliers con winsorizacion (default: TRUE).
#' @param outlier_percentile Percentil para winsorizacion (default: 0.05).
#' @param remove_high_cor Eliminar variables con alta correlacion (default: TRUE).
#' @param cor_threshold Umbral de correlacion para eliminar (default: 0.90).
#' @param remove_high_vif Eliminar variables con alto VIF (default: TRUE).
#' @param vif_threshold Umbral de VIF para eliminar (default: 5).
#' @param run_eda Ejecutar analisis exploratorio (default: TRUE).
#' @param run_shap Calcular valores SHAP (default: TRUE).
#' @param n_shap Numero de observaciones para SHAP (default: 100).
#' @param optimize_threshold Optimizar threshold para clasificacion (default: TRUE).
#' @param threshold_method Metodo de optimizacion: "youden", "f1", "balanced" (default: "youden").
#' @param calibrate_probs Calibrar probabilidades (default: FALSE).
#' @param calibration_method Metodo: "platt" o "isotonic" (default: "platt").
#' @param check_leakage Verificar posible data leakage (default: TRUE).
#' @param nested_cv Usar nested CV para estimacion menos sesgada (default: FALSE).
#' @param analyze_interactions Analizar interacciones y efectos no lineales (default: TRUE).
#' @param seed Semilla para reproducibilidad (default: 2024).
#' @param verbose Mostrar progreso en consola (default: TRUE).
#'
#' @return Objeto de clase easyml con resultados completos del analisis ML.
#'
#' @examples
#' \dontrun{
#' # Ejemplo basico
#' resultado <- easy_ml(
#'   data = mis_datos,
#'   target = "mi_variable",
#'   task = "classification"
#' )
#'
#' # Ver resultados
#' print(resultado)
#' plot(resultado)
#'
#' # Hacer predicciones
#' predict(resultado, nuevos_datos)
#'
#' # =====================================================
#' # EXPORTACION DEL VERBOSE PARA GENERAR REPORTES
#' # =====================================================
#'
#' # Opcion 1: Capturar verbose durante el analisis
#' resultado <- easy_ml_capture(
#'   data = mis_datos,
#'   target = "mi_variable",
#'   task = "classification"
#' )
#'
#' # Exportar verbose como TXT (formato legible)
#' export_verbose_txt(resultado, "mi_analisis.txt")
#'
#' # Exportar verbose como JSON (para reportes con IA)
#' export_verbose_json(resultado, "mi_analisis.json")
#'
#' # Opcion 2: Exportar todo en un solo paso
#' resultado <- easy_ml_export(
#'   data = mis_datos,
#'   target = "mi_variable",
#'   task = "classification",
#'   output_dir = "resultados",
#'   prefix = "analisis_estudio1"
#' )
#' # Esto genera: analisis_estudio1_FECHA.txt y analisis_estudio1_FECHA.json
#'
#' # =====================================================
#' # GENERAR REPORTE CIENTIFICO CON IA
#' # =====================================================
#'
#' # Opcion A: Usar la aplicacion Shiny interactiva
#' launch_report_generator()
#'
#' # Opcion B: Generar reporte directamente desde R (sin Shiny)
#' generate_report_with_ai(
#'   json_path = "mi_analisis.json",
#'   api_key = "sk-proj-...",
#'   output_path = "Reporte_Cientifico.docx",
#'   title = "Analisis Predictivo de Machine Learning",
#'   author = "Dr. Juan Perez",
#'   language = "es",
#'   model = "gpt-4.1-mini"  # Modelo por defecto (economico y eficiente)
#' )
#' }
#'
#' @export
easy_ml <- function(data,
                    target,
                    task = c("auto", "classification", "regression"),
                    models = c("rf", "xgboost", "svm", "nnet", "glm", "tree"),
                    test_split = 0.20,
                    cv_folds = 10,
                    select_metric = NULL,
                    tune_best = TRUE,
                    tune_method = c("random", "grid", "bayes", "racing"),
                    tune_grid = 20,
                    tune_iter = 30,
                    feature_engineering = FALSE,
                    custom_features = NULL,
                    feature_selection = FALSE,
                    balance_classes = FALSE,
                    balance_method = c("smote", "adasyn", "rose", "up", "down"),
                    impute = TRUE,
                    impute_method = c("knn", "median", "mean"),
                    normalize = TRUE,
                    normalize_method = c("zscore", "minmax"),
                    use_pca = FALSE,
                    pca_threshold = 0.95,
                    treat_outliers = TRUE,
                    outlier_percentile = 0.05,
                    remove_high_cor = TRUE,
                    cor_threshold = 0.90,
                    remove_high_vif = TRUE,
                    vif_threshold = 5,
                    run_eda = TRUE,
                    run_shap = TRUE,
                    n_shap = 100,
                    optimize_threshold = TRUE,
                    threshold_method = "youden",
                    calibrate_probs = FALSE,
                    calibration_method = "platt",
                    check_leakage = TRUE,
                    nested_cv = FALSE,
                    analyze_interactions = TRUE,
                    seed = 2024,
                    verbose = TRUE) {

  # =========================================================================
  # CAPTURA INTERNA DEL VERBOSE
  # =========================================================================
  # Ejecutar el analisis mostrando output en tiempo real Y capturando
  # para exportacion posterior. Usa sink() con split=TRUE para lograr ambos.

  if (verbose) {
    # Crear archivo temporal para capturar el output
    temp_file <- tempfile(fileext = ".txt")

    # sink con split=TRUE: muestra en consola Y guarda en archivo
    sink(temp_file, split = TRUE)

    tryCatch({
      resultado <- .easy_ml_internal(
        data = data, target = target, task = task, models = models,
        test_split = test_split, cv_folds = cv_folds, select_metric = select_metric,
        tune_best = tune_best, tune_method = tune_method, tune_grid = tune_grid,
        tune_iter = tune_iter, feature_engineering = feature_engineering,
        custom_features = custom_features, feature_selection = feature_selection,
        balance_classes = balance_classes, balance_method = balance_method,
        impute = impute, impute_method = impute_method,
        normalize = normalize, normalize_method = normalize_method,
        use_pca = use_pca, pca_threshold = pca_threshold,
        treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
        remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
        remove_high_vif = remove_high_vif, vif_threshold = vif_threshold,
        run_eda = run_eda, run_shap = run_shap, n_shap = n_shap,
        optimize_threshold = optimize_threshold, threshold_method = threshold_method,
        calibrate_probs = calibrate_probs, calibration_method = calibration_method,
        check_leakage = check_leakage, nested_cv = nested_cv,
        analyze_interactions = analyze_interactions, seed = seed,
        verbose = TRUE
      )
    }, finally = {
      # Siempre cerrar el sink
      sink()
    })

    # Leer el output capturado del archivo temporal
    if (file.exists(temp_file)) {
      verbose_output <- readLines(temp_file, warn = FALSE)
      unlink(temp_file)  # Eliminar archivo temporal
    } else {
      verbose_output <- character(0)
    }

    # Almacenar el verbose en el resultado para exportacion posterior
    resultado$verbose_text <- paste(verbose_output, collapse = "\n")
    resultado$verbose_lines <- verbose_output

  } else {
    # Sin verbose, ejecutar directamente
    resultado <- .easy_ml_internal(
      data = data, target = target, task = task, models = models,
      test_split = test_split, cv_folds = cv_folds, select_metric = select_metric,
      tune_best = tune_best, tune_method = tune_method, tune_grid = tune_grid,
      tune_iter = tune_iter, feature_engineering = feature_engineering,
      custom_features = custom_features, feature_selection = feature_selection,
      balance_classes = balance_classes, balance_method = balance_method,
      impute = impute, impute_method = impute_method,
      normalize = normalize, normalize_method = normalize_method,
      use_pca = use_pca, pca_threshold = pca_threshold,
      treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
      remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
      remove_high_vif = remove_high_vif, vif_threshold = vif_threshold,
      run_eda = run_eda, run_shap = run_shap, n_shap = n_shap,
      optimize_threshold = optimize_threshold, threshold_method = threshold_method,
      calibrate_probs = calibrate_probs, calibration_method = calibration_method,
      check_leakage = check_leakage, nested_cv = nested_cv,
      analyze_interactions = analyze_interactions, seed = seed,
      verbose = FALSE
    )
  }

  return(resultado)
}


#' @title Funcion Interna de easyML
#' @noRd
.easy_ml_internal <- function(data,
                              target,
                              task = c("auto", "classification", "regression"),
                              models = c("rf", "xgboost", "svm", "nnet", "glm", "tree"),
                              test_split = 0.20,
                              cv_folds = 10,
                              select_metric = NULL,
                              tune_best = TRUE,
                              tune_method = c("random", "grid", "bayes", "racing"),
                              tune_grid = 20,
                              tune_iter = 30,
                              feature_engineering = FALSE,
                              custom_features = NULL,
                              feature_selection = FALSE,
                              balance_classes = FALSE,
                              balance_method = c("smote", "adasyn", "rose", "up", "down"),
                              impute = TRUE,
                              impute_method = c("knn", "median", "mean"),
                              normalize = TRUE,
                              normalize_method = c("zscore", "minmax"),
                              use_pca = FALSE,
                              pca_threshold = 0.95,
                              treat_outliers = TRUE,
                              outlier_percentile = 0.05,
                              remove_high_cor = TRUE,
                              cor_threshold = 0.90,
                              remove_high_vif = TRUE,
                              vif_threshold = 5,
                              run_eda = TRUE,
                              run_shap = TRUE,
                              n_shap = 100,
                              optimize_threshold = TRUE,
                              threshold_method = "youden",
                              calibrate_probs = FALSE,
                              calibration_method = "platt",
                              check_leakage = TRUE,
                              nested_cv = FALSE,
                              analyze_interactions = TRUE,
                              seed = 2024,
                              verbose = TRUE) {

  # Suprimir warnings de versiones de paquetes
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  set.seed(seed)
  task <- match.arg(task)
  tune_method <- match.arg(tune_method)
  start_time <- Sys.time()

  .validate_inputs(data, target, models)

  if (task == "auto") {
    task <- .detect_task(data[[target]])
  }

 # Validar y establecer select_metric
  if (is.null(select_metric)) {
    select_metric <- if (task == "classification") "roc_auc" else "rmse"
  } else {
    valid_class_metrics <- c("roc_auc", "f_meas", "accuracy", "sensitivity",
                              "specificity", "bal_accuracy", "pr_auc")
    valid_reg_metrics <- c("rmse", "rsq", "mae")

    if (task == "classification" && !select_metric %in% valid_class_metrics) {
      stop("Metrica no valida para clasificacion: ", select_metric,
           "\nOpciones: ", paste(valid_class_metrics, collapse = ", "))
    }
    if (task == "regression" && !select_metric %in% valid_reg_metrics) {
      stop("Metrica no valida para regresion: ", select_metric,
           "\nOpciones: ", paste(valid_reg_metrics, collapse = ", "))
    }
  }

  if (verbose) {
    .msg_header("easyML - Machine Learning Automatizado")
    cat("Observaciones:", nrow(data), "\n")
    cat("Variables:", ncol(data) - 1, "\n")
    cat("Variable objetivo:", target, "\n")
    cat("Tarea:", task, "\n")
    cat("Modelos:", paste(models, collapse = ", "), "\n")
  }

  resultado <- list()

  # 3. EDA
  if (run_eda) {
    eda_result <- eda_summary(data = data, target = target, task = task, verbose = verbose)
    resultado$eda <- eda_result
  }

  # 3.5 FEATURE ENGINEERING AUTOMATICO
  if (feature_engineering) {
    fe_result <- auto_feature_engineering(data = data, target = target, verbose = verbose)
    data <- fe_result$data
    resultado$feature_engineering <- fe_result
  }

  # 3.6 FEATURE ENGINEERING DE DOMINIO (custom_features)
  if (!is.null(custom_features) && length(custom_features) > 0) {
    cf_result <- apply_custom_features(data = data, custom_features = custom_features,
                                       verbose = verbose)
    data <- cf_result$data
    resultado$custom_features <- cf_result
  }

  # 4. PREPROCESAMIENTO
  preprocess_result <- preprocess_data(
    data = data, target = target, task = task, test_split = test_split,
    feature_selection = feature_selection,
    balance_classes = balance_classes, balance_method = balance_method,
    impute = impute, impute_method = impute_method,
    normalize = normalize, normalize_method = normalize_method,
    use_pca = use_pca, pca_threshold = pca_threshold,
    treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
    remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
    remove_high_vif = remove_high_vif, vif_threshold = vif_threshold,
    seed = seed, verbose = verbose
  )
  resultado$preprocessing <- preprocess_result

  # 5. MODELADO
  modeling_result <- train_models(
    preprocess_result = preprocess_result, models = models,
    cv_folds = cv_folds, select_metric = select_metric,
    seed = seed, verbose = verbose
  )
  resultado$modeling <- modeling_result

  # 6. TUNING
  if (tune_best) {
    tuning_result <- tune_best_model(
      modeling_result = modeling_result,
      method = tune_method,
      grid_size = tune_grid,
      iter = tune_iter,
      seed = seed,
      verbose = verbose
    )
  } else {
    tuning_result <- list(
      tuned = FALSE, original_model = modeling_result$best_model,
      best_params = NULL,
      final_workflow = .create_final_workflow(modeling_result$best_model, task, preprocess_result$recipe)
    )
    class(tuning_result) <- c("easyml_tuning", "list")
    if (verbose) {
      .print_section(6, "Tuning de Hiperparametros")
      cat("\n    Tuning omitido por configuracion\n")
    }
  }
  resultado$tuning <- tuning_result

  # 7. EVALUACION
  evaluation_result <- evaluate_model(
    tuning_result = tuning_result,
    train_data = preprocess_result$train_data,
    test_data = preprocess_result$test_data,
    target = target, task = task, verbose = verbose
  )
  resultado$evaluation <- evaluation_result

  # 6. INTERPRETABILIDAD
  if (run_shap) {
    interpret_result <- interpret_model(
      evaluation_result = evaluation_result,
      train_data = preprocess_result$train_data,
      test_data = preprocess_result$test_data,
      target = target, n_shap = n_shap, verbose = verbose
    )
    resultado$interpretation <- interpret_result
  } else {
    if (verbose) {
      .print_section(6, "Interpretabilidad del Modelo")
      cat("\n    [!] Seccion omitida: run_shap = FALSE\n")
      cat("    Para calcular importancia de variables y valores SHAP, ejecute\n")
      cat("    easy_ml(..., run_shap = TRUE)\n")
    }
  }

  # 7. FUNCIONALIDADES AVANZADAS
  if (verbose) {
    .print_section(7, "Analisis Avanzado")
    cat("    Esta seccion incluye analisis opcionales que van mas alla de la\n")
    cat("    evaluacion basica. Se llama 'avanzado' porque estos diagnosticos\n")
    cat("    ayudan a detectar problemas sutiles y optimizar el modelo.\n")
  }

  # 7.1 Deteccion de Data Leakage (Post-Modelo)
  if (check_leakage) {
    if (verbose) {
      .print_subsection(7, 1, "Deteccion de Data Leakage (Post-Modelo)")
      cat("    Las verificaciones pre-modelo (correlacion > 0.95 con target,\n")
      cat("    posibles IDs) se realizaron en EDA seccion 1.2.8.\n\n")
      cat("    Aqui se verifican indicadores que solo se pueden evaluar\n")
      cat("    despues de entrenar el modelo.\n\n")
    }
    # Usar importancia del modelo (si no hay SHAP, usar la del modelo final)
    importance_for_leakage <- resultado$importance
    leakage_result <- detect_data_leakage(
      model_results = list(test_metrics = evaluation_result$metrics),
      importance = importance_for_leakage,
      verbose = verbose
    )
    resultado$leakage_check <- leakage_result
    if (verbose) .print_reference("leakage")
  }

  # 7.2 Optimizacion de Threshold (solo clasificacion)
  if (task == "classification" && optimize_threshold) {
    if (verbose) {
      .print_subsection(7, 2, "Optimizacion de Threshold")
      cat("    Por defecto, el modelo clasifica como positivo si la probabilidad\n")
      cat("    es >= 0.5. Pero este umbral no siempre es optimo. La optimizacion\n")
      cat("    busca el mejor punto de corte segun el criterio seleccionado.\n\n")
    }
    threshold_result <- optimize_threshold(
      predictions = evaluation_result$predictions,
      target = target,
      method = threshold_method,
      verbose = verbose
    )
    resultado$threshold_optimization <- threshold_result
    resultado$optimal_threshold <- threshold_result$optimal_threshold
    if (verbose) .print_reference("threshold")
  }

  # 7.3 Analisis de Interacciones y Efectos No Lineales
  if (analyze_interactions) {
    if (verbose) {
      .print_subsection(7, 3, "Analisis de Interacciones y Splines")
      cat("    Este analisis detecta posibles interacciones entre variables y\n")
      cat("    efectos no lineales que podrian mejorar el modelo, especialmente\n")
      cat("    para modelos lineales (GLM). Los modelos de arboles (RF, XGBoost)\n")
      cat("    capturan estos efectos automaticamente.\n\n")
    }
    importance_for_interactions <- if (run_shap && !is.null(interpret_result)) {
      interpret_result$importance
    } else {
      .get_variable_importance(evaluation_result$final_fit)
    }

    interactions_result <- analyze_interactions_splines(
      data = data,
      target = target,
      task = task,
      importance = importance_for_interactions,
      top_n = 5,
      verbose = verbose
    )
    resultado$interactions_analysis <- interactions_result
    if (verbose) {
      .print_reference("interactions")
      .print_reference("splines")
    }
  }

  # 7.4 Calibracion de Probabilidades (solo clasificacion)
  if (task == "classification" && calibrate_probs) {
    if (verbose) .print_subsection(7, 4, "Calibracion de Probabilidades")
    calibration_result <- calibrate_probabilities(
      predictions = evaluation_result$predictions,
      target = target,
      method = calibration_method,
      verbose = verbose
    )
    resultado$calibration <- calibration_result
    if (verbose) .print_reference("calibration")
  }

  # 7.5 Analisis de Residuos Avanzado (solo regresion)
  if (task == "regression") {
    if (verbose) .print_subsection(7, 5, "Analisis de Residuos Avanzado")
    residual_result <- advanced_residual_analysis(
      predictions = evaluation_result$predictions,
      target = target,
      verbose = verbose
    )
    resultado$residual_analysis <- residual_result
    if (verbose) .print_reference("residuals")
  }

  # 7.6 Nested CV (opcional)
  if (nested_cv && modeling_result$best_model %in% c("rf", "xgboost")) {
    if (verbose) .print_subsection(7, 6, "Nested Cross-Validation")
    nested_result <- run_nested_cv(
      data = preprocess_result$train_data,
      target = target,
      task = task,
      recipe = preprocess_result$recipe,
      model_name = modeling_result$best_model,
      outer_folds = 5,
      inner_folds = 5,
      grid_size = 10,
      seed = seed,
      verbose = verbose
    )
    resultado$nested_cv <- nested_result
    if (verbose) .print_reference("nested_cv")
  }

  # 8. GENERACION DE GRAFICOS
  if (verbose) .print_section(8, "Generacion de Graficos")

  # Preparar objeto temporal para generar graficos
  resultado$task <- task
  resultado$target <- target
  resultado$best_model <- modeling_result$best_model
  resultado$test_metrics <- evaluation_result$metrics
  resultado$predictions <- evaluation_result$predictions
  resultado$importance <- if (run_shap) interpret_result$importance else .get_variable_importance(evaluation_result$final_fit)
  resultado$cv_summary <- modeling_result$comparison$summary

  # Generar todos los graficos con catalogo de figuras
  plots_result <- generate_all_plots(resultado, verbose = verbose)
  resultado$plots <- plots_result

  # Crear acceso directo a figuras: resultado$figures$importance, etc.
  resultado$figures <- plots_result$plots
  resultado$figures_catalog <- plots_result$figures_catalog

  # RESULTADO FINAL
  end_time <- Sys.time()
  elapsed <- round(difftime(end_time, start_time, units = "secs"), 1)

  resultado$task <- task
  resultado$target <- target
  resultado$best_model <- modeling_result$best_model
  resultado$best_params <- tuning_result$best_params
  resultado$final_fit <- evaluation_result$final_fit
  resultado$test_metrics <- evaluation_result$metrics
  resultado$predictions <- evaluation_result$predictions
  resultado$importance <- if (run_shap) interpret_result$importance else .get_variable_importance(evaluation_result$final_fit)
  resultado$train_data <- preprocess_result$train_data
  resultado$test_data <- preprocess_result$test_data
  resultado$cv_summary <- modeling_result$comparison$summary

  resultado$metadata <- list(
    n_obs = nrow(data), n_train = nrow(preprocess_result$train_data),
    n_test = nrow(preprocess_result$test_data), n_features = ncol(data) - 1,
    models_trained = models, cv_folds = cv_folds,
    select_metric = select_metric, tuned = tuning_result$tuned,
    tune_method = if (tune_best) tune_method else NA,
    seed = seed, elapsed_time = elapsed, date = Sys.time()
  )

  class(resultado) <- c("easyml", "list")

  if (verbose) {
    .msg_header("Analisis Completado!")
    cat("Tiempo total:", elapsed, "segundos\n")
    cat("Mejor modelo:", .get_model_label(resultado$best_model), "\n")
    if (task == "classification") {
      auc <- resultado$test_metrics$.estimate[resultado$test_metrics$.metric == "roc_auc"]
      cat("ROC-AUC en test:", round(auc, 4), "\n")
      interpretation <- .interpret_metric_value("roc_auc", auc)
      cat("Interpretacion:", interpretation, "\n")
    } else {
      rmse <- resultado$test_metrics$.estimate[resultado$test_metrics$.metric == "rmse"]
      rsq <- resultado$test_metrics$.estimate[resultado$test_metrics$.metric == "rsq"]
      cat("RMSE en test:", round(rmse, 4), "\n")
      cat("R-squared:", round(rsq, 4), "\n")
      interpretation <- .interpret_metric_value("rsq", rsq)
      cat("Interpretacion:", interpretation, "\n")
    }

    cat("\n")
    cat(.line("-"), "\n")
    cat("CONCLUSION\n")
    cat(.line("-"), "\n")
    if (task == "classification") {
      cat("Se entreno un modelo de", .get_model_label(resultado$best_model), "para predecir\n")
      cat("'", target, "'. El modelo fue evaluado en ", nrow(preprocess_result$test_data),
          " observaciones nuevas\n", sep = "")
      cat("(que no uso durante el entrenamiento) y obtuvo un AUC de ", round(auc, 2), ".\n", sep = "")
      if (auc >= 0.8) {
        cat("Este resultado indica que el modelo tiene buena capacidad para\n")
        cat("distinguir entre las clases y puede ser util para hacer predicciones.\n")
      } else if (auc >= 0.7) {
        cat("Este resultado indica capacidad aceptable. El modelo puede ser util\n")
        cat("pero podria mejorarse con mas datos o ingenieria de variables.\n")
      } else {
        cat("Este resultado sugiere que el modelo tiene dificultades para distinguir\n")
        cat("las clases. Se recomienda revisar las variables o recopilar mas datos.\n")
      }
    } else {
      cat("Se entreno un modelo de", .get_model_label(resultado$best_model), "para predecir\n")
      cat("'", target, "'. El modelo fue evaluado en ", nrow(preprocess_result$test_data),
          " observaciones nuevas\n", sep = "")
      cat("y explica el ", round(rsq * 100, 1), "% de la variacion en los datos.\n", sep = "")
      if (rsq >= 0.75) {
        cat("Este es un buen ajuste que indica que el modelo captura bien\n")
        cat("los patrones en los datos y puede ser util para predicciones.\n")
      } else if (rsq >= 0.5) {
        cat("Este es un ajuste moderado. El modelo captura algunos patrones\n")
        cat("pero podria mejorarse con mas variables o datos.\n")
      } else {
        cat("Este ajuste es debil. Se recomienda revisar las variables\n")
        cat("predictoras o considerar modelos alternativos.\n")
      }
    }

    cat("\nPara usar el modelo:\n")
    cat("  - Ver resultados: print(resultado)\n")
    cat("  - Ver graficos: plot(resultado)\n")
    cat("  - Hacer predicciones: predict(resultado, nuevos_datos)\n")
    cat("  - Guardar graficos: save_all_plots(resultado, path = 'carpeta')\n")
    cat(.line("="), "\n")
  }

  # Suprimir warnings de versiones de paquetes al retornar
  suppressWarnings(return(resultado))
}


#' @export
print.easyml <- function(x, ...) {
  cat("\n")
  cat(.line("="), "\n")
  cat("  easyML - Resultados del Analisis\n")
  cat(.line("="), "\n\n")

  cat("CONFIGURACION:\n")
  cat("  Tarea:", x$task, "\n")
  cat("  Variable objetivo:", x$target, "\n")
  cat("  Observaciones (train/test):", x$metadata$n_train, "/", x$metadata$n_test, "\n")
  cat("  Variables:", x$metadata$n_features, "\n")
  cat("  Modelos evaluados:", paste(x$metadata$models_trained, collapse = ", "), "\n")

  cat("\nMEJOR MODELO:", .get_model_label(x$best_model), "\n")

  if (!is.null(x$best_params) && length(x$best_params) > 0) {
    method_labels <- c(
      random = "Random Search",
      grid = "Grid Search",
      bayes = "Bayesian Optimization",
      racing = "Racing ANOVA"
    )
    tune_method <- x$metadata$tune_method
    if (!is.na(tune_method)) {
      cat("  Metodo de tuning:", method_labels[tune_method], "\n")
    }
    cat("  Parametros tuneados:\n")
    for (param in names(x$best_params)) {
      cat("    -", param, ":", x$best_params[[param]], "\n")
    }
  }

  cat("\nMETRICAS EN TEST:\n")
  for (i in 1:nrow(x$test_metrics)) {
    cat("  ", x$test_metrics$.metric[i], ":", round(x$test_metrics$.estimate[i], 4), "\n")
  }

  if (!is.null(x$importance) && nrow(x$importance) > 0) {
    cat("\nTOP 5 VARIABLES IMPORTANTES:\n")
    top_vars <- utils::head(x$importance, 5)
    for (i in 1:nrow(top_vars)) {
      cat("  ", i, ". ", top_vars$Variable[i], " (", round(top_vars$Importance[i], 3), ")\n", sep = "")
    }
  }

  if (!is.null(x$plots) && length(x$plots) > 0) {
    cat("\nGRAFICOS GENERADOS:", length(x$plots), "\n")
    cat("  Disponibles:", paste(names(x$plots), collapse = ", "), "\n")
    cat("  Usar: plot(resultado) para panel combinado\n")
    cat("        plot(resultado, type = 'nombre') para grafico individual\n")
    cat("        save_all_plots(resultado, path = '.') para guardar todos\n")
  }

  cat("\n", .line("-"), "\n")
  cat("Tiempo de ejecucion:", x$metadata$elapsed_time, "segundos\n")
  cat(.line("="), "\n")

  invisible(x)
}


#' @title Predecir con modelo easyML
#' @export
predict.easyml <- function(object, new_data, type = "class", ...) {
  if (type == "prob" && object$task == "classification") {
    stats::predict(object$final_fit, new_data = new_data, type = "prob")
  } else {
    stats::predict(object$final_fit, new_data = new_data)
  }
}


#' @title Resumen del modelo easyML
#' @export
summary.easyml <- function(object, ...) {
  cat("\n=== Resumen easyML ===\n\n")
  cat("Tarea:", object$task, "\n")
  cat("Target:", object$target, "\n")
  cat("Mejor modelo:", .get_model_label(object$best_model), "\n\n")
  cat("--- Comparacion de Modelos (CV) ---\n")
  print(object$cv_summary)
  cat("\n--- Metricas en Test ---\n")
  print(object$test_metrics)
  if (!is.null(object$best_params)) {
    cat("\n--- Hiperparametros Tuneados ---\n")
    print(as.data.frame(object$best_params))
  }
  invisible(object)
}

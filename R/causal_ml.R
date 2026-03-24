# =============================================================================
# causal_ml: Inferencia Causal Automatizada para easyML
# =============================================================================

#' @title causal_ml: Inferencia Causal Automatizada
#'
#' @description
#' Pipeline completo de inferencia causal. Incluye EDA causal, propensity score
#' methods (matching, IPW, AIPW), Double Machine Learning, Causal Forests,
#' Meta-learners (S/T/X), y opcionalmente TMLE y metodos Python (EconML).
#'
#' @param data Data frame con los datos.
#' @param treatment Nombre de la variable de tratamiento (string).
#' @param outcome Nombre de la variable resultado (string).
#' @param covariates Vector de nombres de covariables. NULL = todas las demas columnas.
#' @param methods Vector de metodos a ejecutar: "propensity", "dml", "causal_forest", "meta_learners".
#' @param treatment_type Tipo de tratamiento: "auto", "binary", "continuous".
#' @param ps_model Modelo para propensity score: "logistic", "rf", "xgboost".
#' @param ps_methods Metodos de PS a usar: "matching", "ipw", "doubly_robust".
#' @param matching_ratio Ratio de matching (default: 1).
#' @param caliper Caliper para matching en SDs del PS (default: 0.2).
#' @param dml_model Modelo base para DML: "rf", "xgboost", "lasso".
#' @param dml_n_folds Numero de folds para cross-fitting en DML (default: 5).
#' @param cf_num_trees Numero de arboles para Causal Forest (default: 2000).
#' @param cf_honesty Usar honesty en Causal Forest (default: TRUE).
#' @param meta_learners Meta-learners a ejecutar: "S", "T", "X".
#' @param meta_base_model Modelo base para meta-learners: "rf", "xgboost".
#' @param run_tmle Ejecutar TMLE (default: FALSE).
#' @param use_python Usar metodos Python via reticulate (default: FALSE).
#' @param python_methods Metodos Python: "econml_dml", "econml_forest".
#' @param exclude_cols Columnas a excluir del analisis.
#' @param impute Imputar valores faltantes (default: TRUE).
#' @param impute_method Metodo de imputacion: "median", "knn", "mean".
#' @param normalize Normalizar covariables (default: TRUE).
#' @param normalize_method Metodo: "zscore", "minmax".
#' @param run_eda Ejecutar EDA causal (default: TRUE).
#' @param confidence_level Nivel de confianza para ICs (default: 0.95).
#' @param seed Semilla (default: 2024).
#' @param verbose Mostrar progreso (default: TRUE).
#'
#' @return Objeto de clase \code{causalml} (lista) con componentes:
#' \describe{
#'   \item{params}{Parametros del analisis (treatment, outcome, covariates, etc.)}
#'   \item{eda}{EDA causal: distribucion, balance pre-ajuste, confusores}
#'   \item{preprocessed}{Datos preprocesados y covariables finales}
#'   \item{assumptions}{Verificacion de supuestos: overlap, positivity, balance post-IPW}
#'   \item{propensity}{Resultados de PS: matching, IPW, AIPW con ATE y IC}
#'   \item{advanced}{DML, Causal Forest (con CATE e importancia de variables), TMLE}
#'   \item{meta_learners}{S/T/X-learners con CATE individuales y ATE}
#'   \item{python}{Resultados de EconML (si use_python = TRUE)}
#'   \item{summary_table}{data.frame comparativo: method, ate, se, ci_lower, ci_upper, p_value}
#'   \item{evalue}{E-value para analisis de sensibilidad}
#'   \item{figures}{Lista de graficos ggplot2 (hasta 12 tipos)}
#'   \item{figures_catalog}{Catalogo de figuras con metadatos}
#'   \item{verbose_text}{Texto completo del verbose capturado}
#'   \item{elapsed_time}{Tiempo de ejecucion en segundos}
#' }
#'
#' @examples
#' \dontrun{
#' # Ejemplo basico con datos simulados
#' set.seed(42)
#' n <- 500
#' X1 <- rnorm(n)
#' X2 <- rnorm(n)
#' T <- rbinom(n, 1, plogis(0.5 * X1 - 0.3 * X2))
#' Y <- 2 * T + X1 + 0.5 * X2 + rnorm(n)
#' df <- data.frame(Y = Y, T = T, X1 = X1, X2 = X2)
#'
#' result <- causal_ml(
#'   data = df,
#'   treatment = "T",
#'   outcome = "Y"
#' )
#'
#' print(result)
#' summary(result)
#' plot(result)
#' plot(result, type = "ate_forest")
#' }
#'
#' @export
causal_ml <- function(data,
                      treatment,
                      outcome,
                      covariates = NULL,
                      methods = c("propensity", "dml", "causal_forest", "meta_learners"),
                      treatment_type = c("auto", "binary", "continuous"),
                      ps_model = c("logistic", "rf", "xgboost"),
                      ps_methods = c("matching", "ipw", "doubly_robust"),
                      matching_ratio = 1,
                      caliper = 0.2,
                      dml_model = c("rf", "xgboost", "lasso"),
                      dml_n_folds = 5,
                      cf_num_trees = 2000,
                      cf_honesty = TRUE,
                      meta_learners = c("S", "T", "X"),
                      meta_base_model = c("rf", "xgboost"),
                      run_tmle = FALSE,
                      use_python = FALSE,
                      python_methods = c("econml_dml", "econml_forest"),
                      exclude_cols = NULL,
                      impute = TRUE,
                      impute_method = c("median", "knn", "mean"),
                      normalize = TRUE,
                      normalize_method = c("zscore", "minmax"),
                      run_eda = TRUE,
                      confidence_level = 0.95,
                      seed = 2024,
                      verbose = TRUE) {

  # Captura de verbose
  if (verbose) {
    temp_file <- tempfile(fileext = ".txt")
    sink(temp_file, split = TRUE)

    tryCatch({
      resultado <- .causal_ml_internal(
        data = data, treatment = treatment, outcome = outcome,
        covariates = covariates, methods = methods,
        treatment_type = treatment_type, ps_model = ps_model,
        ps_methods = ps_methods, matching_ratio = matching_ratio,
        caliper = caliper, dml_model = dml_model,
        dml_n_folds = dml_n_folds, cf_num_trees = cf_num_trees,
        cf_honesty = cf_honesty, meta_learners = meta_learners,
        meta_base_model = meta_base_model, run_tmle = run_tmle,
        use_python = use_python, python_methods = python_methods,
        exclude_cols = exclude_cols, impute = impute,
        impute_method = impute_method, normalize = normalize,
        normalize_method = normalize_method, run_eda = run_eda,
        confidence_level = confidence_level, seed = seed, verbose = TRUE
      )
    }, finally = {
      sink()
    })

    if (file.exists(temp_file)) {
      verbose_output <- readLines(temp_file, warn = FALSE)
      unlink(temp_file)
    } else {
      verbose_output <- character(0)
    }

    resultado$verbose_text <- paste(verbose_output, collapse = "\n")
    resultado$verbose_lines <- verbose_output

  } else {
    resultado <- .causal_ml_internal(
      data = data, treatment = treatment, outcome = outcome,
      covariates = covariates, methods = methods,
      treatment_type = treatment_type, ps_model = ps_model,
      ps_methods = ps_methods, matching_ratio = matching_ratio,
      caliper = caliper, dml_model = dml_model,
      dml_n_folds = dml_n_folds, cf_num_trees = cf_num_trees,
      cf_honesty = cf_honesty, meta_learners = meta_learners,
      meta_base_model = meta_base_model, run_tmle = run_tmle,
      use_python = use_python, python_methods = python_methods,
      exclude_cols = exclude_cols, impute = impute,
      impute_method = impute_method, normalize = normalize,
      normalize_method = normalize_method, run_eda = run_eda,
      confidence_level = confidence_level, seed = seed, verbose = FALSE
    )
  }

  return(resultado)
}


#' @title Funcion Interna de causal_ml
#' @noRd
.causal_ml_internal <- function(data, treatment, outcome, covariates,
                                methods, treatment_type, ps_model,
                                ps_methods, matching_ratio, caliper,
                                dml_model, dml_n_folds, cf_num_trees,
                                cf_honesty, meta_learners, meta_base_model,
                                run_tmle, use_python, python_methods,
                                exclude_cols, impute, impute_method,
                                normalize, normalize_method, run_eda,
                                confidence_level, seed, verbose) {

  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  set.seed(seed)
  start_time <- Sys.time()

  # Match args
  treatment_type <- match.arg(treatment_type, c("auto", "binary", "continuous"))
  ps_model <- match.arg(ps_model, c("logistic", "rf", "xgboost"))
  dml_model <- match.arg(dml_model, c("rf", "xgboost", "lasso"))
  meta_base_model <- match.arg(meta_base_model, c("rf", "xgboost"))
  impute_method <- match.arg(impute_method, c("median", "knn", "mean"))
  normalize_method <- match.arg(normalize_method, c("zscore", "minmax"))

  # Validar inputs
  .validate_causal_inputs(data, treatment, outcome, covariates)

  # Determinar covariables
  if (is.null(covariates)) {
    covariates <- setdiff(names(data), c(treatment, outcome))
  }

  # Detectar tipo de tratamiento
  if (treatment_type == "auto") {
    treatment_type <- .detect_treatment_type(data[[treatment]])
  }

  if (verbose) {
    .msg_header("causal_ml - Inferencia Causal Automatizada")
    cat("Observaciones:", nrow(data), "\n")
    cat("Tratamiento:", treatment, "(", treatment_type, ")\n")
    cat("Resultado:", outcome, "\n")
    cat("Covariables:", length(covariates), "\n")
    cat("Metodos:", paste(methods, collapse = ", "), "\n")
    cat("Nivel de confianza:", confidence_level * 100, "%\n")
  }

  resultado <- list()
  resultado$call <- match.call()
  resultado$params <- list(
    treatment = treatment,
    outcome = outcome,
    covariates = covariates,
    methods = methods,
    treatment_type = treatment_type,
    confidence_level = confidence_level,
    seed = seed
  )

  all_summary_rows <- list()

  # =========================================================================
  # 1. EDA Causal
  # =========================================================================
  if (run_eda) {
    resultado$eda <- .causal_eda(data, treatment, outcome, covariates,
                                  treatment_type, verbose)
  }

  # =========================================================================
  # 2. Preprocesamiento
  # =========================================================================
  preprocess_result <- .causal_preprocess(
    data, treatment, outcome, covariates, treatment_type,
    exclude_cols, impute, impute_method, normalize, normalize_method, verbose
  )
  resultado$preprocessed <- preprocess_result
  covariates <- preprocess_result$covariates
  data_proc <- preprocess_result$data

  # =========================================================================
  # 3. Supuestos (primera pasada, sin PS aun)
  # =========================================================================
  # Se ejecutara completo despues de estimar PS

  # =========================================================================
  # 4. Propensity Score Methods
  # =========================================================================
  ps_result_for_assumptions <- NULL

  if ("propensity" %in% methods && treatment_type == "binary") {
    prop_result <- .causal_propensity(
      data_proc, treatment, outcome, covariates,
      ps_model, ps_methods, matching_ratio, caliper,
      confidence_level, verbose
    )
    resultado$propensity <- prop_result
    all_summary_rows <- c(all_summary_rows, prop_result$summary_rows)

    # Guardar PS para assumptions
    ps_result_for_assumptions <- list(ps = prop_result$ps)
  }

  # =========================================================================
  # 3. Verificacion de Supuestos (ahora con PS)
  # =========================================================================
  resultado$assumptions <- .causal_assumptions(
    data_proc, treatment, outcome, covariates,
    treatment_type, ps_result_for_assumptions,
    confidence_level, verbose
  )

  # =========================================================================
  # 5. Metodos Avanzados (DML, Causal Forest, TMLE)
  # =========================================================================
  advanced_methods <- intersect(methods, c("dml", "causal_forest"))

  if (length(advanced_methods) > 0 || run_tmle) {
    adv_result <- .causal_advanced(
      data_proc, treatment, outcome, covariates,
      advanced_methods, dml_model, dml_n_folds,
      cf_num_trees, cf_honesty, run_tmle,
      confidence_level, seed, verbose
    )
    resultado$advanced <- adv_result
    all_summary_rows <- c(all_summary_rows, adv_result$summary_rows)
  }

  # =========================================================================
  # 6. Meta-Learners
  # =========================================================================
  if ("meta_learners" %in% methods && treatment_type == "binary") {
    ml_result <- .causal_metalearners(
      data_proc, treatment, outcome, covariates,
      meta_learners, meta_base_model, confidence_level, seed, verbose
    )
    resultado$meta_learners <- ml_result
    all_summary_rows <- c(all_summary_rows, ml_result$summary_rows)
  }

  # =========================================================================
  # 7. Python Methods (opcional)
  # =========================================================================
  if (use_python) {
    py_result <- .causal_python(
      data_proc, treatment, outcome, covariates,
      python_methods, confidence_level, seed, verbose
    )
    resultado$python <- py_result
    all_summary_rows <- c(all_summary_rows, py_result$summary_rows)
  }

  # =========================================================================
  # Summary Table
  # =========================================================================
  if (length(all_summary_rows) > 0) {
    resultado$summary_table <- do.call(rbind, all_summary_rows)
    rownames(resultado$summary_table) <- NULL
  } else {
    resultado$summary_table <- data.frame(
      method = character(), ate = numeric(), se = numeric(),
      ci_lower = numeric(), ci_upper = numeric(), p_value = numeric()
    )
  }

  # =========================================================================
  # E-value (con el ATE mas robusto)
  # =========================================================================
  if (nrow(resultado$summary_table) > 0) {
    # Usar AIPW si disponible, sino el primero
    best_row <- if ("AIPW (Doubly Robust)" %in% resultado$summary_table$method) {
      resultado$summary_table[resultado$summary_table$method == "AIPW (Doubly Robust)", ]
    } else {
      resultado$summary_table[1, ]
    }

    resultado$evalue <- .calc_evalue(best_row$ate, best_row$se)

    if (verbose) {
      .print_subsection(3, 3, "E-value (Calculado)")
      cat("  Basado en:", best_row$method, "\n")
      cat("  E-value (punto):", round(resultado$evalue$evalue_point, 2), "\n")
      if (!is.na(resultado$evalue$evalue_ci)) {
        cat("  E-value (IC):", round(resultado$evalue$evalue_ci, 2), "\n")
      }
      cat("  Interpretacion: un confusor no medido tendria que estar asociado\n")
      cat("  con tratamiento Y resultado por un factor de al menos\n")
      cat("  ", round(resultado$evalue$evalue_point, 2),
          " para explicar el efecto observado.\n", sep = "")
    }
  }

  # =========================================================================
  # 8. Figuras
  # =========================================================================
  fig_result <- .causal_plots(resultado, verbose)
  resultado$figures <- fig_result$plots
  resultado$figures_catalog <- fig_result$figures_catalog

  # =========================================================================
  # Resumen Final
  # =========================================================================
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  resultado$elapsed_time <- elapsed
  resultado$data_original <- data

  if (verbose) {
    .print_section(9, "Resumen de Resultados")

    if (nrow(resultado$summary_table) > 0) {
      cat("\n  Tabla comparativa de efectos:\n\n")
      st <- resultado$summary_table
      cat(sprintf("  %-25s %8s %8s %10s %10s %10s\n",
                  "Metodo", "ATE", "SE", "IC Inf", "IC Sup", "p-valor"))
      cat("  ", paste(rep("-", 75), collapse = ""), "\n", sep = "")
      for (i in seq_len(nrow(st))) {
        sig <- if (st$p_value[i] < 0.001) "***" else
          if (st$p_value[i] < 0.01) "**" else
            if (st$p_value[i] < 0.05) "*" else ""
        cat(sprintf("  %-25s %8.4f %8.4f %10.4f %10.4f %10s %s\n",
                    st$method[i], st$ate[i], st$se[i],
                    st$ci_lower[i], st$ci_upper[i],
                    format(st$p_value[i], digits = 4), sig))
      }
      cat("\n  Significancia: *** p<0.001, ** p<0.01, * p<0.05\n")
    }

    cat("\n  Figuras generadas:", length(resultado$figures), "\n")
    cat("  Tiempo total:", round(elapsed, 1), "segundos\n")
  }

  class(resultado) <- c("causalml", "list")
  return(resultado)
}


# =============================================================================
# S3 Methods
# =============================================================================

#' @title Print method for causalml
#' @param x Objeto de clase causalml.
#' @param ... Argumentos adicionales (no usados).
#' @export
print.causalml <- function(x, ...) {
  cat("\n")
  cat(.line("="), "\n")
  cat("  causal_ml - Resultados de Inferencia Causal\n")
  cat(.line("="), "\n\n")

  cat("CONFIGURACION:\n")
  cat("  Tratamiento:", x$params$treatment, "(", x$params$treatment_type, ")\n")
  cat("  Resultado:", x$params$outcome, "\n")
  cat("  Covariables:", length(x$params$covariates), "\n")
  cat("  Metodos:", paste(x$params$methods, collapse = ", "), "\n")

  if (!is.null(x$summary_table) && nrow(x$summary_table) > 0) {
    cat("\nRESULTADOS (ATE):\n")
    st <- x$summary_table
    for (i in seq_len(nrow(st))) {
      sig <- if (st$p_value[i] < 0.05) "*" else ""
      cat(sprintf("  %-25s ATE = %7.4f  IC[%7.4f, %7.4f]  p = %s %s\n",
                  st$method[i], st$ate[i], st$ci_lower[i], st$ci_upper[i],
                  format(st$p_value[i], digits = 3), sig))
    }
  }

  if (!is.null(x$evalue)) {
    cat("\nSENSIBILIDAD:\n")
    cat("  E-value:", round(x$evalue$evalue_point, 2), "\n")
  }

  cat("\n  Figuras:", length(x$figures), "| Tiempo:", round(x$elapsed_time, 1), "s\n")

  invisible(x)
}


#' @title Summary method for causalml
#' @param object Objeto de clase causalml.
#' @param ... Argumentos adicionales (no usados).
#' @export
summary.causalml <- function(object, ...) {
  cat("\n=== Resumen causal_ml ===\n\n")
  cat("Tratamiento:", object$params$treatment, "\n")
  cat("Resultado:", object$params$outcome, "\n")
  cat("Tipo:", object$params$treatment_type, "\n\n")

  if (!is.null(object$summary_table) && nrow(object$summary_table) > 0) {
    cat("--- Comparacion de Metodos ---\n")
    print(object$summary_table)
  }

  if (!is.null(object$evalue)) {
    cat("\n--- Analisis de Sensibilidad ---\n")
    cat("E-value (punto):", round(object$evalue$evalue_point, 2), "\n")
    if (!is.na(object$evalue$evalue_ci)) {
      cat("E-value (IC):", round(object$evalue$evalue_ci, 2), "\n")
    }
  }

  invisible(object)
}


#' @title Plot method for causalml
#' @param x Objeto de clase causalml.
#' @param type Tipo de grafico: "all", "treatment", "outcome", "ps", "overlap",
#'   "love", "cate", "heterogeneity", "importance", "ate_forest", "meta",
#'   "evalue", "balance".
#' @param ... Argumentos adicionales (no usados).
#' @export
plot.causalml <- function(x, type = "all", ...) {

  if (is.null(x$figures) || length(x$figures) == 0) {
    message("No hay figuras disponibles")
    return(invisible(NULL))
  }

  if (type == "all") {
    return(x$figures)
  }

  plot_map <- list(
    treatment = "treatment_distribution",
    outcome = "outcome_by_treatment",
    ps = "ps_distribution",
    overlap = "ps_overlap",
    love = "balance_love_plot",
    cate = "cate_distribution",
    heterogeneity = "cate_heterogeneity",
    importance = "variable_importance_cf",
    ate_forest = "ate_forest_plot",
    meta = "meta_learner_cate",
    evalue = "sensitivity_evalue",
    balance = "matching_balance"
  )

  plot_name <- plot_map[[type]]
  if (is.null(plot_name)) plot_name <- type

  if (plot_name %in% names(x$figures)) {
    return(x$figures[[plot_name]])
  }

  available <- names(x$figures)
  message("Tipo '", type, "' no encontrado. Disponibles: ",
          paste(available, collapse = ", "))
  invisible(NULL)
}


#' @title Predict method for causalml (CATE prediction)
#' @param object Objeto de clase causalml.
#' @param new_data Data frame con nuevos datos para predecir CATE. NULL devuelve
#'   las estimaciones CATE del dataset original.
#' @param method Metodo a usar para prediccion: "best" (auto), "causal_forest", "T".
#' @param ... Argumentos adicionales (no usados).
#' @export
predict.causalml <- function(object, new_data = NULL, method = "best", ...) {

  if (is.null(new_data)) {
    # Devolver CATE del dataset original
    cate <- .get_best_cate(object)
    if (is.null(cate)) {
      stop("No hay estimaciones CATE disponibles")
    }
    return(cate)
  }

  # Predecir CATE para nuevos datos
  # Causal Forest (grf)
  if (!is.null(object$advanced$causal_forest$model) &&
      (method == "best" || method == "causal_forest")) {
    cf_model <- object$advanced$causal_forest$model
    X_new <- .prepare_covariate_matrix(new_data, object$params$covariates)
    return(stats::predict(cf_model, X_new)$predictions)
  }

  # T-learner
  if (!is.null(object$meta_learners$T) &&
      (method == "best" || method == "T")) {
    X_new <- new_data[, object$params$covariates, drop = FALSE]
    mu_1 <- stats::predict(object$meta_learners$T$model_1, data = X_new)$predictions
    mu_0 <- stats::predict(object$meta_learners$T$model_0, data = X_new)$predictions
    return(mu_1 - mu_0)
  }

  stop("No hay modelo disponible para prediccion CATE con nuevos datos. ",
       "Metodos que soportan prediccion: causal_forest, T-learner")
}

# =============================================================================
# Funciones auxiliares para easyML
# =============================================================================

# --- Funciones de formato para consola ---

#' @noRd
.line <- function(char = "=", n = 60) {
  paste(rep(char, n), collapse = "")
}

#' @noRd
.color_green <- function(text) {
  paste0("\033[32m", text, "\033[0m")
}

#' @noRd
.color_blue <- function(text) {

  paste0("\033[34m", text, "\033[0m")
}

#' @noRd
.color_bold <- function(text) {
  paste0("\033[1m", text, "\033[0m")
}

#' @noRd
.msg_header <- function(text) {
  cat("\n", .line("="), "\n", sep = "")
  cat(" ", text, "\n", sep = "")
  cat(.line("="), "\n\n", sep = "")
}

#' @noRd
.msg_step <- function(step, total, text) {
  cat("\n[", step, "/", total, "] ", text, "\n", sep = "")
  cat(.line("-"), "\n", sep = "")
}

#' @noRd
.msg_info <- function(text) {
  cat(text, "\n")
}

# --- Funciones de verbose con numeracion jerarquica ---

#' @title Imprimir seccion principal (e.g., "3. Carga y Exploracion de Datos")
#' @noRd
.print_section <- function(number, title) {
  cat("\n")
  cat(.line("="), "\n")
  cat(number, ". ", title, "\n", sep = "")
  cat(.line("="), "\n")
}

#' @title Imprimir subseccion (e.g., "3.1. Estructura de los Datos")
#' @noRd
.print_subsection <- function(section, subsection, title) {
  cat("\n")
  cat(.line("-", 50), "\n")
  cat(section, ".", subsection, ". ", title, "\n", sep = "")
  cat(.line("-", 50), "\n")
}

#' @title Imprimir sub-subseccion (e.g., "3.2.1. Distribucion de la Variable Objetivo")
#' @noRd
.print_subsubsection <- function(section, subsection, subsubsection, title) {
  cat("\n  ", section, ".", subsection, ".", subsubsection, ". ", title, "\n", sep = "")
}

#' @title Imprimir referencia en formato APA 7
#' @noRd
.print_reference <- function(ref_key) {
  references <- list(
    # EDA y Estadistica Descriptiva
    eda = "Tukey, J. W. (1977). Exploratory data analysis. Addison-Wesley.",
    missing = "Little, R. J. A., & Rubin, D. B. (2019). Statistical analysis with missing data (3rd ed.). Wiley.",
    normality = "Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality. Biometrika, 52(3-4), 591-611.",
    outliers = "Tukey, J. W. (1977). Exploratory data analysis. Addison-Wesley.",
    correlation = "Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum.",
    vif = "O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors. Quality & Quantity, 41(5), 673-690.",

    # Preprocesamiento
    train_test = "Hastie, T., Tibshirani, R., & Friedman, J. (2009). The elements of statistical learning (2nd ed.). Springer.",
    cv = "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation. IJCAI, 14(2), 1137-1145.",
    stratified_cv = "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation. IJCAI, 14(2), 1137-1145.",
    normalization = "Hastie, T., Tibshirani, R., & Friedman, J. (2009). The elements of statistical learning (2nd ed.). Springer.",
    smote = "Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). SMOTE: Synthetic minority over-sampling technique. JAIR, 16, 321-357.",
    boruta = "Kursa, M. B., & Rudnicki, W. R. (2010). Feature selection with the Boruta package. Journal of Statistical Software, 36(11), 1-13.",

    # Tratamiento de Outliers y Multicolinealidad
    winsorization = "Dixon, W. J. (1960). Simplified estimation from censored normal samples. The Annals of Mathematical Statistics, 31(2), 385-391.",
    correlation_removal = "Kuhn, M., & Johnson, K. (2013). Applied predictive modeling. Springer. https://doi.org/10.1007/978-1-4614-6849-3",
    vif_removal = "Dormann, C. F., Elith, J., Bacher, S., Buchmann, C., Carl, G., Carre, G., ... & Lautenbach, S. (2013). Collinearity: A review of methods to deal with it and a simulation study evaluating their performance. Ecography, 36(1), 27-46.",

    # Interacciones y Splines
    interactions = "Aiken, L. S., & West, S. G. (1991). Multiple regression: Testing and interpreting interactions. Sage Publications.",
    splines = "Harrell, F. E. (2015). Regression modeling strategies: With applications to linear models, logistic and ordinal regression, and survival analysis (2nd ed.). Springer.",

    # Modelos
    rf = "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.",
    xgboost = "Chen, T., & Guestrin, C. (2016). XGBoost: A scalable tree boosting system. KDD, 785-794.",
    glm = "Nelder, J. A., & Wedderburn, R. W. M. (1972). Generalized linear models. Journal of the Royal Statistical Society A, 135(3), 370-384.",
    svm = "Cortes, C., & Vapnik, V. (1995). Support-vector networks. Machine Learning, 20(3), 273-297.",
    nnet = "Rumelhart, D. E., Hinton, G. E., & Williams, R. J. (1986). Learning representations by back-propagating errors. Nature, 323, 533-536.",
    tree = "Breiman, L., Friedman, J., Stone, C. J., & Olshen, R. A. (1984). Classification and regression trees. CRC Press.",
    nb = "Hand, D. J., & Yu, K. (2001). Idiot's Bayes: Not so stupid after all? International Statistical Review, 69(3), 385-398.",

    # Tuning
    grid_search = "Bergstra, J., & Bengio, Y. (2012). Random search for hyper-parameter optimization. JMLR, 13, 281-305.",
    random_search = "Bergstra, J., & Bengio, Y. (2012). Random search for hyper-parameter optimization. JMLR, 13, 281-305.",
    bayes_opt = "Snoek, J., Larochelle, H., & Adams, R. P. (2012). Practical Bayesian optimization. NeurIPS, 25, 2951-2959.",
    racing = "Maron, O., & Moore, A. W. (1997). The racing algorithm: Model selection for lazy learners. Artificial Intelligence Review, 11, 193-225.",

    # Metricas de Evaluacion
    roc_auc = "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a ROC curve. Radiology, 143(1), 29-36.",
    confusion_matrix = "Fawcett, T. (2006). An introduction to ROC analysis. Pattern Recognition Letters, 27(8), 861-874.",
    f1_score = "Van Rijsbergen, C. J. (1979). Information retrieval (2nd ed.). Butterworth-Heinemann.",
    rmse = "Chai, T., & Draxler, R. R. (2014). Root mean square error (RMSE) or mean absolute error (MAE)? GMD, 7(3), 1247-1250.",
    r_squared = "Nagelkerke, N. J. D. (1991). A note on a general definition of the coefficient of determination. Biometrika, 78(3), 691-692.",
    calibration = "Niculescu-Mizil, A., & Caruana, R. (2005). Predicting good probabilities with supervised learning. ICML, 625-632.",

    # Interpretabilidad
    shap = "Lundberg, S. M., & Lee, S. I. (2017). A unified approach to interpreting model predictions. NeurIPS, 30, 4765-4774.",
    importance = "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.",

    # Analisis Avanzado
    threshold = "Youden, W. J. (1950). Index for rating diagnostic tests. Cancer, 3(1), 32-35.",
    leakage = "Kaufman, S., Rosset, S., Perlich, C., & Stitelman, O. (2012). Leakage in data mining. ACM TKDD, 6(4), 1-21.",
    nested_cv = "Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. BMC Bioinformatics, 7, 91.",
    residuals = "Cook, R. D., & Weisberg, S. (1982). Residuals and influence in regression. Chapman and Hall."
  )

  if (ref_key %in% names(references)) {
    cat("\n    Referencia: ", references[[ref_key]], "\n", sep = "")
  }
}


# --- Funciones de validacion ---

#' @noRd
.validate_inputs <- function(data, target, models) {
  if (!is.data.frame(data)) {
    stop("'data' debe ser un data.frame")
  }

  if (!target %in% names(data)) {
    stop("Variable objetivo '", target, "' no encontrada en los datos")
  }

  valid_models <- c("rf", "xgboost", "svm", "nnet", "glm", "tree", "nb")
  invalid <- setdiff(models, valid_models)
  if (length(invalid) > 0) {
    stop("Modelos no validos: ", paste(invalid, collapse = ", "),
         "\nOpciones: ", paste(valid_models, collapse = ", "))
  }
}

#' @noRd
.detect_task <- function(y) {
  if (is.factor(y) || is.character(y)) {
    return("classification")
  }

  n_unique <- length(unique(y))
  if (n_unique <= 10 && n_unique < length(y) * 0.05) {
    return("classification")
  }

  return("regression")
}

#' @noRd
.prepare_data <- function(data, target, task) {
  # Convertir caracteres a factores
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  # Para clasificacion, asegurar que target sea factor
  if (task == "classification") {
    data[[target]] <- as.factor(data[[target]])
  }

  return(data)
}


# --- Funciones de preprocesamiento ---

#' @noRd
.create_recipe <- function(data, target, task, impute, normalize, balance_classes) {

  # Crear formula
  formula_ml <- stats::as.formula(paste(target, "~ ."))

  # Iniciar receta
  rec <- recipes::recipe(formula_ml, data = data)

  # Imputacion
  if (impute) {
    rec <- rec |>
      recipes::step_impute_median(recipes::all_numeric_predictors()) |>
      recipes::step_impute_mode(recipes::all_nominal_predictors())
  }

  # Dummies para variables categoricas
  rec <- rec |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE)

  # Normalizacion
  if (normalize) {
    rec <- rec |>
      recipes::step_normalize(recipes::all_numeric_predictors())
  }

  # SMOTE para clasificacion desbalanceada
  if (balance_classes && task == "classification") {
    if (requireNamespace("themis", quietly = TRUE)) {
      rec <- rec |>
        themis::step_smote(!!rlang::sym(target), over_ratio = 1)
    } else {
      warning("Paquete 'themis' no instalado. SMOTE no aplicado.")
    }
  }

  return(rec)
}


# --- Funciones de modelos ---

#' @noRd
.get_model_spec <- function(model_name, task) {

  if (model_name == "rf") {
    if (task == "classification") {
      spec <- parsnip::rand_forest(trees = 500) |>
        parsnip::set_engine("ranger", importance = "impurity") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::rand_forest(trees = 500) |>
        parsnip::set_engine("ranger", importance = "impurity") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "xgboost") {
    if (task == "classification") {
      spec <- parsnip::boost_tree(trees = 500, tree_depth = 6, learn_rate = 0.1) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::boost_tree(trees = 500, tree_depth = 6, learn_rate = 0.1) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "svm") {
    if (task == "classification") {
      spec <- parsnip::svm_rbf() |>
        parsnip::set_engine("kernlab") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::svm_rbf() |>
        parsnip::set_engine("kernlab") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "nnet") {
    if (task == "classification") {
      spec <- parsnip::mlp(hidden_units = 10, epochs = 100) |>
        parsnip::set_engine("nnet") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::mlp(hidden_units = 10, epochs = 100) |>
        parsnip::set_engine("nnet") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "glm") {
    if (task == "classification") {
      spec <- parsnip::logistic_reg() |>
        parsnip::set_engine("glm") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::linear_reg() |>
        parsnip::set_engine("lm") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "tree") {
    if (task == "classification") {
      spec <- parsnip::decision_tree() |>
        parsnip::set_engine("rpart") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::decision_tree() |>
        parsnip::set_engine("rpart") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "nb") {
    if (task == "classification") {
      spec <- parsnip::naive_Bayes() |>
        parsnip::set_engine("naivebayes") |>
        parsnip::set_mode("classification")
    } else {
      stop("Naive Bayes solo esta disponible para clasificacion")
    }
  }

  else {
    stop("Modelo no soportado: ", model_name)
  }

  return(spec)
}


# --- Funciones de evaluacion ---

#' @noRd
.summarize_cv_results <- function(cv_results, task) {
  results_list <- lapply(names(cv_results), function(model_name) {
    metrics <- tune::collect_metrics(cv_results[[model_name]])
    metrics$model <- model_name
    metrics
  })

  all_metrics <- do.call(rbind, results_list)

  if (task == "classification") {
    summary_df <- all_metrics |>
      dplyr::select(model, .metric, mean) |>
      tidyr::pivot_wider(names_from = .metric, values_from = mean) |>
      dplyr::arrange(dplyr::desc(roc_auc))
  } else {
    summary_df <- all_metrics |>
      dplyr::select(model, .metric, mean) |>
      tidyr::pivot_wider(names_from = .metric, values_from = mean) |>
      dplyr::arrange(rmse)
  }

  return(as.data.frame(summary_df))
}


#' @noRd
.calculate_test_metrics <- function(predictions, target, task) {

  if (task == "classification") {
    # Obtener nombre de clase positiva
    target_col <- predictions[[target]]
    levels_y <- levels(target_col)
    positive_class <- levels_y[2]  # Segunda clase como positiva
    prob_col <- paste0(".pred_", positive_class)

    metrics <- dplyr::bind_rows(
      yardstick::roc_auc(predictions, truth = !!rlang::sym(target),
                         !!rlang::sym(prob_col)),
      yardstick::accuracy(predictions, truth = !!rlang::sym(target),
                          estimate = .pred_class),
      yardstick::sensitivity(predictions, truth = !!rlang::sym(target),
                             estimate = .pred_class),
      yardstick::specificity(predictions, truth = !!rlang::sym(target),
                             estimate = .pred_class),
      yardstick::f_meas(predictions, truth = !!rlang::sym(target),
                        estimate = .pred_class)
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

  return(metrics)
}


#' @noRd
.get_variable_importance <- function(final_fit) {

  tryCatch({
    imp <- vip::vi(final_fit)
    imp_df <- data.frame(
      Variable = imp$Variable,
      Importance = imp$Importance
    )
    imp_df <- imp_df[order(-imp_df$Importance), ]
    return(imp_df)
  }, error = function(e) {
    return(data.frame(Variable = character(), Importance = numeric()))
  })
}


# --- Funciones de tuning ---

#' @noRd
.tune_model <- function(model_name, recipe, cv_folds, metrics, task,
                        grid_size, verbose) {

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

  wf <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(tune_spec)

  # Grid search
  tune_results <- tune::tune_grid(
    wf,
    resamples = cv_folds,
    grid = grid_size,
    metrics = metrics,
    control = tune::control_grid(verbose = FALSE)
  )

  # Seleccionar mejores parametros
  if (task == "classification") {
    best_params <- tune::select_best(tune_results, metric = "roc_auc")
  } else {
    best_params <- tune::select_best(tune_results, metric = "rmse")
  }

  # Finalizar workflow
  final_wf <- tune::finalize_workflow(wf, best_params)

  # Convertir a lista simple
  params_list <- as.list(best_params)
  params_list$.config <- NULL

  return(list(
    workflow = final_wf,
    best_params = params_list,
    tune_results = tune_results
  ))
}


# =============================================================================
# Analisis de Interacciones y Efectos No Lineales
# =============================================================================

#' @title Analizar Interacciones y Efectos No Lineales
#' @description
#' Detecta posibles interacciones importantes entre variables y sugiere
#' transformaciones no lineales (splines) para mejorar el modelo.
#' @param data Data frame con los datos
#' @param target Nombre de la variable objetivo
#' @param task Tipo de tarea: "classification" o "regression"
#' @param importance Data frame con importancia de variables (opcional)
#' @param top_n Numero de variables top para analizar interacciones (default: 5)
#' @param verbose Mostrar resultados (default: TRUE)
#' @return Lista con resultados del analisis
#' @export
analyze_interactions_splines <- function(data, target, task,
                                         importance = NULL,
                                         top_n = 5,
                                         verbose = TRUE) {

  results <- list()

  # Obtener variables predictoras
  predictors <- setdiff(names(data), target)
  num_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- setdiff(num_vars, target)
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
  cat_vars <- setdiff(cat_vars, target)

  # =========================================================================
  # 1. ANALISIS DE INTERACCIONES
  # =========================================================================

  if (verbose) {
    cat("\n    ANALISIS DE INTERACCIONES\n")
    cat("    -------------------------\n")
    cat("    Las interacciones ocurren cuando el efecto de una variable\n")
    cat("    depende del valor de otra. Por ejemplo, el efecto de 'Age'\n")
    cat("    en la supervivencia puede ser diferente para hombres y mujeres.\n\n")
  }

  # Seleccionar top variables para analizar
  if (!is.null(importance) && nrow(importance) > 0) {
    top_vars <- head(importance$Variable, top_n)
    # Limpiar nombres (quitar sufijos de dummies como _Female, _X1st, etc.)
    top_vars_clean <- unique(gsub("_.*$", "", top_vars))
    top_vars_clean <- intersect(top_vars_clean, predictors)
  } else {
    top_vars_clean <- head(predictors, min(top_n, length(predictors)))
  }

  # Detectar interacciones potenciales
  interactions_detected <- list()

  # Interacciones categorica x numerica (mas interpretables)
  for (cat_var in intersect(cat_vars, top_vars_clean)) {
    for (num_var in intersect(num_vars, top_vars_clean)) {
      if (cat_var != num_var) {
        # Calcular correlacion por grupo
        interaction_strength <- tryCatch({
          group_cors <- tapply(data[[num_var]], data[[cat_var]], function(x) {
            if (length(unique(x)) < 3) return(NA)
            cor(x, as.numeric(data[[target]][data[[cat_var]] == unique(data[[cat_var]])[1]]),
                use = "complete.obs")
          })
          # Variabilidad en correlaciones = evidencia de interaccion
          if (all(is.na(group_cors))) return(0)
          sd(group_cors, na.rm = TRUE)
        }, error = function(e) 0)

        if (!is.na(interaction_strength) && interaction_strength > 0.1) {
          interactions_detected[[paste0(cat_var, " x ", num_var)]] <- list(
            var1 = cat_var,
            var2 = num_var,
            type = "categorica x numerica",
            strength = round(interaction_strength, 3)
          )
        }
      }
    }
  }

  # Interacciones categorica x categorica
  cat_vars_in_top <- intersect(cat_vars, top_vars_clean)
  if (length(cat_vars_in_top) >= 2) {
    cat_pairs <- combn(cat_vars_in_top, 2, simplify = FALSE)
    for (pair in cat_pairs) {
      if (length(pair) == 2) {
        interaction_name <- paste0(pair[1], " x ", pair[2])
        interactions_detected[[interaction_name]] <- list(
          var1 = pair[1],
          var2 = pair[2],
          type = "categorica x categorica",
          strength = NA  # Mas dificil de cuantificar
        )
      }
    }
  }

  results$interactions <- interactions_detected

  if (verbose) {
    if (length(interactions_detected) > 0) {
      cat("    Interacciones potenciales detectadas:\n\n")
      for (int_name in names(interactions_detected)) {
        int_info <- interactions_detected[[int_name]]
        cat("      *", int_name, "\n")
        cat("        Tipo:", int_info$type, "\n")
        if (!is.na(int_info$strength)) {
          cat("        Intensidad:", int_info$strength, "\n")
        }
        cat("\n")
      }

      cat("    Interpretacion:\n")
      cat("      Una interaccion '", names(interactions_detected)[1], "' significa que\n")
      cat("      el efecto de una variable cambia segun el nivel de la otra.\n\n")
    } else {
      cat("    No se detectaron interacciones significativas entre las\n")
      cat("    variables principales.\n\n")
    }
  }

  # =========================================================================
  # 2. ANALISIS DE EFECTOS NO LINEALES (SPLINES)
  # =========================================================================

  if (verbose) {
    cat("    EFECTOS NO LINEALES (SPLINES)\n")
    cat("    ------------------------------\n")
    cat("    Los splines permiten modelar relaciones curvas entre variables.\n")
    cat("    Por ejemplo, la edad puede tener un efecto no lineal donde\n")
    cat("    ninos y ancianos tienen patrones diferentes a adultos.\n\n")
  }

  nonlinear_candidates <- list()

  for (num_var in intersect(num_vars, top_vars_clean)) {
    # Detectar no linealidad usando test de curvatura simple
    x <- data[[num_var]]
    y <- if (task == "classification") {
      as.numeric(data[[target]]) - 1
    } else {
      data[[target]]
    }

    # Eliminar NAs
    complete_idx <- complete.cases(x, y)
    x <- x[complete_idx]
    y <- y[complete_idx]

    if (length(x) < 30) next

    nonlinearity_test <- tryCatch({
      # Comparar modelo lineal vs cuadratico
      x_centered <- x - mean(x)
      lm_linear <- lm(y ~ x_centered)
      lm_quad <- lm(y ~ x_centered + I(x_centered^2))

      # Test F para termino cuadratico
      anova_result <- anova(lm_linear, lm_quad)
      p_value <- anova_result$`Pr(>F)`[2]

      # R2 adicional del termino cuadratico
      r2_linear <- summary(lm_linear)$r.squared
      r2_quad <- summary(lm_quad)$r.squared
      r2_gain <- r2_quad - r2_linear

      # Proteger contra NA (ej. variable binaria donde el termino cuadratico es singular)
      if (is.na(p_value)) p_value <- 1
      if (is.na(r2_gain)) r2_gain <- 0

      list(p_value = p_value, r2_gain = r2_gain)
    }, error = function(e) {
      list(p_value = 1, r2_gain = 0)
    })

    if (nonlinearity_test$p_value < 0.05 || nonlinearity_test$r2_gain > 0.01) {
      nonlinear_candidates[[num_var]] <- list(
        variable = num_var,
        p_value = round(nonlinearity_test$p_value, 4),
        r2_gain = round(nonlinearity_test$r2_gain * 100, 2),
        recommendation = if (nonlinearity_test$p_value < 0.01) {
          "Altamente recomendado"
        } else if (nonlinearity_test$p_value < 0.05) {
          "Recomendado"
        } else {
          "Considerar"
        }
      )
    }
  }

  results$nonlinear <- nonlinear_candidates

  if (verbose) {
    if (length(nonlinear_candidates) > 0) {
      cat("    Variables con efectos no lineales detectados:\n\n")
      for (var_name in names(nonlinear_candidates)) {
        nl_info <- nonlinear_candidates[[var_name]]
        cat("      *", var_name, "\n")
        cat("        p-valor curvatura:", nl_info$p_value, "\n")
        cat("        R2 ganancia:", nl_info$r2_gain, "%\n")
        cat("        Sugerencia:", nl_info$recommendation, "\n\n")
      }
    } else {
      cat("    No se detectaron efectos no lineales significativos.\n")
      cat("    Las relaciones parecen ser aproximadamente lineales.\n\n")
    }
  }

  # =========================================================================
  # 3. SUGERENCIAS DE MEJORA
  # =========================================================================

  if (verbose) {
    cat("    SUGERENCIAS PARA MEJORAR EL MODELO\n")
    cat("    -----------------------------------\n\n")

    has_suggestions <- FALSE

    # Sugerencias de interacciones
    if (length(interactions_detected) > 0) {
      has_suggestions <- TRUE
      cat("    1. INTERACCIONES:\n")
      cat("       Para incluir interacciones en el modelo, puedes:\n\n")

      # Mostrar ejemplo de codigo
      int_example <- names(interactions_detected)[1]
      vars_example <- interactions_detected[[1]]

      cat("       # En R, crear variable de interaccion:\n")
      cat("       data$", vars_example$var1, "_", vars_example$var2,
          " <- interaction(data$", vars_example$var1, ", data$", vars_example$var2, ")\n\n", sep = "")

      cat("       # O en la formula del modelo:\n")
      cat("       formula <- ", target, " ~ ", vars_example$var1, " * ", vars_example$var2, " + ...\n\n", sep = "")
    }

    # Sugerencias de splines
    if (length(nonlinear_candidates) > 0) {
      has_suggestions <- TRUE
      cat("    2. TRANSFORMACIONES NO LINEALES (SPLINES):\n")
      cat("       Para capturar efectos no lineales, puedes usar:\n\n")

      var_example <- names(nonlinear_candidates)[1]

      cat("       # Opcion A: Splines con paquete splines\n")
      cat("       library(splines)\n")
      cat("       formula <- ", target, " ~ ns(", var_example, ", df = 4) + ...\n\n", sep = "")

      cat("       # Opcion B: Restricted Cubic Splines con rms\n")
      cat("       library(rms)\n")
      cat("       formula <- ", target, " ~ rcs(", var_example, ", 4) + ...\n\n", sep = "")
    }

    if (!has_suggestions) {
      cat("    El modelo actual parece estar bien especificado.\n")
      cat("    No se detectaron interacciones o efectos no lineales\n")
      cat("    que requieran atencion inmediata.\n\n")
    }

    cat("    Nota: Los modelos como Random Forest y XGBoost capturan\n")
    cat("    automaticamente interacciones y efectos no lineales.\n")
    cat("    Estas sugerencias son mas relevantes para modelos lineales (GLM).\n")
  }

  # Resumen
  results$summary <- list(
    n_interactions = length(interactions_detected),
    n_nonlinear = length(nonlinear_candidates),
    top_interaction = if (length(interactions_detected) > 0) names(interactions_detected)[1] else NA,
    top_nonlinear = if (length(nonlinear_candidates) > 0) names(nonlinear_candidates)[1] else NA
  )

  return(results)
}

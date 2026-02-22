# =============================================================================
# Funciones de Preprocesamiento para easyML
# =============================================================================

#' @title Pipeline de Preprocesamiento
#'
#' @description
#' Ejecuta el preprocesamiento completo: preparacion de datos, division
#' train/test, feature selection y creacion de receta.
#'
#' @param data Data frame con los datos
#' @param target Nombre de la variable objetivo
#' @param task Tipo de tarea
#' @param test_split Proporcion para test
#' @param feature_selection Usar Boruta para seleccion (default: FALSE)
#' @param balance_classes Balancear clases con SMOTE
#' @param impute Imputar valores faltantes
#' @param normalize Normalizar variables
#' @param treat_outliers Tratar outliers con winsorizacion (default: TRUE)
#' @param outlier_percentile Percentil para winsorizacion (default: 0.05, es decir 5% y 95%)
#' @param remove_high_cor Eliminar variables con alta correlacion (default: TRUE)
#' @param cor_threshold Umbral de correlacion para eliminar (default: 0.90)
#' @param remove_high_vif Eliminar variables con alto VIF (default: TRUE)
#' @param vif_threshold Umbral de VIF para eliminar (default: 5)
#' @param seed Semilla
#' @param verbose Mostrar progreso
#'
#' @return Lista con datos preprocesados y receta
#' @export
preprocess_data <- function(data,
                            target,
                            task = "auto",
                            test_split = 0.20,
                            feature_selection = FALSE,
                            balance_classes = FALSE,
                            balance_method = c("smote", "adasyn", "rose", "up", "down"),
                            impute = TRUE,
                            impute_method = c("median", "mean", "knn"),
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
                            seed = 2024,
                            verbose = TRUE) {

  set.seed(seed)
  balance_method <- match.arg(balance_method)
  impute_method <- match.arg(impute_method)
  normalize_method <- match.arg(normalize_method)

  if (task == "auto") {
    task <- .detect_task(data[[target]])
  }

  if (verbose) {
    .print_section(2, "Preprocesamiento de Datos")
  }

  results <- list()

  # 2.1 Preparar Variable Objetivo
  if (verbose) {
    if (task == "classification") {
      .print_subsection(2, 1, "Preparar Variable Objetivo")
    } else {
      .print_subsection(2, 1, "Preparar Variables")
    }
  }
  data <- prep_target(data, target, task, verbose)
  results$data <- data

  # 2.2 Division Train/Test
  if (verbose) .print_subsection(2, 2, "Division Train/Test")
  split_result <- prep_split(data, target, task, test_split, seed, verbose)
  results$train_data <- split_result$train
  results$test_data <- split_result$test
  results$split <- split_result$split
  if (verbose) .print_reference("train_test")

  # 2.3 Feature Selection (opcional)
  if (feature_selection) {
    if (verbose) .print_subsection(2, 3, "Feature Selection con Boruta")
    fs_result <- prep_feature_selection(results$train_data, target, seed, verbose)
    results$selected_features <- fs_result$selected
    results$boruta_result <- fs_result$boruta
    if (verbose) .print_reference("boruta")
  } else {
    results$selected_features <- NULL
  }

  # 2.4 Definir Receta
  recipe_idx <- if (feature_selection) 4 else 3
  if (verbose) .print_subsection(2, recipe_idx, "Definir Receta de Preprocesamiento")
  results$recipe <- prep_recipe(
    data = results$train_data,
    target = target,
    task = task,
    impute = impute,
    impute_method = impute_method,
    normalize = normalize,
    normalize_method = normalize_method,
    use_pca = use_pca,
    pca_threshold = pca_threshold,
    balance_classes = balance_classes,
    balance_method = balance_method,
    treat_outliers = treat_outliers,
    outlier_percentile = outlier_percentile,
    remove_high_cor = remove_high_cor,
    cor_threshold = cor_threshold,
    remove_high_vif = remove_high_vif,
    vif_threshold = vif_threshold,
    verbose = verbose
  )
  if (verbose) .print_reference("normalization")

  results$task <- task
  results$target <- target
  class(results) <- c("easyml_preprocess", "list")

  return(results)
}


#' @title Preparar variable objetivo
#' @export
prep_target <- function(data, target, task, verbose = TRUE) {
  # Convertir caracteres a factores
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  if (task == "classification") {
    if (!is.factor(data[[target]])) {
      data[[target]] <- as.factor(data[[target]])
    }
    levels_y <- levels(data[[target]])

    if (verbose) {
      cat("    Variable objetivo convertida a factor\n")
      cat("    Clases:", paste(levels_y, collapse = ", "), "\n")
    }
  } else {
    if (!is.numeric(data[[target]])) {
      data[[target]] <- as.numeric(data[[target]])
    }
    if (verbose) {
      cat("    Variable objetivo:", target, "(numerica)\n")
    }
  }

  return(data)
}


#' @title Division Train/Test
#' @export
prep_split <- function(data, target, task, test_split = 0.20, seed = 2024, verbose = TRUE) {
  set.seed(seed)

  if (task == "classification") {
    data_split <- rsample::initial_split(data, prop = 1 - test_split,
                                         strata = !!rlang::sym(target))
  } else {
    data_split <- rsample::initial_split(data, prop = 1 - test_split)
  }

  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  if (verbose) {
    cat("    Proporcion test:", test_split * 100, "%\n")
    cat("    Train:", nrow(train_data), "observaciones\n")
    cat("    Test:", nrow(test_data), "observaciones\n")
  }

  list(
    train = train_data,
    test = test_data,
    split = data_split
  )
}


#' @title Feature Selection con Boruta
#' @export
prep_feature_selection <- function(data, target, seed = 2024, verbose = TRUE) {

  if (!requireNamespace("Boruta", quietly = TRUE)) {
    if (verbose) cat("    [!] Paquete Boruta no instalado. Omitiendo.\n")
    return(list(selected = NULL, boruta = NULL))
  }

  set.seed(seed)

  # Preparar datos
  data_clean <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
    na.omit()

  if (verbose) cat("    Ejecutando Boruta...\n")

  # Ejecutar Boruta
  formula_boruta <- as.formula(paste(target, "~ ."))
  boruta_result <- Boruta::Boruta(
    formula_boruta,
    data = data_clean,
    doTrace = 0,
    maxRuns = 100
  )

  # Obtener variables confirmadas
  boruta_df <- Boruta::attStats(boruta_result)
  boruta_df$Variable <- rownames(boruta_df)

  confirmed <- boruta_df$Variable[boruta_df$decision == "Confirmed"]
  tentative <- boruta_df$Variable[boruta_df$decision == "Tentative"]
  selected <- c(confirmed, tentative)

  if (verbose) {
    cat("    Variables confirmadas:", length(confirmed), "\n")
    cat("    Variables tentativas:", length(tentative), "\n")
    cat("    Variables rechazadas:", sum(boruta_df$decision == "Rejected"), "\n")
    if (length(selected) > 0) {
      cat("\n    Variables seleccionadas:\n")
      for (v in selected) {
        cat("      -", v, "\n")
      }
    }
  }

  list(
    selected = selected,
    boruta = boruta_result,
    importance = boruta_df
  )
}


#' @title Crear receta de preprocesamiento
#' @export
prep_recipe <- function(data, target, task, impute = TRUE,
                        impute_method = c("median", "mean", "knn"),
                        normalize = TRUE,
                        normalize_method = c("zscore", "minmax"),
                        use_pca = FALSE, pca_threshold = 0.95,
                        balance_classes = FALSE,
                        balance_method = c("smote", "adasyn", "rose", "up", "down"),
                        treat_outliers = TRUE,
                        outlier_percentile = 0.05, remove_high_cor = TRUE,
                        cor_threshold = 0.90, remove_high_vif = TRUE,
                        vif_threshold = 5, verbose = TRUE) {

  impute_method <- match.arg(impute_method)
  normalize_method <- match.arg(normalize_method)
  balance_method <- match.arg(balance_method)

  formula_ml <- stats::as.formula(paste(target, "~ ."))
  rec <- recipes::recipe(formula_ml, data = data)

  steps_applied <- character()
  treatments_applied <- list()  # Para almacenar detalles de tratamientos

  # 1. Imputacion (primero para poder calcular correlaciones y VIF)
  if (impute) {
    if (impute_method == "median") {
      rec <- rec |>
        recipes::step_impute_median(recipes::all_numeric_predictors())
      steps_applied <- c(steps_applied, "Imputacion con mediana (variables numericas)")
    } else if (impute_method == "mean") {
      rec <- rec |>
        recipes::step_impute_mean(recipes::all_numeric_predictors())
      steps_applied <- c(steps_applied, "Imputacion con media (variables numericas)")
    } else if (impute_method == "knn") {
      rec <- rec |>
        recipes::step_impute_knn(recipes::all_numeric_predictors())
      steps_applied <- c(steps_applied, "Imputacion con KNN (variables numericas)")
    }
    rec <- rec |>
      recipes::step_impute_mode(recipes::all_nominal_predictors())
    steps_applied <- c(steps_applied, "Imputacion con moda (variables categoricas)")
  }

  # 2. Tratamiento de Outliers con Winsorizacion
  if (treat_outliers) {
    # Detectar variables con outliers antes de aplicar
    num_vars <- names(data)[sapply(data, is.numeric)]
    num_vars <- setdiff(num_vars, target)

    if (length(num_vars) > 0) {
      outlier_info <- .detect_outliers_for_treatment(data, num_vars)

      if (outlier_info$n_vars_with_outliers > 0) {
        # Pre-calcular limites de winsorizacion para cada variable
        lp <- outlier_percentile
        up <- 1 - outlier_percentile

        # Aplicar winsorizacion usando step_mutate con limites pre-calculados
        for (var in outlier_info$vars_with_outliers) {
          # Calcular limites para esta variable
          x <- data[[var]]
          q_low <- quantile(x, probs = lp, na.rm = TRUE)
          q_high <- quantile(x, probs = up, na.rm = TRUE)

          # Crear expresion de winsorizacion con valores literales
          rec <- rec |>
            recipes::step_mutate(
              !!rlang::sym(var) := pmax(pmin(!!rlang::sym(var), !!q_high), !!q_low)
            )
        }

        steps_applied <- c(steps_applied, "Winsorizacion de outliers")
        treatments_applied$outliers <- list(
          vars_treated = outlier_info$vars_with_outliers,
          n_vars = outlier_info$n_vars_with_outliers,
          percentile = outlier_percentile
        )
      }
    }
  }

  # 3. Eliminacion de variables con alta correlacion
  if (remove_high_cor) {
    num_vars <- names(data)[sapply(data, is.numeric)]
    num_vars <- setdiff(num_vars, target)

    if (length(num_vars) >= 2) {
      cor_info <- .detect_high_correlation(data, num_vars, cor_threshold)

      if (length(cor_info$vars_to_remove) > 0) {
        # Usar step_select negativo para eliminar variables
        vars_to_keep <- setdiff(names(data), cor_info$vars_to_remove)
        rec <- rec |>
          recipes::step_rm(tidyselect::any_of(cor_info$vars_to_remove))

        steps_applied <- c(steps_applied, "Eliminacion por alta correlacion")
        treatments_applied$correlation <- list(
          vars_removed = cor_info$vars_to_remove,
          n_vars = length(cor_info$vars_to_remove),
          threshold = cor_threshold,
          pairs = cor_info$high_cor_pairs
        )
      }
    }
  }

  # 4. Eliminacion de variables con alto VIF
  if (remove_high_vif) {
    # Obtener variables restantes despues de eliminar por correlacion
    num_vars <- names(data)[sapply(data, is.numeric)]
    num_vars <- setdiff(num_vars, target)

    # Excluir las ya eliminadas por correlacion
    if (!is.null(treatments_applied$correlation)) {
      num_vars <- setdiff(num_vars, treatments_applied$correlation$vars_removed)
    }

    if (length(num_vars) >= 2) {
      vif_info <- .detect_high_vif(data, num_vars, target, vif_threshold)

      if (length(vif_info$vars_to_remove) > 0) {
        rec <- rec |>
          recipes::step_rm(tidyselect::any_of(vif_info$vars_to_remove))

        steps_applied <- c(steps_applied, "Eliminacion por alto VIF")
        treatments_applied$vif <- list(
          vars_removed = vif_info$vars_to_remove,
          n_vars = length(vif_info$vars_to_remove),
          threshold = vif_threshold,
          vif_values = vif_info$high_vif_values
        )
      }
    }
  }

  # 5. Dummies para variables categoricas
  rec <- rec |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE)
  steps_applied <- c(steps_applied, "Dummy encoding")

  # 6. Normalizacion
  if (normalize) {
    if (normalize_method == "zscore") {
      rec <- rec |>
        recipes::step_normalize(recipes::all_numeric_predictors())
      steps_applied <- c(steps_applied, "Normalizacion (z-score)")
    } else if (normalize_method == "minmax") {
      rec <- rec |>
        recipes::step_range(recipes::all_numeric_predictors(), min = 0, max = 1)
      steps_applied <- c(steps_applied, "Normalizacion (Min-Max [0,1])")
    }
  }

  # 6.5 PCA (opcional, despues de normalizacion)
  if (use_pca) {
    rec <- rec |>
      recipes::step_pca(recipes::all_numeric_predictors(), threshold = pca_threshold)
    steps_applied <- c(steps_applied, paste0("PCA (threshold = ", pca_threshold, ")"))
  }

  # 7. Balanceo de clases para clasificacion desbalanceada
  if (balance_classes && task == "classification") {
    if (requireNamespace("themis", quietly = TRUE)) {
      rec <- switch(balance_method,
        "smote" = rec |> themis::step_smote(!!rlang::sym(target), over_ratio = 1),
        "adasyn" = rec |> themis::step_adasyn(!!rlang::sym(target), over_ratio = 1),
        "rose" = rec |> themis::step_rose(!!rlang::sym(target), over_ratio = 1),
        "up" = rec |> themis::step_upsample(!!rlang::sym(target), over_ratio = 1),
        "down" = rec |> themis::step_downsample(!!rlang::sym(target), over_ratio = 1)
      )
      balance_label <- toupper(balance_method)
      steps_applied <- c(steps_applied, paste0(balance_label, " (balanceo)"))
    } else {
      if (verbose) cat("    [!] Paquete themis no instalado. Balanceo omitido.\n")
    }
  }

  # Verbose: mostrar todos los pasos y tratamientos aplicados
  if (verbose) {
    cat("    La receta define las transformaciones que se aplicaran a los datos\n")
    cat("    antes de entrenar los modelos. Estos pasos se ejecutan automaticamente\n")
    cat("    dentro de la validacion cruzada para evitar data leakage.\n\n")

    cat("    Pasos configurados:\n")
    for (step in steps_applied) {
      desc <- switch(step,
        "Imputacion con mediana (variables numericas)" = "Reemplaza NA con la mediana de cada variable",
        "Imputacion con media (variables numericas)" = "Reemplaza NA con la media de cada variable",
        "Imputacion con KNN (variables numericas)" = "Reemplaza NA usando K vecinos mas cercanos",
        "Imputacion con moda (variables categoricas)" = "Reemplaza NA con el valor mas frecuente",
        "Winsorizacion de outliers" = paste0("Limita valores extremos al percentil ",
                                              outlier_percentile * 100, "-", (1 - outlier_percentile) * 100),
        "Eliminacion por alta correlacion" = paste0("Elimina variables con |r| > ", cor_threshold),
        "Eliminacion por alto VIF" = paste0("Elimina variables con VIF > ", vif_threshold),
        "Dummy encoding" = "Convierte categorias a variables binarias (0/1)",
        "Normalizacion (z-score)" = "Estandariza a media=0 y desviacion estandar=1",
        "Normalizacion (Min-Max [0,1])" = "Escala cada variable al rango [0, 1]",
        "SMOTE (balanceo)" = "Genera casos sinteticos para balancear clases (SMOTE)",
        "ADASYN (balanceo)" = "Genera casos sinteticos adaptativos (ADASYN)",
        "ROSE (balanceo)" = "Genera casos sinteticos con ROSE",
        "UP (balanceo)" = "Sobremuestreo aleatorio de la clase minoritaria",
        "DOWN (balanceo)" = "Submuestreo aleatorio de la clase mayoritaria",
        {
          # Manejar PCA y otros pasos dinamicos
          if (grepl("^PCA", step)) "Reduce dimensionalidad conservando varianza"
          else ""
        }
      )
      cat("      -", step, "\n")
      if (nchar(desc) > 0) {
        cat("        ", desc, "\n")
      }
    }

    # Mostrar detalles de tratamientos aplicados
    cat("\n    Tratamientos automaticos:\n")

    # Outliers
    if (treat_outliers) {
      if (!is.null(treatments_applied$outliers)) {
        cat("\n      [+] WINSORIZACION DE OUTLIERS:\n")
        cat("          Variables tratadas:", treatments_applied$outliers$n_vars, "\n")
        if (treatments_applied$outliers$n_vars <= 10) {
          for (v in treatments_applied$outliers$vars_treated) {
            cat("            -", v, "\n")
          }
        } else {
          for (v in treatments_applied$outliers$vars_treated[1:5]) {
            cat("            -", v, "\n")
          }
          cat("            ... y", treatments_applied$outliers$n_vars - 5, "mas\n")
        }
        cat("          Metodo: Valores limitados al percentil",
            treatments_applied$outliers$percentile * 100, "-",
            (1 - treatments_applied$outliers$percentile) * 100, "\n")
        .print_reference("winsorization")
      } else {
        cat("\n      [ok] WINSORIZACION: No se detectaron outliers significativos\n")
      }
    } else {
      cat("\n      [-] WINSORIZACION: Desactivada (treat_outliers = FALSE)\n")
    }

    # Correlacion
    if (remove_high_cor) {
      if (!is.null(treatments_applied$correlation)) {
        cat("\n      [+] ELIMINACION POR ALTA CORRELACION:\n")
        cat("          Variables eliminadas:", treatments_applied$correlation$n_vars, "\n")
        for (v in treatments_applied$correlation$vars_removed) {
          cat("            -", v, "\n")
        }
        cat("          Pares con alta correlacion detectados:\n")
        for (pair in treatments_applied$correlation$pairs[1:min(3, length(treatments_applied$correlation$pairs))]) {
          cat("            -", pair, "\n")
        }
        .print_reference("correlation_removal")
      } else {
        cat("\n      [ok] CORRELACION: No se detectaron variables con |r| >", cor_threshold, "\n")
      }
    } else {
      cat("\n      [-] CORRELACION: Desactivada (remove_high_cor = FALSE)\n")
    }

    # VIF
    if (remove_high_vif) {
      if (!is.null(treatments_applied$vif)) {
        cat("\n      [+] ELIMINACION POR ALTO VIF (MULTICOLINEALIDAD):\n")
        cat("          Variables eliminadas:", treatments_applied$vif$n_vars, "\n")
        for (i in seq_along(treatments_applied$vif$vars_removed)) {
          v <- treatments_applied$vif$vars_removed[i]
          vif_val <- treatments_applied$vif$vif_values[i]
          cat("            -", v, "(VIF =", round(vif_val, 2), ")\n")
        }
        .print_reference("vif_removal")
      } else {
        cat("\n      [ok] MULTICOLINEALIDAD: No se detectaron variables con VIF >", vif_threshold, "\n")
      }
    } else {
      cat("\n      [-] MULTICOLINEALIDAD: Desactivada (remove_high_vif = FALSE)\n")
    }

    cat("\n")
  }

  # Almacenar informacion de tratamientos en atributos de la receta
  attr(rec, "treatments_applied") <- treatments_applied

  rec
}


#' @title Detectar outliers para tratamiento
#' @noRd
.detect_outliers_for_treatment <- function(data, num_vars, iqr_multiplier = 1.5) {
  vars_with_outliers <- character()

  for (v in num_vars) {
    x <- data[[v]]
    if (all(is.na(x))) next

    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - iqr_multiplier * iqr
    upper <- q3 + iqr_multiplier * iqr
    n_outliers <- sum(x < lower | x > upper, na.rm = TRUE)

    if (n_outliers > 0) {
      vars_with_outliers <- c(vars_with_outliers, v)
    }
  }

  list(
    vars_with_outliers = vars_with_outliers,
    n_vars_with_outliers = length(vars_with_outliers)
  )
}


#' @title Detectar variables con alta correlacion
#' @noRd
.detect_high_correlation <- function(data, num_vars, threshold = 0.90) {
  # Calcular matriz de correlacion
  data_num <- data[, num_vars, drop = FALSE]
  data_num <- data_num[complete.cases(data_num), ]

  if (nrow(data_num) < 3) {
    return(list(vars_to_remove = character(), high_cor_pairs = character()))
  }

  cor_matrix <- cor(data_num, use = "pairwise.complete.obs")

  # Encontrar pares con alta correlacion
  cor_upper <- cor_matrix
  cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
  high_cors <- which(abs(cor_upper) > threshold, arr.ind = TRUE)

  if (nrow(high_cors) == 0) {
    return(list(vars_to_remove = character(), high_cor_pairs = character()))
  }

  # Determinar cuales eliminar (la que tiene menor correlacion promedio con el resto)
  vars_to_remove <- character()
  high_cor_pairs <- character()

  for (i in 1:nrow(high_cors)) {
    v1 <- rownames(cor_matrix)[high_cors[i, 1]]
    v2 <- colnames(cor_matrix)[high_cors[i, 2]]
    r <- cor_matrix[high_cors[i, 1], high_cors[i, 2]]

    high_cor_pairs <- c(high_cor_pairs, paste0(v1, " - ", v2, " (r = ", round(r, 3), ")"))

    # Si ninguna ya esta marcada para eliminar, eliminar la que tenga mayor correlacion promedio
    if (!(v1 %in% vars_to_remove) && !(v2 %in% vars_to_remove)) {
      mean_cor_v1 <- mean(abs(cor_matrix[v1, ]), na.rm = TRUE)
      mean_cor_v2 <- mean(abs(cor_matrix[v2, ]), na.rm = TRUE)

      if (mean_cor_v1 > mean_cor_v2) {
        vars_to_remove <- c(vars_to_remove, v1)
      } else {
        vars_to_remove <- c(vars_to_remove, v2)
      }
    }
  }

  list(
    vars_to_remove = unique(vars_to_remove),
    high_cor_pairs = high_cor_pairs
  )
}


#' @title Detectar variables con alto VIF
#' @noRd
.detect_high_vif <- function(data, num_vars, target, threshold = 5) {
  # Filtrar datos completos
  data_num <- data[, num_vars, drop = FALSE]
  data_num <- data_num[complete.cases(data_num), ]

  if (nrow(data_num) < length(num_vars) + 1 || length(num_vars) < 2) {
    return(list(vars_to_remove = character(), high_vif_values = numeric()))
  }

  # Calcular VIF iterativamente, eliminando la variable con mayor VIF cada vez
  vars_to_remove <- character()
  high_vif_values <- numeric()
  current_vars <- num_vars

  repeat {
    if (length(current_vars) < 2) break

    # Calcular VIF para variables actuales
    vif_values <- tryCatch({
      sapply(current_vars, function(v) {
        others <- setdiff(current_vars, v)
        if (length(others) == 0) return(1)
        formula <- as.formula(paste(v, "~", paste(others, collapse = " + ")))
        model <- lm(formula, data = data_num[, current_vars, drop = FALSE])
        r2 <- summary(model)$r.squared
        if (r2 >= 1) return(Inf)
        1 / (1 - r2)
      })
    }, error = function(e) {
      rep(1, length(current_vars))
    })

    names(vif_values) <- current_vars

    # Encontrar la variable con mayor VIF
    max_vif <- max(vif_values, na.rm = TRUE)

    if (max_vif <= threshold || is.infinite(max_vif)) break

    var_to_remove <- names(which.max(vif_values))
    vars_to_remove <- c(vars_to_remove, var_to_remove)
    high_vif_values <- c(high_vif_values, max_vif)
    current_vars <- setdiff(current_vars, var_to_remove)
  }

  list(
    vars_to_remove = vars_to_remove,
    high_vif_values = high_vif_values
  )
}

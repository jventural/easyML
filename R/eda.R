# =============================================================================
# Funciones de Analisis Exploratorio de Datos (EDA) para easyML
# =============================================================================

#' @title Analisis Exploratorio de Datos
#'
#' @description
#' Realiza un analisis exploratorio completo de los datos incluyendo
#' distribucion de variables, valores faltantes, outliers y correlaciones.
#'
#' @param data Data frame con los datos
#' @param target Nombre de la variable objetivo (opcional para EDA basico)
#' @param task Tipo de tarea: "classification", "regression" o "auto"
#' @param verbose Mostrar progreso (default: TRUE)
#'
#' @return Lista con resultados del EDA
#' @export
eda_summary <- function(data, target = NULL, task = "auto", verbose = TRUE) {

  # Si no hay target, hacer EDA basico
  if (is.null(target)) {
    return(eda_basic(data, verbose = verbose))
  }

  if (task == "auto") {
    task <- .detect_task(data[[target]])
  }

  if (verbose) {
    .print_section(1, "Carga y Exploracion de Datos")
  }

  results <- list()

  # 1.1 Estructura
  if (verbose) .print_subsection(1, 1, "Estructura de los Datos")
  results$structure <- eda_structure(data, verbose = verbose)

  # 1.2 EDA
  if (verbose) .print_subsection(1, 2, "Analisis Exploratorio de Datos (EDA)")
  if (verbose) .print_reference("eda")

  # 1.2.1 Distribucion del target
  if (task == "classification") {
    if (verbose) .print_subsubsection(1, 2, 1, "Distribucion de la Variable Objetivo")
    results$target_dist <- eda_target_classification(data, target, verbose)
  } else {
    if (verbose) .print_subsubsection(1, 2, 1, paste("Distribucion de", target))
    results$target_dist <- eda_target_regression(data, target, verbose)

    if (verbose) .print_subsubsection(1, 2, 2, "Test de Normalidad")
    results$normality <- eda_normality(data, target, verbose)
    if (verbose) .print_reference("normality")
  }

  # Missing values
  missing_idx <- if (task == "classification") 2 else 3
  if (verbose) .print_subsubsection(1, 2, missing_idx, "Analisis de Valores Faltantes")
  results$missing <- eda_missing(data, verbose)
  if (verbose) .print_reference("missing")

  # Variables categoricas
  cat_idx <- missing_idx + 1
  if (task == "classification") {
    if (verbose) .print_subsubsection(1, 2, cat_idx, "Variables Categoricas")
  } else {
    if (verbose) .print_subsubsection(1, 2, cat_idx, paste("Variables Categoricas vs", target))
  }
  results$categorical <- eda_categorical(data, target, verbose)

  # Variables numericas
  num_idx <- cat_idx + 1
  if (verbose) .print_subsubsection(1, 2, num_idx, "Variables Numericas")
  results$numeric <- eda_numeric(data, target, verbose)

  # Outliers
  out_idx <- num_idx + 1
  if (verbose) .print_subsubsection(1, 2, out_idx, "Deteccion de Outliers")
  results$outliers <- eda_outliers(data, verbose = verbose)
  if (verbose) .print_reference("outliers")

  # Correlacion
  cor_idx <- out_idx + 1
  if (verbose) .print_subsubsection(1, 2, cor_idx, "Matriz de Correlacion")
  results$correlation <- eda_correlation(data, target, verbose)
  if (verbose) .print_reference("correlation")

  # VIF
  vif_idx <- cor_idx + 1
  if (verbose) .print_subsubsection(1, 2, vif_idx, "Analisis de Multicolinealidad (VIF)")
  results$vif <- eda_vif(data, target, verbose)
  if (verbose) .print_reference("vif")

  # Data Leakage (verificacion basica)
  leakage_idx <- vif_idx + 1
  if (verbose) .print_subsubsection(1, 2, leakage_idx, "Verificacion de Data Leakage")
  results$leakage_check <- eda_leakage(data, target, verbose)
  if (verbose) .print_reference("leakage")

  results$task <- task
  results$target <- target
  class(results) <- c("easyml_eda", "list")

  return(results)
}


#' @title Estructura de los datos
#' @param data Data frame
#' @param verbose Mostrar resultados
#' @return Lista con informacion estructural
#' @export
eda_structure <- function(data, verbose = TRUE) {
  n_obs <- nrow(data)
  n_vars <- ncol(data)

  # Tipos de variables
  var_types <- sapply(data, function(x) {
    if (is.numeric(x)) "numeric"
    else if (is.factor(x) || is.character(x)) "categorical"
    else class(x)[1]
  })

  n_numeric <- sum(var_types == "numeric")
  n_categorical <- sum(var_types == "categorical")

  if (verbose) {
    cat("    Observaciones:", n_obs, "\n")
    cat("    Variables:", n_vars, "\n")
    cat("      - Numericas:", n_numeric, "\n")
    cat("      - Categoricas:", n_categorical, "\n")
  }

  list(
    n_obs = n_obs,
    n_vars = n_vars,
    n_numeric = n_numeric,
    n_categorical = n_categorical,
    var_types = var_types
  )
}


#' @title Distribucion del target (clasificacion)
#' @export
eda_target_classification <- function(data, target, verbose = TRUE) {
  y <- data[[target]]
  if (!is.factor(y)) y <- as.factor(y)

  freq <- table(y)
  prop <- prop.table(freq) * 100

  if (verbose) {
    cat("    Distribucion de clases:\n")
    for (i in seq_along(freq)) {
      cat("      -", names(freq)[i], ":", freq[i],
          "(", round(prop[i], 1), "%)\n")
    }

    # Balance
    ratio <- max(freq) / min(freq)
    if (ratio > 3) {
      cat("    [!] Clases desbalanceadas (ratio:", round(ratio, 1), ")\n")
    }
  }

  list(
    frequencies = freq,
    proportions = prop,
    n_classes = length(freq),
    imbalance_ratio = max(freq) / min(freq)
  )
}


#' @title Distribucion del target (regresion)
#' @export
eda_target_regression <- function(data, target, verbose = TRUE) {
  y <- data[[target]]

  stats_y <- list(
    mean = mean(y, na.rm = TRUE),
    sd = sd(y, na.rm = TRUE),
    median = median(y, na.rm = TRUE),
    min = min(y, na.rm = TRUE),
    max = max(y, na.rm = TRUE),
    skewness = (mean(y, na.rm = TRUE) - median(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
  )

  if (verbose) {
    cat("    Estadisticas de", target, ":\n")
    cat("      - Media:", round(stats_y$mean, 2), "\n")
    cat("      - DE:", round(stats_y$sd, 2), "\n")
    cat("      - Mediana:", round(stats_y$median, 2), "\n")
    cat("      - Rango: [", round(stats_y$min, 2), ",", round(stats_y$max, 2), "]\n")
  }

  stats_y
}


#' @title Test de normalidad
#' @export
eda_normality <- function(data, target, verbose = TRUE) {
  y <- data[[target]]

  if (!is.numeric(y)) {
    if (verbose) {
      cat("    [!] Variable objetivo '", target, "' no es numerica.\n", sep = "")
      cat("        Shapiro-Wilk solo aplica a variables continuas (regresion).\n")
    }
    return(invisible(NULL))
  }

  # Shapiro-Wilk (max 5000 obs)
  if (length(y) <= 5000) {
    shapiro_test <- shapiro.test(y)
  } else {
    shapiro_test <- shapiro.test(sample(y, 5000))
  }

  if (verbose) {
    cat("    Shapiro-Wilk test:\n")
    cat("      - W =", round(shapiro_test$statistic, 4), "\n")
    cat("      - p-value =", format.pval(shapiro_test$p.value, digits = 4), "\n")
    if (shapiro_test$p.value < 0.05) {
      cat("      - [!] Distribucion NO normal (p < 0.05)\n")
    } else {
      cat("      - Distribucion aproximadamente normal\n")
    }
  }

  shapiro_test
}


#' @title Analisis de valores faltantes
#' @export
eda_missing <- function(data, verbose = TRUE) {
  missing_count <- colSums(is.na(data))
  missing_pct <- round(missing_count / nrow(data) * 100, 2)

  missing_df <- data.frame(
    variable = names(missing_count),
    n_missing = as.numeric(missing_count),
    pct_missing = as.numeric(missing_pct)
  )
  missing_df <- missing_df[order(-missing_df$n_missing), ]

  total_missing <- sum(missing_count)
  total_pct <- round(total_missing / (nrow(data) * ncol(data)) * 100, 2)

  if (verbose) {
    cat("    Total valores faltantes:", total_missing,
        "(", total_pct, "% del dataset)\n")

    vars_with_missing <- missing_df[missing_df$n_missing > 0, ]
    if (nrow(vars_with_missing) > 0) {
      cat("    Variables con missing:\n")
      for (i in 1:min(5, nrow(vars_with_missing))) {
        cat("      -", vars_with_missing$variable[i], ":",
            vars_with_missing$n_missing[i],
            "(", vars_with_missing$pct_missing[i], "%)\n")
      }
    } else {
      cat("    [OK] No hay valores faltantes\n")
    }
  }

  list(
    by_variable = missing_df,
    total = total_missing,
    total_pct = total_pct
  )
}


#' @title Analisis de variables categoricas
#' @export
eda_categorical <- function(data, target, verbose = TRUE) {
  # Identificar variables categoricas
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  cat_vars <- setdiff(cat_vars, target)

  if (verbose) {
    cat("    Variables categoricas:", length(cat_vars), "\n")
    for (v in cat_vars) {
      n_levels <- length(unique(data[[v]]))
      cat("      -", v, ":", n_levels, "niveles\n")
    }
  }

  list(
    variables = cat_vars,
    n_levels = sapply(data[cat_vars], function(x) length(unique(x)))
  )
}


#' @title Analisis de variables numericas
#' @export
eda_numeric <- function(data, target, verbose = TRUE) {
  num_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- setdiff(num_vars, target)

  if (length(num_vars) == 0) {
    if (verbose) cat("    No hay variables numericas\n")
    return(list(variables = character()))
  }

  stats <- do.call(rbind, lapply(num_vars, function(v) {
    x <- data[[v]]
    data.frame(
      variable = v,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  }))

  if (verbose) {
    cat("    Variables numericas:", length(num_vars), "\n")
    cat("    Resumen estadistico:\n")
    print(stats, row.names = FALSE)
  }

  list(variables = num_vars, stats = stats)
}


#' @title Deteccion de outliers
#' @param data Data frame
#' @param verbose Mostrar resultados
#' @param iqr_multiplier Multiplicador del IQR (default: 3 para ser conservador)
#' @export
eda_outliers <- function(data, verbose = TRUE, iqr_multiplier = 3) {
  num_vars <- names(data)[sapply(data, is.numeric)]

  if (length(num_vars) == 0) {
    if (verbose) cat("    No hay variables numericas para analizar outliers\n")
    return(data.frame(variable = character(), n_outliers = integer(), pct = numeric()))
  }

  outlier_summary <- lapply(num_vars, function(v) {
    x <- data[[v]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - iqr_multiplier * iqr
    upper <- q3 + iqr_multiplier * iqr
    n_outliers <- sum(x < lower | x > upper, na.rm = TRUE)
    pct_outliers <- round(n_outliers / sum(!is.na(x)) * 100, 2)

    data.frame(variable = v, n_outliers = n_outliers, pct = pct_outliers)
  })

  outlier_df <- do.call(rbind, outlier_summary)
  outlier_df <- outlier_df[order(-outlier_df$n_outliers), ]

  if (verbose) {
    vars_with_outliers <- outlier_df[outlier_df$n_outliers > 0, ]
    cat("    Metodo: IQR x", iqr_multiplier, "(conservador)\n")
    cat("    Variables con outliers:", nrow(vars_with_outliers), "de", length(num_vars), "\n")
    if (nrow(vars_with_outliers) > 0) {
      for (i in 1:min(5, nrow(vars_with_outliers))) {
        cat("      -", vars_with_outliers$variable[i], ":",
            vars_with_outliers$n_outliers[i], "outliers",
            "(", vars_with_outliers$pct[i], "%)\n")
      }
    } else {
      cat("    [OK] No se detectaron outliers extremos\n")
    }
  }

  outlier_df
}


#' @title Matriz de correlacion
#' @export
eda_correlation <- function(data, target, verbose = TRUE) {
  num_vars <- names(data)[sapply(data, is.numeric)]

  if (length(num_vars) < 2) {
    if (verbose) cat("    Insuficientes variables numericas para correlacion\n")
    return(NULL)
  }

  cor_matrix <- cor(data[num_vars], use = "pairwise.complete.obs")

  # Correlaciones con target
  if (target %in% num_vars) {
    target_cors <- cor_matrix[target, ]
    target_cors <- sort(abs(target_cors), decreasing = TRUE)
    target_cors <- target_cors[names(target_cors) != target]

    if (verbose) {
      cat("    Top correlaciones con", target, ":\n")
      for (i in 1:min(5, length(target_cors))) {
        cat("      -", names(target_cors)[i], ":", round(target_cors[i], 3), "\n")
      }
    }
  }

  # Correlaciones altas entre predictores
  cor_upper <- cor_matrix
  cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
  high_cors <- which(abs(cor_upper) > 0.8, arr.ind = TRUE)

  if (verbose && nrow(high_cors) > 0) {
    cat("    [!] Correlaciones altas (|r| > 0.8):\n")
    for (i in 1:min(3, nrow(high_cors))) {
      v1 <- rownames(cor_matrix)[high_cors[i, 1]]
      v2 <- colnames(cor_matrix)[high_cors[i, 2]]
      cat("      -", v1, "-", v2, ":", round(cor_matrix[high_cors[i, 1], high_cors[i, 2]], 3), "\n")
    }
  }

  cor_matrix
}


#' @title Analisis VIF
#' @export
eda_vif <- function(data, target, verbose = TRUE) {
  num_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- setdiff(num_vars, target)

  if (length(num_vars) < 2) {
    if (verbose) cat("    Insuficientes variables para VIF\n")
    return(NULL)
  }

  # Calcular VIF simple
  tryCatch({
    df_num <- data[, num_vars, drop = FALSE]
    df_num <- df_num[complete.cases(df_num), ]

    if (nrow(df_num) < ncol(df_num) + 1) {
      if (verbose) cat("    Insuficientes observaciones para VIF\n")
      return(NULL)
    }

    # VIF manual
    vif_values <- sapply(num_vars, function(v) {
      others <- setdiff(num_vars, v)
      formula <- as.formula(paste(v, "~", paste(others, collapse = " + ")))
      model <- lm(formula, data = df_num)
      r2 <- summary(model)$r.squared
      1 / (1 - r2)
    })

    vif_df <- data.frame(
      variable = names(vif_values),
      VIF = round(vif_values, 2)
    )
    vif_df <- vif_df[order(-vif_df$VIF), ]

    if (verbose) {
      high_vif <- vif_df[vif_df$VIF > 5, ]
      if (nrow(high_vif) > 0) {
        cat("    [!] Variables con VIF > 5 (posible multicolinealidad):\n")
        for (i in 1:nrow(high_vif)) {
          cat("      -", high_vif$variable[i], ":", high_vif$VIF[i], "\n")
        }
      } else {
        cat("    [OK] No hay multicolinealidad severa (VIF < 5)\n")
      }
    }

    vif_df
  }, error = function(e) {
    if (verbose) cat("    No se pudo calcular VIF:", conditionMessage(e), "\n")
    NULL
  })
}


#' @title EDA Basico (sin variable objetivo)
#'
#' @description
#' Realiza un analisis exploratorio basico sin requerir variable objetivo.
#' Util para explorar un dataset antes de definir el modelo.
#'
#' @param data Data frame con los datos
#' @param verbose Mostrar progreso (default: TRUE)
#'
#' @return Lista con resultados del EDA basico
#' @export
eda_basic <- function(data, verbose = TRUE) {

  if (verbose) {
    .print_section(1, "Analisis Exploratorio de Datos")
  }

  results <- list()

  # 1.1 Estructura
  if (verbose) .print_subsection(1, 1, "Estructura del Dataset")
  results$structure <- eda_structure(data, verbose = verbose)

  # 1.2 Valores faltantes
  if (verbose) .print_subsection(1, 2, "Valores Faltantes")
  results$missing <- eda_missing(data, verbose = verbose)

  # 1.3 Variables numericas
  if (verbose) .print_subsection(1, 3, "Variables Numericas")
  results$numeric <- eda_numeric_basic(data, verbose = verbose)

  # 1.4 Variables categoricas
  if (verbose) .print_subsection(1, 4, "Variables Categoricas")
  results$categorical <- eda_categorical_basic(data, verbose = verbose)

  # 1.5 Outliers
  if (verbose) .print_subsection(1, 5, "Deteccion de Outliers")
  results$outliers <- eda_outliers(data, verbose = verbose)

  # 1.6 Correlaciones
  if (verbose) .print_subsection(1, 6, "Matriz de Correlacion")
  results$correlation <- eda_correlation_basic(data, verbose = verbose)

  class(results) <- c("easyml_eda", "list")
  return(results)
}


#' @title Analisis de variables numericas (basico)
#' @param data Data frame
#' @param verbose Mostrar resultados
#' @return Data frame con estadisticos
#' @export
eda_numeric_basic <- function(data, verbose = TRUE) {
  num_vars <- names(data)[sapply(data, is.numeric)]

  if (length(num_vars) == 0) {
    if (verbose) cat("    No hay variables numericas\n")
    return(data.frame())
  }

  stats <- do.call(rbind, lapply(num_vars, function(v) {
    x <- data[[v]]
    data.frame(
      variable = v,
      n = sum(!is.na(x)),
      missing = sum(is.na(x)),
      mean = round(mean(x, na.rm = TRUE), 2),
      sd = round(sd(x, na.rm = TRUE), 2),
      min = round(min(x, na.rm = TRUE), 2),
      median = round(median(x, na.rm = TRUE), 2),
      max = round(max(x, na.rm = TRUE), 2)
    )
  }))

  if (verbose) {
    cat("    Variables numericas:", length(num_vars), "\n\n")
    print(stats, row.names = FALSE)
  }

  stats
}


#' @title Analisis de variables categoricas (basico)
#' @param data Data frame
#' @param verbose Mostrar resultados
#' @return Lista con informacion de variables categoricas
#' @export
eda_categorical_basic <- function(data, verbose = TRUE) {
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

  if (length(cat_vars) == 0) {
    if (verbose) cat("    No hay variables categoricas\n")
    return(list(variables = character(), n_levels = integer()))
  }

  n_levels <- sapply(data[cat_vars], function(x) length(unique(x)))

  if (verbose) {
    cat("    Variables categoricas:", length(cat_vars), "\n")
    for (v in cat_vars) {
      cat("      -", v, ":", n_levels[v], "niveles\n")
    }
  }

  list(variables = cat_vars, n_levels = n_levels)
}


#' @title Matriz de correlacion (basico)
#' @param data Data frame
#' @param verbose Mostrar resultados
#' @return Matriz de correlacion
#' @export
eda_correlation_basic <- function(data, verbose = TRUE) {
  num_vars <- names(data)[sapply(data, is.numeric)]

  if (length(num_vars) < 2) {
    if (verbose) cat("    Insuficientes variables numericas para correlacion\n")
    return(NULL)
  }

  cor_matrix <- cor(data[num_vars], use = "pairwise.complete.obs")

  # Correlaciones altas entre variables
  cor_upper <- cor_matrix
  cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
  high_cors <- which(abs(cor_upper) > 0.7, arr.ind = TRUE)

  if (verbose) {
    if (nrow(high_cors) > 0) {
      cat("    Correlaciones altas (|r| > 0.7):\n")
      for (i in 1:min(5, nrow(high_cors))) {
        v1 <- rownames(cor_matrix)[high_cors[i, 1]]
        v2 <- colnames(cor_matrix)[high_cors[i, 2]]
        r <- cor_matrix[high_cors[i, 1], high_cors[i, 2]]
        cat("      -", v1, "-", v2, ":", round(r, 3), "\n")
      }
      if (nrow(high_cors) > 5) {
        cat("      ... y", nrow(high_cors) - 5, "mas\n")
      }
    } else {
      cat("    [OK] No hay correlaciones muy altas (|r| > 0.7)\n")
    }
  }

  cor_matrix
}


#' @title Verificacion de Data Leakage (basico)
#' @description
#' Detecta posibles indicadores de data leakage ANTES de entrenar el modelo.
#' Verifica: correlacion muy alta con target (>0.95) y posibles IDs.
#' @param data Data frame
#' @param target Variable objetivo
#' @param verbose Mostrar resultados
#' @return Lista con alertas de leakage
#' @export
eda_leakage <- function(data, target, verbose = TRUE) {

  warnings <- list()
  warning_count <- 0

  if (verbose) {
    cat("    (Data leakage = usar informacion del futuro para predecir)\n\n")
    cat("    Verificando:\n")
    cat("      - Variables con correlacion > 0.95 con target\n")
    cat("      - Posibles IDs (valores unicos por fila)\n\n")
  }

  # 1. Variables con correlacion muy alta con target (>0.95)
  num_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- setdiff(num_vars, target)

  if (length(num_vars) > 0) {
    # Convertir target a numerico si es factor
    if (is.factor(data[[target]])) {
      target_numeric <- as.numeric(data[[target]])
    } else {
      target_numeric <- data[[target]]
    }

    high_cor_vars <- c()
    high_cor_values <- c()

    for (v in num_vars) {
      cor_val <- tryCatch({
        abs(cor(data[[v]], target_numeric, use = "complete.obs"))
      }, error = function(e) NA)

      if (!is.na(cor_val) && cor_val > 0.95) {
        high_cor_vars <- c(high_cor_vars, v)
        high_cor_values <- c(high_cor_values, round(cor_val, 3))
        warning_count <- warning_count + 1
      }
    }

    if (length(high_cor_vars) > 0) {
      warnings$high_correlation <- list(
        type = "CORRELACION MUY ALTA CON TARGET",
        variables = high_cor_vars,
        correlations = high_cor_values,
        message = "Posible leakage: variable casi identica al target"
      )
    }
  }

  # 2. Posibles IDs (variables con valores unicos por fila)
  possible_ids <- c()
  for (v in names(data)) {
    if (v == target) next
    n_unique <- length(unique(data[[v]]))
    n_total <- nrow(data)

    # Si tiene valores unicos por fila o casi (>95%)
    if (n_unique == n_total || (n_unique > 0.95 * n_total && !is.numeric(data[[v]]))) {
      possible_ids <- c(possible_ids, v)
      warning_count <- warning_count + 1
    }
  }

  if (length(possible_ids) > 0) {
    warnings$possible_ids <- list(
      type = "POSIBLES IDs",
      variables = possible_ids,
      message = "Variables con valores unicos (no aportan informacion predictiva)"
    )
  }

  # Mostrar resultados
  if (verbose) {
    if (warning_count > 0) {
      cat("    [!] Se detectaron", warning_count, "posibles indicadores de leakage:\n\n")

      if (!is.null(warnings$high_correlation)) {
        cat("      CORRELACION MUY ALTA CON TARGET (r > 0.95):\n")
        for (i in seq_along(warnings$high_correlation$variables)) {
          cat("        -", warnings$high_correlation$variables[i],
              "(r =", warnings$high_correlation$correlations[i], ")\n")
        }
        cat("        [!] Estas variables podrian contener informacion del target\n")
        cat("        [!] Considere eliminarlas antes de modelar\n\n")
      }

      if (!is.null(warnings$possible_ids)) {
        cat("      POSIBLES IDs (valores unicos por fila):\n")
        for (v in warnings$possible_ids$variables) {
          cat("        -", v, "\n")
        }
        cat("        [!] Los IDs no aportan informacion predictiva\n")
        cat("        [!] Elimine estas variables antes de modelar\n\n")
      }

    } else {
      cat("    [ok] No se detectaron indicadores de data leakage\n")
      cat("         - Ninguna variable con correlacion > 0.95 con target\n")
      cat("         - No se detectaron posibles IDs\n")
    }
  }

  list(
    warnings = warnings,
    n_warnings = warning_count,
    has_leakage = warning_count > 0
  )
}

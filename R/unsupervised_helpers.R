
# =============================================================================
# Funciones auxiliares para Aprendizaje No Supervisado
# =============================================================================

#' Validar inputs de unsupervised_ml
#' @noRd
.validate_unsupervised_inputs <- function(data, methods, exclude_cols) {

  # Validar data

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' debe ser un data.frame o matrix.")
  }

  if (nrow(data) < 10) {
    stop("Se necesitan al menos 10 observaciones.")
  }

  if (ncol(data) < 2) {
    stop("Se necesitan al menos 2 variables.")
  }

  # Validar methods
  valid_methods <- c("clustering", "reduction", "anomaly")
  invalid <- setdiff(methods, valid_methods)
  if (length(invalid) > 0) {
    stop("Metodos no validos: ", paste(invalid, collapse = ", "),
         "\nOpciones: ", paste(valid_methods, collapse = ", "))
  }

  # Validar exclude_cols
  if (!is.null(exclude_cols)) {
    missing_cols <- setdiff(exclude_cols, names(data))
    if (length(missing_cols) > 0) {
      warning("Columnas no encontradas (seran ignoradas): ",
              paste(missing_cols, collapse = ", "))
    }
  }

  # Verificar que quedan variables numericas
  if (!is.null(exclude_cols)) {
    remaining <- names(data)[!names(data) %in% exclude_cols]
  } else {
    remaining <- names(data)
  }

  n_numeric <- sum(sapply(data[remaining], is.numeric))
  if (n_numeric < 2) {
    stop("Se necesitan al menos 2 variables numericas despues de excluir columnas.")
  }

  invisible(TRUE)
}


#' Obtener perfil de clusters (caracteristicas por cluster)
#'
#' @param result Objeto de clase \code{unsupervisedml}.
#' @param algorithm Nombre del algoritmo (default: mejor).
#' @param original_data Data frame original con todas las variables.
#'
#' @return Data frame con medias por cluster y diferencias.
#'
#' @export
cluster_profile <- function(result, algorithm = NULL, original_data = NULL) {

  if (!inherits(result, "unsupervisedml")) {
    stop("El objeto debe ser de clase 'unsupervisedml'.")
  }

  if (is.null(result$clustering)) {
    stop("No hay resultados de clustering disponibles.")
  }

  # Seleccionar algoritmo
  if (is.null(algorithm)) {
    if (!is.null(result$evaluation$best_algorithm)) {
      algorithm <- result$evaluation$best_algorithm$algorithm
    } else {
      algorithm <- names(result$clustering$results)[1]
    }
  }

  if (!algorithm %in% names(result$clustering$results)) {
    stop("Algoritmo '", algorithm, "' no encontrado. Disponibles: ",
         paste(names(result$clustering$results), collapse = ", "))
  }

  labels <- result$clustering$results[[algorithm]]$labels

  # Datos a perfilar
  if (!is.null(original_data)) {
    data_to_use <- original_data
  } else {
    data_to_use <- result$preprocessed$data_before_norm
  }

  # Solo variables numericas
  numeric_cols <- names(data_to_use)[sapply(data_to_use, is.numeric)]
  data_num <- data_to_use[, numeric_cols, drop = FALSE]

  # Filtrar ruido (DBSCAN)
  valid_idx <- labels > 0
  data_num <- data_num[valid_idx, , drop = FALSE]
  labels_valid <- labels[valid_idx]

  # Medias por cluster
  profiles <- aggregate(. ~ Cluster, data = cbind(Cluster = labels_valid, data_num), mean)

  # Agregar fila global
  global_means <- c(Cluster = 0, colMeans(data_num, na.rm = TRUE))
  profiles <- rbind(profiles, global_means)
  profiles$Cluster[nrow(profiles)] <- "Global"

  # Tamanios
  sizes <- c(table(labels_valid), Global = sum(valid_idx))

  cat("\n")
  cat(.line("="), "\n")
  cat(" Perfil de Clusters -", algorithm, "\n")
  cat(.line("="), "\n\n")

  cat("Tamanio de clusters:\n")
  for (i in seq_along(sizes)) {
    cat(sprintf("  Cluster %s: %d (%.1f%%)\n",
                names(sizes)[i], sizes[i],
                sizes[i] / sum(valid_idx) * 100))
  }
  cat("\n")

  cat("Medias por cluster:\n")
  print(profiles, row.names = FALSE, digits = 3)

  invisible(profiles)
}


#' Asignar clusters a nuevos datos
#'
#' @param result Objeto de clase \code{unsupervisedml}.
#' @param newdata Data frame con nuevos datos.
#' @param algorithm Algoritmo a usar (default: mejor).
#'
#' @return Vector de labels de cluster.
#'
#' @export
predict.unsupervisedml <- function(result, newdata, algorithm = NULL, ...) {

  if (is.null(result$clustering)) {
    stop("No hay resultados de clustering disponibles.")
  }

  if (is.null(algorithm)) {
    if (!is.null(result$evaluation$best_algorithm)) {
      algorithm <- result$evaluation$best_algorithm$algorithm
    } else {
      algorithm <- names(result$clustering$results)[1]
    }
  }

  # Preprocesar newdata
  prep_params <- result$preprocessed

  # Seleccionar columnas usadas
  used_cols <- names(prep_params$data_clean)
  if (!all(used_cols %in% names(newdata))) {
    missing <- setdiff(used_cols, names(newdata))
    stop("Faltan variables en newdata: ", paste(missing, collapse = ", "))
  }

  newdata_clean <- newdata[, used_cols, drop = FALSE]

  # Normalizar
  if (!is.null(prep_params$norm_params)) {
    for (v in names(prep_params$norm_params)) {
      if (v %in% names(newdata_clean)) {
        params <- prep_params$norm_params[[v]]
        if (params$method == "zscore") {
          newdata_clean[[v]] <- (newdata_clean[[v]] - params$center) / params$scale
        } else if (params$method == "minmax") {
          range_val <- params$max - params$min
          if (range_val > 0) {
            newdata_clean[[v]] <- (newdata_clean[[v]] - params$min) / range_val
          }
        }
      }
    }
  }

  new_mat <- as.matrix(newdata_clean)

  # Predecir segun algoritmo
  if (algorithm == "kmeans") {
    centers <- result$clustering$results$kmeans$centers
    # Asignar al centroide mas cercano
    dists <- apply(new_mat, 1, function(x) {
      apply(centers, 1, function(c) sum((x - c)^2))
    })
    if (is.matrix(dists)) {
      labels <- apply(dists, 2, which.min)
    } else {
      labels <- which.min(dists)
    }

  } else if (algorithm == "gmm") {
    if (!is.null(result$clustering$results$gmm$model)) {
      pred <- predict(result$clustering$results$gmm$model, newdata = new_mat)
      labels <- pred$classification
    } else {
      stop("Modelo GMM no disponible para prediccion.")
    }

  } else if (algorithm == "hierarchical") {
    # Asignar al centroide mas cercano del clustering original
    orig_labels <- result$clustering$results$hierarchical$labels
    orig_data <- as.matrix(result$preprocessed$data_clean)
    centers <- aggregate(orig_data, by = list(Cluster = orig_labels), FUN = mean)
    centers_mat <- as.matrix(centers[, -1])

    dists <- apply(new_mat, 1, function(x) {
      apply(centers_mat, 1, function(c) sum((x - c)^2))
    })
    if (is.matrix(dists)) {
      labels <- centers$Cluster[apply(dists, 2, which.min)]
    } else {
      labels <- centers$Cluster[which.min(dists)]
    }

  } else {
    stop("Prediccion no soportada para algoritmo: ", algorithm)
  }

  return(labels)
}

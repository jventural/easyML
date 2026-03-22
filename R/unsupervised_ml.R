
# =============================================================================
# unsupervised_ml: Funcion Principal para Aprendizaje No Supervisado
# =============================================================================

#' @title unsupervised_ml: Aprendizaje No Supervisado Automatizado
#'
#' @description
#' Funcion principal que ejecuta un pipeline completo de aprendizaje no supervisado.
#' Automatiza EDA, preprocesamiento, reduccion de dimensionalidad, clustering,
#' evaluacion de clusters, deteccion de anomalias y visualizacion.
#'
#' @param data Data frame con los datos.
#' @param methods Vector con los tipos de analisis a ejecutar:
#'   "clustering", "reduction", "anomaly" (default: todos).
#' @param clustering_algorithms Algoritmos de clustering:
#'   "kmeans", "hierarchical", "dbscan", "gmm" (default: todos).
#' @param k Numero de clusters. NULL (default) busca automaticamente el optimo.
#' @param k_range Rango de k a evaluar cuando k = NULL (default: 2:10).
#' @param distance Metrica de distancia: "euclidean", "manhattan", "gower" (default: "euclidean").
#' @param hclust_method Metodo de enlace para clustering jerarquico:
#'   "ward.D2", "complete", "average", "single" (default: "ward.D2").
#' @param dbscan_eps Radio epsilon para DBSCAN. NULL (default) lo calcula automaticamente.
#' @param dbscan_minPts Puntos minimos para DBSCAN (default: 5).
#' @param gmm_type Tipo de covarianza para GMM/mclust (default: "VVV").
#' @param reduction_methods Metodos de reduccion de dimensionalidad:
#'   "pca", "tsne", "umap" (default: todos).
#' @param n_components Numero de dimensiones objetivo para t-SNE y UMAP (default: 2).
#' @param pca_threshold Varianza acumulada a conservar con PCA (default: 0.95).
#' @param tsne_perplexity Perplejidad para t-SNE (default: 30).
#' @param umap_n_neighbors Vecinos para UMAP (default: 15).
#' @param umap_min_dist Distancia minima para UMAP (default: 0.1).
#' @param anomaly_methods Metodos de deteccion de anomalias:
#'   "isolation_forest", "lof", "mahalanobis" (default: todos).
#' @param contamination Proporcion esperada de anomalias (default: 0.05).
#' @param lof_k Numero de vecinos para LOF (default: 20).
#' @param exclude_cols Nombres de columnas a excluir del analisis (IDs, texto, etc.).
#' @param impute Imputar valores faltantes (default: TRUE).
#' @param impute_method Metodo de imputacion: "knn", "median", "mean" (default: "median").
#' @param normalize Normalizar variables numericas (default: TRUE).
#' @param normalize_method Metodo: "zscore", "minmax" (default: "zscore").
#' @param treat_outliers Winsorizar outliers antes de analizar (default: FALSE).
#' @param outlier_percentile Percentil para winsorizacion (default: 0.05).
#' @param remove_high_cor Eliminar variables con alta correlacion (default: TRUE).
#' @param cor_threshold Umbral de correlacion (default: 0.90).
#' @param run_eda Ejecutar analisis exploratorio (default: TRUE).
#' @param seed Semilla para reproducibilidad (default: 2024).
#' @param verbose Mostrar progreso en consola (default: TRUE).
#'
#' @return Objeto de clase \code{unsupervisedml} con resultados completos.
#'
#' @examples
#' \dontrun{
#' # Ejemplo basico con iris (sin Species)
#' resultado <- unsupervised_ml(
#'   data = iris[, 1:4]
#' )
#'
#' # Solo clustering con k fijo
#' resultado <- unsupervised_ml(
#'   data = mis_datos,
#'   methods = "clustering",
#'   k = 3
#' )
#'
#' # Analisis completo con configuracion
#' resultado <- unsupervised_ml(
#'   data = mis_datos,
#'   exclude_cols = c("id", "nombre"),
#'   clustering_algorithms = c("kmeans", "hierarchical"),
#'   k_range = 2:8,
#'   reduction_methods = c("pca", "umap"),
#'   anomaly_methods = "lof"
#' )
#'
#' # Ver resultados
#' print(resultado)
#' summary(resultado)
#' plot(resultado)
#' }
#'
#' @export
unsupervised_ml <- function(data,
                            methods = c("clustering", "reduction", "anomaly"),
                            # Clustering
                            clustering_algorithms = c("kmeans", "hierarchical", "dbscan", "gmm"),
                            k = NULL,
                            k_range = 2:10,
                            distance = c("euclidean", "manhattan", "gower"),
                            hclust_method = c("ward.D2", "complete", "average", "single"),
                            dbscan_eps = NULL,
                            dbscan_minPts = 5,
                            gmm_type = "VVV",
                            # Reduction
                            reduction_methods = c("pca", "tsne", "umap"),
                            n_components = 2,
                            pca_threshold = 0.95,
                            tsne_perplexity = 30,
                            umap_n_neighbors = 15,
                            umap_min_dist = 0.1,
                            # Anomaly
                            anomaly_methods = c("isolation_forest", "lof", "mahalanobis"),
                            contamination = 0.05,
                            lof_k = 20,
                            # Preprocessing
                            exclude_cols = NULL,
                            impute = TRUE,
                            impute_method = c("median", "knn", "mean"),
                            normalize = TRUE,
                            normalize_method = c("zscore", "minmax"),
                            treat_outliers = FALSE,
                            outlier_percentile = 0.05,
                            remove_high_cor = TRUE,
                            cor_threshold = 0.90,
                            # General
                            run_eda = TRUE,
                            seed = 2024,
                            verbose = TRUE) {

  # =========================================================================
  # CAPTURA INTERNA DEL VERBOSE
  # =========================================================================
  if (verbose) {
    temp_file <- tempfile(fileext = ".txt")
    sink(temp_file, split = TRUE)

    tryCatch({
      resultado <- .unsupervised_ml_internal(
        data = data, methods = methods,
        clustering_algorithms = clustering_algorithms,
        k = k, k_range = k_range, distance = distance,
        hclust_method = hclust_method,
        dbscan_eps = dbscan_eps, dbscan_minPts = dbscan_minPts,
        gmm_type = gmm_type,
        reduction_methods = reduction_methods,
        n_components = n_components, pca_threshold = pca_threshold,
        tsne_perplexity = tsne_perplexity,
        umap_n_neighbors = umap_n_neighbors, umap_min_dist = umap_min_dist,
        anomaly_methods = anomaly_methods,
        contamination = contamination, lof_k = lof_k,
        exclude_cols = exclude_cols,
        impute = impute, impute_method = impute_method,
        normalize = normalize, normalize_method = normalize_method,
        treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
        remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
        run_eda = run_eda, seed = seed, verbose = TRUE
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
    resultado <- .unsupervised_ml_internal(
      data = data, methods = methods,
      clustering_algorithms = clustering_algorithms,
      k = k, k_range = k_range, distance = distance,
      hclust_method = hclust_method,
      dbscan_eps = dbscan_eps, dbscan_minPts = dbscan_minPts,
      gmm_type = gmm_type,
      reduction_methods = reduction_methods,
      n_components = n_components, pca_threshold = pca_threshold,
      tsne_perplexity = tsne_perplexity,
      umap_n_neighbors = umap_n_neighbors, umap_min_dist = umap_min_dist,
      anomaly_methods = anomaly_methods,
      contamination = contamination, lof_k = lof_k,
      exclude_cols = exclude_cols,
      impute = impute, impute_method = impute_method,
      normalize = normalize, normalize_method = normalize_method,
      treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
      remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
      run_eda = run_eda, seed = seed, verbose = FALSE
    )
  }

  return(resultado)
}


# =============================================================================
# FUNCION INTERNA PRINCIPAL
# =============================================================================

#' @noRd
.unsupervised_ml_internal <- function(data,
                                       methods,
                                       clustering_algorithms,
                                       k, k_range, distance, hclust_method,
                                       dbscan_eps, dbscan_minPts, gmm_type,
                                       reduction_methods,
                                       n_components, pca_threshold,
                                       tsne_perplexity,
                                       umap_n_neighbors, umap_min_dist,
                                       anomaly_methods,
                                       contamination, lof_k,
                                       exclude_cols,
                                       impute, impute_method,
                                       normalize, normalize_method,
                                       treat_outliers, outlier_percentile,
                                       remove_high_cor, cor_threshold,
                                       run_eda, seed, verbose) {

  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  set.seed(seed)
  distance <- match.arg(distance, c("euclidean", "manhattan", "gower"))
  hclust_method <- match.arg(hclust_method, c("ward.D2", "complete", "average", "single"))
  impute_method <- match.arg(impute_method, c("median", "knn", "mean"))
  normalize_method <- match.arg(normalize_method, c("zscore", "minmax"))
  start_time <- Sys.time()

  # Validar inputs
  .validate_unsupervised_inputs(data, methods, exclude_cols)

  resultado <- list()
  resultado$call <- match.call()
  resultado$params <- list(
    methods = methods,
    clustering_algorithms = clustering_algorithms,
    k = k, k_range = k_range, distance = distance,
    hclust_method = hclust_method,
    reduction_methods = reduction_methods,
    n_components = n_components,
    anomaly_methods = anomaly_methods,
    contamination = contamination,
    seed = seed
  )

  if (verbose) {
    .msg_header("unsupervised_ml - Aprendizaje No Supervisado Automatizado")
    cat("Observaciones:", nrow(data), "\n")
    cat("Variables totales:", ncol(data), "\n")
    if (!is.null(exclude_cols)) {
      cat("Variables excluidas:", paste(exclude_cols, collapse = ", "), "\n")
    }
    cat("Analisis solicitados:", paste(methods, collapse = ", "), "\n")
    cat("Semilla:", seed, "\n")
  }

  # =========================================================================
  # SECCION 1: EDA
  # =========================================================================
  if (run_eda) {
    eda_result <- .unsupervised_eda(data, exclude_cols, verbose)
    resultado$eda <- eda_result
  }

  # =========================================================================
  # SECCION 2: PREPROCESAMIENTO
  # =========================================================================
  prep_result <- .unsupervised_preprocess(
    data = data, exclude_cols = exclude_cols,
    impute = impute, impute_method = impute_method,
    normalize = normalize, normalize_method = normalize_method,
    treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
    remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
    verbose = verbose
  )
  resultado$preprocessed <- prep_result
  data_processed <- prep_result$data_clean

  # =========================================================================
  # SECCION 3: REDUCCION DE DIMENSIONALIDAD
  # =========================================================================
  if ("reduction" %in% methods) {
    reduction_result <- .unsupervised_reduction(
      data_processed = data_processed,
      reduction_methods = reduction_methods,
      n_components = n_components,
      pca_threshold = pca_threshold,
      tsne_perplexity = tsne_perplexity,
      umap_n_neighbors = umap_n_neighbors,
      umap_min_dist = umap_min_dist,
      seed = seed,
      verbose = verbose
    )
    resultado$reduction <- reduction_result
  }

  # =========================================================================
  # SECCION 4: CLUSTERING
  # =========================================================================
  if ("clustering" %in% methods) {
    clustering_result <- .unsupervised_clustering(
      data_processed = data_processed,
      algorithms = clustering_algorithms,
      k = k, k_range = k_range,
      distance = distance,
      hclust_method = hclust_method,
      dbscan_eps = dbscan_eps, dbscan_minPts = dbscan_minPts,
      gmm_type = gmm_type,
      seed = seed,
      verbose = verbose
    )
    resultado$clustering <- clustering_result
  }

  # =========================================================================
  # SECCION 5: EVALUACION DE CLUSTERS
  # =========================================================================
  if ("clustering" %in% methods) {
    eval_result <- .unsupervised_evaluate(
      data_processed = data_processed,
      clustering_result = resultado$clustering,
      verbose = verbose
    )
    resultado$evaluation <- eval_result
  }

  # =========================================================================
  # SECCION 6: DETECCION DE ANOMALIAS
  # =========================================================================
  if ("anomaly" %in% methods) {
    anomaly_result <- .unsupervised_anomaly(
      data_processed = data_processed,
      anomaly_methods = anomaly_methods,
      contamination = contamination,
      lof_k = lof_k,
      seed = seed,
      verbose = verbose
    )
    resultado$anomalies <- anomaly_result
  }

  # =========================================================================
  # SECCION 7: VISUALIZACION
  # =========================================================================
  figures <- .unsupervised_plots(
    data_processed = data_processed,
    resultado = resultado,
    methods = methods,
    verbose = verbose
  )
  resultado$figures <- figures

  # =========================================================================
  # TIEMPO TOTAL
  # =========================================================================
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  resultado$elapsed_time <- elapsed

  if (verbose) {
    cat("\n")
    cat(.line("="), "\n")
    cat("ANALISIS COMPLETADO en ", elapsed, " segundos\n", sep = "")
    cat(.line("="), "\n")
  }

  # Datos originales para referencia
  resultado$data_original <- data

  class(resultado) <- "unsupervisedml"
  return(resultado)
}


# =============================================================================
# METODOS S3
# =============================================================================

#' @export
print.unsupervisedml <- function(x, ...) {
  cat("\n")
  cat(.line("="), "\n")
  cat(" unsupervised_ml - Resultados\n")
  cat(.line("="), "\n\n")

  cat("Observaciones:", nrow(x$data_original), "\n")
  cat("Variables procesadas:", ncol(x$preprocessed$data_clean), "\n")
  cat("Tiempo:", x$elapsed_time, "segundos\n\n")

  # Clustering
  if (!is.null(x$clustering)) {
    cat("--- Clustering ---\n")
    cat("k optimo:", x$clustering$optimal_k, "\n")
    cat("Algoritmos:", paste(names(x$clustering$results), collapse = ", "), "\n")

    if (!is.null(x$evaluation)) {
      best <- x$evaluation$best_algorithm
      cat("Mejor algoritmo:", best$algorithm,
          "(Silhouette =", round(best$silhouette, 3), ")\n")
    }
    cat("\n")
  }

  # Reduccion
  if (!is.null(x$reduction)) {
    cat("--- Reduccion de Dimensionalidad ---\n")
    if (!is.null(x$reduction$pca)) {
      n_comp <- x$reduction$pca$n_components
      var_exp <- round(x$reduction$pca$cumulative_variance[n_comp] * 100, 1)
      cat("PCA:", n_comp, "componentes (", var_exp, "% varianza)\n")
    }
    if (!is.null(x$reduction$tsne)) cat("t-SNE: completado\n")
    if (!is.null(x$reduction$umap)) cat("UMAP: completado\n")
    cat("\n")
  }

  # Anomalias
  if (!is.null(x$anomalies)) {
    cat("--- Deteccion de Anomalias ---\n")
    for (method_name in names(x$anomalies$results)) {
      n_anom <- sum(x$anomalies$results[[method_name]]$is_anomaly)
      cat(method_name, ":", n_anom, "anomalias detectadas\n")
    }
    if (!is.null(x$anomalies$consensus)) {
      n_cons <- sum(x$anomalies$consensus$is_anomaly)
      cat("Consenso:", n_cons, "anomalias (detectadas por mayoria)\n")
    }
    cat("\n")
  }

  cat("Figuras generadas:", length(x$figures), "\n")
  invisible(x)
}


#' @export
summary.unsupervisedml <- function(object, ...) {
  x <- object

  cat("\n")
  cat(.line("="), "\n")
  cat(" unsupervised_ml - Resumen Detallado\n")
  cat(.line("="), "\n\n")

  # Datos
  cat("DATOS\n")
  cat(.line("-", 40), "\n")
  cat("  Observaciones:", nrow(x$data_original), "\n")
  cat("  Variables originales:", ncol(x$data_original), "\n")
  cat("  Variables procesadas:", ncol(x$preprocessed$data_clean), "\n")
  if (!is.null(x$preprocessed$removed_cols) && length(x$preprocessed$removed_cols) > 0) {
    cat("  Variables removidas:", paste(x$preprocessed$removed_cols, collapse = ", "), "\n")
  }
  cat("\n")

  # Evaluacion de clusters
  if (!is.null(x$evaluation)) {
    cat("COMPARACION DE ALGORITMOS DE CLUSTERING\n")
    cat(.line("-", 40), "\n")
    comp <- x$evaluation$comparison
    if (!is.null(comp) && nrow(comp) > 0) {
      print(comp, row.names = FALSE)
    }
    cat("\n")
    best <- x$evaluation$best_algorithm
    cat("  >> Mejor algoritmo:", best$algorithm, "\n")
    cat("     Silhouette:", round(best$silhouette, 4), "\n")
    cat("     Calinski-Harabasz:", round(best$calinski_harabasz, 2), "\n")
    cat("     Davies-Bouldin:", round(best$davies_bouldin, 4), "\n")
    cat("\n")
  }

  # PCA loadings
  if (!is.null(x$reduction$pca)) {
    cat("PCA - VARIANZA EXPLICADA\n")
    cat(.line("-", 40), "\n")
    pca_res <- x$reduction$pca
    n_show <- min(10, length(pca_res$variance_explained))
    for (i in seq_len(n_show)) {
      cat(sprintf("  PC%d: %.1f%% (acum: %.1f%%)\n",
                  i,
                  pca_res$variance_explained[i] * 100,
                  pca_res$cumulative_variance[i] * 100))
    }
    cat("\n")
  }

  invisible(x)
}


#' @export
plot.unsupervisedml <- function(x, which = NULL, ...) {
  if (is.null(x$figures) || length(x$figures) == 0) {
    message("No hay figuras disponibles.")
    return(invisible(x))
  }

  if (is.null(which)) {
    # Mostrar catalogo
    cat("\nFiguras disponibles:\n")
    for (i in seq_along(x$figures)) {
      cat(sprintf("  [%d] %s\n", i, names(x$figures)[i]))
    }
    cat("\nUsa plot(resultado, which = 1) para ver una figura especifica.\n")
    cat("Usa plot(resultado, which = 'all') para ver todas.\n")
  } else if (identical(which, "all")) {
    for (fig in x$figures) {
      if (inherits(fig, "gg") || inherits(fig, "ggplot")) {
        print(fig)
      }
    }
  } else if (is.numeric(which)) {
    for (i in which) {
      if (i >= 1 && i <= length(x$figures)) {
        fig <- x$figures[[i]]
        if (inherits(fig, "gg") || inherits(fig, "ggplot")) {
          print(fig)
        }
      } else {
        message("Indice ", i, " fuera de rango (1-", length(x$figures), ")")
      }
    }
  }

  invisible(x)
}

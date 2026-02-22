#' Graficar Curva de Aprendizaje
#'
#' Genera un grafico de la curva de aprendizaje mostrando como el rendimiento
#' del modelo cambia con diferentes tamanos de muestra.
#'
#' @param x Objeto de clase ml_sample_size
#' @param show_target Mostrar linea horizontal del target (default TRUE)
#' @param show_fit Mostrar curva ajustada de ley de potencia (default TRUE)
#' @param show_ci Mostrar intervalo de confianza (default TRUE)
#' @param ci_level Nivel de confianza (default 0.95)
#' @param title Titulo del grafico (default auto)
#' @param ... Argumentos adicionales (no usados)
#'
#' @return Un objeto ggplot si ggplot2 esta disponible, sino un grafico base R
#'
#' @examples
#' \dontrun{
#' result <- ml_sample_size(
#'   task = "classification",
#'   metric = "auc",
#'   target = 0.80
#' )
#' plot_learning_curve(result)
#' }
#'
#' @export
plot_learning_curve <- function(x,
                                 show_target = TRUE,
                                 show_fit = TRUE,
                                 show_ci = TRUE,
                                 ci_level = 0.95,
                                 title = NULL,
                                 ...) {

  if (!inherits(x, "ml_sample_size")) {
    stop("x debe ser de clase 'ml_sample_size'")
  }

  df <- x$summary
  df <- df[order(df$n), ]

  # Calcular IC
  z <- qnorm(1 - (1 - ci_level) / 2)
  df$ci_low <- df$mean - z * df$se
  df$ci_high <- df$mean + z * df$se

  # Valores predichos por la curva (si existe)
  if (!is.null(x$curve_fit) && show_fit) {
    co <- coef(x$curve_fit)
    if (x$settings$target_dir == "higher_better") {
      df$fitted <- co["a"] - co["b"] * df$n^(-co["c"])
    } else {
      df$fitted <- co["a"] + co["b"] * df$n^(-co["c"])
    }
  }

  # Titulo automatico
  if (is.null(title)) {
    title <- paste0("Curva de Aprendizaje (", toupper(x$settings$model), " - ",
                    toupper(x$settings$metric), ")")
    if (!is.na(x$recommend_n)) {
      title <- paste0(title, "\nN recomendado = ", x$recommend_n)
    }
  }

  # Intentar usar ggplot2 si esta disponible
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    .plot_ggplot(df, x, show_target, show_fit, show_ci, title)
  } else {
    .plot_base(df, x, show_target, show_fit, show_ci, title)
  }
}


#' Plot con ggplot2
#' @noRd
.plot_ggplot <- function(df, x, show_target, show_fit, show_ci, title) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = n, y = mean)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(linewidth = 0.8)

  # Intervalo de confianza
  if (show_ci) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      alpha = 0.2, fill = "steelblue"
    )
  }

  # Curva ajustada
  if (show_fit && "fitted" %in% names(df)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = fitted),
      linetype = "dashed", color = "red", linewidth = 0.8
    )
  }

  # Linea de target
  if (show_target && !is.null(x$settings$target)) {
    p <- p + ggplot2::geom_hline(
      yintercept = x$settings$target,
      linetype = "dotted", color = "darkgreen", linewidth = 0.8
    ) +
      ggplot2::annotate(
        "text",
        x = max(df$n) * 0.9,
        y = x$settings$target,
        label = paste("Target =", x$settings$target),
        vjust = -0.5, color = "darkgreen"
      )
  }

  # Marcar N recomendado
  if (!is.na(x$recommend_n)) {
    p <- p + ggplot2::geom_vline(
      xintercept = x$recommend_n,
      linetype = "dashed", color = "orange", linewidth = 0.8
    )
  }

  p <- p +
    ggplot2::labs(
      title = title,
      x = "Tamano de muestra (n)",
      y = paste0(toupper(x$settings$metric), " (media +/- ", round((1 - 0.05/2) * 100), "% CI)")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      axis.title = ggplot2::element_text(size = 12)
    )

  print(p)
  invisible(p)
}


#' Plot con graficos base R
#' @noRd
.plot_base <- function(df, x, show_target, show_fit, show_ci, title) {
  # Rango del eje Y
  y_range <- range(c(df$ci_low, df$ci_high), na.rm = TRUE)
  if (!is.null(x$settings$target)) {
    y_range <- range(c(y_range, x$settings$target))
  }

  plot(df$n, df$mean, type = "b", pch = 19,
       xlab = "Tamano de muestra (n)",
       ylab = toupper(x$settings$metric),
       main = title,
       ylim = y_range)

  # IC como poligono
  if (show_ci) {
    polygon(
      c(df$n, rev(df$n)),
      c(df$ci_low, rev(df$ci_high)),
      col = rgb(0.27, 0.51, 0.71, 0.2), border = NA
    )
    lines(df$n, df$mean, type = "b", pch = 19)
  }

  # Curva ajustada
  if (show_fit && "fitted" %in% names(df)) {
    lines(df$n, df$fitted, lty = 2, col = "red", lwd = 2)
  }

  # Target
  if (show_target && !is.null(x$settings$target)) {
    abline(h = x$settings$target, lty = 3, col = "darkgreen", lwd = 2)
    text(max(df$n) * 0.8, x$settings$target,
         paste("Target =", x$settings$target), pos = 3, col = "darkgreen")
  }

  # N recomendado
  if (!is.na(x$recommend_n)) {
    abline(v = x$recommend_n, lty = 2, col = "orange", lwd = 2)
  }

  # Leyenda
  legend_items <- c("Observado")
  legend_lty <- c(1)
  legend_col <- c("black")

  if (show_fit && "fitted" %in% names(df)) {
    legend_items <- c(legend_items, "Ajuste power-law")
    legend_lty <- c(legend_lty, 2)
    legend_col <- c(legend_col, "red")
  }

  if (show_target && !is.null(x$settings$target)) {
    legend_items <- c(legend_items, "Target")
    legend_lty <- c(legend_lty, 3)
    legend_col <- c(legend_col, "darkgreen")
  }

  legend("bottomright", legend = legend_items, lty = legend_lty, col = legend_col, lwd = 2)
}


#' Graficar distribucion de metricas por tamano de muestra
#'
#' Muestra boxplots de la distribucion de la metrica para cada tamano de muestra
#'
#' @param x Objeto de clase ml_sample_size
#' @param ... Argumentos adicionales
#'
#' @export
plot_distribution <- function(x, ...) {
  if (!inherits(x, "ml_sample_size")) {
    stop("x debe ser de clase 'ml_sample_size'")
  }

  raw <- x$raw

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(raw, ggplot2::aes(x = factor(n), y = metric_value)) +
      ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
      ggplot2::labs(
        title = paste0("Distribucion de ", toupper(x$settings$metric), " por tamano de muestra"),
        x = "Tamano de muestra (n)",
        y = toupper(x$settings$metric)
      ) +
      ggplot2::theme_minimal()

    if (!is.null(x$settings$target)) {
      p <- p + ggplot2::geom_hline(yintercept = x$settings$target,
                                    linetype = "dashed", color = "red")
    }

    print(p)
    invisible(p)
  } else {
    boxplot(metric_value ~ n, data = raw,
            xlab = "Tamano de muestra (n)",
            ylab = toupper(x$settings$metric),
            main = paste0("Distribucion de ", toupper(x$settings$metric)),
            col = "steelblue")

    if (!is.null(x$settings$target)) {
      abline(h = x$settings$target, lty = 2, col = "red", lwd = 2)
    }
  }
}


#' Plot All Graphs for Sample Size Results
#'
#' Displays learning curve and distribution plots side by side.
#'
#' @param x Object of class ml_sample_size
#' @param ... Additional arguments
#'
#' @export
plot_all.ml_sample_size <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plot_all")
  }

  # Capture plots without printing them individually
  p1 <- suppressMessages(plot_learning_curve(x))
  p2 <- suppressMessages(plot_distribution(x))

  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- p1 + p2 + patchwork::plot_layout(ncol = 2)
    print(combined)
  } else {
    print(p1)
    print(p2)
  }
  invisible(list(learning_curve = p1, distribution = p2))
}

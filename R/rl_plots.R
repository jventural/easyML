# =============================================================================
# Figuras para reinforcement_ml (12 tipos)
# =============================================================================

#' @noRd
.rl_plots <- function(resultado, verbose = TRUE) {

  if (verbose) {
    .print_section(6, "Generacion de Figuras RL")
  }

  plots <- list()
  figures_catalog <- list()
  fig_num <- 0
  mode <- resultado$params$mode
  results <- resultado$results

  theme_rl <- ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      legend.position = "bottom"
    )

  palette <- c("#1565C0", "#FF5722", "#4CAF50", "#FF9800", "#9C27B0",
               "#00BCD4", "#795548", "#E91E63")

  # --- Fig 1: Reward Curves ---
  fig_num <- fig_num + 1
  tryCatch({
    df_list <- lapply(names(results), function(nm) {
      rh <- results[[nm]]$reward_history
      ma <- .moving_average(rh, window = min(50, length(rh) %/% 5 + 1))
      data.frame(t = seq_along(ma), reward = ma, algorithm = nm)
    })
    df_reward <- do.call(rbind, df_list)

    x_label <- if (mode == "bandits") "Round" else "Episode"

    plots$reward_curves <- ggplot2::ggplot(df_reward,
                                            ggplot2::aes(x = t, y = reward, color = algorithm)) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.8) +
      ggplot2::scale_color_manual(values = palette[seq_along(results)]) +
      ggplot2::labs(title = "Reward Curves (Moving Average)",
                    x = x_label, y = "Mean Reward", color = "Algorithm") +
      theme_rl

    figures_catalog$reward_curves <- list(
      number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
      title = "Reward Curves", description = "Cumulative moving average reward per algorithm",
      filename = paste0("Figura_", fig_num, "_Reward_Curves.png"))
    if (verbose) cat("  Figura", fig_num, ": Reward Curves\n")
  }, error = function(e) if (verbose) cat("  Error Fig", fig_num, ":", e$message, "\n"))

  # --- Fig 2: Regret Curves (bandits) ---
  if (mode == "bandits") {
    fig_num <- fig_num + 1
    tryCatch({
      df_reg <- do.call(rbind, lapply(names(results), function(nm) {
        rg <- results[[nm]]$regret_history
        if (is.null(rg)) return(NULL)
        data.frame(t = seq_along(rg), regret = rg, algorithm = nm)
      }))
      if (!is.null(df_reg) && nrow(df_reg) > 0) {
        plots$regret_curves <- ggplot2::ggplot(df_reg,
                                                ggplot2::aes(x = t, y = regret, color = algorithm)) +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::scale_color_manual(values = palette[seq_along(results)]) +
          ggplot2::labs(title = "Cumulative Regret", x = "Round", y = "Cumulative Regret",
                        color = "Algorithm") +
          theme_rl
        figures_catalog$regret_curves <- list(
          number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
          title = "Cumulative Regret", description = "Cumulative regret over rounds",
          filename = paste0("Figura_", fig_num, "_Regret_Curves.png"))
        if (verbose) cat("  Figura", fig_num, ": Regret Curves\n")
      }
    }, error = function(e) if (verbose) cat("  Error Fig", fig_num, ":", e$message, "\n"))

    # --- Fig 3: Action Distribution ---
    fig_num <- fig_num + 1
    tryCatch({
      algo1 <- names(results)[1]
      params1 <- results[[algo1]]$params
      if (!is.null(params1$arm_counts)) {
        df_arms <- data.frame(arm = seq_along(params1$arm_counts),
                              count = params1$arm_counts)
        plots$action_distribution <- ggplot2::ggplot(df_arms,
                                                      ggplot2::aes(x = factor(arm), y = count, fill = factor(arm))) +
          ggplot2::geom_col(alpha = 0.8) +
          ggplot2::scale_fill_manual(values = palette[seq_along(params1$arm_counts)]) +
          ggplot2::labs(title = paste("Action Distribution -", algo1),
                        x = "Arm", y = "Times Selected") +
          theme_rl + ggplot2::theme(legend.position = "none")
        figures_catalog$action_distribution <- list(
          number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
          title = "Action Distribution", description = "How often each arm was selected",
          filename = paste0("Figura_", fig_num, "_Action_Distribution.png"))
        if (verbose) cat("  Figura", fig_num, ": Action Distribution\n")
      }
    }, error = function(e) if (verbose) cat("  Error Fig", fig_num, ":", e$message, "\n"))

    # --- Fig 12: Thompson Posteriors ---
    if ("thompson" %in% names(results)) {
      fig_num <- fig_num + 1
      tryCatch({
        tp <- results$thompson$params
        x_seq <- seq(0, 1, length.out = 200)
        df_post <- do.call(rbind, lapply(seq_along(tp$alpha), function(a) {
          data.frame(x = x_seq,
                     density = stats::dbeta(x_seq, tp$alpha[a], tp$beta[a]),
                     arm = paste("Arm", a))
        }))
        plots$thompson_posteriors <- ggplot2::ggplot(df_post,
                                                      ggplot2::aes(x = x, y = density, color = arm, fill = arm)) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_area(alpha = 0.15) +
          ggplot2::scale_color_manual(values = palette[seq_along(tp$alpha)]) +
          ggplot2::scale_fill_manual(values = palette[seq_along(tp$alpha)]) +
          ggplot2::labs(title = "Thompson Sampling Posteriors",
                        subtitle = "Beta posterior distributions per arm",
                        x = "Reward Probability", y = "Density") +
          theme_rl
        figures_catalog$thompson_posteriors <- list(
          number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
          title = "Thompson Sampling Posteriors",
          description = "Beta posterior distributions for each arm",
          filename = paste0("Figura_", fig_num, "_Thompson_Posteriors.png"))
        if (verbose) cat("  Figura", fig_num, ": Thompson Posteriors\n")
      }, error = function(e) if (verbose) cat("  Error Fig", fig_num, ":", e$message, "\n"))
    }
  }

  # --- Fig 5: Q-Value Heatmap (MDP) ---
  if (mode == "mdp") {
    best_algo <- resultado$best_algorithm
    if (!is.null(best_algo) && !is.null(results[[best_algo]]$q_table)) {
      fig_num <- fig_num + 1
      tryCatch({
        Q <- results[[best_algo]]$q_table
        if (is.matrix(Q) && nrow(Q) <= 100) {
          action_names <- if (!is.null(resultado$environment$action_names)) {
            resultado$environment$action_names
          } else paste("A", seq_len(ncol(Q)))
          df_q <- expand.grid(State = seq_len(nrow(Q)), Action = action_names)
          df_q$Value <- as.vector(Q)

          plots$q_value_heatmap <- ggplot2::ggplot(df_q,
                                                    ggplot2::aes(x = Action, y = factor(State), fill = Value)) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_gradient2(low = "#D32F2F", mid = "#FFF9C4", high = "#388E3C") +
            ggplot2::labs(title = paste("Q-Value Heatmap -", best_algo),
                          x = "Action", y = "State") +
            theme_rl
          figures_catalog$q_value_heatmap <- list(
            number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
            title = "Q-Value Heatmap", description = "State-action values of best algorithm",
            filename = paste0("Figura_", fig_num, "_Q_Value_Heatmap.png"))
          if (verbose) cat("  Figura", fig_num, ": Q-Value Heatmap\n")
        }
      }, error = function(e) if (verbose) cat("  Error Fig", fig_num, ":", e$message, "\n"))
    }

    # --- Fig 7: Episode Returns ---
    fig_num <- fig_num + 1
    tryCatch({
      df_ep <- do.call(rbind, lapply(names(results), function(nm) {
        rh <- results[[nm]]$reward_history
        if (length(rh) > 1) {
          data.frame(episode = seq_along(rh), return_val = rh, algorithm = nm)
        }
      }))
      if (!is.null(df_ep) && nrow(df_ep) > 0) {
        plots$episode_returns <- ggplot2::ggplot(df_ep,
                                                  ggplot2::aes(x = episode, y = return_val, color = algorithm)) +
          ggplot2::geom_line(alpha = 0.3) +
          ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
          ggplot2::scale_color_manual(values = palette[seq_along(results)]) +
          ggplot2::labs(title = "Episode Returns", x = "Episode", y = "Return",
                        color = "Algorithm") +
          theme_rl
        figures_catalog$episode_returns <- list(
          number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
          title = "Episode Returns", description = "Return per episode with smoothing",
          filename = paste0("Figura_", fig_num, "_Episode_Returns.png"))
        if (verbose) cat("  Figura", fig_num, ": Episode Returns\n")
      }
    }, error = function(e) if (verbose) cat("  Error Fig", fig_num, ":", e$message, "\n"))
  }

  # --- Fig 9: Algorithm Comparison ---
  if (!is.null(resultado$summary_table) && nrow(resultado$summary_table) > 0) {
    fig_num <- fig_num + 1
    tryCatch({
      st <- resultado$summary_table
      metric_col <- if (mode == "bandits") "total_regret" else "mean_reward"
      metric_label <- if (mode == "bandits") "Total Regret (lower=better)" else "Mean Reward (higher=better)"

      st$algorithm <- factor(st$algorithm, levels = st$algorithm[order(st[[metric_col]])])

      plots$algorithm_comparison <- ggplot2::ggplot(st,
                                                     ggplot2::aes(x = algorithm, y = .data[[metric_col]],
                                                                  fill = algorithm)) +
        ggplot2::geom_col(alpha = 0.8, width = 0.6) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = palette[seq_len(nrow(st))]) +
        ggplot2::labs(title = "Algorithm Comparison", x = "", y = metric_label) +
        theme_rl + ggplot2::theme(legend.position = "none")
      figures_catalog$algorithm_comparison <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "Algorithm Comparison",
        description = paste("Comparison of algorithms by", metric_label),
        filename = paste0("Figura_", fig_num, "_Algorithm_Comparison.png"))
      if (verbose) cat("  Figura", fig_num, ": Algorithm Comparison\n")
    }, error = function(e) if (verbose) cat("  Error Fig", fig_num, ":", e$message, "\n"))
  }

  if (verbose) cat("\n  Total figuras generadas:", length(plots), "\n")

  list(plots = plots, figures_catalog = figures_catalog)
}

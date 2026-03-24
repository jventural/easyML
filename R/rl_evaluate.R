# =============================================================================
# Evaluacion de politicas para reinforcement_ml
# =============================================================================

#' @noRd
.rl_evaluate <- function(results, mode, env, verbose) {

  if (verbose) {
    .print_section(4, "Evaluacion de Politicas")
  }

  summary_rows <- list()

  for (algo_name in names(results)) {
    res <- results[[algo_name]]
    if (is.null(res)) next

    if (verbose) {
      cat("\n  ", algo_name, ":\n", sep = "")
    }

    if (mode == "bandits") {
      mean_r <- res$mean_reward
      cum_r <- res$cumulative_reward
      total_reg <- res$total_regret

      if (verbose) {
        cat("    Reward medio: ", round(mean_r, 4), "\n")
        cat("    Reward acumulado: ", round(cum_r, 2), "\n")
        cat("    Regret total: ", round(total_reg, 2), "\n")
        if (is.numeric(res$policy) && length(res$policy) == 1) {
          cat("    Brazo elegido: ", res$policy, "\n")
          if (!is.null(env$optimal_arm)) {
            cat("    Brazo optimo: ", env$optimal_arm,
                " (correcto: ", res$policy == env$optimal_arm, ")\n", sep = "")
          }
        }
      }

      summary_rows[[algo_name]] <- .create_rl_summary_row(
        algo_name, mean_r, cum_r, total_reg, res$training_time
      )

    } else {
      # MDP
      n_ep <- length(res$reward_history)
      mean_r <- mean(res$reward_history)
      last100 <- mean(utils::tail(res$reward_history, min(100, n_ep)))

      # Detectar convergencia
      if (n_ep >= 100) {
        first_half <- mean(res$reward_history[1:(n_ep %/% 2)])
        second_half <- mean(res$reward_history[(n_ep %/% 2 + 1):n_ep])
        converged <- abs(second_half - first_half) < abs(first_half) * 0.1 + 0.01
      } else {
        converged <- NA
      }

      if (verbose) {
        cat("    Return medio: ", round(mean_r, 4), "\n")
        cat("    Return medio (ultimos 100): ", round(last100, 4), "\n")
        if (!is.na(converged)) {
          cat("    Convergencia: ", if (converged) "Si" else "No (considerar mas episodios)", "\n")
        }
      }

      summary_rows[[algo_name]] <- data.frame(
        algorithm = algo_name,
        mean_reward = round(mean_r, 4),
        cumulative_reward = round(sum(res$reward_history), 2),
        total_regret = round(if (!is.null(res$total_regret)) res$total_regret else NA, 2),
        training_time = round(res$training_time, 3),
        stringsAsFactors = FALSE
      )
    }
  }

  # Tabla comparativa
  if (length(summary_rows) > 0) {
    summary_table <- do.call(rbind, summary_rows)
    rownames(summary_table) <- NULL
  } else {
    summary_table <- data.frame()
  }

  # Mejor algoritmo
  if (nrow(summary_table) > 0) {
    if (mode == "bandits") {
      best_idx <- which.min(summary_table$total_regret)
    } else {
      best_idx <- which.max(summary_table$mean_reward)
    }
    best_algorithm <- summary_table$algorithm[best_idx]
  } else {
    best_algorithm <- NA
  }

  if (verbose) {
    .print_section(5, "Comparacion de Algoritmos")
    if (nrow(summary_table) > 0) {
      cat("\n")
      print(summary_table, row.names = FALSE)
      cat("\n  Mejor algoritmo: ", best_algorithm, "\n")
    }
  }

  list(
    summary_table = summary_table,
    best_algorithm = best_algorithm
  )
}

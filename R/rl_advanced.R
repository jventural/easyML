# =============================================================================
# Wrappers para paquetes avanzados de RL
# =============================================================================

#' @noRd
.rl_train_advanced <- function(env, mode, algorithms, n_simulations,
                               n_episodes, seed, verbose) {
  results <- list()

  # --- contextual package (bandits avanzados) ---
  if (mode == "bandits" && .check_rl_package("contextual", "Bandits avanzados")) {
    if (verbose) cat("\n  Usando paquete 'contextual' para simulacion avanzada...\n")

    tryCatch({
      bandit <- contextual::BasicBernoulliBandit$new(weights = env$probs)
      agents <- list()

      if ("epsilon_greedy" %in% algorithms) {
        policy_eg <- contextual::EpsilonGreedyPolicy$new(epsilon = 0.1)
        agents[["EpsilonGreedy_ctx"]] <- contextual::Agent$new(policy_eg, bandit)
      }
      if ("ucb1" %in% algorithms) {
        policy_ucb <- contextual::UCB1Policy$new()
        agents[["UCB1_ctx"]] <- contextual::Agent$new(policy_ucb, bandit)
      }
      if ("thompson" %in% algorithms) {
        policy_ts <- contextual::ThompsonSamplingPolicy$new()
        agents[["Thompson_ctx"]] <- contextual::Agent$new(policy_ts, bandit)
      }

      if (length(agents) > 0) {
        simulator <- contextual::Simulator$new(
          agents = agents,
          horizon = n_simulations,
          simulations = 10,
          save_context = FALSE
        )
        history <- simulator$run()

        if (verbose) {
          cat("  Simulacion contextual completada.\n")
          cat("  Horizonte:", n_simulations, "x 10 simulaciones\n")
        }

        results$contextual_history <- history
      }
    }, error = function(e) {
      if (verbose) cat("  Error con paquete contextual:", e$message, "\n")
    })
  }

  # --- ReinforcementLearning package ---
  if (mode == "mdp" && .check_rl_package("ReinforcementLearning", "RL tabulares avanzados")) {
    if (verbose) cat("\n  Paquete 'ReinforcementLearning' disponible para analisis adicional.\n")
  }

  results
}

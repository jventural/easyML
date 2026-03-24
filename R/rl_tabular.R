# =============================================================================
# Q-Learning y SARSA para reinforcement_ml
# =============================================================================

#' @noRd
.rl_train_tabular <- function(env, algorithms, n_episodes, n_steps,
                              learning_rate, discount,
                              epsilon, epsilon_decay, min_epsilon,
                              seed, verbose) {

  set.seed(seed)
  results <- list()

  for (algo in algorithms) {
    if (!algo %in% c("qlearning", "sarsa")) next

    if (verbose) {
      algo_label <- if (algo == "qlearning") "Q-Learning" else "SARSA"
      .print_subsection(3, match(algo, algorithms), paste0("Algoritmo: ", algo_label))
    }

    t_start <- proc.time()["elapsed"]

    res <- if (algo == "qlearning") {
      .run_qlearning(env, n_episodes, n_steps, learning_rate, discount,
                     epsilon, epsilon_decay, min_epsilon)
    } else {
      .run_sarsa(env, n_episodes, n_steps, learning_rate, discount,
                 epsilon, epsilon_decay, min_epsilon)
    }

    t_end <- proc.time()["elapsed"]
    res$training_time <- as.numeric(t_end - t_start)

    if (verbose) {
      cat("  Episodios:", n_episodes, "\n")
      cat("  Return medio (ultimos 100):", round(mean(utils::tail(res$reward_history, 100)), 3), "\n")
      cat("  Tiempo:", round(res$training_time, 3), "s\n")

      # Mostrar politica para grids pequenos
      if (env$n_states <= 25 && !is.null(env$action_names)) {
        cat("\n  Politica aprendida:\n")
        for (s in seq_len(env$n_states)) {
          best_a <- which.max(res$q_table[s, ])
          cat("    Estado ", sprintf("%2d", s), ": ",
              env$action_names[best_a], "\n", sep = "")
        }
      }

      .print_rl_reference(algo)
    }

    results[[algo]] <- res
  }

  results
}

# --- Q-Learning ---
#' @noRd
.run_qlearning <- function(env, n_episodes, n_steps, lr, gamma,
                           epsilon, eps_decay, eps_min) {
  ns <- env$n_states
  na <- env$n_actions
  Q <- matrix(0, nrow = ns, ncol = na)
  episode_returns <- numeric(n_episodes)
  eps <- epsilon

  for (ep in seq_len(n_episodes)) {
    state <- env$reset()
    total_reward <- 0

    for (step in seq_len(n_steps)) {
      action <- .epsilon_greedy_action(Q[state, ], eps, na)
      result <- env$step(state, action)
      next_state <- result$next_state
      reward <- result$reward
      done <- result$done

      # Q-Learning update (off-policy: usa max Q)
      Q[state, action] <- Q[state, action] +
        lr * (reward + gamma * max(Q[next_state, ]) - Q[state, action])

      total_reward <- total_reward + reward
      state <- next_state

      if (done) break
    }

    episode_returns[ep] <- total_reward
    eps <- max(eps_min, eps * eps_decay)
  }

  # Derivar politica
  policy <- apply(Q, 1, which.max)

  .create_rl_result(
    algorithm = "qlearning",
    policy = policy,
    reward_history = episode_returns,
    q_table = Q,
    params = list(lr = lr, gamma = gamma, final_epsilon = eps,
                  n_episodes = n_episodes)
  )
}

# --- SARSA ---
#' @noRd
.run_sarsa <- function(env, n_episodes, n_steps, lr, gamma,
                       epsilon, eps_decay, eps_min) {
  ns <- env$n_states
  na <- env$n_actions
  Q <- matrix(0, nrow = ns, ncol = na)
  episode_returns <- numeric(n_episodes)
  eps <- epsilon

  for (ep in seq_len(n_episodes)) {
    state <- env$reset()
    action <- .epsilon_greedy_action(Q[state, ], eps, na)
    total_reward <- 0

    for (step in seq_len(n_steps)) {
      result <- env$step(state, action)
      next_state <- result$next_state
      reward <- result$reward
      done <- result$done

      next_action <- .epsilon_greedy_action(Q[next_state, ], eps, na)

      # SARSA update (on-policy: usa la accion realmente tomada)
      Q[state, action] <- Q[state, action] +
        lr * (reward + gamma * Q[next_state, next_action] - Q[state, action])

      total_reward <- total_reward + reward
      state <- next_state
      action <- next_action

      if (done) break
    }

    episode_returns[ep] <- total_reward
    eps <- max(eps_min, eps * eps_decay)
  }

  policy <- apply(Q, 1, which.max)

  .create_rl_result(
    algorithm = "sarsa",
    policy = policy,
    reward_history = episode_returns,
    q_table = Q,
    params = list(lr = lr, gamma = gamma, final_epsilon = eps,
                  n_episodes = n_episodes)
  )
}

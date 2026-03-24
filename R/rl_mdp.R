# =============================================================================
# Value Iteration y Policy Iteration para MDPs conocidos
# =============================================================================

#' @noRd
.rl_train_mdp_exact <- function(transition_prob, reward_matrix, discount,
                                algorithms, verbose) {
  results <- list()
  ns <- nrow(reward_matrix)
  na <- ncol(reward_matrix)

  for (algo in algorithms) {
    if (!algo %in% c("value_iteration", "policy_iteration")) next

    if (verbose) {
      algo_label <- if (algo == "value_iteration") "Value Iteration" else "Policy Iteration"
      cat("\n  --- ", algo_label, " ---\n", sep = "")
    }

    t_start <- proc.time()["elapsed"]

    # Intentar MDPtoolbox primero
    if (.check_rl_package("MDPtoolbox", algo)) {
      res <- tryCatch(
        .run_mdp_toolbox(transition_prob, reward_matrix, discount, algo, verbose),
        error = function(e) {
          if (verbose) cat("  MDPtoolbox error:", e$message, "\n  Usando implementacion nativa...\n")
          NULL
        }
      )
      if (!is.null(res)) {
        t_end <- proc.time()["elapsed"]
        res$training_time <- as.numeric(t_end - t_start)
        results[[algo]] <- res
        if (verbose) {
          cat("  Iteraciones:", res$parameters$iterations, "\n")
          cat("  Convergencia: Si\n")
          .print_rl_reference(algo)
        }
        next
      }
    }

    # Implementacion nativa
    res <- if (algo == "value_iteration") {
      .run_value_iteration_native(transition_prob, reward_matrix, discount, verbose)
    } else {
      .run_policy_iteration_native(transition_prob, reward_matrix, discount, verbose)
    }

    t_end <- proc.time()["elapsed"]
    res$training_time <- as.numeric(t_end - t_start)
    results[[algo]] <- res

    if (verbose) {
      cat("  Iteraciones:", res$parameters$iterations, "\n")
      .print_rl_reference(algo)
    }
  }

  results
}

# --- Value Iteration nativo ---
#' @noRd
.run_value_iteration_native <- function(P, R, gamma, verbose, tol = 1e-6, max_iter = 1000) {
  ns <- nrow(R)
  na <- ncol(R)
  V <- rep(0, ns)

  for (iter in seq_len(max_iter)) {
    V_new <- numeric(ns)
    for (s in seq_len(ns)) {
      q_vals <- numeric(na)
      for (a in seq_len(na)) {
        # R[s,a] + gamma * sum(P[s,a,s'] * V[s'])
        q_vals[a] <- R[s, a] + gamma * sum(P[s, a, ] * V)
      }
      V_new[s] <- max(q_vals)
    }
    if (max(abs(V_new - V)) < tol) {
      V <- V_new
      break
    }
    V <- V_new
  }

  # Extraer politica
  policy <- integer(ns)
  Q <- matrix(0, ns, na)
  for (s in seq_len(ns)) {
    for (a in seq_len(na)) {
      Q[s, a] <- R[s, a] + gamma * sum(P[s, a, ] * V)
    }
    policy[s] <- which.max(Q[s, ])
  }

  .create_rl_result(
    algorithm = "value_iteration",
    policy = policy,
    reward_history = V,
    q_table = Q,
    params = list(gamma = gamma, iterations = iter, V = V)
  )
}

# --- Policy Iteration nativo ---
#' @noRd
.run_policy_iteration_native <- function(P, R, gamma, verbose, max_iter = 100) {
  ns <- nrow(R)
  na <- ncol(R)
  policy <- sample.int(na, ns, replace = TRUE)
  V <- rep(0, ns)

  for (iter in seq_len(max_iter)) {
    # Policy Evaluation
    for (eval_iter in 1:100) {
      V_new <- numeric(ns)
      for (s in seq_len(ns)) {
        a <- policy[s]
        V_new[s] <- R[s, a] + gamma * sum(P[s, a, ] * V)
      }
      if (max(abs(V_new - V)) < 1e-6) break
      V <- V_new
    }

    # Policy Improvement
    policy_stable <- TRUE
    for (s in seq_len(ns)) {
      q_vals <- numeric(na)
      for (a in seq_len(na)) {
        q_vals[a] <- R[s, a] + gamma * sum(P[s, a, ] * V)
      }
      best_a <- which.max(q_vals)
      if (best_a != policy[s]) {
        policy[s] <- best_a
        policy_stable <- FALSE
      }
    }

    if (policy_stable) break
  }

  Q <- matrix(0, ns, na)
  for (s in seq_len(ns)) {
    for (a in seq_len(na)) {
      Q[s, a] <- R[s, a] + gamma * sum(P[s, a, ] * V)
    }
  }

  .create_rl_result(
    algorithm = "policy_iteration",
    policy = policy,
    reward_history = V,
    q_table = Q,
    params = list(gamma = gamma, iterations = iter, V = V)
  )
}

# --- Wrapper MDPtoolbox ---
#' @noRd
.run_mdp_toolbox <- function(P, R, gamma, algo, verbose) {
  # MDPtoolbox espera P como lista de matrices
  ns <- dim(P)[1]
  na <- dim(P)[2]
  P_list <- lapply(seq_len(na), function(a) P[, a, ])

  if (algo == "value_iteration") {
    sol <- MDPtoolbox::mdp_value_iteration(P_list, R, gamma)
  } else {
    sol <- MDPtoolbox::mdp_policy_iteration(P_list, R, gamma)
  }

  .create_rl_result(
    algorithm = algo,
    policy = sol$policy,
    reward_history = sol$V,
    params = list(gamma = gamma, iterations = sol$iter, V = sol$V)
  )
}

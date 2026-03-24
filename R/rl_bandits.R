# =============================================================================
# Algoritmos de Bandits para reinforcement_ml
# =============================================================================

#' @noRd
.rl_train_bandits <- function(env, algorithms, n_simulations,
                              epsilon, ucb_c, thompson_prior,
                              linucb_alpha, context_dim,
                              seed, verbose) {

  set.seed(seed)
  results <- list()
  is_contextual <- !is.null(context_dim) && context_dim > 0

  for (algo in algorithms) {
    if (verbose) {
      .print_subsection(3, match(algo, algorithms), paste0("Algoritmo: ", algo))
    }

    t_start <- proc.time()["elapsed"]

    res <- switch(algo,
      "epsilon_greedy" = .run_epsilon_greedy(env, n_simulations, epsilon, verbose),
      "ucb1" = .run_ucb1(env, n_simulations, ucb_c, verbose),
      "thompson" = .run_thompson(env, n_simulations, thompson_prior, verbose),
      "linucb" = if (is_contextual) .run_linucb(env, n_simulations, linucb_alpha, verbose) else NULL,
      "contextual_ts" = if (is_contextual) .run_contextual_ts(env, n_simulations, verbose) else NULL,
      NULL
    )

    if (is.null(res)) {
      if (verbose) cat("  Algoritmo ", algo, " no aplicable en este modo.\n")
      next
    }

    t_end <- proc.time()["elapsed"]
    res$training_time <- as.numeric(t_end - t_start)

    if (verbose) {
      cat("  Reward acumulado: ", round(res$cumulative_reward, 2), "\n")
      cat("  Regret total: ", round(res$total_regret, 2), "\n")
      cat("  Tiempo: ", round(res$training_time, 3), "s\n")
      .print_rl_reference(algo)
    }

    results[[algo]] <- res
  }

  results
}

# --- Epsilon-Greedy ---
#' @noRd
.run_epsilon_greedy <- function(env, n_rounds, epsilon, verbose) {
  n_arms <- env$n_arms
  counts <- rep(0, n_arms)
  values <- rep(0, n_arms)
  rewards <- numeric(n_rounds)
  actions <- integer(n_rounds)

  for (t in seq_len(n_rounds)) {
    if (stats::runif(1) < epsilon) {
      action <- sample.int(n_arms, 1)
    } else {
      max_val <- max(values)
      best <- which(values == max_val)
      action <- if (length(best) == 1) best else sample(best, 1)
    }

    reward <- env$pull(action)
    counts[action] <- counts[action] + 1
    values[action] <- values[action] + (reward - values[action]) / counts[action]

    rewards[t] <- reward
    actions[t] <- action
  }

  regret <- .calc_cumulative_regret(rewards, env$optimal_reward)
  policy <- which.max(values)

  .create_rl_result(
    algorithm = "epsilon_greedy",
    policy = policy,
    reward_history = rewards,
    q_table = values,
    params = list(epsilon = epsilon, arm_counts = counts, arm_values = values),
    regret_history = regret
  )
}

# --- UCB1 ---
#' @noRd
.run_ucb1 <- function(env, n_rounds, c_param, verbose) {
  n_arms <- env$n_arms
  counts <- rep(0, n_arms)
  values <- rep(0, n_arms)
  rewards <- numeric(n_rounds)
  actions <- integer(n_rounds)

  for (t in seq_len(n_rounds)) {
    # Explorar brazos no probados
    unvisited <- which(counts == 0)
    if (length(unvisited) > 0) {
      action <- unvisited[1]
    } else {
      ucb_vals <- values + c_param * sqrt(log(t) / counts)
      action <- which.max(ucb_vals)
    }

    reward <- env$pull(action)
    counts[action] <- counts[action] + 1
    values[action] <- values[action] + (reward - values[action]) / counts[action]

    rewards[t] <- reward
    actions[t] <- action
  }

  regret <- .calc_cumulative_regret(rewards, env$optimal_reward)

  .create_rl_result(
    algorithm = "ucb1",
    policy = which.max(values),
    reward_history = rewards,
    q_table = values,
    params = list(c = c_param, arm_counts = counts, arm_values = values),
    regret_history = regret
  )
}

# --- Thompson Sampling ---
#' @noRd
.run_thompson <- function(env, n_rounds, prior, verbose) {
  n_arms <- env$n_arms
  alpha <- rep(prior[1], n_arms)
  beta_param <- rep(prior[2], n_arms)
  rewards <- numeric(n_rounds)
  actions <- integer(n_rounds)

  for (t in seq_len(n_rounds)) {
    # Muestrear de la posterior Beta
    samples <- sapply(seq_len(n_arms), function(a) {
      stats::rbeta(1, alpha[a], beta_param[a])
    })
    action <- which.max(samples)

    reward <- env$pull(action)
    if (reward == 1) {
      alpha[action] <- alpha[action] + 1
    } else {
      beta_param[action] <- beta_param[action] + 1
    }

    rewards[t] <- reward
    actions[t] <- action
  }

  regret <- .calc_cumulative_regret(rewards, env$optimal_reward)
  posterior_means <- alpha / (alpha + beta_param)

  .create_rl_result(
    algorithm = "thompson",
    policy = which.max(posterior_means),
    reward_history = rewards,
    q_table = posterior_means,
    params = list(alpha = alpha, beta = beta_param,
                  posterior_means = posterior_means),
    regret_history = regret
  )
}

# --- LinUCB (Contextual Bandit) ---
#' @noRd
.run_linucb <- function(env, n_rounds, alpha_param, verbose) {
  n_arms <- env$n_arms
  d <- env$context_dim

  # Inicializar matrices por brazo
  A <- lapply(seq_len(n_arms), function(a) diag(d))
  b <- lapply(seq_len(n_arms), function(a) rep(0, d))

  rewards <- numeric(n_rounds)
  actions <- integer(n_rounds)

  for (t in seq_len(n_rounds)) {
    context <- env$generate_context()

    # UCB para cada brazo
    ucb_vals <- sapply(seq_len(n_arms), function(a) {
      A_inv <- solve(A[[a]])
      theta_hat <- A_inv %*% b[[a]]
      p <- as.numeric(t(theta_hat) %*% context +
                        alpha_param * sqrt(as.numeric(t(context) %*% A_inv %*% context)))
      p
    })

    action <- which.max(ucb_vals)
    reward <- env$pull(action, context)

    # Actualizar
    A[[action]] <- A[[action]] + context %*% t(context)
    b[[action]] <- b[[action]] + reward * context

    rewards[t] <- reward
    actions[t] <- action
  }

  # Regret contextual
  optimal_rewards <- sapply(seq_len(n_rounds), function(t) {
    ctx <- env$generate_context()
    env$optimal_reward(ctx)
  })
  regret <- cumsum(mean(optimal_rewards) - rewards)

  .create_rl_result(
    algorithm = "linucb",
    policy = list(A = A, b = b),
    reward_history = rewards,
    params = list(alpha = alpha_param, context_dim = d),
    regret_history = regret
  )
}

# --- Contextual Thompson Sampling ---
#' @noRd
.run_contextual_ts <- function(env, n_rounds, verbose) {
  n_arms <- env$n_arms
  d <- env$context_dim

  # Prior: N(0, I) para theta de cada brazo
  B <- lapply(seq_len(n_arms), function(a) diag(d))
  mu <- lapply(seq_len(n_arms), function(a) rep(0, d))
  f <- lapply(seq_len(n_arms), function(a) rep(0, d))

  rewards <- numeric(n_rounds)
  actions <- integer(n_rounds)

  for (t in seq_len(n_rounds)) {
    context <- env$generate_context()

    # Muestrear theta de la posterior
    sampled_rewards <- sapply(seq_len(n_arms), function(a) {
      B_inv <- solve(B[[a]])
      theta_sample <- MASS_mvrnorm_simple(mu[[a]], B_inv)
      as.numeric(t(theta_sample) %*% context)
    })

    action <- which.max(sampled_rewards)
    reward <- env$pull(action, context)

    # Actualizar posterior
    B[[action]] <- B[[action]] + context %*% t(context)
    f[[action]] <- f[[action]] + reward * context
    mu[[action]] <- solve(B[[action]]) %*% f[[action]]

    rewards[t] <- reward
    actions[t] <- action
  }

  regret <- cumsum(rep(mean(rewards), n_rounds) * 1.2 - rewards)

  .create_rl_result(
    algorithm = "contextual_ts",
    policy = list(B = B, mu = mu),
    reward_history = rewards,
    params = list(context_dim = d),
    regret_history = regret
  )
}

# --- mvrnorm simple sin MASS ---
#' @noRd
MASS_mvrnorm_simple <- function(mu, sigma) {
  d <- length(mu)
  L <- tryCatch(chol(sigma), error = function(e) {
    chol(sigma + diag(d) * 1e-6)
  })
  z <- stats::rnorm(d)
  mu + as.numeric(t(L) %*% z)
}

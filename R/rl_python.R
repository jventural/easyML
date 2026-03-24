# =============================================================================
# Wrappers Python (Stable-Baselines3 + Gymnasium) para reinforcement_ml
# =============================================================================

#' @noRd
.rl_train_python <- function(gym_env, python_algo, n_episodes,
                             seed, verbose) {

  if (verbose) {
    cat("\n  --- Metodos Python (Stable-Baselines3) ---\n")
  }

  results <- list()

  if (!.check_rl_package("reticulate", "Python integration")) {
    return(results)
  }

  python_available <- tryCatch(reticulate::py_available(initialize = TRUE),
                                error = function(e) FALSE)
  if (!python_available) {
    if (verbose) cat("  Python no disponible.\n")
    return(results)
  }

  sb3_available <- tryCatch(reticulate::py_module_available("stable_baselines3"),
                             error = function(e) FALSE)
  gym_available <- tryCatch(reticulate::py_module_available("gymnasium"),
                             error = function(e) FALSE)

  if (!sb3_available || !gym_available) {
    if (verbose) {
      cat("  Paquetes Python requeridos no instalados.\n")
      cat("  Instalar con: pip install stable-baselines3 gymnasium\n")
    }
    return(results)
  }

  gymnasium <- reticulate::import("gymnasium")
  sb3 <- reticulate::import("stable_baselines3")
  np <- reticulate::import("numpy")

  for (algo in python_algo) {
    if (verbose) cat("\n  Entrenando ", toupper(algo), " en ", gym_env, "...\n", sep = "")

    tryCatch({
      env_py <- gymnasium$make(gym_env)

      total_timesteps <- as.integer(n_episodes * 200L)

      model <- switch(algo,
        "dqn" = sb3$DQN("MlpPolicy", env_py, verbose = 0L,
                         seed = as.integer(seed)),
        "ppo" = sb3$PPO("MlpPolicy", env_py, verbose = 0L,
                         seed = as.integer(seed)),
        stop("Algoritmo Python no soportado: ", algo)
      )

      model$learn(total_timesteps = total_timesteps)

      # Evaluar
      eval_rewards <- numeric(100)
      for (ep in seq_len(100)) {
        obs_reset <- env_py$reset(seed = as.integer(seed + ep))
        obs <- obs_reset[[1]]
        total_r <- 0
        done <- FALSE
        steps <- 0L
        while (!done && steps < 500L) {
          action_result <- model$predict(obs, deterministic = TRUE)
          action <- action_result[[1]]
          step_result <- env_py$step(action)
          obs <- step_result[[1]]
          reward <- as.numeric(step_result[[2]])
          terminated <- as.logical(step_result[[3]])
          truncated <- as.logical(step_result[[4]])
          done <- terminated || truncated
          total_r <- total_r + reward
          steps <- steps + 1L
        }
        eval_rewards[ep] <- total_r
      }

      env_py$close()

      if (verbose) {
        cat("  ", toupper(algo), " completado\n", sep = "")
        cat("  Return medio (eval):", round(mean(eval_rewards), 2), "\n")
        cat("  Return SD:", round(stats::sd(eval_rewards), 2), "\n")
        .print_rl_reference(algo)
      }

      results[[paste0("python_", algo)]] <- .create_rl_result(
        algorithm = paste0("python_", algo),
        policy = "neural_network",
        reward_history = eval_rewards,
        params = list(gym_env = gym_env, timesteps = total_timesteps,
                      model = model)
      )

    }, error = function(e) {
      if (verbose) cat("  Error en ", algo, ": ", e$message, "\n", sep = "")
    })
  }

  results
}

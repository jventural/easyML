# =============================================================================
# reinforcement_ml: Aprendizaje por Refuerzo Automatizado para easyML
# =============================================================================

#' @title reinforcement_ml: Aprendizaje por Refuerzo Automatizado
#'
#' @description
#' Pipeline completo de aprendizaje por refuerzo. Soporta dos modos:
#' bandits (multi-armed y contextuales) y MDP (Q-Learning, SARSA,
#' Value/Policy Iteration). Incluye entornos incorporados (GridWorld,
#' CliffWalking, FrozenLake), comparacion de algoritmos y visualizacion.
#' Opcionalmente integra Python (Stable-Baselines3) para Deep RL.
#'
#' @param mode Modo de operacion: "bandits" o "mdp".
#' @param env Entorno custom: lista con funciones $step(state, action) y $reset().
#' @param env_name Nombre de entorno incorporado: "gridworld", "cliffwalking", "frozenlake".
#' @param grid_size Tamano de grilla para gridworld/frozenlake (default: 4).
#' @param n_states Numero de estados para MDPs custom.
#' @param n_actions Numero de acciones para MDPs custom.
#' @param transition_prob Array P[s,a,s'] de probabilidades de transicion.
#' @param reward_matrix Matriz R[s,a] de recompensas.
#' @param bandit_data Data frame historico para bandits offline (columnas: action, reward).
#' @param n_arms Numero de brazos para bandits (default: 5).
#' @param arm_probs Vector de probabilidades reales por brazo (simulacion).
#' @param context_dim Dimension del contexto para bandits contextuales.
#' @param algorithms Algoritmos a ejecutar: "auto" o vector con:
#'   Bandits: "epsilon_greedy", "ucb1", "thompson", "linucb", "contextual_ts".
#'   MDP: "qlearning", "sarsa", "value_iteration", "policy_iteration".
#' @param epsilon Parametro epsilon para epsilon-greedy (default: 0.1).
#' @param ucb_c Parametro de exploracion para UCB1 (default: 2).
#' @param thompson_prior Prior Beta(alpha, beta) para Thompson Sampling (default: c(1,1)).
#' @param linucb_alpha Parametro alpha para LinUCB (default: 1.0).
#' @param learning_rate Tasa de aprendizaje para Q-Learning/SARSA (default: 0.1).
#' @param discount Factor de descuento gamma (default: 0.99).
#' @param epsilon_decay Factor de decaimiento de epsilon por episodio (default: 0.995).
#' @param min_epsilon Epsilon minimo (default: 0.01).
#' @param n_episodes Numero de episodios de entrenamiento (default: 1000).
#' @param n_steps Pasos maximos por episodio en MDP (default: 100).
#' @param use_python Usar metodos Python via reticulate (default: FALSE).
#' @param python_algo Algoritmos Python: "dqn", "ppo".
#' @param gym_env Nombre del entorno Gymnasium (default: "CartPole-v1").
#' @param n_simulations Numero de rondas para bandits (default: 1000).
#' @param run_comparison Ejecutar comparacion de algoritmos (default: TRUE).
#' @param seed Semilla (default: 2024).
#' @param verbose Mostrar progreso (default: TRUE).
#'
#' @return Objeto de clase \code{reinforcementml} (lista) con componentes:
#' \describe{
#'   \item{params}{Parametros del analisis (mode, algorithms, etc.)}
#'   \item{environment}{Informacion del entorno (name, n_states, n_actions)}
#'   \item{results}{Lista de resultados por algoritmo (policy, reward_history, q_table)}
#'   \item{summary_table}{data.frame comparativo de algoritmos}
#'   \item{best_algorithm}{Nombre del mejor algoritmo}
#'   \item{figures}{Lista de graficos ggplot2}
#'   \item{figures_catalog}{Catalogo de figuras con metadatos}
#'   \item{verbose_text}{Texto completo del verbose capturado}
#'   \item{elapsed_time}{Tiempo de ejecucion en segundos}
#' }
#'
#' @examples
#' \dontrun{
#' # Bandits basico
#' result <- reinforcement_ml(
#'   mode = "bandits",
#'   n_arms = 10,
#'   n_simulations = 5000
#' )
#' print(result)
#' plot(result, type = "regret")
#'
#' # MDP con GridWorld
#' result_mdp <- reinforcement_ml(
#'   mode = "mdp",
#'   env_name = "gridworld",
#'   n_episodes = 2000
#' )
#' plot(result_mdp, type = "qvalues")
#'
#' # Bandits contextuales
#' result_ctx <- reinforcement_ml(
#'   mode = "bandits",
#'   n_arms = 5,
#'   context_dim = 3,
#'   algorithms = c("linucb", "contextual_ts")
#' )
#' }
#'
#' @export
reinforcement_ml <- function(mode = c("bandits", "mdp"),
                             env = NULL,
                             env_name = NULL,
                             grid_size = 4,
                             n_states = NULL,
                             n_actions = NULL,
                             transition_prob = NULL,
                             reward_matrix = NULL,
                             bandit_data = NULL,
                             n_arms = 5,
                             arm_probs = NULL,
                             context_dim = NULL,
                             algorithms = "auto",
                             epsilon = 0.1,
                             ucb_c = 2,
                             thompson_prior = c(1, 1),
                             linucb_alpha = 1.0,
                             learning_rate = 0.1,
                             discount = 0.99,
                             epsilon_decay = 0.995,
                             min_epsilon = 0.01,
                             n_episodes = 1000,
                             n_steps = 100,
                             use_python = FALSE,
                             python_algo = c("dqn", "ppo"),
                             gym_env = "CartPole-v1",
                             n_simulations = 1000,
                             run_comparison = TRUE,
                             seed = 2024,
                             verbose = TRUE) {

  if (verbose) {
    temp_file <- tempfile(fileext = ".txt")
    sink(temp_file, split = TRUE)

    tryCatch({
      resultado <- .reinforcement_ml_internal(
        mode = mode, env = env, env_name = env_name, grid_size = grid_size,
        n_states = n_states, n_actions = n_actions,
        transition_prob = transition_prob, reward_matrix = reward_matrix,
        bandit_data = bandit_data, n_arms = n_arms, arm_probs = arm_probs,
        context_dim = context_dim, algorithms = algorithms,
        epsilon = epsilon, ucb_c = ucb_c, thompson_prior = thompson_prior,
        linucb_alpha = linucb_alpha, learning_rate = learning_rate,
        discount = discount, epsilon_decay = epsilon_decay,
        min_epsilon = min_epsilon, n_episodes = n_episodes,
        n_steps = n_steps, use_python = use_python,
        python_algo = python_algo, gym_env = gym_env,
        n_simulations = n_simulations, run_comparison = run_comparison,
        seed = seed, verbose = TRUE
      )
    }, finally = { sink() })

    if (file.exists(temp_file)) {
      verbose_output <- readLines(temp_file, warn = FALSE)
      unlink(temp_file)
    } else {
      verbose_output <- character(0)
    }
    resultado$verbose_text <- paste(verbose_output, collapse = "\n")
    resultado$verbose_lines <- verbose_output

  } else {
    resultado <- .reinforcement_ml_internal(
      mode = mode, env = env, env_name = env_name, grid_size = grid_size,
      n_states = n_states, n_actions = n_actions,
      transition_prob = transition_prob, reward_matrix = reward_matrix,
      bandit_data = bandit_data, n_arms = n_arms, arm_probs = arm_probs,
      context_dim = context_dim, algorithms = algorithms,
      epsilon = epsilon, ucb_c = ucb_c, thompson_prior = thompson_prior,
      linucb_alpha = linucb_alpha, learning_rate = learning_rate,
      discount = discount, epsilon_decay = epsilon_decay,
      min_epsilon = min_epsilon, n_episodes = n_episodes,
      n_steps = n_steps, use_python = use_python,
      python_algo = python_algo, gym_env = gym_env,
      n_simulations = n_simulations, run_comparison = run_comparison,
      seed = seed, verbose = FALSE
    )
  }

  return(resultado)
}


#' @noRd
.reinforcement_ml_internal <- function(mode, env, env_name, grid_size,
                                       n_states, n_actions,
                                       transition_prob, reward_matrix,
                                       bandit_data, n_arms, arm_probs,
                                       context_dim, algorithms,
                                       epsilon, ucb_c, thompson_prior,
                                       linucb_alpha, learning_rate,
                                       discount, epsilon_decay, min_epsilon,
                                       n_episodes, n_steps,
                                       use_python, python_algo, gym_env,
                                       n_simulations, run_comparison,
                                       seed, verbose) {

  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  set.seed(seed)
  start_time <- Sys.time()
  mode <- match.arg(mode, c("bandits", "mdp"))

  # =========================================================================
  # 1. Setup y Validacion
  # =========================================================================
  if (verbose) {
    .msg_header("reinforcement_ml - Aprendizaje por Refuerzo")
    cat("Modo:", mode, "\n")
    .print_rl_reference("rl_general")
  }

  resultado <- list()
  resultado$call <- match.call()

  # Auto-select algorithms
  has_transition <- !is.null(transition_prob)
  if (identical(algorithms, "auto")) {
    algorithms <- .auto_select_algorithms(mode, context_dim, has_transition)
  }

  if (verbose) cat("Algoritmos:", paste(algorithms, collapse = ", "), "\n")

  resultado$params <- list(
    mode = mode, algorithms = algorithms, seed = seed,
    n_simulations = n_simulations, n_episodes = n_episodes
  )

  # =========================================================================
  # 2. Configurar Entorno
  # =========================================================================
  if (verbose) .print_section(2, "Configuracion del Entorno")

  if (mode == "bandits") {
    is_contextual <- !is.null(context_dim) && context_dim > 0

    if (is_contextual) {
      env <- make_contextual_bandit_env(n_arms, context_dim, seed)
    } else {
      env <- make_bandit_env(n_arms, arm_probs, seed)
    }

    if (verbose) {
      cat("  Tipo: ", if (is_contextual) "Contextual Bandit" else "Multi-Armed Bandit", "\n")
      cat("  Brazos:", env$n_arms, "\n")
      if (!is_contextual && !is.null(env$probs)) {
        cat("  Probabilidades: ", paste(round(env$probs, 3), collapse = ", "), "\n")
        cat("  Brazo optimo: ", env$optimal_arm,
            " (p = ", round(env$optimal_reward, 3), ")\n", sep = "")
      }
      if (is_contextual) cat("  Dimension contexto:", context_dim, "\n")
      cat("  Rondas:", n_simulations, "\n")
    }

    resultado$environment <- list(
      name = env$name, type = env$type,
      n_arms = env$n_arms, context_dim = context_dim
    )

  } else {
    # MDP
    if (!is.null(env_name)) {
      env <- .load_environment(env_name, grid_size)
    } else if (is.null(env)) {
      env <- make_gridworld(size = grid_size)
      if (verbose) cat("  Entorno por defecto: GridWorld ", grid_size, "x", grid_size, "\n")
    }

    if (verbose) {
      cat("  Entorno:", env$name, "\n")
      cat("  Estados:", env$n_states, "\n")
      cat("  Acciones:", env$n_actions, "\n")
      if (!is.null(env$action_names)) {
        cat("  Nombres de acciones:", paste(env$action_names, collapse = ", "), "\n")
      }
      cat("  Episodios:", n_episodes, "\n")
      cat("  Max pasos/episodio:", n_steps, "\n")
    }

    resultado$environment <- list(
      name = env$name, type = env$type,
      n_states = env$n_states, n_actions = env$n_actions,
      action_names = env$action_names
    )
  }

  # =========================================================================
  # 3. Entrenamiento
  # =========================================================================
  if (verbose) .print_section(3, "Entrenamiento de Algoritmos")

  all_results <- list()

  if (mode == "bandits") {
    bandit_algos <- intersect(algorithms,
                              c("epsilon_greedy", "ucb1", "thompson", "linucb", "contextual_ts"))
    if (length(bandit_algos) > 0) {
      bandit_results <- .rl_train_bandits(
        env, bandit_algos, n_simulations,
        epsilon, ucb_c, thompson_prior, linucb_alpha,
        context_dim, seed, verbose
      )
      all_results <- c(all_results, bandit_results)
    }
  } else {
    # Tabular RL
    tabular_algos <- intersect(algorithms, c("qlearning", "sarsa"))
    if (length(tabular_algos) > 0) {
      tabular_results <- .rl_train_tabular(
        env, tabular_algos, n_episodes, n_steps,
        learning_rate, discount, epsilon, epsilon_decay,
        min_epsilon, seed, verbose
      )
      all_results <- c(all_results, tabular_results)
    }

    # Exact MDP methods
    exact_algos <- intersect(algorithms, c("value_iteration", "policy_iteration"))
    if (length(exact_algos) > 0 && has_transition) {
      exact_results <- .rl_train_mdp_exact(
        transition_prob, reward_matrix, discount, exact_algos, verbose
      )
      all_results <- c(all_results, exact_results)
    }
  }

  resultado$results <- all_results

  # =========================================================================
  # 4-5. Evaluacion y Comparacion
  # =========================================================================
  if (run_comparison && length(all_results) > 0) {
    eval_result <- .rl_evaluate(all_results, mode, env, verbose)
    resultado$summary_table <- eval_result$summary_table
    resultado$best_algorithm <- eval_result$best_algorithm
  } else {
    resultado$summary_table <- data.frame()
    resultado$best_algorithm <- if (length(all_results) > 0) names(all_results)[1] else NA
  }

  # =========================================================================
  # Python (opcional)
  # =========================================================================
  if (use_python && mode == "mdp") {
    python_results <- .rl_train_python(gym_env, python_algo, n_episodes, seed, verbose)
    if (length(python_results) > 0) {
      resultado$results <- c(resultado$results, python_results)
      resultado$python <- python_results
    }
  }

  # =========================================================================
  # 6. Figuras
  # =========================================================================
  fig_result <- .rl_plots(resultado, verbose)
  resultado$figures <- fig_result$plots
  resultado$figures_catalog <- fig_result$figures_catalog

  # =========================================================================
  # Final
  # =========================================================================
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  resultado$elapsed_time <- elapsed

  if (verbose) {
    .msg_header("Analisis Completado!")
    cat("Modo:", mode, "\n")
    cat("Algoritmos:", length(all_results), "\n")
    if (!is.na(resultado$best_algorithm)) {
      cat("Mejor algoritmo:", resultado$best_algorithm, "\n")
    }
    cat("Figuras:", length(resultado$figures), "\n")
    cat("Tiempo:", round(elapsed, 1), "s\n")
  }

  class(resultado) <- c("reinforcementml", "list")
  return(resultado)
}


# =============================================================================
# S3 Methods
# =============================================================================

#' @title Print method for reinforcementml
#' @param x Objeto de clase reinforcementml.
#' @param ... Argumentos adicionales (no usados).
#' @export
print.reinforcementml <- function(x, ...) {
  cat("\n")
  cat(.line("="), "\n")
  cat("  reinforcement_ml - Resultados\n")
  cat(.line("="), "\n\n")

  cat("CONFIGURACION:\n")
  cat("  Modo:", x$params$mode, "\n")
  cat("  Entorno:", x$environment$name, "\n")
  cat("  Algoritmos:", paste(names(x$results), collapse = ", "), "\n")

  if (!is.null(x$summary_table) && nrow(x$summary_table) > 0) {
    cat("\nRESULTADOS:\n")
    st <- x$summary_table
    for (i in seq_len(nrow(st))) {
      if (x$params$mode == "bandits") {
        cat(sprintf("  %-20s Reward=%.3f  Regret=%.1f\n",
                    st$algorithm[i], st$mean_reward[i], st$total_regret[i]))
      } else {
        cat(sprintf("  %-20s Mean Return=%.3f\n",
                    st$algorithm[i], st$mean_reward[i]))
      }
    }
  }

  if (!is.na(x$best_algorithm)) {
    cat("\nMEJOR ALGORITMO:", x$best_algorithm, "\n")
  }

  cat("Figuras:", length(x$figures), "| Tiempo:", round(x$elapsed_time, 1), "s\n")
  invisible(x)
}

#' @title Summary method for reinforcementml
#' @param object Objeto de clase reinforcementml.
#' @param ... Argumentos adicionales (no usados).
#' @export
summary.reinforcementml <- function(object, ...) {
  cat("\n=== Resumen reinforcement_ml ===\n\n")
  cat("Modo:", object$params$mode, "\n")
  cat("Entorno:", object$environment$name, "\n\n")

  if (!is.null(object$summary_table) && nrow(object$summary_table) > 0) {
    cat("--- Comparacion de Algoritmos ---\n")
    print(object$summary_table, row.names = FALSE)
  }

  cat("\nMejor algoritmo:", object$best_algorithm, "\n")
  invisible(object)
}

#' @title Plot method for reinforcementml
#' @param x Objeto de clase reinforcementml.
#' @param type Tipo de grafico: "all", "reward", "regret", "actions",
#'   "qvalues", "returns", "comparison", "posteriors".
#' @param ... Argumentos adicionales (no usados).
#' @export
plot.reinforcementml <- function(x, type = "all", ...) {
  if (is.null(x$figures) || length(x$figures) == 0) {
    message("No hay figuras disponibles")
    return(invisible(NULL))
  }

  if (type == "all") return(x$figures)

  plot_map <- list(
    reward = "reward_curves",
    regret = "regret_curves",
    actions = "action_distribution",
    qvalues = "q_value_heatmap",
    returns = "episode_returns",
    comparison = "algorithm_comparison",
    posteriors = "thompson_posteriors"
  )

  plot_name <- plot_map[[type]]
  if (is.null(plot_name)) plot_name <- type

  if (plot_name %in% names(x$figures)) return(x$figures[[plot_name]])

  message("Tipo '", type, "' no encontrado. Disponibles: ",
          paste(names(x$figures), collapse = ", "))
  invisible(NULL)
}

#' @title Predict method for reinforcementml
#' @param object Objeto de clase reinforcementml.
#' @param state Estado actual (numerico para MDP, vector de contexto para bandits contextuales).
#' @param algorithm Algoritmo a usar. Default: el mejor.
#' @param ... Argumentos adicionales (no usados).
#' @export
predict.reinforcementml <- function(object, state = NULL, algorithm = NULL, ...) {
  if (is.null(algorithm)) algorithm <- object$best_algorithm
  if (is.na(algorithm) || !algorithm %in% names(object$results)) {
    stop("Algoritmo no encontrado: ", algorithm)
  }

  res <- object$results[[algorithm]]
  policy <- res$policy

  if (is.null(state)) {
    return(policy)
  }

  if (is.numeric(policy) && length(policy) >= state) {
    return(policy[state])
  }

  if (!is.null(res$q_table) && is.matrix(res$q_table)) {
    return(which.max(res$q_table[state, ]))
  }

  stop("No se puede predecir accion para el estado dado con ", algorithm)
}

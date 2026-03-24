# =============================================================================
# Funciones auxiliares para reinforcement_ml
# =============================================================================

# --- Validacion ---

#' @noRd
.validate_rl_inputs <- function(mode, env, env_name, bandit_data,
                                n_arms, arm_probs, algorithms) {
  mode <- match.arg(mode, c("bandits", "mdp"))

  if (mode == "bandits") {
    if (!is.null(bandit_data) && !is.data.frame(bandit_data)) {
      stop("'bandit_data' debe ser un data.frame con columnas: action, reward (y opcionalmente contexto)")
    }
    if (!is.null(arm_probs)) {
      if (!is.numeric(arm_probs) || any(arm_probs < 0) || any(arm_probs > 1)) {
        stop("'arm_probs' debe ser un vector numerico con valores entre 0 y 1")
      }
    }
    if (n_arms < 2) stop("'n_arms' debe ser >= 2")
  }

  if (mode == "mdp") {
    if (is.null(env) && is.null(env_name) && is.null(transition_prob)) {
      # Se usara entorno por defecto
    }
    if (!is.null(env) && !is.list(env) && !is.function(env)) {
      stop("'env' debe ser una lista con funciones $step() y $reset(), o un nombre de entorno")
    }
  }

  # Validar algoritmos
  valid_bandit <- c("epsilon_greedy", "ucb1", "thompson", "linucb", "contextual_ts")
  valid_mdp <- c("qlearning", "sarsa", "value_iteration", "policy_iteration")
  valid_all <- c(valid_bandit, valid_mdp, "auto")

  if (!identical(algorithms, "auto")) {
    invalid <- setdiff(algorithms, valid_all)
    if (length(invalid) > 0) {
      stop("Algoritmos no validos: ", paste(invalid, collapse = ", "),
           "\nOpciones bandits: ", paste(valid_bandit, collapse = ", "),
           "\nOpciones MDP: ", paste(valid_mdp, collapse = ", "))
    }
  }

  return(mode)
}

#' @noRd
.auto_select_algorithms <- function(mode, context_dim, has_transition) {
  if (mode == "bandits") {
    algos <- c("epsilon_greedy", "ucb1", "thompson")
    if (!is.null(context_dim) && context_dim > 0) {
      algos <- c(algos, "linucb", "contextual_ts")
    }
  } else {
    algos <- c("qlearning", "sarsa")
    if (has_transition) {
      algos <- c(algos, "value_iteration", "policy_iteration")
    }
  }
  algos
}

# --- Utilidades de epsilon-greedy ---

#' @noRd
.epsilon_greedy_action <- function(q_values, epsilon, n_actions) {
  if (stats::runif(1) < epsilon) {
    sample.int(n_actions, 1)
  } else {
    # Romper empates al azar
    max_q <- max(q_values)
    best <- which(q_values == max_q)
    if (length(best) == 1) best else sample(best, 1)
  }
}

# --- Calculo de regret ---

#' @noRd
.calc_cumulative_regret <- function(rewards, optimal_reward) {
  cumsum(optimal_reward - rewards)
}

# --- Moving average ---

#' @noRd
.moving_average <- function(x, window = 50) {
  n <- length(x)
  if (n < window) window <- n
  result <- numeric(n)
  cs <- cumsum(x)
  for (i in seq_len(n)) {
    start <- max(1, i - window + 1)
    result[i] <- (cs[i] - if (start > 1) cs[start - 1] else 0) / (i - start + 1)
  }
  result
}

# --- Formato de resultado por algoritmo ---

#' @noRd
.create_rl_result <- function(algorithm, policy, reward_history,
                              q_table = NULL, params = list(),
                              regret_history = NULL, training_time = 0) {
  list(
    algorithm = algorithm,
    policy = policy,
    reward_history = reward_history,
    regret_history = regret_history,
    q_table = q_table,
    parameters = params,
    mean_reward = mean(reward_history),
    cumulative_reward = sum(reward_history),
    total_regret = if (!is.null(regret_history)) utils::tail(regret_history, 1) else NA,
    training_time = training_time
  )
}

# --- Summary row para comparacion ---

#' @noRd
.create_rl_summary_row <- function(algorithm, mean_reward, cumulative_reward,
                                   total_regret, training_time) {
  data.frame(
    algorithm = algorithm,
    mean_reward = round(mean_reward, 4),
    cumulative_reward = round(cumulative_reward, 2),
    total_regret = round(total_regret, 2),
    training_time = round(training_time, 3),
    stringsAsFactors = FALSE
  )
}

# --- Referencias RL ---

#' @noRd
.print_rl_reference <- function(ref_key) {
  references <- list(
    rl_general = "Sutton, R. S., & Barto, A. G. (2018). Reinforcement learning: An introduction (2nd ed.). MIT Press.",
    epsilon_greedy = "Sutton, R. S., & Barto, A. G. (2018). Reinforcement learning: An introduction (2nd ed., Ch. 2). MIT Press.",
    ucb1 = "Auer, P., Cesa-Bianchi, N., & Fischer, P. (2002). Finite-time analysis of the multiarmed bandit problem. Machine Learning, 47(2-3), 235-256.",
    thompson_sampling = "Thompson, W. R. (1933). On the likelihood that one unknown probability exceeds another. Biometrika, 25(3-4), 285-294.",
    linucb = "Li, L., Chu, W., Langford, J., & Schapire, R. E. (2010). A contextual-bandit approach to personalized news article recommendation. WWW, 661-670.",
    contextual_ts = "Agrawal, S., & Goyal, N. (2013). Thompson sampling for contextual bandits with linear payoffs. ICML, 127-135.",
    qlearning = "Watkins, C. J. C. H., & Dayan, P. (1992). Q-learning. Machine Learning, 8(3-4), 279-292.",
    sarsa = "Rummery, G. A., & Niranjan, M. (1994). On-line Q-learning using connectionist systems. Cambridge University.",
    value_iteration = "Bellman, R. (1957). Dynamic programming. Princeton University Press.",
    policy_iteration = "Howard, R. A. (1960). Dynamic programming and Markov processes. MIT Press.",
    dqn = "Mnih, V., et al. (2015). Human-level control through deep reinforcement learning. Nature, 518(7540), 529-533.",
    ppo = "Schulman, J., Wolski, F., Dhariwal, P., Radford, A., & Klimov, O. (2017). Proximal policy optimization algorithms. arXiv:1707.06347."
  )

  if (ref_key %in% names(references)) {
    cat("\n    Referencia: ", references[[ref_key]], "\n", sep = "")
  }
}

# --- Verificar paquete opcional (reutiliza patron de causal) ---

#' @noRd
.check_rl_package <- function(pkg, purpose = "") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Paquete '", pkg, "' no instalado.")
    if (nchar(purpose) > 0) msg <- paste0(msg, " Requerido para: ", purpose, ".")
    msg <- paste0(msg, "\n  Instalar con: install.packages('", pkg, "')")
    warning(msg, call. = FALSE)
    return(FALSE)
  }
  return(TRUE)
}

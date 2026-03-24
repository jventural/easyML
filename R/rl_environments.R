# =============================================================================
# Entornos incorporados para reinforcement_ml
# =============================================================================

#' @noRd
make_bandit_env <- function(n_arms = 5, probs = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(probs)) {
    probs <- stats::runif(n_arms, 0.1, 0.9)
  }
  n_arms <- length(probs)
  optimal_arm <- which.max(probs)
  optimal_reward <- max(probs)

  list(
    name = "MultiArmedBandit",
    type = "bandit",
    n_arms = n_arms,
    n_states = 1,
    n_actions = n_arms,
    probs = probs,
    optimal_arm = optimal_arm,
    optimal_reward = optimal_reward,
    pull = function(arm) {
      as.integer(stats::runif(1) < probs[arm])
    },
    reset = function() 1L,
    step = function(state, action) {
      r <- as.integer(stats::runif(1) < probs[action])
      list(next_state = 1L, reward = r, done = FALSE)
    }
  )
}

#' @noRd
make_contextual_bandit_env <- function(n_arms = 5, context_dim = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # Coeficientes lineales: reward = context %*% theta_a + noise
  thetas <- matrix(stats::rnorm(context_dim * n_arms), nrow = context_dim, ncol = n_arms)

  list(
    name = "ContextualBandit",
    type = "contextual_bandit",
    n_arms = n_arms,
    n_states = NA,
    n_actions = n_arms,
    context_dim = context_dim,
    thetas = thetas,
    generate_context = function() {
      stats::rnorm(context_dim)
    },
    pull = function(arm, context) {
      mu <- sum(context * thetas[, arm])
      reward <- 1 / (1 + exp(-mu))  # sigmoid
      as.integer(stats::runif(1) < reward)
    },
    optimal_action = function(context) {
      expected <- sapply(seq_len(n_arms), function(a) {
        1 / (1 + exp(-sum(context * thetas[, a])))
      })
      which.max(expected)
    },
    optimal_reward = function(context) {
      expected <- sapply(seq_len(n_arms), function(a) {
        1 / (1 + exp(-sum(context * thetas[, a])))
      })
      max(expected)
    }
  )
}

#' @noRd
make_gridworld <- function(size = 4, goal = NULL, obstacles = NULL) {
  n_states <- size * size
  n_actions <- 4  # 1=up, 2=down, 3=left, 4=right
  if (is.null(goal)) goal <- n_states  # esquina inferior derecha
  if (is.null(obstacles)) obstacles <- integer(0)
  action_names <- c("Up", "Down", "Left", "Right")

  # Movimientos: (fila_delta, col_delta)
  moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))

  state_to_rc <- function(s) c((s - 1) %/% size + 1, (s - 1) %% size + 1)
  rc_to_state <- function(r, c) (r - 1) * size + c

  step_fn <- function(state, action) {
    if (state == goal) return(list(next_state = goal, reward = 0, done = TRUE))

    rc <- state_to_rc(state)
    mv <- moves[[action]]
    nr <- rc[1] + mv[1]
    nc <- rc[2] + mv[2]

    # Verificar limites
    if (nr < 1 || nr > size || nc < 1 || nc > size) {
      nr <- rc[1]; nc <- rc[2]
    }
    ns <- rc_to_state(nr, nc)

    # Verificar obstaculos
    if (ns %in% obstacles) {
      ns <- state
    }

    reward <- if (ns == goal) 1 else -0.04
    done <- ns == goal

    list(next_state = ns, reward = reward, done = done)
  }

  list(
    name = paste0("GridWorld_", size, "x", size),
    type = "mdp",
    n_states = n_states,
    n_actions = n_actions,
    action_names = action_names,
    size = size,
    goal = goal,
    obstacles = obstacles,
    start = 1L,
    reset = function() 1L,
    step = step_fn,
    state_to_rc = state_to_rc,
    rc_to_state = rc_to_state
  )
}

#' @noRd
make_cliffwalking <- function() {
  rows <- 4; cols <- 12
  n_states <- rows * cols
  n_actions <- 4
  start <- (rows - 1) * cols + 1  # (4,1) = estado 37

  goal <- rows * cols              # (4,12) = estado 48
  cliff <- ((rows - 1) * cols + 2):(rows * cols - 1)  # estados 38-47

  moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  state_to_rc <- function(s) c((s - 1) %/% cols + 1, (s - 1) %% cols + 1)
  rc_to_state <- function(r, c) (r - 1) * cols + c

  step_fn <- function(state, action) {
    rc <- state_to_rc(state)
    mv <- moves[[action]]
    nr <- min(max(rc[1] + mv[1], 1), rows)
    nc <- min(max(rc[2] + mv[2], 1), cols)
    ns <- rc_to_state(nr, nc)

    if (ns %in% cliff) {
      return(list(next_state = start, reward = -100, done = FALSE))
    }
    if (ns == goal) {
      return(list(next_state = goal, reward = -1, done = TRUE))
    }
    list(next_state = ns, reward = -1, done = FALSE)
  }

  list(
    name = "CliffWalking",
    type = "mdp",
    n_states = n_states,
    n_actions = n_actions,
    action_names = c("Up", "Down", "Left", "Right"),
    rows = rows, cols = cols,
    start = start, goal = goal, cliff = cliff,
    reset = function() start,
    step = step_fn,
    state_to_rc = state_to_rc
  )
}

#' @noRd
make_frozenlake <- function(size = 4, is_slippery = TRUE) {
  n_states <- size * size
  n_actions <- 4
  goal <- n_states

  # Hoyos predefinidos para 4x4
  if (size == 4) {
    holes <- c(6, 8, 11, 12)  # posiciones clasicas
  } else {
    # Generar hoyos aleatorios (~20%)
    n_holes <- max(1, round(n_states * 0.2))
    holes <- sample(setdiff(2:(n_states - 1), goal), n_holes)
  }

  moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  state_to_rc <- function(s) c((s - 1) %/% size + 1, (s - 1) %% size + 1)
  rc_to_state <- function(r, c) (r - 1) * size + c

  do_move <- function(state, action) {
    rc <- state_to_rc(state)
    mv <- moves[[action]]
    nr <- min(max(rc[1] + mv[1], 1), size)
    nc <- min(max(rc[2] + mv[2], 1), size)
    rc_to_state(nr, nc)
  }

  step_fn <- function(state, action) {
    if (is_slippery) {
      # 1/3 prob de ir a donde quieres, 1/3 a cada perpendicular
      perp <- if (action <= 2) c(3, 4) else c(1, 2)
      r <- stats::runif(1)
      if (r < 1/3) {
        actual_action <- perp[1]
      } else if (r < 2/3) {
        actual_action <- perp[2]
      } else {
        actual_action <- action
      }
    } else {
      actual_action <- action
    }

    ns <- do_move(state, actual_action)

    if (ns %in% holes) {
      return(list(next_state = ns, reward = 0, done = TRUE))
    }
    if (ns == goal) {
      return(list(next_state = ns, reward = 1, done = TRUE))
    }
    list(next_state = ns, reward = 0, done = FALSE)
  }

  list(
    name = paste0("FrozenLake_", size, "x", size),
    type = "mdp",
    n_states = n_states,
    n_actions = n_actions,
    action_names = c("Up", "Down", "Left", "Right"),
    size = size, goal = goal, holes = holes,
    is_slippery = is_slippery,
    start = 1L,
    reset = function() 1L,
    step = step_fn,
    state_to_rc = state_to_rc
  )
}

#' @noRd
.load_environment <- function(env_name, grid_size = 4) {
  switch(env_name,
    "gridworld" = make_gridworld(size = grid_size),
    "cliffwalking" = make_cliffwalking(),
    "frozenlake" = make_frozenlake(size = grid_size),
    stop("Entorno no reconocido: ", env_name,
         "\nOpciones: gridworld, cliffwalking, frozenlake")
  )
}

# =============================================================================
# Ejemplo de uso de reinforcement_ml()
# =============================================================================

library(easyML)

# --- 1. Multi-Armed Bandits (basico) ---

result_bandit <- reinforcement_ml(
  mode = "bandits",
  n_arms = 10,
  n_simulations = 5000
)
print(result_bandit)
plot(result_bandit, type = "regret")
plot(result_bandit, type = "posteriors")

# --- 2. Bandits con probabilidades conocidas ---

result_known <- reinforcement_ml(
  mode = "bandits",
  arm_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
  n_simulations = 3000,
  algorithms = c("epsilon_greedy", "ucb1", "thompson")
)
summary(result_known)

# --- 3. Bandits Contextuales ---

result_ctx <- reinforcement_ml(
  mode = "bandits",
  n_arms = 5,
  context_dim = 3,
  algorithms = c("linucb", "contextual_ts"),
  n_simulations = 2000
)
plot(result_ctx, type = "reward")

# --- 4. Q-Learning en GridWorld ---

result_grid <- reinforcement_ml(
  mode = "mdp",
  env_name = "gridworld",
  grid_size = 4,
  algorithms = c("qlearning", "sarsa"),
  n_episodes = 2000,
  learning_rate = 0.1,
  discount = 0.99
)
print(result_grid)
plot(result_grid, type = "returns")
plot(result_grid, type = "qvalues")

# Predecir accion para estado 1
predict(result_grid, state = 1)

# --- 5. CliffWalking (Q-Learning vs SARSA) ---

result_cliff <- reinforcement_ml(
  mode = "mdp",
  env_name = "cliffwalking",
  algorithms = c("qlearning", "sarsa"),
  n_episodes = 5000,
  n_steps = 200
)
plot(result_cliff, type = "comparison")

# --- 6. FrozenLake ---

result_frozen <- reinforcement_ml(
  mode = "mdp",
  env_name = "frozenlake",
  grid_size = 4,
  n_episodes = 10000,
  learning_rate = 0.8,
  discount = 0.95
)

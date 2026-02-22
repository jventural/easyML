# =============================================================================
# Test script for block_split()
# =============================================================================

# Source the function
source("D:/14. LIBRERIAS/easyML_github/R/block_split.R")

cat("=== TEST 1: Basic usage with simulated data ===\n\n")

# Create test data simulating survey responses over time
set.seed(42)
n <- 500
test_data <- data.frame(
  id = 1:n,
  response_date = seq(as.Date("2024-01-01"), by = "day", length.out = n),
  age = sample(18:65, n, replace = TRUE),
  score = rnorm(n, mean = 50, sd = 10),
  outcome = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
)

# Test basic split
result1 <- block_split(
  data = test_data,
  order_col = "response_date",
  num_blocks = 5,
  test_prop = 0.20,
  seed = 123
)

print(result1)

cat("\n--- Verification ---\n")
cat("Train rows:", nrow(result1$train), "\n")
cat("Test rows:", nrow(result1$test), "\n")
cat("Sum equals total:", nrow(result1$train) + nrow(result1$test) == n, "\n")
cat("No overlap:", length(intersect(result1$train_idx, result1$test_idx)) == 0, "\n")


cat("\n\n=== TEST 2: With stratification ===\n\n")

result2 <- block_split(
  data = test_data,
  order_col = "response_date",
  num_blocks = 5,
  test_prop = 0.20,
  stratify = "outcome",
  seed = 123
)

print(result2)

# Check class proportions
cat("--- Class proportions ---\n")
cat("Original data:\n")
print(prop.table(table(test_data$outcome)))
cat("\nTraining set:\n")
print(prop.table(table(result2$train$outcome)))
cat("\nTest set:\n")
print(prop.table(table(result2$test$outcome)))


cat("\n\n=== TEST 3: Without order column (uses row order) ===\n\n")

result3 <- block_split(
  data = test_data,
  num_blocks = 4,
  test_prop = 0.25,
  seed = 456
)

print(result3)


cat("\n\n=== TEST 4: With exact test_n ===\n\n")

result4 <- block_split(
  data = test_data,
  order_col = "id",
  num_blocks = 5,
  test_n = 100,  # Exactly 100 observations for test
  seed = 789
)

print(result4)
cat("Exact test size achieved:", nrow(result4$test), "\n")


cat("\n\n=== TEST 5: Proportional blocks (balance_blocks = FALSE) ===\n\n")

# Create unequal data (some periods have more responses)
unequal_data <- test_data[c(1:100, 101:150, 151:350, 351:400, 401:500), ]
unequal_data$row_num <- 1:nrow(unequal_data)

result5 <- block_split(
  data = unequal_data,
  order_col = "row_num",
  num_blocks = 5,
  test_prop = 0.20,
  balance_blocks = FALSE,
  seed = 123
)

print(result5)


cat("\n\n=== TEST 6: Edge case - small dataset ===\n\n")

small_data <- test_data[1:50, ]

result6 <- block_split(
  data = small_data,
  num_blocks = 5,
  test_prop = 0.20,
  seed = 123
)

print(result6)


cat("\n\n=== TEST 7: Verify test set comes from all blocks ===\n\n")

# Check that test observations come from different parts of the data
test_ids <- result1$test$id
cat("Test IDs range:\n")
cat("  Min ID:", min(test_ids), "\n")
cat("  Max ID:", max(test_ids), "\n")
cat("  IDs from first 100:", sum(test_ids <= 100), "\n")
cat("  IDs from 101-200:", sum(test_ids > 100 & test_ids <= 200), "\n")
cat("  IDs from 201-300:", sum(test_ids > 200 & test_ids <= 300), "\n")
cat("  IDs from 301-400:", sum(test_ids > 300 & test_ids <= 400), "\n")
cat("  IDs from 401-500:", sum(test_ids > 400), "\n")


cat("\n\n=== ALL TESTS COMPLETED ===\n")

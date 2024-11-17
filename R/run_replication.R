#' Run Parallel Replications with Optional Parallel Processing
#'
#' This function runs multiple replications of a simulation for all conditions,
#' with an option to use parallel processing.
#'
#' @param conditions A data frame containing the grid of conditions with `n_students` and `n_items`.
#' @param n_replications Number of replications to run for each condition.
#' @param parallel Logical. If TRUE, runs simulations in parallel.
#' @param num_cores Number of cores to use for parallel processing. Defaults to all available cores minus one.
#' @return A list of results, including theta, item, and correlation summaries for all conditions.
#' @examples
#' conditions <- expand.grid(n_students = c(500, 1000), n_items = c(12, 24))
#' results <- run_replications(conditions, n_replications = 100, parallel = TRUE)
#' @importFrom parallel makeCluster stopCluster clusterExport clusterEvalQ detectCores
#' @importFrom pbapply pblapply
#' @export
run_replications <- function(conditions, n_replications = 100, parallel = TRUE, num_cores = detectCores() - 1) {

  # Function to process each condition
  process_condition <- function(i) {
    run_condition(conditions[i, ], n_replications)
  }

  if (parallel) {
    # Run in parallel
    cl <- makeCluster(num_cores)
    on.exit(stopCluster(cl))  # Ensure the cluster stops when the function exits
    clusterExport(cl, c("simulate_and_estimate", "generate_item_par", "aggregate_results", "run_condition"))
    clusterEvalQ(cl, {
      library(MASS)
      library(mirt)
      library(dplyr)
      library(tibble)
      library(pbapply)
    })
    all_conditions_results <- pblapply(1:nrow(conditions), process_condition, cl = cl)
  } else {
    # Run sequentially
    all_conditions_results <- lapply(1:nrow(conditions), process_condition)
  }

  # Combine results
  final_theta_results <- do.call(rbind, lapply(all_conditions_results, function(res) res$Theta_Summary))
  final_item_results <- do.call(rbind, lapply(all_conditions_results, function(res) res$Item_Summary))
  final_correlation_results <- do.call(rbind, lapply(all_conditions_results, function(res) res$Correlation_Summary))

  list(
    Theta_Results = final_theta_results,
    Item_Results = final_item_results,
    Correlation_Results = final_correlation_results
  )
}

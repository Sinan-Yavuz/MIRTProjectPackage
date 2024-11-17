#' Run Simulation for a Single Condition
#'
#' This function runs multiple replications of a simulation for a given condition,
#' aggregating the results across all replications.
#'
#' @param condition A data frame row representing a single condition with `n_students` and `n_items`.
#' @param n_replications Number of replications to run for the condition.
#' @return A list containing aggregated results for the condition.
#' @examples
#' conditions <- data.frame(n_students = c(500, 1000), n_items = c(12, 24))
#' run_condition(conditions[1, ], 100)
#' @export
run_condition <- function(condition, n_replications = 100) {
  n_students <- condition$n_students
  n_items <- condition$n_items

  # Define fixed item parameters for the condition
  item_pars <- generate_item_par(n_items)
  a <- item_pars$a
  b <- item_pars$b

  # Run replications
  rep_results <- lapply(1:n_replications, function(rep) {
    simulate_and_estimate(seed = rep, a, b, n_students = n_students, n_items = n_items)
  })

  # Aggregate results for the condition
  aggregated_results <- aggregate_results(rep_results)

  # Add condition metadata to the results
  aggregated_results$Theta_Summary <- aggregated_results$Theta_Summary %>%
    mutate(n_students = n_students, n_items = n_items)
  aggregated_results$Item_Summary <- aggregated_results$Item_Summary %>%
    mutate(n_students = n_students, n_items = n_items)
  aggregated_results$Correlation_Summary <- aggregated_results$Correlation_Summary %>%
    mutate(n_students = n_students, n_items = n_items)

  return(aggregated_results)
}

#' Aggregate Simulation Results Across Replications
#'
#' This function aggregates results from multiple replications of simulations, computing
#' metrics such as bias and RMSE for both theta estimates and item parameters.
#'
#' @param all_results A list of results from multiple replications.
#' @return A list containing aggregated theta, item, and correlation summaries.
#' @examples
#' all_results <- list(run_condition(conditions[1, ], 2), run_condition(conditions[2, ], 2))
#' aggregate_results(all_results)
#' @export
aggregate_results <- function(all_results) {
  # Combine correlation results
  correlation_results <- do.call(rbind, lapply(all_results, function(res) {
    if (!is.null(res$Correlation_Results)) res$Correlation_Results else NULL
  }))

  if (!is.null(correlation_results) && nrow(correlation_results) > 0) {
    correlation_summary <- correlation_results %>%
      summarize(
        True_Correlation = mean(True_Correlation, na.rm = TRUE),
        Estimated_Correlation = mean(Estimated_Correlation, na.rm = TRUE),
        Correlation_Bias = mean(Correlation_Bias, na.rm = TRUE)
      )
  } else {
    correlation_summary <- data.frame(
      True_Correlation = NA,
      Estimated_Correlation = NA,
      Correlation_Bias = NA
    )
  }

  # Extract and combine all Theta Results
  theta_results <- do.call(rbind, lapply(all_results, function(res) {
    if (!is.null(res$Theta_Results)) res$Theta_Results else NULL
  }))

  if (!is.null(theta_results) && nrow(theta_results) > 0) {
    # Calculate RMSE properly: Avg MSE across replications, then take the square root
    theta_rmse <- theta_results %>%
      filter(Metric == "RMSE") %>%
      group_by(Model, Dimension) %>%
      summarize(
        MSE_Average = mean(Value^2, na.rm = TRUE),  # Compute average MSE
        RMSE = sqrt(MSE_Average),                  # Compute RMSE
        .groups = "drop"
      )

    theta_summary <- theta_results %>%
      filter(Metric != "RMSE") %>%
      group_by(Model, Metric, Dimension) %>%
      summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      bind_rows(
        theta_rmse %>% mutate(Metric = "RMSE", Average_Value = RMSE) %>% select(-MSE_Average, -RMSE)
      )
  } else {
    theta_summary <- data.frame()
  }

  # Extract and combine all Item Results
  # Extract and combine all Item Results
item_results <- do.call(rbind, lapply(all_results, function(res) {
  if (!is.null(res$Item_Results)) {
    tibble::tibble(
      Model = c(rep("UIRT", 4), rep("MIRT", 8)),  # Adjust this length
      Metric = c(
        "Bias_Discrimination", "RMSE_Discrimination", "Bias_Difficulty", "RMSE_Difficulty",  # UIRT
        "Bias_Discrimination", "RMSE_Discrimination", "Bias_Difficulty", "RMSE_Difficulty",  # MIRT Avg
        "Bias_Discrimination_a1", "Bias_Discrimination_a2", "RMSE_Discrimination_a1", "RMSE_Discrimination_a2"  # MIRT Individual
      ),
      Value = c(
        res$Item_Results$UIRT$Bias_Discrimination,
        res$Item_Results$UIRT$RMSE_Discrimination,
        res$Item_Results$UIRT$Bias_Difficulty,
        res$Item_Results$UIRT$RMSE_Difficulty,
        res$Item_Results$MIRT$Avg_Bias_Discrimination,
        res$Item_Results$MIRT$Avg_RMSE_Discrimination,
        res$Item_Results$MIRT$Bias_Difficulty,
        res$Item_Results$MIRT$RMSE_Difficulty,
        res$Item_Results$MIRT$Bias_Discrimination["a1"],
        res$Item_Results$MIRT$Bias_Discrimination["a2"],
        res$Item_Results$MIRT$RMSE_Discrimination["a1"],
        res$Item_Results$MIRT$RMSE_Discrimination["a2"]
      )
    )
  } else {
    NULL
  }
}))

  if (!is.null(item_results) && nrow(item_results) > 0) {
    item_summary <- item_results %>%
      group_by(Model, Metric) %>%
      summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")
  } else {
    item_summary <- data.frame()
  }

  # Return summaries as a list
  list(
    Theta_Summary = theta_summary,
    Item_Summary = item_summary,
    Correlation_Summary = correlation_summary
  )
}

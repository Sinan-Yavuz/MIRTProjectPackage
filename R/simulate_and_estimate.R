#' Simulate Response Data and Estimate IRT Parameters
#'
#' This function simulates item responses for a given set of item parameters and estimates
#' parameters using unidimensional (UIRT) and multidimensional (MIRT) IRT models.
#'
#' @param seed Integer seed for reproducibility.
#' @param a Matrix of discrimination parameters for items.
#' @param b Vector of difficulty parameters for items.
#' @param n_students Number of students to simulate responses for.
#' @param n_items Number of items in the simulation.
#' @return A list containing theta results, item parameter results, and correlation results.
#' @examples
#' params <- generate_item_par(12)
#' simulate_and_estimate(seed = 1, a = params$a, b = params$b, n_students = 500, n_items = 12)
#' @export
simulate_and_estimate <- function(seed, a, b, n_students = 500, n_items = 12) {
  set.seed(123 * seed)  # Set seed for reproducibility

  # Step 1: Define covariance structure and simulate Theta
  Theta_cov <- matrix(c(1, 0.6, 0.6, 1), ncol = 2)
  Theta_true <- MASS::mvrnorm(n_students, mu = c(0, 0), Sigma = Theta_cov)  # True theta
  response_data <- simdata(a = a, d = b, itemtype = '2PL', Theta = Theta_true)

  # Step 2: Estimate models
  uirt_model <- mirt(response_data, 1, itemtype = '2PL')  # Unidimensional
  mirt_model <- mirt(response_data, 2, itemtype = '2PL')  # Multidimensional

  # Step 3: Extract theta scores
  uirt_theta_scores <- fscores(uirt_model)  # UIRT theta
  mirt_theta_scores <- fscores(mirt_model)  # MIRT theta

  # Step 4: Compute estimated correlation
  estimated_correlation <- cor(mirt_theta_scores[, 1], mirt_theta_scores[, 2])
  correlation_bias <- estimated_correlation - 0.6

  # Step 5: Extract item parameters
  true_a <- a  # True discrimination parameters
  true_b <- b  # True difficulty parameters

  # UIRT parameters
  uirt_estimates <- coef(uirt_model, IRTpars = TRUE, simplify = TRUE)
  uirt_a <- uirt_estimates$items[, "a"]  # Discrimination
  uirt_b <- uirt_estimates$items[, "b"]  # Difficulty

  # MIRT parameters
  mirt_estimates <- coef(mirt_model, IRTpars = TRUE, simplify = TRUE)
  mirt_a <- mirt_estimates$items[, c("a1", "a2")]  # Discrimination for two factors
  mirt_b <- mirt_estimates$items[, "b"]           # Difficulty
  mirt_b[is.na(mirt_b)] <- 0  # Handle NAs in difficulty

  # Step 6: Compute Bias and RMSE for theta scores
  bias_uirt <- mean(uirt_theta_scores[, 1] - Theta_true[, 1])
  rmse_uirt <- sqrt(mean((uirt_theta_scores[, 1] - Theta_true[, 1])^2))

  bias_mirt <- mean(colMeans(mirt_theta_scores - Theta_true))
  rmse_mirt <- mean(sqrt(colMeans((mirt_theta_scores - Theta_true)^2)))

  theta_composite <- (Theta_true[, 1] + Theta_true[, 2]) / 2
  bias_uirt_composite <- mean(uirt_theta_scores[, 1] - theta_composite)
  rmse_uirt_composite <- sqrt(mean((uirt_theta_scores[, 1] - theta_composite)^2))

  # Step 7: Compute Bias and RMSE for item parameters
  # UIRT
  bias_uirt_a <- mean(uirt_a - rowSums(true_a))  # Bias for UIRT discrimination
  rmse_uirt_a <- sqrt(mean((uirt_a - rowSums(true_a))^2))  # RMSE for UIRT discrimination
  bias_uirt_b <- mean(uirt_b - true_b)  # Bias for UIRT difficulty
  rmse_uirt_b <- sqrt(mean((uirt_b - true_b)^2))  # RMSE for UIRT difficulty

  # MIRT
  bias_mirt_a <- colMeans(mirt_a - true_a)  # Bias for MIRT discrimination (a1 and a2 separately)
  rmse_mirt_a <- sqrt(colMeans((mirt_a - true_a)^2))  # RMSE for MIRT discrimination (a1 and a2 separately)
  bias_mirt_b <- mean(mirt_b - true_b)  # Bias for MIRT difficulty
  rmse_mirt_b <- sqrt(mean((mirt_b - true_b)^2))  # RMSE for MIRT difficulty

  # Compute average bias and RMSE for MIRT discrimination
  avg_bias_mirt_a <- mean(bias_mirt_a)  # Average bias for MIRT discrimination (a1 and a2)
  avg_rmse_mirt_a <- mean(rmse_mirt_a)  # Average RMSE for MIRT discrimination (a1 and a2)

  # Step 8: Return results as a list
  list(
    Theta_Results = data.frame(
      Model = c("UIRT", "UIRT", "MIRT", "MIRT", "UIRT_Composite", "UIRT_Composite"),
      Metric = c("Bias", "RMSE", "Bias", "RMSE", "Bias", "RMSE"),
      Dimension = c("Dim-1", "Dim-1", "Average", "Average", "Composite", "Composite"),
      Value = c(bias_uirt, rmse_uirt, bias_mirt, rmse_mirt, bias_uirt_composite, rmse_uirt_composite)
    ),
    Item_Results = list(
      UIRT = list(
        Bias_Discrimination = bias_uirt_a,
        RMSE_Discrimination = rmse_uirt_a,
        Bias_Difficulty = bias_uirt_b,
        RMSE_Difficulty = rmse_uirt_b
      ),
      MIRT = list(
        Bias_Discrimination = bias_mirt_a,  # Individual biases for a1 and a2
        RMSE_Discrimination = rmse_mirt_a,  # Individual RMSEs for a1 and a2
        Avg_Bias_Discrimination = avg_bias_mirt_a,  # Average bias for MIRT discrimination
        Avg_RMSE_Discrimination = avg_rmse_mirt_a,  # Average RMSE for MIRT discrimination
        Bias_Difficulty = bias_mirt_b,
        RMSE_Difficulty = rmse_mirt_b
      )
    ),
    Correlation_Results = data.frame(
      True_Correlation = 0.6,
      Estimated_Correlation = estimated_correlation,
      Correlation_Bias = correlation_bias
    )
  )
}

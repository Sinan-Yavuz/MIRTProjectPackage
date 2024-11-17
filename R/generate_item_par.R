#' Generate Fixed Item Parameters
#'
#' This function generates fixed item parameters for simulation.
#'
#' @param n_items Number of items to simulate.
#' @return A list containing discrimination (`a`) and difficulty (`b`) parameters.
#' @examples
#' generate_item_par(12)
#' @export
generate_item_par <- function(n_items) {
  set.seed(12345) # for item parameters
  a <- matrix(0, nrow = n_items, ncol = 2)
  factor1_items <- sample(1:n_items, size = floor(n_items / 2))
  factor2_items <- setdiff(1:n_items, factor1_items)
  a[factor1_items, 1] <- runif(length(factor1_items), 0.8, 2.0)
  a[factor2_items, 2] <- runif(length(factor2_items), 0.8, 2.0)
  b <- runif(n_items, -2, 2)
  return(list(a = a, b = b))
}

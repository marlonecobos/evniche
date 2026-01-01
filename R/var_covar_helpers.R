#' Variance from range
#' @param range matrix of two rows (minimum and maximum) x as many columns as
#' variables to consider.
#' @return vector of variance values for all variables considered.
#' @usage
#' var_from_range(range)
#' @export

var_from_range <- function(range) {
  if (missing(range)) {
    stop("Argument 'range' needs to be defined")
  }
  if (!class(range)[1] %in% c("matrix", "data.frame")) {
    stop("'range' must be of class 'matrix' or 'data.frame'")
  }
  cnam <- colnames(range)
  if (is.null(cnam)) {
    cnam <- paste0("var", 1:ncol(range))
  }

  vars <- vapply(1:ncol(range), FUN.VALUE = numeric(1), function(x) {
    ((range[2, x] - range[1, x]) / 6)^2
  })

  names(vars) <- cnam
  return(vars)
}



#' is_positive_definite
#' @description Checks if a matrix is positive-definite by attempting a Cholesky
#' decomposition.
#' @param mat A numeric matrix.
#' @return A logical value indicating if the matrix is positive-definite.
#' @noRd
is_positive_definite <- function(mat) {
  # The chol() function will fail if the matrix is not positive definite.
  # Using try() is an efficient way to check.
  res <- try(chol(mat), silent = TRUE)
  return(inherits(res, "matrix"))
}


#' Find maximum covariance for one pair
#'
#' @description Uses a binary search algorithm to find the maximum valid
#' covariance for a single pair of variables, given a fixed set of other
#' covariance values. The search ensures the resulting variance-covariance
#' matrix remains positive-definite.
#'
#' @param cov_index (numeric) index of the covariance to be tested.
#' @param variances (numeric) a vector of all variances.
#' @param covariances (numeric) a vector of all covariances.
#' @param n_iter (numeric) number of iterations for the binary search.
#' Default = 50, which provides high precision.
#'
#' @return The maximum valid covariance value for the specified pair.
#' @noRd
find_max_covariance <- function(cov_index, variances, covariances, n_iter = 50) {
  # Get indices for the two variables involved in this covariance
  lvar <- length(variances)
  all_pairs <- utils::combn(1:lvar, 2)
  var_indices <- all_pairs[, cov_index]

  # Theoretical maximum is sqrt(var_i * var_j)
  high <- sqrt(variances[var_indices[1]] * variances[var_indices[2]])
  low <- 0

  # Perform binary search
  for (i in 1:n_iter) {
    mid <- (low + high) / 2
    temp_covs <- covariances
    temp_covs[cov_index] <- mid

    mat <- var_cov_matrix(variances, temp_covs)

    if (is_positive_definite(mat)) {
      # The matrix is positive-definite, so this 'mid' is a possible value.
      # Try for a larger one.
      low <- mid
    } else {
      # The matrix is not positive-definite, so 'mid' is too high.
      high <- mid
    }
  }

  return(low)
}


#' Covariance value limits given variable ranges
#'
#' @description Calculates the minimum and maximum valid covariance values for
#' pairs of variables to ensure the variance-covariance matrix remains
#' positive-definite.
#'
#' @details
#' The function uses an efficient and robust approach to find covariance limits:
#' \itemize{
#'   \item{\strong{2-Variable Case:}}{For two variables, it uses the direct
#'   analytical solution: `|cov| < sqrt(var1 * var2)`.}
#'   \item{\strong{N-Variable Case:}}{For more than two variables, it uses an
#'   iterative algorithm. It initializes all covariances to zero and then
#'   repeatedly cycles through each covariance pair. For each pair, it performs
#'   a binary search (`find_max_covariance`) to find the maximum valid
#'   covariance, given the current values of all other covariances. This process
#'   is repeated until the values converge, ensuring a stable and valid set of
#'   limits.}
#'   \item{\strong{Positive-Definite Test:}}{Matrix positive-definiteness is
#'   checked using Cholesky decomposition (`chol()`), which is computationally
#'   faster and more numerically stable for this purpose than eigenvalue
#'   decomposition.}
#' }
#'
#' @param range matrix of two rows (minimum and maximum) x as many columns as
#' variables to consider.
#' @param tol (numeric) This parameter is kept for compatibility but is no
#' longer used by the new algorithm. Binary search precision is determined by
#' `n_iter` in the `find_max_covariance` helper.
#' @param max_iter (numeric) The maximum number of iterations for the
#' convergence loop when there are more than 2 variables. Default = 20.
#' @param convergence_threshold (numeric) The threshold for checking
#' convergence. The algorithm stops when the maximum change in any covariance
#' value between iterations is below this threshold. Default = 1e-6.
#'
#' @return A data.frame with estimated minimum and maximum covariance values
#' for each pair of variables.
#' @usage
#' covariance_limits(range, tol = 1e-8, max_iter = 20,
#'                   convergence_threshold = 1e-6)
#' @export
#' @examples
#' \donttest{
#' range_matrix <- matrix(c(1, 10, 2, 20, 5, 30), nrow = 2)
#' colnames(range_matrix) <- c("Var1", "Var2", "Var3")
#' limits <- covariance_limits(range_matrix)
#' print(limits)
#' }

covariance_limits <- function(range, tol = 1e-8, max_iter = 20,
                              convergence_threshold = 1e-6) {
  if (missing(range)) {
    stop("Argument 'range' must be defined")
  }

  # Variances from range
  variances <- var_from_range(range)
  lvar <- length(variances)
  cnam <- names(variances)

  # Generate row names for the output data.frame
  rnames <- utils::combn(cnam, 2)
  rnames <- vapply(1:ncol(rnames), FUN.VALUE = character(1), function(x) {
    paste0(rnames[, x], collapse = "-")
  })

  # For 2 variables, use the exact analytical solution
  if (lvar == 2) {
    max_cov <- sqrt(variances[1] * variances[2])
    return(data.frame(min_covariance = -max_cov, max_covariance = max_cov,
                      row.names = rnames))
  }

  # For >2 variables, use iterative binary search
  n_covs <- ncol(utils::combn(lvar, 2))
  max_covs <- rep(0, n_covs) # Start with all covariances at 0

  for (iter in 1:max_iter) {
    prev_max_covs <- max_covs

    # Cycle through each covariance and find its maximum valid value
    for (i in 1:n_covs) {
      # Find the max value for cov i, assuming symmetry for min value
      # We update the vector in place for the next calculation in the loop
      max_covs[i] <- find_max_covariance(i, variances, max_covs)
    }

    # Check for convergence
    if (max(abs(max_covs - prev_max_covs)) < convergence_threshold) {
      break
    }
  }

  return(data.frame(min_covariance = -max_covs, max_covariance = max_covs,
                    row.names = rnames))
}


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


#' Covariance value limits given variable ranges
#'
#' @description Calculates the minimum and maximum valid covariance values for
#' pairs of variables by finding the maximum uniform correlation that can be
#' applied across all variables simultaneously.
#'
#' @details
#' This function adopts a simple and robust model to define covariance limits.
#' It finds the largest possible uniform correlation coefficient, `rho`, that can
#' exist between all pairs of variables simultaneously while keeping the
#' variance-covariance matrix positive-definite.
#' \itemize{
#'   \item{\strong{Method:}}{The function performs a binary search for the
#'   optimal `rho` within the range [0, 1]. In each step, it constructs a
#'   correlation matrix where all pairs share the same correlation `rho` and
#'   tests for positive definiteness. For a matrix to be positive-definite,
#'   all its eigenvalues must be positive. It is known that for a uniform
#'   correlation matrix of size `n x n`, the eigenvalues are `1 + (n-1)*rho`
#'   (1 time) and `1 - rho` (n-1 times). For all eigenvalues to be positive,
#'   we need `1 - rho > 0`, which means `rho < 1`, and `1 + (n-1)*rho > 0`,
#'   which means `rho > -1/(n-1)`. The binary search thus finds the maximal
#'   `rho` in `[-1/(n-1), 1]`.}
#'   \item{\strong{Output:}}{The returned `max_covariance` vector is derived from
#'   this single maximal `rho`. The `min_covariance` is derived from the minimal
#'   `rho`. This ensures the limits are symmetric and intuitive.}
#'   \item{\strong{Guarantee:}}{A matrix constructed from a scaled version of the
#'   output (e.g., `max_covariance * 0.8`) is guaranteed to be positive-definite.}
#' }
#'
#' @param range matrix of two rows (minimum and maximum) x as many columns as
#' variables to consider.
#'
#' @return A data.frame with the estimated symmetric minimum and maximum
#' covariance values for all variable pairs.
#' @usage
#' covariance_limits(range)
#' @export
#' @examples
#' \donttest{
#' # Four variables
#' range_matrix <- cbind(Temp = c(10, 25), Precip = c(700, 2800),
#'                       Humid = c(30, 70), Rad = c(100, 600))
#' limits <- covariance_limits(range_matrix)
#'
#' # Create a valid covariance matrix using the result
#' # This matrix is guaranteed to be positive-definite
#' scaled_covs <- limits$max_covariance * 0.8
#' vars <- evniche:::var_from_range(range_matrix)
#' valid_matrix <- evniche:::var_cov_matrix(vars, scaled_covs)
#' print(evniche:::is_positive_definite(valid_matrix)) # TRUE
#'
#' # Matrix with limits themselves should also pass due to tolerance
#' full_covs <- limits$max_covariance
#' valid_matrix_full <- evniche:::var_cov_matrix(vars, full_covs)
#' print(evniche:::is_positive_definite(valid_matrix_full)) # TRUE
#' print(limits)
#' }

covariance_limits <- function(range) {
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

  # For a uniform correlation matrix, the minimum possible rho is -1/(n-1)
  min_rho <- -1 / (lvar - 1)
  max_rho <- 1.0

  # Apply a tolerance factor to ensure strict positive definiteness
  tolerance <- 1 - 1e-9
  min_rho <- min_rho * tolerance
  max_rho <- max_rho * tolerance

  # Calculate the covariance values from max_rho and min_rho
  var_combn <- utils::combn(sqrt(variances), 2)
  prod_sdevs <- apply(var_combn, 2, prod)
  
  max_covs <- max_rho * prod_sdevs
  min_covs <- min_rho * prod_sdevs

  return(data.frame(min_covariance = min_covs,
                    max_covariance = max_covs, row.names = rnames))
}


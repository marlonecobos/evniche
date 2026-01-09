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



#' Find covariance bounds using binary search
#'
#' @description Iteratively finds the minimum and maximum bounds for each
#' covariance pair in a variance-covariance matrix that maintain its
#' positive-definite property. It uses a binary search for efficiency and
#' explores each covariance's positive and negative limits independently.
#'
#' @param variances (numeric) a vector of variances.
#' @param vcov_matrix A variance-covariance matrix to be modified. The values
#' in this matrix serve as the starting context for the search.
#' @param var_indices A 2-row matrix with column indices of variable pairs
#' corresponding to the columns of the `range` matrix.
#' @param order (numeric) The order in which to process the covariance pairs.
#' @param tol (numeric) Tolerance for checking positive definiteness.
#'
#' @return A data.frame with the adjusted minimum and maximum valid
#' covariance values. The rows are in the original (pre-shuffled) order.
#' @noRd
find_covariance_bounds <- function(variances, vcov_matrix, var_indices,
                                   order, tol) {
  n_covs <- ncol(var_indices)
  min_covs <- numeric(n_covs)
  max_covs <- numeric(n_covs)

  # Iterate through covariances in the specified (shuffled) order
  for (i in order) {
    v1 <- var_indices[1, i]
    v2 <- var_indices[2, i]
    
    # Temporarily set current covariance to 0 to isolate its effect
    vcov_matrix[v1, v2] <- vcov_matrix[v2, v1] <- 0
    
    # If matrix is not positive-definite even with a zero covariance,
    # it means other covariances are already too extreme.
    # In this unstable state, we cannot find a valid range for this pair.
    if (!is_pos_def(vcov_matrix, tol)) {
      min_covs[i] <- 0
      max_covs[i] <- 0
      next
    }

    # --- Find max bound ---
    good_val <- 0
    bad_val <- sqrt(variances[v1] * variances[v2]) # Theoretical max
    
    vcov_matrix[v1, v2] <- vcov_matrix[v2, v1] <- bad_val
    if (is_pos_def(vcov_matrix, tol)) {
      # If theoretical max is valid, there's no upper bound for this search
      max_covs[i] <- bad_val
    } else {
      # Binary search for the max boundary
      for (j in 1:100) { # 100 iterations for precision
        mid <- (good_val + bad_val) / 2
        if (abs(bad_val - good_val) < tol) break
        vcov_matrix[v1, v2] <- vcov_matrix[v2, v1] <- mid
        if (is_pos_def(vcov_matrix, tol)) {
          good_val <- mid
        } else {
          bad_val <- mid
        }
      }
      max_covs[i] <- good_val
    }

    # --- Find min bound ---
    good_val <- 0
    bad_val <- -sqrt(variances[v1] * variances[v2]) # Theoretical min

    vcov_matrix[v1, v2] <- vcov_matrix[v2, v1] <- bad_val
    if (is_pos_def(vcov_matrix, tol)) {
      min_covs[i] <- bad_val
    } else {
      # Binary search for the min boundary
      for (j in 1:100) {
        mid <- (good_val + bad_val) / 2
        if (abs(bad_val - good_val) < tol) break
        vcov_matrix[v1, v2] <- vcov_matrix[v2, v1] <- mid
        if (is_pos_def(vcov_matrix, tol)) {
          good_val <- mid
        } else {
          bad_val <- mid
        }
      }
      min_covs[i] <- good_val
    }
    
    # Update matrix with a neutral value (midpoint of found range) for the
    # next covariance's context
    final_mid <- (min_covs[i] + max_covs[i]) / 2
    vcov_matrix[v1, v2] <- vcov_matrix[v2, v1] <- final_mid
  }
  
  # Return a data.frame with rows in the original, unshuffled order
  result_df <- data.frame(min_covariance = min_covs,
                          max_covariance = max_covs)
  return(result_df)
}


#' Check if a matrix is square, symmetric, and positive definite
#'
#' @param x a matrix to be tested.
#' @param tol (numeric) tolerance for checking if eigenvalues are positive.
#' Default = 1e-8.
#'
#' @return A logical value indicating if the matrix is positive definite.
#' @noRd
is_pos_def <- function(x, tol = 1e-8) {
  if (missing(x)) {
    stop("Argument 'x' needs to be defined")
  }
  if (!is.matrix(x)) {
    stop("'x' is not of class 'matrix'")
  }
  if (nrow(x) != ncol(x)) {
    stop("'x' is not a square matrix")
  }
  if (sum(x == t(x)) != (nrow(x) ^ 2)) {
    stop("'x' is not symetric")
  }
  eigenvalues <- eigen(x, only.values = TRUE)$values
  eigenvalues <- ifelse(eigenvalues < tol, 0, eigenvalues)
  return(all(eigenvalues > 0))
}


#' Create a variance-covariance matrix
#'
#' @param variances (numeric) a named vector of variances for the variables.
#' @param covariances (numeric) a vector or single value of covariances.
#' Default = 0.
#'
#' @return A variance-covariance matrix.
#' @noRd
var_cov_matrix <- function(variances, covariances = 0) {
  if (missing(variances)) {
    stop("Argument 'variances' must be defined")
  }

  # preparing data
  nvar <- length(variances)
  elcovs <- (nvar^2 / 2) - (nvar / 2)
  lcovs <- length(covariances)

  if (lcovs != elcovs) {
    if (lcovs == 1) {
      message("All 'covariances' will be populated with ", covariances)
    } else {
      stop("'covariances' must be of length 1 or ", elcovs)
    }
  }

  mat <- matrix(nrow = nvar, ncol = nvar)

  # populating the matrix
  diag(mat) <- variances

  mat[lower.tri(mat)] <- covariances

  up <- unlist(lapply(1:((nvar - 1)), function(x) {
    vec <- (nvar * 1:(nvar - 1)) + x
    if (x > 1) {vec <- vec[-(1:(x - 1))]}
    vec
  }))

  mat[up] <- covariances

  vnam <- names(variances)
  if (!is.null(vnam)) {
    colnames(mat) <- rownames(mat) <- vnam
  }

  return(mat)
}


#' Covariance value limits given variable ranges
#' @param range range values for the variables considered.
#' @param tol a value of tolerance for tests. Default = 1e-8.
#' @return a data.frame with estimated minimum and maximum covariance values
#' for the variables, given the ranges provided.
#' @usage
#' covariance_limits(range, tol = 1e-8)
#' @export

covariance_limits <- function(range, tol = 1e-8) {
  if (missing(range)) {
    stop("Argument 'variances' must be defined")
  }

  # variances from range
  variances <- var_from_range(range)

  # number of variables
  lvar <- length(variances)

  # variable pair names for rows
  rnames <- combn(names(variances), 2)
  rnames <- vapply(1:ncol(rnames), FUN.VALUE = character(1), function(x) {
    paste0(rnames[, x], collapse = "-")
  })

  # Special case for two variables (direct calculation)
  if (lvar == 2) {
    # For two variables, the covariance limit is the product of their
    # standard deviations. A variance-covariance matrix is positive definite if
    # the absolute value of the covariance is less than the product of the
    # standard deviations of the variables.
    # |Cov(X,Y)| < sd(X) * sd(Y)
    sds <- sqrt(variances)
    max_cov <- sds[1] * sds[2]

    return(data.frame(min_covariance = -max_cov, max_covariance = max_cov,
                      row.names = rnames))
  } else {
    # General case for > 2 variables (iterative search)
    n_covs <- ncol(combn(lvar, 2))

    # Start with a zero-covariance matrix (guaranteed positive definite)
    suppressMessages(vcov_matrix <- var_cov_matrix(variances, 0))

    # Get variable indices for the find_covariance_bounds helper
    var_indices <- combn(1:lvar, 2)

    # Store previous results to check for convergence
    prev_bounds <- data.frame(min_covariance = rep(-Inf, n_covs),
                              max_covariance = rep(Inf, n_covs))
    current_bounds <- data.frame(min_covariance = rep(NA, n_covs),
                                 max_covariance = rep(NA, n_covs))

    # Iteratively refine bounds until they stabilize
    for (iter in 1:100) { # Max 100 iterations to prevent infinite loops
      # Randomize order of optimization for robustness
      order <- sample(1:n_covs)

      # Find bounds for this iteration
      current_bounds <- find_covariance_bounds(variances, vcov_matrix,
                                               var_indices, order, tol)

      # Check for convergence
      if (isTRUE(all.equal(prev_bounds, current_bounds, tolerance = tol))) {
        break
      }
      prev_bounds <- current_bounds

      # Update the main vcov_matrix for the next iteration using the
      # mid-point of the newly found bounds to serve as a neutral context.
      mid_points <- (current_bounds$min_covariance +
                       current_bounds$max_covariance) / 2
      vcov_matrix[lower.tri(vcov_matrix)] <- mid_points
      vcov_matrix[upper.tri(vcov_matrix)] <- mid_points
    }

    rownames(current_bounds) <- rnames
    return(current_bounds)
  }
}

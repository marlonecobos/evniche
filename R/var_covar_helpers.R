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



#' Test and adjust covariance values
#'
#' @description Iteratively tests and adjusts covariance values to find the
#' boundaries that maintain a positive-definite covariance matrix. This is a
#' helper function for \code{\link{covariance_limits}}.
#'
#' @param variances (numeric) a vector of variances.
#' @param covariances (numeric) a vector of covariances to be tested.
#' @param tol (numeric) tolerance for checking positive definiteness.
#' Default = 1e-8.
#'
#' @return A data.frame with the adjusted minimum and maximum valid
#' covariance values.
#' @noRd
covar_test <- function(variances, covariances, tol = 1e-8) {
  if (missing(variances)) {
    stop("Argument 'variances' must be defined")
  }
  if (missing(covariances)) {
    stop("Argument 'covariances' must be defined")
  }

  lvar <- length(variances)
  mat <- var_cov_matrix(variances, covariances)

  lo <- which(lower.tri(mat))
  up <- unlist(lapply(1:((lvar - 1)), function(x) {
    vec <- (lvar * 1:(lvar - 1)) + x
    if (x > 1) {
      vec <- vec[-(1:(x - 1))]
    }
    vec
  }))

  add <- covariances / 100

  covss <- vector(mode = "numeric")

  for (x in 1:length(covariances)) {
    cov1 <- covariances[x]
    ad <- add[x]
    cov1 <- cov1 + ad

    mat[lo[x]] <- cov1; mat[up[x]] <- cov1

    if (is_pos_def(mat, tol = tol) == FALSE) {
      cov1 <- covariances[x]
    } else {
      cond <- FALSE
      adp <- c(NA, NA)
      adp1 <- c(ad, ad)

      while (cond == FALSE) {
        adp[1] <- cov1
        mat[lo[x]] <- cov1; mat[up[x]] <- cov1

        if (is_pos_def(mat, tol = tol) == TRUE) {
          cov1 <- cov1 + ad
          adp[2] <- cov1

          mat[lo[x]] <- cov1; mat[up[x]] <- cov1
          if (is_pos_def(mat, tol = tol) == TRUE) {
            cov1 <- cov1 + ad
          } else {
            cov1 <- cov1 - ad
            break()
          }
        } else {
          cov1 <- cov1 - ad
          adp[2] <- cov1

          mat[lo[x]] <- cov1; mat[up[x]] <- cov1
          if (is_pos_def(mat, tol = tol) == TRUE) {
            break()
          } else {
            cov1 <- cov1 - ad
          }
        }
        if (any(adp %in% adp1)) {
          ad <- ad / 10
        }
        adp1 <- adp
      }
    }

    mat[lo[x]] <- cov1; mat[up[x]] <- cov1

    covss[x] <- cov1
  }

  return(data.frame(min_covariance = -covss, max_covariance = covss))
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

  # rownames matrix
  rnames <- combn(names(variances), 2)
  rnames <- vapply(1:ncol(rnames), FUN.VALUE = character(1), function(x) {
    paste0(rnames[, x], collapse = "-")
  })

  # Special case for two variables
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
    # General case for more than two variables

    # first step, finding relatively good covariances
    cond <- FALSE
    varcom <- combn(variances, 2)
    covs1 <- apply(varcom, 2, mean)
    add <- covs1 / 100

    while (cond == FALSE) {
      mat1 <- var_cov_matrix(variances, covs1)

      if (is_pos_def(mat1, tol = tol) == TRUE) {
        covs1 <- covs1 + add

        mat1 <- var_cov_matrix(variances, covs1)
        if (is_pos_def(mat1, tol = tol) == TRUE) {
          covs1 <- covs1 + add
        } else {
          covs1 <- covs1 - add
          break()
        }
      } else {
        covs1 <- covs1 - add

        mat1 <- var_cov_matrix(variances, covs1)
        if (is_pos_def(mat1, tol = tol) == TRUE) {
          break()
        } else {
          add <- ifelse(covs1 <= add, add / 10, add)
          covs1 <- covs1 - add
        }
      }
    }

    # second step, test by modifying cov by cov, if needed
    covars <- covar_test(variances, covs1, tol = tol)
    cond <- identical(covs1, covars$max_covariance)

    if (!cond) {
      while (cond == FALSE) {
        ctest <- covars$max_covariance
        covars <- covar_test(variances, covars$max_covariance, tol = tol)
        cond <- identical(ctest, covars$max_covariance)
      }
    }
    rownames(covars) <- rnames

    return(covars)
  }
}

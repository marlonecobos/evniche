#' Convert color name to hexadecimal
#'
#' @param color_name (character) name of a color.
#'
#' @return A character string of the color in hexadecimal format.
#' @noRd
cname_2hex <- function(color_name) {
  cname_2rgb <- c(col2rgb(color_name))
  rgb(cname_2rgb[1], cname_2rgb[2], cname_2rgb[3], maxColorValue = 255)
}


#' Floor a number to a specified number of decimal places
#'
#' @param x (numeric) number to be processed.
#' @param level (numeric) number of decimal places to be used. Default = 1.
#'
#' @return A numeric value rounded down to the specified decimal level.
#' @noRd
floor_dec <- function(x, level = 1) {
  round(x - (5 * 10^(-level-1)), level)
}


#' Ceiling a number to a specified number of decimal places
#'
#' @param x (numeric) number to be processed.
#' @param level (numeric) number of decimal places to be used. Default = 1.
#'
#' @return A numeric value rounded up to the specified decimal level.
#' @noRd
ceiling_dec <- function(x, level = 1) {
  round(x + (5 * 10^(-level-1)), level)
}


#' Detect number of decimals to round
#'
#' @param x (numeric) number to be evaluated.
#'
#' @return A numeric value indicating the number of decimal places to round to.
#' @noRd
dec_2round <- function(x) {
  ns <- strsplit(as.character(c(abs(x))), ".", fixed = TRUE)[[1]]
  nmax <- as.numeric(ns[1])
  ifelse(nmax >= 100, 0, ifelse(nmax >= 10, 1, ifelse(nmax >= 1, 2, 3)))
}


#' Round covariance limit values and calculate a step
#'
#' @param min (numeric) minimum value.
#' @param max (numeric) maximum value.
#'
#' @return A named numeric vector with rounded minimum and maximum values,
#' and a step value.
#' @noRd
round_covlimstep <- function(min, max) {
  nu <- dec_2round(max)
  return(c(min = ceiling_dec(min, nu),
           max = floor_dec(max, nu),
           step = round(floor_dec(max, nu) / 50, nu)))
}


#' Calculate centroid from ranges
#'
#' @param range matrix or data.frame of two rows (minimum and maximum) and as
#' many columns as variables.
#'
#' @return A numeric vector of centroid values.
#' @noRd
centroid <- function(range) {
  return(apply(range, 2, mean))
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

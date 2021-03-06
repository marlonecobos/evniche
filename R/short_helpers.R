# hexadecimal from color name
cname_2hex <- function(color_name) {
  cname_2rgb <- c(col2rgb(color_name))
  rgb(cname_2rgb[1], cname_2rgb[2], cname_2rgb[3], maxColorValue = 255)
}

# ceiling and floor for decimals
floor_dec <- function(x, level = 1) {
  round(x - (5 * 10^(-level-1)), level)
}
ceiling_dec <- function(x, level = 1) {
  round(x + (5 * 10^(-level-1)), level)
}

# detect number of decimals to round
dec_2round <- function(x) {
  ns <- strsplit(as.character(c(abs(x))), ".", fixed = TRUE)[[1]]
  nmax <- as.numeric(ns[1])
  ifelse(nmax >= 100, 0, ifelse(nmax >= 10, 1, ifelse(nmax >= 1, 2, 3)))
}

# round covariance limit values
round_covlimstep <- function(min, max) {
  nu <- dec_2round(max)
  return(c(min = ceiling_dec(min, nu),
           max = floor_dec(max, nu),
           step = round(floor_dec(max, nu) / 50, nu)))
}

# centroid from ranges
centroid <- function(range) {
  return(apply(range, 2, mean))
}

# check if matrix is square, symmetric, and positive definite
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


# fill variance covariance matrices
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

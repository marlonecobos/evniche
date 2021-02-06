#' Helper function to calculate niche volume
#' @param n_dimensions (numeric) number of dimensions to be considered.
#' @param semi_axes_length (numeric) length of ellipsoid axes.
#' @return A numeric value representing the volume of the ellipsoidal niche.
#' @export

ell_volume <- function (n_dimensions, semi_axes_length) {
  # detecting potential errors
  if (missing(n_dimensions)) {
    stop("Argument 'n_dimensions' must be defined")
  }
  if (missing(semi_axes_length)) {
    stop("Argument 'semi_axes_length' must be defined")
  }

  # calculations
  term1 <- 2 * pi^(n_dimensions / 2)
  term2 <- n_dimensions * gamma(n_dimensions / 2)
  term3 <- prod(semi_axes_length)
  term4 <- (term1 / term2) * term3

  return(term4)
}


#' Calculate ellipsoid characteristics (semi-axes, volume, axes coordinates)
#' @param centroid (numeric) vector of values representing the centroid of the
#' data of interest.
#' @param covariance_matrix numeric matrix containing values of variance and
#' covariance derived from the data of interest, that define the shape and limits
#' of the ellipsoid.
#' @param level (numeric) the confidence level of a confidence region
#' for the ellipsoid. Default = 0.95. See details.
#'
#' @return
#' A list containing the data used and the volume, semi-axes length, and axes
#' coordinates for the ellipsoid described by the arguments defined.
#'
#' @export

ell_features <- function(centroid, covariance_matrix, level = 0.95) {
  # detecting potential errors
  if (missing(centroid)) {
    stop("Argument 'centroid' must be defined")
  }
  if (missing(covariance_matrix)) {
    stop("Argument 'covariance_matrix' must be defined")
  }

  # calculating ellipsoid characteristics
  ## eigenvalues and vectors
  ndim <- length(centroid)
  sigma_i <- solve(covariance_matrix) / stats::qchisq(level, df = ndim)
  s_eigen <- eigen(sigma_i)
  s_eigenval <- s_eigen$values
  s_eigenvec <- s_eigen$vectors

  ## semi axes length
  stds <- 1 / sqrt(s_eigenval)
  names(stds) <- letters[1:ndim]

  ## volume
  volume <- ell_volume(ndim, stds)

  ## axes coordinates
  axes_coordinates <- lapply(1:ndim, function(x) {
    coor <- rbind((centroid + s_eigenvec[, x] * stds[x]),
                  (centroid - s_eigenvec[, x] * stds[x]))
    rownames(coor) <- paste0("vec_", 1:2)
    return(coor)
  })
  names(axes_coordinates) <- names(stds)

  # returning results
  return(list(centroid = centroid, covariance_matrix = covariance_matrix,
              level = level, volume = volume, semi_axes = stds,
              axes_coordinates = axes_coordinates))
}


# generate data from virtual niche
virtual_data <- function(ell_features, from = c("ellipsoid", "prediction"),
                         data = NULL, prediction = NULL, n = 100, tol = 1e-8) {
  # detecting potential errors
  if (missing(ell_features)) {
    stop("Argument 'ell_features' must be defined")
  }

  # ellipsoid features
  cent <- ell_features$centroid
  cov_mat <- ell_features$covariance_matrix
  level <- ell_features$level

  # preparing prediction if needed
  if (from == "prediction") {
    if (is.null(data)) {
      stop("Argument 'data' must be defined if 'from' = 'prediction'")
    }
    if (is.null(prediction)) {
      stop("Argument 'prediction' must be defined")
    } else {
      if (!is.null(prediction$suitability_trunc)) {
        clpre <- class(prediction$suitability_trunc)[1]
        if (clpre %in% c("RasterLayer")) {
          data <- raster::rasterToPoints(data)
          suit <- na.omit(prediction$suitability_trunc[])
        } else {
          suit <- prediction$suitability_trunc
        }
      } else {
        stop("Please 'Run calculations' with truncated suitability")
      }
    }
  }

  # calculations
  if (from[1] == "ellipsoid") {
    v_data <- MASS::mvrnorm(n = n, mu = cent, Sigma = cov_mat, tol = tol)

  } else {
    v_data <- data[sample(nrow(data), n, prob = suit), ]
  }

  return(v_data)
}

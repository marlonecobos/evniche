#' Predict mahalanobis and suitability from ellipsoidal virtual niche
#'
#' @param data matrix, data.frame, or SpatRaster containing variables of
#' interest included in ellipsoid \code{features}.
#' @param features list of characteristics that defined the ellipsoid. This
#' list ca be obtained using the function \code{\link{ell_features}}.
#' @param longitude (character) if data is a data.frame or matrix, name of the
#' column with longitudes (if present). Default = NULL.
#' @param latitude (character) if data is a data.frame or matrix, name of the
#' column with latitudes (if present). Default = NULL.
#' @param tol (numeric) the tolerance for detecting linear dependencies.
#' Default = 1e-8.
#' @param include_suitability (logical) whether to include suitability
#' predictions. Default = TRUE.
#' @param include_truncated (logical) whether to include suitability
#' predictions truncated based on ellipsoid limits (level). Default = TRUE.
#'
#' @return
#' A list of predictions in vector or SpatRaster format, based on the features
#' of the ellipsoid niche.
#'
#' @usage
#' ell_predict(data, features, longitude = NULL, latitude = NULL,
#'             tol = 1e-60, include_suitability = TRUE,
#'             include_truncated = TRUE)
#'
#' @importFrom stats mahalanobis
#'
#' @export

ell_predict <- function(data, features, longitude = NULL, latitude = NULL,
                        tol = 1e-60, include_suitability = TRUE,
                        include_truncated = TRUE) {
  # detecting potential errors
  if (missing(data)) {
    stop("Argument 'data' must be defined")
  }
  if (missing(features)) {
    stop("Argument 'features' must be defined")
  }

  # preparing data and ellipsoid features
  cldat <- class(data)[1]
  if (cldat == "SpatRaster") {
    lay <- data[[1]]
    data <- na.omit(data[])
  }

  cent <- features$centroid
  cov_mat <- features$covariance_matrix
  level <- features$level
  v_cols <- !colnames(data) %in% c(longitude, latitude)
  ndim <- ncol(data[, v_cols])

  # mahalanobis distance
  if (cldat == "SpatRaster") {
    maha <- lay
    ma <- stats::mahalanobis(x = data, center = cent, cov = cov_mat, tol = tol)
    maha[!is.na(maha[])] <- ma
    names(maha) <- "mahalanobis"
  } else {
    maha <- stats::mahalanobis(x = data[, v_cols], center = cent, cov = cov_mat,
                               tol = tol)
  }

  # suitability
  if (include_suitability == TRUE) {
    if (cldat == "SpatRaster") {
      suit <- lay
      su <- exp(-0.5 * ma)
      suit[!is.na(suit[])] <- su
      names(suit) <- "suitability"
    } else {
      suit <- exp(-0.5 * maha)
    }

    ## suitability inside only
    if (include_truncated == TRUE) {
      chi_sq <- stats::qchisq(level, ndim)

      if (cldat == "SpatRaster") {
        suit_t <- lay
        suit_t[!is.na(suit_t[])] <- ifelse(ma / chi_sq <= 1, su, 0)
        names(suit_t) <- "suitability_trunc"
      } else {
        suit_t <- ifelse(maha / chi_sq <= 1, suit, 0)
      }
    } else {
      suit_t <- NULL
    }
  } else {
    suit <- NULL
    if (include_truncated == TRUE) {
      warning("Truncation cannot be calculated if suitability is not included")
    }
  }

  return(list(mahalanobis = maha, suitability = suit,
              suitability_trunc = suit_t))
}

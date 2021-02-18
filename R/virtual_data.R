#' Generate new data based on virtual niche
#'
#' @param features list of characteristics that defined the ellipsoid. This
#' list ca be obtained using the function \code{\link{ell_features}}.
#' @param from (character) where to generate or sample data from. Options are
#' "ellipsoid" or "prediction". Default = "ellipsoid" .
#' @param data matrix or data.frame containing values (at least environmental
#' coordinates) used to obtain \code{prediction} based on \code{features}.
#' Needed if \code{from} = "prediction" and the list used in argument
#' \code{prediction} contains results of class numeric. Not required if the list
#' used in argument \code{prediction} contains results of class RasterLayer.
#' @param prediction list of predictions based on \code{features} and
#' \code{data}, obtained using the function \code{\link{ell_predict}}.
#' Needed if \code{from} = "prediction".
#' @param n (numeric) size of data to be generated (number of points).
#' Default = 100.
#' @param tol (numeric) the tolerance for detecting linear dependencies.
#' Default = 1e-8.
#'
#' @return
#'
#' @usage
#' virtual_data(features, from = c("ellipsoid", "prediction"),
#'              data = NULL, prediction = NULL, n = 100, tol = 1e-8)
#'
#' @details
#' Generation of virtual data is done using the function
#' \code{\link[MASS]{mvrnorm}} when \code{from} = "ellipsoid".
#'
#' Virtual data is generated according to suitability values (multivariate
#' normal probabilities) when \code{from} = "prediction". In this case defining
#' the arguments \code{data} and \code{prediction} is mandatory if the elements
#' in \code{prediction} are of class numeric. If elements in \code{prediction}
#' are of class RasterLayer, \code{data} is not required.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats na.omit
#' @importFrom raster rasterToPoints
#'
#' @export

virtual_data <- function(features, from = c("ellipsoid", "prediction"),
                         data = NULL, prediction = NULL, n = 100, tol = 1e-8) {
  # detecting potential errors
  if (missing(features)) {
    stop("Argument 'features' must be defined")
  }

  # ellipsoid features
  cent <- features$centroid
  cov_mat <- features$covariance_matrix
  level <- features$level

  # preparing prediction if needed
  if (from[1] == "prediction") {
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
        stop("Use function 'ell_predict' to obtain truncated suitability")
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

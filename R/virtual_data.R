#' Generate new data based on virtual niche
#'
#' @param features list of characteristics that defined the ellipsoid. This
#' list ca be obtained using the function \code{\link{ell_features}}.
#' @param from (character) where to generate or sample data from. Options are
#' "ellipsoid" or "prediction". Default = "ellipsoid" .
#' @param data matrix or data.frame containing values (at least environmental
#' values) used to obtain \code{prediction} based on \code{features}.
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
#' A matrix or data.frame with the virtual data generated.
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
#' @importFrom terra as.data.frame
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
      stop("Argument 'data' must be defined if argument 'from' = 'prediction'")
    }

    if (is.null(prediction)) {
      stop("Argument 'prediction' must be defined")
    } else {
      if (!is.null(prediction$suitability_trunc)) {
        clpre <- class(prediction$suitability_trunc)[1]
        if (clpre == "SpatRaster") {
          data <- terra::as.data.frame(data, xy = TRUE)
          suit <- terra::as.data.frame(pred_host1$suitability_trunc)[, 1]
        } else {
          suit <- prediction$suitability_trunc
        }

        ## virtual data if prediction
        v_data <- data[sample(nrow(data), n, prob = suit), ]

      } else {
        stop("Use function 'ell_predict' to obtain truncated suitability")
      }
    }
  } else {
    # virtual data from ellipsoid
    v_data <- MASS::mvrnorm(n = n, mu = cent, Sigma = cov_mat, tol = tol)
  }

  return(v_data)
}

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



#' Suggest scaling factors for variables
#'
#' @description
#' Suggests simple scaling factors (powers of 10) to bring the magnitude of
#' variables closer to a target value. This can be useful for improving the
#' numerical stability of algorithms that are sensitive to the scale of input
#' data or for making results easier to interpret.
#'
#' @param range_matrix A matrix or data.frame with two rows (minimum and
#' maximum) and as many columns as variables. Column names should match the
#' variable names to be scaled.
#' @param target_magnitude (numeric) The desired order of magnitude for the
#' variables after scaling. Default = 10.
#'
#' @return A data.frame with three columns: `Variable` (the name of the
#' variable), `Scaling_Value` (the factor to multiply or divide by), and
#' `Operation` (either "multiply", "divide", or "none").
#'
#' @export
#' @examples
#' # Create a range matrix for variables with different scales
#' range_data <- data.frame(var_small = c(0.01, 0.05),
#'                          var_large = c(1000, 5000),
#'                          var_medium = c(5, 12))
#'
#' # Get scaling suggestions
#' scaling_suggestions <- suggest_scaling(range_data)
#' print(scaling_suggestions)
#'
#' # Create a sample dataset
#' my_data <- data.frame(var_small = c(0.02, 0.03, 0.04),
#'                       var_large = c(1500, 3000, 4500),
#'                       var_medium = c(6, 8, 10))
#'
#' # Apply the scaling suggestions
#' scaled_data <- apply_scaling(my_data, scaling_suggestions)
#' print(scaled_data)
suggest_scaling <- function(range_matrix, target_magnitude = 10) {
  vars <- colnames(range_matrix)

  # Calculate the mid-point of each range to determine the general scale
  midpoints <- colMeans(range_matrix)

  # Calculate the power of 10 needed
  # log10(target/mid) gives the exponent. Rounding it gives us the nearest clean power.
  exponents <- round(log10(target_magnitude / midpoints))

  scaling_values <- 10^abs(exponents)
  operations <- ifelse(exponents >= 0, "multiply", "divide")

  # For cases where the scale is already perfect (exponent 0)
  operations[exponents == 0] <- "none"
  scaling_values[exponents == 0] <- 1

  res <- data.frame(
    Variable = vars,
    Scaling_Value = scaling_values,
    Operation = operations,
    stringsAsFactors = FALSE
  )

  return(res)
}



#' Apply suggested scaling factors to data
#'
#' @description
#' Applies the scaling operations suggested by \code{\link{suggest_scaling}}
#' to a dataset.
#'
#' @param data A data.frame or matrix containing the data to be scaled.
#' Column names should match the 'Variable' column in `scaling_df`.
#' @param scaling_df A data.frame produced by \code{\link{suggest_scaling}},
#' containing the instructions on how to scale each variable.
#'
#' @return A new data.frame or matrix with the data scaled according to the
#' provided instructions.
#'
#' @export
#' @examples
#' # For a complete example, please see the documentation for `suggest_scaling`.
#' # ?suggest_scaling
apply_scaling <- function(data, scaling_df) {
  # Work on a copy to avoid overwriting original data
  data_scaled <- data

  for (i in 1:nrow(scaling_df)) {
    var <- scaling_df$Variable[i]
    val <- scaling_df$Scaling_Value[i]
    op  <- scaling_df$Operation[i]

    if (var %in% colnames(data_scaled)) {
      if (op == "multiply") {
        data_scaled[, var] <- data_scaled[, var] * val
      } else if (op == "divide") {
        data_scaled[, var] <- data_scaled[, var] / val
      }
    }
  }
  return(data_scaled)
}

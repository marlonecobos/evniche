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

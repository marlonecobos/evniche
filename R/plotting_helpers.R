#' Plot 2D ellipsoids in multiple panels
#'
#' @description Plots all 2D combinations of variables from niche features
#' or a background dataset.
#'
#' @param features a list of features of the ellipsoid niche obtained with
#' \code{\link{ell_features}}. Default = NULL.
#' @param background matrix or data.frame of background points to be plotted.
#' Default = NULL.
#' @param par_list a list of parameters for plotting.
#' \itemize{
#'   \item \code{mar}: margins of the plot.
#'   \item \code{asp}: aspect ratio.
#'   \item \code{cex}: character expansion factor.
#'   \item \code{col_bg}: background color.
#'   \item \code{col_elem}: color for plot elements (axes, labels).
#'   \item \code{y_las}: orientation of y-axis labels.
#' }
#' @param lp_list a list of parameters for lines and points.
#' \itemize{
#'   \item \code{lty}: line type.
#'   \item \code{lwd}: line width.
#'   \item \code{col_l}: line color.
#'   \item \code{alpha_l}: alpha for line color.
#'   \item \code{pch}: point character.
#'   \item \code{cex_p}: character expansion factor for points.
#'   \item \code{col_p}: point color.
#'   \item \code{alpha_p}: alpha for point color.
#' }
#'
#' @return A 2D plot displaying ellipsoid projections.

plot_2D <- function(features = NULL, background = NULL,
                    par_list = list(mar = c(4.5, 4.5, 1, 1), asp = NA,
                                    cex = 1, col_bg = "black",
                                    col_elem = "white", y_las = 0),
                    lp_list = list(lty = 1, lwd = 1, col_l = "yellow",
                                   alpha_l = 1, pch = 1, cex_p = 1,
                                   col_p = "green", alpha_p = 0.5)) {
  # initial test
  if (is.null(features) & is.null(background)) {
    stop("Either 'features' or 'background' must be defined")
  }

  # preparing data and parameters
  if (!is.null(features)) {
    matvc <- features$covariance_matrix
    cents <- features$centroid
    lev <- features$level

    combb <- combn(colnames(matvc), 2)
  }

  if (!is.null(background) & is.null(features)) {
    combb <- combn(colnames(background), 2)
  }

  # par settings
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  nl <- ncol(combb)
  mfrow <- c(ceiling(nl / ceiling(sqrt(nl))), ceiling(sqrt(nl)))

  par(mfrow = mfrow, bg = par_list$col_bg, cex = par_list$cex,
      mar = par_list$mar, col = par_list$col_elem, col.axis = par_list$col_elem,
      col.lab = par_list$col_elem, fg = par_list$col_elem)

  # plotting in loop
  for (i in 1:nl) {
    ## ellipse
    if (!is.null(features)) {
      ell <- ellipse::ellipse(x = matvc[combb[, i], combb[, i]],
                              centre = cents[combb[, i]], level = lev)
    }

    ## limits of plot
    if (!is.null(background) & !is.null(features)) {
      blims <- apply(rbind(background[, combb[, i]], ell), 2, range)
    } else {
      if (!is.null(background)) {
        blims <- apply(background[, combb[, i]], 2, range)
      } else {
        blims <- apply(ell, 2, range)
      }
    }

    ## plot base
    plot(blims, col = NA, asp = par_list$asp, las = par_list$y_las)

    ## background
    if (!is.null(background)) {
      colp <- scales::alpha(lp_list$col_p, lp_list$alpha_p)
      points(background[, combb[, i]], col = colp, pch = lp_list$pch,
             cex = lp_list$cex_p)
    }

    ## ellipse
    if (!is.null(features)) {
      coll <- scales::alpha(lp_list$col_l, lp_list$alpha_l)
      suppressWarnings(
        lines(ell, col = coll, lty = lp_list$lty, lwd = lp_list$lwd)
      )
    }
  }
}



#' Plot 3D ellipsoids in multiple panels
#'
#' @description Plots all 3D combinations of variables from niche features
#' or a background dataset.
#'
#' @param features a list of features of the ellipsoid niche obtained with
#' \code{\link{ell_features}}. Default = NULL.
#' @param background matrix or data.frame of background points to be plotted.
#' Default = NULL.
#' @param par_list a list of parameters for plotting.
#' \itemize{
#'   \item \code{asp}: aspect ratio.
#'   \item \code{cex}: character expansion factor.
#'   \item \code{col_bg}: background color.
#'   \item \code{col_material}: color for plot elements (axes, labels).
#'   \item \code{box}: logical, whether to draw a box around the plot.
#' }
#' @param lp_list a list of parameters for lines and points.
#' \itemize{
#'   \item \code{type_ell}: type of ellipsoid visualization (e.g., "wire").
#'   \item \code{col_ell}: ellipsoid color.
#'   \item \code{alpha_ell}: alpha for ellipsoid color.
#'   \item \code{size_p}: size of points.
#'   \item \code{col_p}: point color.
#'   \item \code{alpha_p}: alpha for point color.
#' }
#'
#' @return A 3D plot displaying ellipsoid projections.

plot_3D <- function(features = NULL, background = NULL,
                    par_list = list(asp = c(1, 1, 1), cex = 1, col_bg = "black",
                                    col_material = "white", box = FALSE),
                    lp_list = list(type_ell = "wire", col_ell = "yellow",
                                   alpha_ell = 1, size_p = 3, col_p = "green",
                                   alpha_p = 0.5)) {
  # initial test
  if (is.null(features) & is.null(background)) {
    stop("Either 'features' or 'background' must be defined")
  }

  # preparing data and parameters
  if (!is.null(features)) {
    matvc <- features$covariance_matrix
    cents <- features$centroid
    lev <- features$level

    combb <- combn(colnames(matvc), 3)
  }

  if (!is.null(background) & is.null(features)) {
    combb <- combn(colnames(background), 3)
  }

  # par settings
  nl <- ncol(combb)
  mfrow <- c(ceiling(nl / ceiling(sqrt(nl))), ceiling(sqrt(nl)))

  #opar3d <- par3d(no.readonly = TRUE)
  #on.exit(par3d(opar3d))

  rgl::par3d(cex = par_list$cex)
  rgl::mfrow3d(mfrow[1], mfrow[2])

  # plotting in loop
  for (i in 1:nl) {
    labs <- combb[, i]
    ## ellipse
    if (!is.null(features)) {
      ell <- rgl::ellipse3d(x = matvc[combb[, i], combb[, i]],
                            centre = cents[combb[, i]], level = lev)
    }

    ## plot
    ## only background
    if (!is.null(background) & is.null(features)) {
      rgl::plot3d(background[, combb[, i]], col = lp_list$col_p,
                  alpha = lp_list$alpha_p, size = lp_list$size_p,
                  box = par_list$box, axes = FALSE, xlab = "",
                  ylab = "", zlab = "")
    }

    ## only ellipse
    if (!is.null(features) & is.null(background)) {
      suppressWarnings(
        rgl::plot3d(ell, col = lp_list$col_ell, alpha = lp_list$alpha_ell,
                    type = lp_list$type_ell, box = par_list$box,
                    axes = FALSE, xlab = "", ylab = "", zlab = "")
      )
    }

    ## background and ellipse
    if (!is.null(background) & !is.null(features)) {
      rgl::plot3d(background[, combb[, i]], col = lp_list$col_p,
                  alpha = lp_list$alpha_p, size = lp_list$size_p,
                  box = par_list$box, axes = FALSE, xlab = "",
                  ylab = "", zlab = "")

      suppressWarnings(
        rgl::plot3d(ell, col = lp_list$col_ell, alpha = lp_list$alpha_ell,
                    type = lp_list$type_ell, add = T)
      )
    }
    rgl::bg3d(color = par_list$col_bg)
    rgl::axes3d(color = par_list$col_material, box = par_list$box)
    rgl::title3d(xlab = labs[1], ylab = labs[2], zlab = labs[3],
            color = par_list$col_material)
    rgl::aspect3d(par_list$asp)
  }
}



# ---------------
# plotting geographic predictions
#plot_geography <- function(prediction, base) {
#
#}
#
#
#
#sel <- gsub("cov", "", cov_combn)
#
#### plotting
#for (i in 1:length(cov_combn)) {
#  scv <- as.numeric(strsplit(sel[i], "")[[1]])
#  vp <- datauser$varus[scv]
#
#  ellc1 <- ellipse::ellipse(x = matvc[scv, scv], centre = cents[scv],
#                            level = lev)
#
#  if (!is.null(datain$vrasters) & input$backt == "Raster") {
#    blims <- apply(rbind(datain$vrasters[, vp], ellc1), 2, range)
#    plot(blims, col = NA, asp = ppar$asp)
#    points(datain$vrasters[, vp], col = ppar$cp, pch = ppar$pch,
#           cex = ppar$pcx)
#  }
#
#  if (!is.null(datain$vcsvs) & input$backt == "CSV") {
#    blims <- apply(rbind(datain$vcsvs[, vp], ellc1), 2, range)
#    plot(blims, col = NA, asp = ppar$asp)
#    points(datain$vcsvs[, vp], col = ppar$cp, pch = ppar$pch,
#           cex = ppar$pcx)
#  }
#
#  if (!is.null(datain$vrasters) & !is.null(datain$vcsvs)) {
#    blims <- apply(ellc1, 2, range)
#    plot(blims, col = NA, asp = ppar$asp)
#  }
#
#  lines(ellc1, col = ppar$cl, lty = ppar$lty, lwd = ppar$lwd)
#}
#

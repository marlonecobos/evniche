#' Export ellipsoid niche metadata
#'
#' @description Saves ellipsoid niche metadata as RData and a TXT file.
#'
#' @param path (character) path to the directory where the folder will be created.
#' @param folder_name (character) name of the folder to be created to save data.
#' @param parameters a data.frame with initial parameters used to create the niche.
#' @param features a list of features of the ellipsoid niche obtained with
#' \code{\link{ell_features}}.
#' @param overwrite (logical) whether to overwrite the folder if it already exists.
#' Default = FALSE.
#'
#' @return Creates a folder containing a .RData file with parameters and
#' features, and a .txt file with a summary of the metadata.

export_metadata <- function(path, folder_name, parameters, features,
                            overwrite = FALSE) {
  if (missing(path)) {stop("Argument 'path' must be defined")}
  if (missing(folder_name)) {stop("Argument 'folder_name' must be defined")}
  if (missing(parameters)) {stop("Argument 'parameters' must be defined")}
  if (missing(features)) {stop("Argument 'features' must be defined")}

  # checking if overwrite
  pfol <- paste0(path, "/", folder_name)
  if (dir.exists(pfol) & overwrite == FALSE) {
    stop("Folder already exist, change 'folder_name' or use 'overwrite' = TRUE")
  }
  if (dir.exists(pfol) & overwrite == TRUE) {
    unlink(pfol, recursive = TRUE, force = TRUE)
  }

  # preparing file names
  rdname <- paste0(pfol, "/evniche_metadata.RData")
  txname <- paste0(pfol, "/evniche_metadata.txt")


  # saving information
  dir.create(pfol)

  ## RData
  save(parameters, features, file = rdname)

  ## text file
  sink(file = txname, type = "output")
  cat("ELLIPSOID METADATA\n")
  cat("\nInitial Parameters:")
  param <- data.frame(Variables = colnames(parameters),
                      Minimum = parameters[1, ], Maximum = parameters[2, ])
  print(knitr::kable(param, row.names = FALSE))
  cat("\nEllipsoid Features:")
  cat("\n\nLevel:", features$level, sep = "\t")
  cat("\n\nVolume:", features$volume, sep = "\t")
  cat("\n\nCentroid:")
  print(knitr::kable(t(features$centroid)))
  cat("\nCovariance matrix:")
  print(knitr::kable(features$covariance_matrix))
  cat("\nSemi-axes length:")
  print(knitr::kable(t(features$semi_axes)))
  cat("\nAxes coordinates:")
  a_cord <- features$axes_coordinates
  cords <- lapply(1:length(a_cord), function(x) {
    cat("\n ", letters[x], ":", sep = "")
    print(knitr::kable(features$axes_coordinates[[x]]))
  })
  sink()
}


#' Open a graphics device to save 2D plots
#'
#' @param path (character) path to the directory where the plot will be saved.
#' @param name (character) name of the file to be saved.
#' @param format (character) format of the file. Options are "PNG", "JPG", "TIFF".
#' Default = "PNG".
#' @param width (numeric) width of the plotting device. Default = 7.
#' @param height (numeric) height of the plotting device. Default = 7.
#' @param units (character) units for width and height. Default = "in".
#' @param res (numeric) resolution of the image. Default = 300.
#'
#' @return An open graphics device of the specified format.
#' @noRd
explot_device2D <- function(path, name, format = "PNG", width = 7, height = 7,
                            units = "in", res = 300) {
  if (missing(path)) {stop("Argument 'path' must be defined")}
  if (missing(name)) {stop("Argument 'name' must be defined")}

  # opening device depending on format
  if (format == "PNG") {
    png(paste0(path, "/", name, ".png"), width = width, height = height,
        units = units, res = res)
  }
  if (format == "JPG") {
    jpeg(paste0(path, "/", name, ".jpg"), width = width, height = height,
         units = units, res = res)
  }
  if (format == "TIFF") {
    tiff(paste0(path, "/", name, ".tif"), width = width, height = height,
         units = units, res = res)
  }
}

#explot_device3D

#export_new_data

#export_prediction

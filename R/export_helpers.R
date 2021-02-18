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

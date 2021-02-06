#' Run \emph{evniche}
#'
#' @description Run the \emph{evniche} application.
#'
#' @param in_browser (logical) whether to run \code{\link{evniche}} in browser.
#' If TRUE evniche will run in browser (when defined in\code{[base]{options}}).
#' If FALSE, the app will run as defined by default in shiny.
#' @param ... additional arguments to be passed to \code{\link[shiny]{runApp}}.
#'
#' @usage
#' run_evniche(in_browser = TRUE, ...)
#'
#' @examples
#' if (interactive()){
#'   run_evniche()
#' }

run_evniche <- function(in_browser = TRUE, ...) {
  app_path <- system.file("shiny", package = "evniche")

  if (in_browser == TRUE) {
    return(shiny::runApp(app_path, launch.browser = in_browser, ...))
  } else {
    return(shiny::runApp(app_path, ...))
  }
}

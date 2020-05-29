#' @export
pour_app <- function() {
  appDir <- system.file("pour_app", package = "mixed")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `mixed`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

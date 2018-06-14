#' Run the woonuitbreidingsgebieden application
#' @return no return value
#' @importFrom shiny runApp
#' @export
runMacrobenthosApp <- function() {
  appDir <- system.file("ui", package = "macrobenthosapp")

  if (appDir == "") {
    stop("Could not find example directory.
             Try re-installing `macrobenthosapp`.", call. = FALSE)
  }

  runApp(appDir)
}

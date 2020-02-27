

#' start OES shiny gui
#'
#' @author K. Juraic
#' @return
#' @export
#'
#' @examples
#'  \dontrun{run_oes_gui()}
run_oes_gui <- function() {
  appDir <- system.file("shiny_examples", "oes_gui", package = "roes")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `roes`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
  return(TRUE)
}

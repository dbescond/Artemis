#' open Rilostat apps  
#'

#' @author ILO bescond  
#' @keywords ILO
#' @export


runApps <- function(example) {

example <- as.character(substitute(example))
if(example%in%"") {example <- "Rilostat"}
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-apps", package = "Rilostat"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runApps()` with a valid example app as an argument.n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-apps", example, package = "Rilostat")
  shiny::runApp(appDir)
}
#' @import shiny
.onLoad <- function(libname, pkgname) {
  addResourcePath("flamingo", system.file("www", package = "flamingo"))
}

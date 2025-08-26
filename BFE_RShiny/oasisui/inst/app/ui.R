###############################################################################

#' ui
#'
#' @rdname ui
#'
#' @param request ui before authentification.
#'
#' @return ui before authentification.
#'
#' @export

ui <- function(request) {

  fluidPage(
    tags$head(
      #tags$script('window.onbeforeunload = function(event) {return "";};'),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/oasisui-tweaks.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/oasisui-table.css"),
      tags$link(rel = "icon", type = "image/x-icon", href = "img/favicon_oasis.png"),
      bsplus::use_bs_tooltip()
    ),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
       script = system.file("app", "www", "js", "oasisui.js", package = "oasisui"),
       functions = NULL
    )

    title = "OasisUI",

    reactiveConditionalPanelsUI(
      "appUI",
      list(
        loggedout = loginDialogUI("login"),
        loggedin = uiOutput("authUI")
      )
    )

  ) # End of fluidpage
}


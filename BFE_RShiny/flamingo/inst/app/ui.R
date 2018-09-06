# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

ui <- function(request) {

  shinyUI(

    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
        tags$link(rel = "icon", type = "image/x-icon", href = "img/favicon.png")
      ),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(script = system.file("app", "www", "js", "flamingo.js", package = "flamingo")),

      title = "Flamingo",

      reactiveConditionalPanelsUI(
        "appUI",
        list(
          loggedout = loginDialogUI("login"),
          loggedin = uiOutput("authUI")
        )
      )

    ) # End of fluidpage
  ) # End of ShinyUI
}


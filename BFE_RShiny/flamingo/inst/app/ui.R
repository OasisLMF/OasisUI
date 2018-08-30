# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

ui <- function(request) {

  shinyUI(

    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "assets/css/bootstrap.css")
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


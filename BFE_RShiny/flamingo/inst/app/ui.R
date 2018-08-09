# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

ui <- function(request) {

  shinyUI(

    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
      ),
      shinyjs::useShinyjs(),
      title = "Flamingo",

      conditionalPanelsUI(
        "appUI",
        list(
          loggedout = loginDialogUI("login"),
          loggedin = uiOutput("authUI")
        )
      )

    ) # End of fluidpage
  ) # End of ShinyUI
}


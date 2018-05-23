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
          
          shinyjs::hidden(
              div(id = "clientstate", 
                  verbatimTextOutput("id"),
                  verbatimTextOutput("menu")
              )
          ),
          
          conditionalPanel(
              condition = "output['id'] == -1",
              loginDialogUI("login")
          ),
          
          # render actual content if login is completed 
          conditionalPanel(
              condition = "output['id'] != -1",
              uiOutput("authUI")
          )
      ) # End of fluidpage
  ) # End of ShinyUI
}


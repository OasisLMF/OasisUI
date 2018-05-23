
#' @rdname landingPage
#' @importFrom shinyBS bsButton
#' @importFrom DT dataTableOutput
#' @export
landingPageUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
      fluidRow(
          column(10,
              h1("Flamingo 1.0"),
              h4("Oasis Business Front End")
          ),
          column(2,
              br(),
              wellPanel(
                  textOutput(ns("textOutputHeaderData2"))
              ))
      ),
      fluidRow(
          column(2,
              bsButton(ns("abuttonexpmngt"), "Exposure Management",
                  style = "btn btn-primary", size = "default", type = "action",
                  block = TRUE),
              bsButton(ns("abuttonprmngt"), "Process Management",
                  style = "btn btn-primary", size = "default", type = "action",
                  block = TRUE),
              bsButton(ns("abuttonfilemngt"), "File Management",
                  style = "btn btn-primary", size = "default", type = "action",
                  block = TRUE),
              bsButton(ns("abuttonsysconf"), "System Configuration",
                  style = "btn btn-primary", size = "default", type = "action",
                  block = TRUE),
              bsButton(ns("abuttonuseradmin"), "User Administration",
                  style = "btn btn-primary", size = "default", type = "action",
                  block = TRUE),
              bsButton(ns("abuttonlogout"), "Logout",
                  style = "btn btn-primary", size = "default", type = "action",
                  block = TRUE)
          ),
          column(10,
              
              wellPanel(
                  h4("Process Runs Inbox"),
                  dataTableOutput(ns("tableInbox")),
                  actionButton(ns("abuttongotorun"), "Goto Run Details",
                      class = "btn btn-primary", align = "right"),
                  actionButton(ns("refreshInbox"), "Refresh",
                      class = "btn btn-primary", align = "right"),
                  downloadButton(ns("PRIdownloadexcel"),
                      label="Export to Excel")
              )),
          
          column(12, align = 'right',
              em("Powered by RShiny", style = "color:gray; font-size:10pt"))
      )
  )
  
}

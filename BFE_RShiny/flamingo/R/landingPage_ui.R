#' @rdname landingPage
#' @importFrom DT dataTableOutput
#' @export
landingPageUI<- function(id) {
  
  ns <- NS(id)
  
  tagList(
             wellPanel(
                 h4("Process Runs Inbox"),
                 dataTableOutput(ns("tableInbox")),
                 actionButton(ns("abuttongotorun"), "Goto Run Details",
                     class = "btn btn-primary", align = "right"),
                 actionButton(ns("refreshInbox"), "Refresh",
                     class = "btn btn-primary", align = "right"),
                 downloadButton(ns("PRIdownloadexcel"),
                     label="Export to csv")
             )
             # img(src = "landingpage.png", width = "70%") # to be replaced with proper image
  )
}

#' @rdname pagestructureUI
#' @importFrom shinyBS bsButton
#' @importFrom shinyWidgets panel
#' @export
pageheaderUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    dropdownButton(label = "accountDD",
                   circle = TRUE, status = "default",
                   icon = icon("user"),
                   size = "s",
                   right = TRUE,
                   textOutput(ns("textOutputHeaderData2")),
                   bsButton(ns("abuttonuseradmin"), "User Administration",
                            style = "btn btn-primary", size = "default", type = "action",
                            block = TRUE),
                   bsButton(ns("abuttonlogout"), "Logout",
                            style = "btn btn-primary", size = "default", type = "action",
                            block = TRUE)
    )
  )
}



#' @rdname pagestructureUI
#' @importFrom shinyBS bsButton
#' @importFrom shinyWidgets actionBttn
#' @export
pagestructureUI <- function(id) {

  ns <- NS(id)

  tagList(
    uiOutput(ns("sidebar"))

   )

}

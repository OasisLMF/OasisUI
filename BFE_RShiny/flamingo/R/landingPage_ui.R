#' @rdname landingPage
#' @importFrom DT dataTableOutput
#' @importFrom shinyBS bsTooltip
#' @export
landingPageUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      h4("Process Runs Inbox"),
      dataTableOutput(ns("tableInbox")),
      actionButton(ns("abuttongotorun"), "Goto Run Details",
                   class = "btn btn-primary", align = "right"),
      bsTooltip(ns("abuttongotorun"), landing_page$abuttongotorun,
                placement = "right", options = list(container = "body")),
      actionButton(ns("refreshInbox"), "Refresh",
                   class = "btn btn-primary", align = "right"),
      downloadButton(ns("PRIdownloadexcel"),
                     label = "Export to csv"),
      bsTooltip(ns("PRIdownloadexcel"), landing_page$PRIdownloadexcel,
                placement = "right", options = list(container = "body"))
    )
    # img(src = "landingpage.png", width = "70%") # to be replaced with proper image
  )
}

#' @rdname pagestructure
#' @importFrom shinyBS bsButton
#' @importFrom shinyWidgets dropdownButton
#' @export
pageheaderUI <- function(id) {
  
  ns <- NS(id)
  attachDependencies(value = flamingoHtmlDependencies(), 
                     tagList(
                       div( id = ns("accountDDmenu"),
                            dropdownButton(inputId = ns("accountDD"),
                                           circle = TRUE, status = "default",
                                           icon = icon("user"),
                                           size = "s",
                                           right = TRUE,
                                           textOutput(ns("textOutputHeaderData2")),
                                           bsButton(ns("abuttonuseradmin"), "User Administration",
                                                    style = "btn btn-primary", size = "default", type = "action",
                                                    block = TRUE),
                                           bsButton(ns("abuttondefineaccount"), "Define Account",
                                                    style = "btn btn-primary", size = "default", type = "action",
                                                    block = TRUE),
                                           bsTooltip(ns("abuttondefineaccount"),
                                                     landing_page$abuttondefineaccount,
                                                     placement = "left",
                                                     options   = list(container = "body")),
                                           bsButton(ns("abuttonsysconf"), "System Configuration",
                                                    style = "btn btn-primary", size = "default", type = "action",
                                                    block = TRUE),
                                           bsTooltip(ns("abuttonsysconf"), 
                                                     landing_page$abuttonsysconf, 
                                                     placement = "left", 
                                                     options   = list(container = "body")),
                                           bsButton(ns("abuttonlogout"), "Logout",
                                                    style = "btn btn-primary", size = "default", type = "action",
                                                    block = TRUE)
                            )
                       )
                     )
  )
}



#' @rdname pagestructure
#' @export
pagestructureUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    actionButton(inputId = ns("abuttoncollapsesidebar"), icon = icon("ellipsis-v"), label = NULL),
    uiOutput(ns("sidebar"))
  )
  
}

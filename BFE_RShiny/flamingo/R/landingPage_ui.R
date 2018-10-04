#' @rdname landingPage
#' @importFrom DT DTOutput
#' @importFrom shinyBS bsTooltip
#' @export
landingPageUI <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      h4("Process Runs Inbox"),
      DTOutput(ns("tableInbox")),
      actionButton(ns("abuttongotorun"), "Browse Processes Outputs",
                   class = "btn btn-primary", align = "right"),
      bsTooltip(ns("abuttongotorun"), landing_page$abuttongotorun,
                placement = "right", options = list(container = "body")),
      actionButton(inputId = ns("refreshInbox"), label = "Refresh", align = "right"),
      downloadButton(ns("PRIdownloadexcel"),
                     label = "Export to csv"),
      bsTooltip(ns("PRIdownloadexcel"), landing_page$PRIdownloadexcel,
                placement = "right", options = list(container = "body"))
    )
    # img(src = "landingpage.png", width = "70%") # to be replaced with proper image
  )
}

#' @rdname pagestructure
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets dropdownButton
#' @export
pageheaderUI <- function(id) {
  ns <- NS(id)
  tagList(
    div( id = ns("accountDDmenu"),
         dropdownButton(inputId = ns("accountDD"),
                        circle = TRUE, status = "default",
                        icon = icon("user"),
                        size = "s",
                        right = TRUE,
                        textOutput(ns("textOutputHeaderData2")),
                        actionButton(ns("abuttonuseradmin"), class = "btn btn-primary",
                                     label = "User Administration", align = "center", width = "100%"),

                        actionButton(ns("abuttondefineaccount"), class = "btn btn-primary",
                                     label = "Define Account", align = "center", width = "100%"),
                        bsTooltip(ns("abuttondefineaccount"),
                                  landing_page$abuttondefineaccount,
                                  placement = "left",
                                  options   = list(container = "body")),

                        actionButton(ns("abuttonsysconf"), class = "btn btn-primary",
                                     label = "System Configuration", align = "center", width = "100%"),
                        bsTooltip(ns("abuttonsysconf"),
                                  landing_page$abuttonsysconf,
                                  placement = "left",
                                  options   = list(container = "body")),

			actionButton(ns("abuttonlogout"), class = "btn btn-primary",
                                     label = "Logout", align = "center", width = "100%")
         )
    )
  )
}



#' @rdname pagestructure
#' @export
pagestructureUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("sidebar"))
  )
}

#' @rdname landingPage
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#' @export
landingPageUI <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      h4("Process Runs Inbox"),
      DTOutput(ns("tableInbox")),
      actionButton(ns("abuttongotorun"), "Browse Processes Outputs",
                   class = "btn btn-primary", align = "right") %>%
        bs_embed_tooltip(title = landing_page$abuttongotorun, placement = "right"),
      actionButton(inputId = ns("refreshInbox"), label = "Refresh", align = "right"),
      downloadButton(ns("PRIdownloadexcel"),
                     label = "Export to csv") %>%
        bs_embed_tooltip(title = landing_page$PRIdownloadexcel, placement = "right")
    )
    # img(src = "landingpage.png", width = "70%") # to be replaced with proper image
  )
}

#' @rdname pagestructure
#' @importFrom bsplus bs_embed_tooltip
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
                                     label = "Define Account", align = "center", width = "100%") %>%
                          bs_embed_tooltip(title = landing_page$abuttondefineaccount, placement = "right"),

                        actionButton(ns("abuttonsysconf"), class = "btn btn-primary",
                                     label = "System Configuration", align = "center", width = "100%") %>%
                          bs_embed_tooltip(title = landing_page$abuttonsysconf, placement = "right"),

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

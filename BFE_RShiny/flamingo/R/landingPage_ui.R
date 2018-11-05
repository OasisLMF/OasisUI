#' landingPage
#'
#' @rdname landingPage
#' 
#' @template params-module-ui
#'
#'  @return List of tags.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
landingPageUI <- function(id) {
  ns <- NS(id)

  tagList(
    wellPanel(
      h4("Process Runs Inbox"),
      DTOutput(ns("tableInbox")),
      flamingoButton(ns("abuttongotorun"), "Browse Processes Outputs",
                   align = "right") %>%
        bs_embed_tooltip(title = landing_page$abuttongotorun, placement = "right"),
      actionButton(inputId = ns("refreshInbox"), label = "Refresh", align = "right"),
      downloadButton(ns("PRIdownloadexcel"),
                     label = "Export to csv") %>%
        bs_embed_tooltip(title = landing_page$PRIdownloadexcel, placement = "right")
    )
    # img(src = "landingpage.png", width = "70%") # to be replaced with proper image
  )
}

#' pageheaderUI
#'
#' @rdname pageheader
#' 
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom shinyWidgets dropdownButton
#'
#' @export
pageheaderUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("accountDDmenu"),
         dropdownButton(inputId = ns("accountDD"),
                        circle = TRUE, status = "default",
                        icon = icon("user"),
                        size = "s",
                        right = TRUE,
                        textOutput(ns("textOutputHeaderData2")),
                        flamingoButton(ns("abuttonuseradmin"),
                                     label = "User Administration", align = "center", width = "100%"),

                        ### TODO: recheck allignment of the comment after fixing dtyle of action buttons
                        flamingoButton(ns("abuttondefineaccount"),
                                     label = "Define Account", align = "center", width = "100%") %>%
                          bs_embed_tooltip(title = landing_page$abuttondefineaccount, placement = "left"),

                        flamingoButton(ns("abuttonsysconf"),
                                     label = "System Configuration", align = "center", width = "100%") %>%
                          bs_embed_tooltip(title = landing_page$abuttonsysconf, placement = "left"),

                        flamingoButton(ns("abuttonlogout"),
                                     label = "Logout", align = "center", width = "100%")
         ) %>%
           bs_embed_tooltip(title = landing_page$accountDD, placement = "right")
    )
  )
}



#' pagestructureUI
#'
#' @rdname pagestructure
#'
#' @template params-module-ui
#'
#' @return Lits of tags
#'
#' @export
pagestructureUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("sidebar"))
  )
}

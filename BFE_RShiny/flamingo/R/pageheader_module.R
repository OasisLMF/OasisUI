# Page Header Module -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' pageheaderUI
#'
#' @rdname pageheader
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

                       # flamingoButton(ns("abuttonuseradmin"),
                       #                label = "User Administration", align = "center", width = "100%"),
                       # flamingoButton(ns("abuttondefineaccount"),
                       #                label = "Define Account", align = "center", width = "100%") %>%
                       #   bs_embed_tooltip(title = landing_page$abuttondefineaccount, placement = "left"),
                       #
                       # flamingoButton(ns("abuttonsysconf"),
                       #                label = "System Configuration", align = "center", width = "100%") %>%
                       #   bs_embed_tooltip(title = landing_page$abuttonsysconf, placement = "left"),

                       flamingoButton(ns("abuttonlogout"),
                                      label = "Logout", align = "center", width = "100%")  %>%
                         bs_embed_tooltip(title = landing_page$abuttonlogout, placement = "right")
        ) %>%
          bs_embed_tooltip(title = landing_page$accountDD, placement = "right")
    )
  )
}
# Server -----------------------------------------------------------------------
#' pageheader
#'
#' @rdname pageheader
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-logMessage
#' @template params-active
#'
#' @param user reactive expression yielding user name
#'
#' @return For \code{pageheader()}, list of reactives:
#' \itemize{
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' }.
#'
#' @importFrom shinyWidgets toggleDropdownButton
#'
#' @export
pageheader <- function(input, output, session, user, logMessage = message, active = reactive(TRUE)) {

  ns <- session$ns

  navigation_state <- reactiveNavigation()

  # Greeter ---------------------------------------------------
  output$textOutputHeaderData2 <- renderText(paste("User Name:", user()))

  observeEvent(input$abuttonuseradmin, {
    updateNavigation(navigation_state, "UA")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  observeEvent(input$abuttondefineaccount, {
    updateNavigation(navigation_state, "DA")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  observeEvent(input$abuttonsysconf, {
    updateNavigation(navigation_state, "SC")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  observeEvent(input$abuttonhome, {
    updateNavigation(navigation_state, "LP")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  .logoutModal <- function() {
    ns <- session$ns
    modalDialog(label = "modaldialoguelogout",
                title = "Important message",
                "Are you sure that you want to log out?",
                footer = tagList(
                  flamingoButton(inputId = ns("abuttonlogoutcontinue"), label = "Continue Logout"),
                  modalButton(label = "Dismiss")
                ),
                size = "s",
                easyClose = TRUE)
  }

  observeEvent(input$abuttonlogout,
               showModal(.logoutModal())
  )

  observeEvent(input$abuttonlogoutcontinue,
               removeModal()
  )

  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      logout = reactive(input$abuttonlogoutcontinue)
    )
  )

  moduleOutput
}

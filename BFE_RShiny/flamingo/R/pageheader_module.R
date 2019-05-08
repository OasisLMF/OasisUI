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
pageheader <- function(input, output, session, user, active = reactive(TRUE)) {

  ns <- session$ns

  navigation_state <- reactiveNavigation()

  # Greeter ---------------------------------------------------
  output$textOutputHeaderData2 <- renderText(paste("User Name:", user()))

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

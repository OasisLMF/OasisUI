# Page Header Module -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
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
                                      label = "Logout", align = "center", width = "100%")
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
#' @template params-flamingo-module
#' 
#' @param reloadMillis Amount of time to wait between table updates;
#' see \link{invalidateLater}.
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
pageheader <- function(input, output, session, user,
                       reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {
  
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
                "Are you sure you want to log out?",
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
  
  ### Button permissions ---- --------------------------------------------------
  # observe(if (active()) {
  #   landingPageButtonUpdate(session, dbSettings, user())
  # })
  
  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      logout = reactive(input$abuttonlogoutcontinue)
    )
  )
  
  moduleOutput
}


#' Landing Page Access Control
#'
#' @rdname landingPageButtonUpdate
#'
#' @description Disable/Enable menu buttons based on permissions in database.
#'
#' @template params-module
#' @template params-flamingo-module
#' 
#' @return NULL
#'
#' @export
landingPageButtonUpdate <- function(session, dbSettings,
                                    logMessage = message) {
  
  logMessage("Checking Permissions")
  
  # TODO: use shinyjs enable / disable on actionButtons according to permissions
  
  # .updateButton <- function(db_resourceId, btn_inputId) {
  #   permission <- flamingoDBCheckPermissions(dbSettings, user, db_resourceId)
  #   if (identical(permission, character(0))) {
  #     updateButton(session, session$ns(btn_inputId), disabled = TRUE)
  #   } else {
  #     updateButton(session, session$ns(btn_inputId), disabled = TRUE)
  #   }
  # }
  
  # Not used anywhere else, probably just forgotten
  # ("600", "abuttonenquiry")
  
  #.updateButton("700", "abuttonanalysis")
  #.updateButton("700", "abuttonbrowse")
  
  # .updateButton("904", "abuttonuseradmin")
  
  # Not used anywhere else, probably just forgotten
  # ("950", "abuttonworkflowadmin")
  
  # .updateButton("200", "abuttonsysconf")
  
  #.updateButton("300", "abuttonfilemngt")
  
  invisible()
}

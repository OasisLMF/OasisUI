# Page Structure Module --------------------------------------------------------

# UI ---------------------------------------------------------------------------
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

# Server -----------------------------------------------------------------------
#' Page Structure
#'
#' @rdname pagestructure
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param reloadMillis Amount of time to wait between table updates;
#' see \link{invalidateLater}.
#' 
#' @return collapsed status of panel.
#'
#' @importFrom shinyWidgets toggleDropdownButton
#' 
#' @export
pagestructure <- function(input, output, session, dbSettings,
                          reloadMillis = 10000, logMessage = message,
                          active = reactive(TRUE)) {
  
  ns <- session$ns
  
  navigation_state <- reactiveNavigation()
  
  state <- reactiveValues(
    collapsed = FALSE
  )
  
  observeEvent(input$abuttoncollapsesidebar, {
    state$collapsed <- !state$collapsed
  })
  
  observe({
    output$sidebar <-
      renderUI(pagestructureSidebar(ns, state$collapsed))
  })
  
  
  ### Navigation Menu ----------------------------------------------------------
  
  observeEvent(input$abuttondefineanasingle, {
    updateNavigation(navigation_state, "SA")
    toggleDropdownButton(ns("abuttonanalysis"))
  })
  
  observeEvent(input$abuttondefineanabatch, {
    updateNavigation(navigation_state, "BA")
    toggleDropdownButton(ns("abuttonanalysis"))
  })
  
  observeEvent(input$abuttonbrowseSBR, {
    updateNavigation(navigation_state, "SBR")
    toggleDropdownButton(ns("abuttonbrowse"))
  })
  
  observeEvent(input$abuttonbrowseBBR, {
    updateNavigation(navigation_state, "BBR")
    toggleDropdownButton(ns("abuttonbrowse"))
  })
  
  observeEvent(input$abuttonbrowseCBR, {
    updateNavigation(navigation_state, "CBR")
    toggleDropdownButton(ns("abuttonbrowse"))
  })
  
  
  observeEvent(input$abuttonhome, {
    updateNavigation(navigation_state, "LP")
  })
  
  observeEvent(input$abuttonfilemngt, {
    updateNavigation(navigation_state, "FM")
  })
  
  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      collapsed = reactive(state$collapsed)
    ) # placeholder
  )
  
  moduleOutput
  
}
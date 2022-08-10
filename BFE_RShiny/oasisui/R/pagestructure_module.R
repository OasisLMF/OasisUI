# Page Structure Module --------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' pagestructureUI
#'
#' @rdname pagestructure
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
#' @template params-active
#'
#' @return collapsed status of panel.
#'
#' @importFrom shinyWidgets toggleDropdownButton
#'
#' @export
pagestructure <- function(input, output, session,
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
    output$sidebar <- renderUI(pagestructureSidebar(ns, state$collapsed))
  })


  ### Navigation Menu ----------------------------------------------------------

  observeEvent(input$abuttondefineanasingle, ignoreInit = TRUE, {
    updateNavigation(navigation_state, "SA")
    # RSc: check this?! somehow not working properly anymore.
    toggleDropdownButton(ns("abuttonanalysis"), session = session)
    # input$abuttonanalysis_state
    # session$sendInputMessage(paste0(inputId, "_state"), list(id = inputId))
    # session$sendInputMessage(paste0(ns("abuttonanalysis"), "_state"), list(id = ns("abuttonanalysis")))
    # session$sendInputMessage(ns("abuttonanalysis"), list("aria-expanded" = "false"))
  })

  observeEvent(input$abuttondefineanabatch, ignoreInit = TRUE, {
    updateNavigation(navigation_state, "BA")
    toggleDropdownButton(ns("abuttonanalysis"), session = session)
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

  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      collapsed = reactive(state$collapsed)
    ) # placeholder
  )

  moduleOutput

}

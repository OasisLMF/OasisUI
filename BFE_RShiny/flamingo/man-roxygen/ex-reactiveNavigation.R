if (interactive()) {
  library(shiny)
  nav_states <- LETTERS[1:8]
  prefixlog <- function(prefix, logger = message) {
    function(...) cat(prefix, ">", ..., "\n", sep = "")
  }
  # navigation button module ----
  navigationButtonUI <- function(id, nav_to) {
    ns <- NS(id)
    shiny::actionButton(
      ns("action"),
      label = sprintf("Navigate to %s", nav_to)
    )
  }
  navigationButton <- function(input, output, session, nav_to) {
    navigation_state <- reactiveNavigation()
    observeEvent(input$action, {
      cat("Navigate to", nav_to, "\n")
      updateNavigation(navigation_state, nav_to)
    })
    outputNavigation(navigation_state)
  }
  # sidebar module, a collection of navigationButton modules ----
  navigationSidebarUI <- function(id) {
    ns <- NS(id)
    tagList(lapply(
      nav_states,
      function(nav_to) {
        fluidRow(navigationButtonUI(ns(sprintf("action-%s", nav_to)), nav_to))
      }
    ))
  }
  navigationSidebar <- function(input, output, session) {
    modules <- lapply(
      nav_states, function(nav_to) {
        callModule(navigationButton, sprintf("action-%s", nav_to), nav_to)
      }
    )
    # observe and return navigation from the inner modules
    navigation_state <- reactiveNavigation()
    observeModuleNavigation(
      navigation_state, modules,
      logger = prefixlog("> SIDEBAR")
    )
    outputNavigation(navigation_state)
  }
  # main panel module ----
  mainPanelModuleUI <- function(id) {
    ns <- NS(id)
    fluidPage(
      selectInput(ns("main_slider"), "Navigate to:", nav_states)
    )
  }
  mainPanelModule <- function(input, output, session) {
    # observe and return navigation
    navigation_state <- reactiveNavigation()
    observeEvent(input$main_slider, {
      cat("Navigate to", input$main_slider, "\n")
      updateNavigation(navigation_state, input$main_slider)
    })
    outputNavigation(navigation_state)
  }
  # actual app ----
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Modular reactive navigation"),
    fluidPage(
      div("App navigation state"),
      verbatimTextOutput("nav_display", placeholder = TRUE)
    ),
    sidebarLayout(
      sidebarPanel(navigationSidebarUI("sidebar")),
      mainPanel(mainPanelModuleUI("main"))
    )
  )
  server <- function(input, output) {
    navigation_state <- reactiveNavigation(head(nav_states, 1))
    # reactive app-level navigation state, force_react = FALSE allows reacting
    # only to actual changes
    nav_to <- getNavigation(outputNavigation(navigation_state, force_react = FALSE))
    output$nav_display <- nav_to
    observe(prefixlog("> APP_NAV")(" => " , nav_to()))
    # call modules abd observe their navigation
    modules <- list(
      callModule(navigationSidebar, "sidebar"),
      callModule(mainPanelModule, "main")
    )
    observeModuleNavigation(navigation_state, modules, logger = prefixlog("> SERVER"))
  }
  shinyApp(ui = ui, server = server)
}

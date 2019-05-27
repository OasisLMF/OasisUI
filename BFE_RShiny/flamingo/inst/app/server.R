###############################################################################

# UI content that is rendered once the user has authenticated
source(file.path(".", "ui_auth.R"), local = TRUE)$value

#' server
#'
#' @rdname server
#'
#' @template params-module
#'
#' @return Server.
#' @export

server <- function(input, output, session) {

  #clean up folder upon login
  clean_downloadedData()

  #set api
  session$userData$api_hub <- APIHub$new(host = APISettings$server, port = APISettings$port)


  # active main panel based on the reactive navigation state
  navigation_state <- reactiveNavigation("LP")
  main_visible <- getNavigation(outputNavigation(
    # force_react = FALSE allows reacting only to actual changes
    navigation_state, force_react = FALSE
  ))

  result <- reactiveValues(
    user = FLAMINGO_GUEST_ID,
    WidthMain = 9,
    WidthSide = 3,
    preselPanel = 1
  )

  authenticated <- reactive({
    # reactive expression yielding TRUE when:
    # - user has been matched to an id
    # - ui has been rendered (input values are available)
    auth <- (result$user != FLAMINGO_GUEST_ID) &&
      !is.null(input$authUIRenderCallback)

    if (auth) logMessage("Authenticated")

    return(auth)
  }) %>% debounce(100)

  # submodules ----

  loginDialogModule <- callModule(
    loginDialog, "login",
    logout = reactive(auth_modules$pageheader$logout())
  )

  observe({
    result$user <- loginDialogModule$user()
  })

  # list of modules for the authenticated UI
  auth_modules <- list()

  auth_modules$pageheader <- callModule(
    pageheader, "pageheader",
    user = reactive(result$user),
    active = reactive(authenticated())
  )

  auth_modules$pagestructure <- callModule(
    pagestructure, "pagestructure",
    active = reactive(authenticated())
  )

  auth_modules$landingPage <- callModule(
    landingPage, "landingPage",
    active = reactive(authenticated() && main_visible() == "LP")
  )

  auth_modules$singleAna <- callModule(
    singleAna,
    id = "singleAna",
    preselPanel = reactive(result$preselPanel),
    selectAnaID = auth_modules$visualizationSBR$selectAnaID,
    selectPortfolioID = auth_modules$visualizationSBR$selectPortfolioID,
    active = reactive(authenticated() && main_visible() == "SA")
  )

  auth_modules$batchAna <- callModule(
    batchAna,
    id = "batchAna",
    active = reactive(authenticated() && main_visible() == "BA")
  )

  auth_modules$visualizationSBR <- callModule(
    visualizationSBR,
    id = "visualizationSBR",
    preselAnaId = auth_modules$landingPage$anaID,
    anaID = auth_modules$singleAna$anaID,
    active = reactive(authenticated() && main_visible() == "SBR")
  )

  auth_modules$visualizationBBR <- callModule(
    visualizationBBR,
    id = "visualizationBBR",
    preselAnaId = reactive(NULL),
    anaID = reactive(NULL),
    active = reactive(authenticated() && main_visible() == "BBR")
  )

  auth_modules$visualizationCBR <- callModule(
    visualizationCBR,
    id = "visualizationCBR",
    preselAnaId = reactive(NULL),
    anaID =  reactive(NULL),
    active = reactive(authenticated() && main_visible() == "CBR")
  )

  # authenticated ----
  # show the logged-in part of the UI if login is completed
  appState <- reactive(
    # cannot use authenticated() here and below in renderUI(... authUI(...))!
    #if (authenticated()) "loggedin" else "loggedout"
    if (result$user != FLAMINGO_GUEST_ID) "loggedin" else "loggedout"
  )
  callModule(reactiveConditionalPanels, "appUI", appState)

  ### Module input parameters depending on othe rmodules outputs ---

  # UI non-reactive to (result$WidthSide) and (result$Widthmain)
  output$authUI <- renderUI(
    #if (authenticated()) {
    if (result$user != FLAMINGO_GUEST_ID) {
      authUI(
        WidthSide = isolate(result$WidthSide),
        WidthMain = isolate(result$WidthMain)
      )
    }
  )
  # reactivity via dynamicColumn module
  callModule(dynamicColumn, "sidebar", reactive(result$WidthSide))
  callModule(dynamicColumn, "main", reactive(result$WidthMain))

  observe(result$logout <- auth_modules$pageheader$logout())

  # preselected panel
  observe({
    if (!is.null(auth_modules$visualizationSBR$preselPanel)) {
      result$preselPanel <- auth_modules$visualizationSBR$preselPanel()
    } else {
      result$preselPanel <- 1
    }
  })

  # collapse / expand sidebar ----
  observe({
    if (auth_modules$pagestructure$collapsed()) {
      result$WidthMain <- 11
      result$WidthSide <- 1
    } else {
      result$WidthMain <- 9
      result$WidthSide <- 3
    }
  })

  ### navigation ----
  # observe the possible navigation state propagated from any module
  observeModuleNavigation(navigation_state, auth_modules, logger = NULL)

  # activate the main panel the user navigates to
  callModule(reactiveConditionalPanels, "mainPanel", main_visible)


  # TODO: Consider simplifying the logging of the navigation using the `logger`
  # argument of `observeModuleNavigation()` above.
  observe(if (authenticated()) {
    switch(main_visible(),

           "SA" = { # go to Define programme single submenu
             loginfo(paste("Navigate to Define Single Analysis"),
                     logger = "flamingo.module")
           },

           "BA" = { # go to Define programme batch submenu
             loginfo(paste("Navigate to Define Batch Analysis"),
                     logger = "flamingo.module")
           },

           "SBR" = { # go to Single browse submenu
             loginfo(paste("Navigate to Single browse"),
                     logger = "flamingo.module")
           },

           "BBR" = { # go to Batch browse submenu
             loginfo(paste("Navigate to Batch browse"),
                     logger = "flamingo.module")
           },

           "CBR" = { # go to Compare Runs submenu
             loginfo(paste("Navigate to compare runs"),
                     logger = "flamingo.module")
           },

           "LP" = { # go to Landing Page
             loginfo(paste("Navigate to Landing Page"),
                     logger = "flamingo.module")
           },

           stop("page does not exist")
    )
  })
}

# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

# UI content that is rendered once the user has authenticated
source(file.path(".", "ui_auth.R"), local = TRUE)$value


server <- function(input, output, session) {

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
  .callModule <- function(...) {
    callModule(..., dbSettings = dbSettings)
  }

  loginDialogModule <- .callModule(
    loginDialog, "login",
    logMessage = logMessage,
    logout = reactive(auth_modules$pageheader$logout())
  )

  observe({
    result$user <- loginDialogModule$user()
  })

  # list of modules for the authenticated UI
  auth_modules <- list()

  auth_modules$pageheader <- .callModule(
    pageheader, "pageheader",
    user = reactive(result$user),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated())
  )

  auth_modules$pagestructure <- .callModule(
    pagestructure, "pagestructure",
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated())
  )

  auth_modules$landingPage <- .callModule(
    landingPage, "landingPage",
    user = reactive(result$user),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated() && main_visible() == "LP")
  )

  auth_modules$DA <- .callModule(
    accountDefinition,
    id = "accountDefinition",
    active = reactive(authenticated() && main_visible() == "DA")
  )

  auth_modules$programmeDefinitionSingle <- .callModule(
    programmeDefinitionSingle,
    id = "programmeDefinitionSingle",
    apiSettings = apiSettings,
    user = reactive(result$user),
    preselRunId =  auth_modules$landingPage$runId,
    preselProcId =  auth_modules$landingPage$procId,
    preselPanel = reactive(result$preselPanel),
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && main_visible() == "PS")
  )

  auth_modules$programmeDefinitionBatch <- .callModule(
    programmeDefinitionBatch,
    id = "programmeDefinitionBatch",
    apiSettings = apiSettings,
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && main_visible() == "PB")
  )

  auth_modules$visualizationSBR <- .callModule(
    visualizationSBR,
    id = "visualizationSBR",
    apiSettings = apiSettings,
    user = reactive(result$user),
    preselRunId =  auth_modules$landingPage$runId,
    processRunId = auth_modules$programmeDefinitionSingle$processRunId,
    logMessage = logMessage,
    active = reactive(authenticated() && main_visible() == "SBR")
  )

  auth_modules$visualizationBBR <- .callModule(
    visualizationBBR,
    id = "visualizationBBR",
    apiSettings = apiSettings,
    user = reactive(result$user),
    preselRunId = reactive(-1),
    processRunId = reactive(-1),
    logMessage = logMessage,
    active = reactive(authenticated() && main_visible() == "BBR")
  )

  auth_modules$visualizationCBR <- .callModule(
    visualizationCBR,
    id = "visualizationCBR",
    apiSettings = apiSettings,
    user = reactive(result$user),
    preselRunId = auth_modules$landingPage$runId,
    processRunId =  auth_modules$programmeDefinitionSingle$processRunId,
    logMessage = logMessage,
    active = reactive(authenticated() && main_visible() == "CBR")
  )

  # preselected panel
  observe({
    if (!is.null(auth_modules$visualizationSBR$preselPanel)) {
      result$preselPanel <- auth_modules$visualizationSBR$preselPanel()
    } else {
      result$preselPanel <- 1
    }
  })

  auth_modules$fileViewer <- .callModule(
    fileViewer,
    id = "fileViewer",
    logMessage = logMessage,
    active = reactive(authenticated()) # && main_visible() == "FM" ) #&& input$fm == "fileviewer"))
  )

  auth_modules$modelSupplierPage <- .callModule(
    modelSupplierPage,
    id = "modelSupplierPage",
    active = reactive(authenticated()) # && main_visible() == "SC" )# && input$sc == "Model"))
  )

  auth_modules$userAdminDefinition <- .callModule(
    userAdminDefinition,
    id = "userAdminDefinition",
    active = reactive(authenticated() && main_visible() == "UA" && input$ua == "defineuser"),
    user = reactive(result$user)
  )

  auth_modules$companyDefinition <- .callModule(
    companyDefinition,
    id = "companyDefinition",
    active = reactive(authenticated() && main_visible() == "UA" && input$ua == "definecompany")
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

           "DA" = { # go to Define account submenu
             loginfo(paste("Navigate to Define Account"),
                     logger = "flamingo.module")
           },

           "PS" = { # go to Define programme single submenu
             loginfo(paste("Navigate to Define Process Single"),
                     logger = "flamingo.module")
           },

           "PB" = { # go to Define programme batch submenu
             loginfo(paste("Navigate to Define Process Batch"),
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

           "UA" = { # go to User Admin submenu
             loginfo(paste("Navigate to User Admin"),
                     logger = "flamingo.module")
             updateTabsetPanel(session, "ua", selected = "defineuser")
           },

           "FM" = { # go to File Management submenu
             loginfo(paste("Navigate to File Management"),
                     logger = "flamingo.module")
           },

           "SC" = { # go to System Configuration submenu
             loginfo(paste("Navigate to System Config"),
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

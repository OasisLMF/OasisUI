# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

# UI content that is rendered once the user has authenticated
source(file.path(".", "ui_auth.R"), local = TRUE)$value


server <- function(input, output, session) {

  `%>%` <- magrittr::`%>%`

  # active main panel based on the reactive navigation state
  navigation_state <- reactiveNavigation("LP")
  main_visible <- getNavigation(outputNavigation(
    # force_react = FALSE allows reacting only to actual changes
    navigation_state, force_react = FALSE
  ))

  result <- reactiveValues(
    userId = FLAMINGO_GUEST_ID,
    userName = "",
    WidthMain = 9,
    WidthSide = 3,
    preselPanel = panelsProgrammeWorkflow[1]
  )

  authenticated <- reactive({
    # reactive expression yielding TRUE when:
    # - user has been matched to an id
    # - ui has been rendered (input values are available)
    auth <- (result$userId != FLAMINGO_GUEST_ID) &&
      !is.null(input$authUIRenderCallback)

    if (auth) logMessage("Authenticated")

    return(auth)
  }) %>% debounce(100)

  ### submodules ----
  .callModule <- function(...) {
    callModule(..., dbSettings = dbSettings)
  }

  loginDialogModule <- .callModule(
    loginDialog, "login",
    logMessage = logMessage,
    logout = reactive(auth_modules$pageheader$logout())
  )

  # list of modules for the authenticated UI
  auth_modules <- list()

  auth_modules$pageheader <- .callModule(
    pageheader, "pageheader",
    userId = reactive(result$userId),
    userName = reactive(result$userName),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated())
  )

  auth_modules$pagestructure <- .callModule(
    pagestructure, "pagestructure",
    userId = reactive(result$userId),
    userName = reactive(result$userName),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated())
  )

  auth_modules$landingPage <- .callModule(
    landingPage, "landingPage",
    userId = reactive(result$userId),
    userName = reactive(result$userName),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated() && main_visible() == "LP")
  )

  auth_modules$DA <- .callModule(
    accountDefinition,
    id = "DA",
    active = reactive(authenticated() && main_visible() == "DA")
  )

  auth_modules$programmeDefinitionSingle <- .callModule(
    programmeDefinitionSingle,
    id = "programmeDefinitionSingle",
    apiSettings = apiSettings,
    userId = reactive(result$userId),
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
    userId = reactive(result$userId),
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && main_visible() == "PB")
  )

  auth_modules$browseprogrammes <- .callModule(
    browseprogrammes,
    id = "browseprogrammes",
    apiSettings = apiSettings,
    userId = reactive(result$userId),
    runIdList =  auth_modules$landingPage$runIdList,
    preselRunId =  auth_modules$landingPage$runId,
    processRunId = auth_modules$programmeDefinitionSingle$processRunId,
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && main_visible() == "BR")
  )

  
  # preselected panel
  observe({if (!is.null(auth_modules$panelOutputModule$preselPanel)) {
    result$preselPanel <- auth_modules$panelOutputModule$preselPanel
  } else {
    result$preselPanel <- panelsProgrammeWorkflow[1]
  }
  })

  auth_modules$fileViewer <- .callModule(
    fileViewer,
    id = "fileViewer",
    logMessage = logMessage,
    active = reactive(authenticated()) #&& input$fm == "fileviewer"))
  )

  auth_modules$modelSupplierPage <- .callModule(
    modelSupplierPage,
    id = "modelSupplierPage",
    active = reactive(authenticated())# && input$sc == "Model"))
  )

  auth_modules$userAdminDefinition <- .callModule(
    userAdminDefinition,
    id = "userAdminDefinition",
    active = reactive(authenticated() && input$ua == "defineuser"),
    userId = reactive(result$userId)
  )

  auth_modules$companyDefinition <- .callModule(
    companyDefinition,
    id = "companyDefinition",
    active = reactive(authenticated() && input$ua == "definecompany")
  )
  
  ### authentication ----
  # show the logged-in part of the UI if login is completed
  appState <- reactive(
    if (result$userId == FLAMINGO_GUEST_ID) "loggedout" else "loggedin"
  )
  callModule(reactiveConditionalPanels, "appUI", appState)

  observe({
    result$userId <- loginDialogModule$userId()
    result$userName <- loginDialogModule$userName()
  })

  ### Module input parameters depending on othe rmodules outputs ---
  
  # UI non-reactive to (result$WidthSide) and (result$Widthmain)
  output$authUI <- renderUI(
    if (result$userId != FLAMINGO_GUEST_ID) {
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

  ### collapse / expand sidebar ----
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

           "WF" = { # go to Workflow submenu
             loginfo(paste("Navigate to Process Management, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "DA" = { # go to Define account submenu
             loginfo(paste("Navigate to Define Account, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "PS" = { # go to Define programme single submenu
             loginfo(paste("Navigate to Define Process Single, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "PB" = { # go to Define programme batch submenu
             loginfo(paste("Navigate to Define Process Batch, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "BR" = { # go to Define Define programme batch submenu
             loginfo(paste("Navigate to Browse, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "UA" = { # go to User Admin submenu
             loginfo(paste("Navigate to User Admin, userId: ", result$userId),
                     logger = "flamingo.module")
             updateTabsetPanel(session, "ua", selected = "defineuser")
           },

           "FM" = { # go to File Management submenu
             loginfo(paste("Navigate to File Management, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "SC" = { # go to System Configuration submenu
             loginfo(paste("Navigate to System Config, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "LP" = { # go to Landing Page
             loginfo(paste("Navigate to Landing Page, userId:", result$userId),
                     logger = "flamingo.module")
           },

           stop("page does not exist")

    )

  })

}

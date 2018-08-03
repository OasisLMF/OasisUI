# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

# UI content that is rendered once the user has authenticated
source(file.path(".", "ui_auth.R"), local = TRUE)$value


server <- function(input, output, session) {

  `%>%` <- magrittr::`%>%`

  result <- reactiveValues(
    userId = FLAMINGO_GUEST_ID,
    userName = "",
    navigate = "LP",
    counter = 0,
    WidthMain = 9,
    WidthSide = 3
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

  # collapse / expand sidebar
  observeEvent(input$abuttoncollapsesidebar, {
    result$counter <- result$counter + 1
    if ((result$counter %% 2) == 0) {
      result$WidthMain <- 9
      result$WidthSide <- 3
    } else {
      result$WidthMain <- 11
      result$WidthSide <- 1
    }
  })

  ### submodules ----
  modules <- list()


  .callModule <- function(...) {
    callModule(..., dbSettings = dbSettings)
  }

  pageheaderModule <- .callModule(
    pageheader, "pageheader",
    userId = reactive(result$userId),
    userName = reactive(result$userName),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated())
  )

  pagestructureModule <- .callModule(
    pagestructure, "pagestructure",
    userId = reactive(result$userId),
    userName = reactive(result$userName),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated()),
    W = reactive(result$WidthMain)
  )

  landingPageModule <- .callModule(
    landingPage, "landingPage",
    userId = reactive(result$userId),
    userName = reactive(result$userName),
    reloadMillis = reloadMillis,
    logMessage = logMessage,
    active = reactive(authenticated() && result$navigate == "LP")
  )

  loginDialogModule <- .callModule(
    loginDialog, "login",
    logMessage = logMessage,
    logout = reactive(pageheaderModule$logout())
  )

  accountDefinitionModule <- .callModule(
    accountDefinition,
    id = "accountDefinition",
    active = reactive(authenticated() && result$navigate == "DA")
  )

  programmeDefinitionSingleModule <- .callModule(
    programmeDefinitionSingle,
    id = "programmeDefinitionSingle",
    apiSettings = apiSettings,
    userId = reactive(result$userId),
    preselRunId = landingPageModule$runId,
    preselProcId = landingPageModule$procId,
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && result$navigate == "PS")
  )

  programmeDefinitionBatchModule <- .callModule(
    programmeDefinitionBatch,
    id = "programmeDefinitionBatch",
    apiSettings = apiSettings,
    userId = reactive(result$userId),
    preselRunId = landingPageModule$runId,
    preselProcId = landingPageModule$procId,
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && result$navigate == "PB")
  )

  browseprogrammesModule <- .callModule(
    browseprogrammes,
    id = "browseprogrammes",
    apiSettings = apiSettings,
    userId = reactive(result$userId),
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && result$navigate == "BR")
  )

  programmeDefinitionModule <- .callModule(
    programmeDefinition,
    id = "programmeDefinition",
    apiSettings = apiSettings,
    userId = reactive(result$userId),
    logMessage = logMessage,
    reloadMillis = reloadMillis,
    active = reactive(authenticated() && input$em == "defineProg")
  )

  # accountDefinitionModule <- .callModule(
  #   accountDefinition,
  #   id = "accountDefinition",
  #   active = reactive(authenticated() && input$em == "defineAccount")
  # )

  processRunPageModule <- .callModule(
    processRunPage,
    id = "processRunPage",
    apiSettings = apiSettings,
    logMessage = logMessage,
    userId = reactive(result$userId),
    active = reactive(authenticated()), #&& input$pr == "processrun"),
    preselRunId = landingPageModule$runId,
    preselProcId = landingPageModule$procId,
    progOasisId = programmeDefinitionModule$progOasisId,
    reloadMillis = reloadMillis
  )

  fileViewerModule <- .callModule(
    fileViewer,
    id = "fileViewer",
    logMessage = logMessage,
    active = reactive(authenticated()) #&& input$fm == "fileviewer"))
  )

  modelSupplierPageModule <- .callModule(
    modelSupplierPage,
    id = "modelSupplierPage",
    active = reactive(authenticated())# && input$sc == "Model"))
  )

  userAdminDefinitionModule <- .callModule(
    userAdminDefinition,
    id = "userAdminDefinition",
    active = reactive(authenticated() && input$ua == "defineuser"),
    userId = reactive(result$userId)
  )

  companyDefinitionModule <- .callModule(
    companyDefinition,
    id = "companyDefinition",
    active = reactive(authenticated() && input$ua == "definecompany")
  )

  ### authentication ----
  output$id <- reactive(result$userId)
  outputOptions(output, "id", suspendWhenHidden = FALSE)

  observe({
    result$userId <- loginDialogModule$userId()
    result$userName <- loginDialogModule$userName()
  })

  # UI non-reactive to (result$WidthSide) and (result$Widthmain)
  output$authUI <- renderUI(
    if (result$userId != FLAMINGO_GUEST_ID) {
      authUI(
        WidthSide = isolate(result$WidthSide),
        WidthMain = isolate(result$WidthMain)
      )
    }
  )
  # reactivity via flexColumn module
  callModule(flexColumn, "sidebar", reactive(result$WidthSide))
  callModule(flexColumn, "main", reactive(result$WidthMain))


  observe(result$logout <- pageheaderModule$logout())

  ### navigation ----
  observe(if (authenticated()) {
    if (!is.null(page <- pageheaderModule$navigate())) {
      result$navigate <- page
    }
  })

  observe(if (authenticated()) {
    if (!is.null(page <- pagestructureModule$navigate())) {
      result$navigate <- page
    }
  })

  observe(if (authenticated()) {
    if (!is.null(page <- landingPageModule$navigate())) {
      result$navigate <- page
    }
  })

  observe(if (authenticated()) {
    if (!is.null(page <- programmeDefinitionModule$navigate())) {
      result$navigate <- page
    }
  })

  observe(if (authenticated()) {
    if (!is.null(page <- userAdminDefinitionModule$navigate())) {
      result$navigate <- page
    }
  })

  observe(if (authenticated()) {
    if (!is.null(page <- programmeDefinitionSingleModule$navigate())) {
      result$navigate <- page
    }
  })

  #placeholder
  observe(if (authenticated()) {
    if (!is.null(page <- programmeDefinitionBatchModule$navigate())) {
      result$navigate <- page
    }
  })

  output$menu <- reactive(result$navigate)
  outputOptions(output, "menu", suspendWhenHidden = FALSE)

  observe(if (authenticated()) {

    switch(result$navigate,

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

           "PB" = { # go to Define Define programme batch submenu
             loginfo(paste("Navigate to Define Process Batch, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "BR" = { # go to Define Define programme batch submenu
             loginfo(paste("Navigate to Browse, userId: ", result$userId),
                     logger = "flamingo.module")
           },

           "EM" = { # go to Exposure Management submenu
             loginfo(paste("Navigate to Exposure Management, userId: ", result$userId),
                     logger = "flamingo.module")
             updateTabsetPanel(session, "em", selected = "defineProg")
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

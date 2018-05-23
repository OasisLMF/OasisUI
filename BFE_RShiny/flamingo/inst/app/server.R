# Flamingo Shiny
# 
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

# UI content that is rendered once the user has authenticated
source(file.path(".", "ui_auth.R"), local = TRUE)$value

server <- function(input, output, session) {
  
  result <- reactiveValues(
      userId = FLAMINGO_GUEST_ID,
      userName = "",
      navigate = "LP"
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

  
  ### submodules
  
  modules <- list()
  
  landingPageModule <- callModule(landingPage, "landingPage",
      userId = reactive(result$userId),
      userName = reactive(result$userName),
      dbSettings = dbSettings,
      reloadMillis = reloadMillis,
      logMessage = logMessage,
      active = reactive(authenticated() && result$navigate == "LP"))
  
  loginDialogModule <- callModule(loginDialog, "login",
      dbSettings = dbSettings,
      logMessage = logMessage,
      logout = reactive(landingPageModule$logout()))
  
  programmeDefinitionModule <- callModule(programmeDefinition,
      id = "programmeDefinition",
      dbSettings = dbSettings,
      apiSettings = apiSettings,
      userId = reactive(result$userId),
      logMessage = logMessage,
      reloadMillis = reloadMillis,
      active = reactive(authenticated() && input$em == "defineProg"))
  
  accountDefinitionModule <- callModule(accountDefinition,
      id = "accountDefinition",
      dbSettings = dbSettings,
      active = reactive(authenticated() && input$em == "defineAccount"))
  
  processRunPageModule <- callModule(processRunPage,
      id = "processRunPage",
      dbSettings = dbSettings,
      apiSettings = apiSettings,
      logMessage = logMessage,
      userId = reactive(result$userId),
      active = reactive(authenticated() && input$pr == "processrun"),
      preselRunId = landingPageModule$runId,
      preselProcId = landingPageModule$procId,
      progOasisId = programmeDefinitionModule$progOasisId,
      reloadMillis = reloadMillis)
  
  fileViewerModule <- callModule(fileViewer,
      id = "fileViewer",
      dbSettings = dbSettings,
      logMessage = logMessage,
      active = reactive(authenticated() && input$fm == "fileviewer"))
  
  modelSupplierPageModule <- callModule(modelSupplierPage,
      id = "modelSupplierPage",
      dbSettings = dbSettings,
      active = reactive(authenticated() && input$sc == "Model"))
  
  userAdminDefinitionModule <- callModule(userAdminDefinition,
      id = "userAdminDefinition",
      dbSettings = dbSettings,
      active = reactive(authenticated() && input$ua == "defineuser"),
      userId = reactive(result$userId))
  
  companyDefinitionModule <- callModule(companyDefinition,
      id = "companyDefinition",
      dbSettings = dbSettings,
      active = reactive(authenticated() && input$ua == "definecompany"))
  
  
  
  ### authentication
  
  output$id <- reactive(result$userId)
  outputOptions(output, "id", suspendWhenHidden=FALSE)
  
  observe({
        result$userId <- loginDialogModule$userId()
        result$userName <- loginDialogModule$userName()
      })
  
  output$authUI <- renderUI(if (result$userId != FLAMINGO_GUEST_ID) authUI())
  
  observe(result$logout <- landingPageModule$logout())
  
  
  
  ### navigation
  
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
  
  observe(if (authenticated() && (
            input$em == "emlp" ||
            input$pr == "prlp" ||
            input$fm == "fmlp" ||
            input$sc == "sclp" ||
            input$ua == "ualp")) {
        
        result$navigate <- "LP"
      })
  
  output$menu <- reactive(result$navigate)
  outputOptions(output, "menu", suspendWhenHidden = FALSE)

  observe(if (authenticated()) {
        
        switch(result$navigate,
          
          "WF" = { # go to Workflow submenu
            loginfo(paste("Navigate to Process Management, userId: ", result$userId),
                logger = "flamingo.module")
            updateTabsetPanel(session, "pr", selected = "processrun")
          },
          
          "EM" = { # go to Exposure Management submenu
            loginfo(paste("Navigate to Exposure Management, userId: ", result$userId),
                logger = "flamingo.module")
            updateTabsetPanel(session, "em", selected = "defineProg")
          },
          
          "UA" = { # go to User Admin submenu
            loginfo(paste("Navigate to User Admin, userId: ", result$userId),
                logger = "flamingo.module")
            updateTabsetPanel(session, "ua", selected = "definecompany")
          },
          
          "FM" = { # go to File Management submenu
            loginfo(paste("Navigate to File Management, userId: ", result$userId),
                logger = "flamingo.module")
            updateTabsetPanel(session, "fm", selected = "fileviewer")
          },
          
          "SC" = { # go to System Configuration submenu
            loginfo(paste("Navigate to System Config, userId: ", result$userId),
                logger = "flamingo.module")
            updateTabsetPanel(session, "sc", selected = "Model")
          },
          
          "LP" = { # go to Landing Page
            loginfo(paste("Navigate to Landing Page, userId:", result$userId),
                logger = "flamingo.module")
            updateTabsetPanel(session, "em", selected = "emtemp")
            updateTabsetPanel(session, "pr", selected = "prtemp")
            updateTabsetPanel(session, "ua", selected = "uatemp")
            updateTabsetPanel(session, "wa", selected = "watemp")
            updateTabsetPanel(session, "fm", selected = "fmtemp")
            updateTabsetPanel(session, "sc", selected = "sctemp")
          },
          
          stop("page does not exist")
      )
    })
  
}

#' Single Programme Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return For \code{programmeDefinitionSingle()}, list of reactives.
#' @template return-outputNavigation
#' @rdname programmeDefinitionSingle
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDT dataTableProxy selectRows
#' @importFrom dplyr mutate select
#' @importFrom shinyBS toggleModal
#' @importFrom shinyjs onclick js
#' @export
programmeDefinitionSingle <- function(input, output, session, dbSettings,
                                      apiSettings, userId, active = reactive(TRUE), logMessage = message,
                                      preselRunId = reactive(-1),
                                      preselProcId = reactive(-1),
                                      preselPanel = "1",
                                      reloadMillis = 10000) {

  ns <- session$ns

  # Reactive Values and parameters ------------------------------------------

  navigation_state <- reactiveNavigation()

  result <- reactiveValues(
    #reactive values for the programme table
    DPProgData = NULL,
    DPProgData_selected_rows = 0,
    # SL file to view
    viewSLfile = NULL,
    # SA file to view
    viewSAfile = NULL,
    #reactive values for the programme details table
    progDetails = NULL,
    progDetailsCounter = 0,
    #reactive values for the programme model table
    POData = NULL,
    PODataCounter = 0,
    POData_selected_rows = 0,
    #reactive values for the programme model detail table
    progFiles = NULL,
    progFilesCounter = 0,
    #reactive values for the process runs table
    prcrundata = NULL,
    prcrundataCounter = 0,
    prcrundata_selected_rows = 0,
    #reactive value for the log table
    logsCounter = 0,
    #Id of the Programme Model
    progOasisId = -1,
    #Id of the Process Run
    prrunid = -1,
    #flag to know if the user is creating or amending a programme
    prog_flag = ""
  )

  workflowSteps <- callModule(singleProgrammeWorkflowSteps, "workflowsteps")

  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")

  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")

  # Newly created account needs to be added to choices
  observe(if (active()) {
    accounts <- getAccountName(dbSettings)
    updateSelectInput(session, "sinputDPAccountName",
                      choices = createSelectOptions(accounts, "Select Account"),
                      selected = isolate(input$sinputDPAccountName))
  })

  # Panels switch ------------------------------------------------------------

  # Make sure the first view is reset to first panel
  observe(if (active()) {
    workflowSteps$update("1")
    .reloadDPProgData()
  })

  observeEvent(workflowSteps$step(), {
    switch(
      workflowSteps$step(),
      "1" = {
        logMessage("showing panelDefineProgramme")
        .hideDivs()
        .defaultCreateProg()
      },
      "2" = {
        logMessage("showing panelProgrammeTable panelAssociateModel")
        .hideDivs()
        .defaultAssociateModel()
        #.reloadDPProgData()
      },
      "3" = {
        logMessage("showing panelDefineIDs panelProgrammeModelTable panelDefineOutputs")
        .hideDivs()
        .defaultConfigOutput()
        .reloadPOData()
      },
      "4" = {
        logMessage("showing panelDefineIDs panelProcessRunTable")
        .hideDivs()
        .defaultRun()
        .reloadRunData()
      }
    )
  })

  # Panel Create Programme ----------------------------------------------------

  ### Submit Button
  onclick("abuttonProgSubmit", {
    idxSel <- 1
    if (result$prog_flag == "C") {
      query <- paste0("exec dbo.createProg [", input$tinputDPProgName,
                      "],", input$sinputDPAccountName, ", [",
                      input$sinputTransformname, "]")
      res <- executeDbQuery(dbSettings, query)
      if (is.null(res)) {
        showNotification(type = "error",
                         paste("Failed to create a Programme - ", input$tinputDPProgName))
      } else {
        showNotification(type = "message",
                         paste("Programme ", input$tinputDPProgName, " created."))
      }
    } else {
      if (result$prog_flag == "A" & result$DPProgData_selected_rows != 0) {
        idxSel <- input$tableDPprog_rows_selected
        query <- paste0("exec dbo.updateProg ", result$DPProgData[result$DPProgData_selected_rows, 1],
                        ",[", input$tinputDPProgName,"],", input$sinputDPAccountName,
                        ", [", input$sinputTransformname, "]")
        res <- executeDbQuery(dbSettings, query)
        message(paste("A res is:", res))
        if (is.null(res)) {
          showNotification(type = "error",
                           paste("Failed to amend a Programme - ", result$DPProgData[input$tableDPprog_rows_selected, 2]))
        } else {
          showNotification(type = "message",
                           paste("Programme ", result$DPProgData[input$tableDPprog_rows_selected, 2], " amended."))
        }
      } else {
        showNotification(type = "warning",
                         paste("Please select a Programme to amend first."))
      }
    }
    #result$DPProgData_selected_rows <- 1
    # #Reload Programme Table
    .reloadDPProgData()
    # update result$DPProgDataCounter --> a) reload table result$DPProgData, ### b) update result$DPProgData_selected_rows
    ### b2) update selectprogrammeID drop-down --> selectRows(dataTableProxy("tableDPprog"),rowToSelect), update tableDPprog_rows_selected, .reloadPOData()
    # a2) [498], update selectprogrammeID drop-down through observeEvent() --> selectRows(dataTableProxy("tableDPprog"), rowToSelect), update tableDPprog_rows_selected, .reloadPOData()
    ### update result$DPProgData_selected_rows
    logMessage(paste("updating tableDPprog select because programme table was reloaded:", idxSel))
    selectRows(dataTableProxy("tableDPprog"), idxSel)
    logMessage(paste("selected row is:", input$tableDPprog_rows_selected))
  })

  ### Clear Programme Definition panel
  onclick("abuttonProgCancel",{
    .clearDPAccountSelection()
    .clearProgrammeName()
    .clearSourceFilesSelection()
    .clearTransformNameSelection()
  })

  ### Load Programme Button
  onclick("buttonloadcanmodpr",{
    if (result$DPProgData_selected_rows != 0) {
      loadprogdata <- loadProgrammeData(apiSettings,
                                        progId = result$DPProgData[result$DPProgData_selected_rows,1])
      if (loadprogdata == 'success' || loadprogdata == 'Success') {
        showNotification(type = "message", "Initiating load programme data...")
        workflowSteps$update("2")
      } else {
        showNotification(type = "error", "Failed to load programme data.")
      }
    } else {
      showNotification(type = "warning", "Please select a Programme to load programme data.")
    }
    .reloadDPProgData()
  })

  ### > Source Files ----

  ### Upload Location/Account File
  onclick("abuttonSLFileUpload", {
    inFile <- input$SLFile
    flc <- getFileLocationPath(dbSettings, "Exposure File")
    flcopy <- file.copy(inFile$datapath,
                        file.path(flc,inFile[1,1]), overwrite = TRUE)
    logMessage(file.path(flc,inFile[1,1]))
    if (result$DPProgData_selected_rows != 0) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(dbSettings,
                                     inFile[1,1], "Source Loc File", 101, flc, userId(),
                                     "Prog", result$DPProgData[result$DPProgData_selected_rows,1])
        if (!is.null(recordId)) {
          showNotification(type = "message",
                           paste("New File record id: ", recordId, " created."))
          .reloadProgDetails()
        } else {
          showNotification(type = "error", "Could not create file record.")
        }
      } else{
        showNotification(type = "error", "File transfer failed.")
      }
    }
  })

  onclick("abuttonSAFileUpload", {
    inFile <- input$SAFile
    flc <- getFileLocationPath(dbSettings, "Exposure File")
    flcopy <- file.copy(inFile$datapath,
                        file.path(flc, inFile[1,1]), overwrite = TRUE)
    logMessage(file.path(flc,inFile[1,1]))
    if (result$DPProgData_selected_rows != 0) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(dbSettings,
                                     inFile[1,1], "Source Acc File", 102, flc, userId(),
                                     "Prog", result$DPProgData[result$DPProgData_selected_rows,1])
        if (!is.null(recordId)) {
          showNotification(type = "message",
                           paste("New File record id: ", recordId, " created."))
          .reloadProgDetails()
        } else {
          showNotification(type = "error", "Could not create file record.")
        }
      } else{
        showNotification(type = "error", "File transfer failed.")
      }
    }
  })

  ### Link Location/Account File
  onclick("abuttonSLFileLink",{
    if (result$DPProgData_selected_rows != 0) {
      res <- executeDbQuery(dbSettings,
                            paste("exec dbo.updateSourceLocationFileForProg ",
                                  input$sinputselectSLFile, ", ", result$DPProgData[result$DPProgData_selected_rows,1]))
      if (input$sinputselectSLFile != "") {
        if (is.null(res)) {
          showNotification(type = "error", "Failed to link the File!")
        } else {
          showNotification(type = "message",
                           paste("Location File linked to Programme", result$DPProgData[result$DPProgData_selected_rows,2]))
        }
      } else {
        showNotification(type = "warning", "Please select a file to Link")
      }
    }
  })

  onclick("abuttonSAFileLink",{
    if (result$DPProgData_selected_rows != 0) {
      res <- executeDbQuery(dbSettings,
                            paste("exec dbo.updateSourceAccountFileForProg ",
                                  input$sinputselectSAFile, ", ", result$DPProgData[result$DPProgData_selected_rows,1]))
      if (input$sinputselectSAFile != "") {
        if (is.null(res)) {
          showNotification(type = "error", "Failed to link the File!")
        } else {
          showNotification(type = "message",
                           paste("Location File linked to Programme", result$DPProgData[result$DPProgData_selected_rows,2]))
        }
      } else {
        showNotification(type = "warning", "Please select a file to Link")
      }
    }
  })

  ### Display File Upload/link options
  observe(if (active()) {
    if (input$sinputSLFile == "U") {
      show("divSLFileUpload")
      disable("abuttonSLFileUpload")
      hide("divSLFileSelect")
    } else if (input$sinputSLFile == "S") {
      show("divSLFileSelect")
      hide("divSLFileUpload")
    }
  })

  observe(if (active()) {
    if (input$sinputSAFile == "U") {
      show("divSAFileUpload")
      disable("abuttonSAFileUpload")
      hide("divSAFileSelect")
    } else if (input$sinputSAFile == "S") {
      show("divSAFileSelect")
      hide("divSAFileUpload")
    }
  })

  ### On change Location/Account file upload dropdown
  observe(if (active()) {
    if (input$sinputSLFile == "U") {
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SLFile
      if (!is.null(inFile)) {
        enable("abuttonSLFileUpload")
      }
    } else {
      if (input$sinputSLFile == "S") {
        SLfiles <- getFileSourceLocationFile(dbSettings)
        updateSelectInput(session, "sinputselectSLFile",
                          choices = createSelectOptions(SLfiles, labelCol = 1, valueCol = 2))
      }
    }
  })

  observe(if (active()) {
    if (input$sinputSAFile == "U") {
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SAFile
      if (!is.null(inFile)) {
        enable("abuttonSAFileUpload")
      }
    } else {
      if (input$sinputSAFile == "S") {
        SAfiles <- getFileSourceAccountFile(dbSettings)
        updateSelectInput(session, "sinputselectSAFile",
                          choices = createSelectOptions(SAfiles, labelCol = 1, valueCol = 2))
      }
    }
  })

  ### View source files
  #Source Location
  onclick("abuttonSLFileView", {
    if (input$sinputselectSLFile != "") {
      toggleModal(session, "bsmodalviewSLfile", toggle = "open")
    } else {
      toggleModal(session, "bsmodalviewSLfile", toggle = "close")
      showNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSLfile <- renderDT({
    if (!is.null(result$viewSLfile)) {
      datatable(
        result$viewSLfile,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'none'),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    }
  })

  #Source Account
  onclick("abuttonSAFileView", {
    if (input$sinputselectSLFile != "") {
      toggleModal(session, "bsmodalviewSAfile", toggle = "open")
    } else {
      toggleModal(session, "bsmodalviewSAfile", toggle = "close")
      showNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSAfile <- renderDT({
    if (!is.null(result$viewSAfile)) {
      datatable(
        result$viewSAfile,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'none'),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    }
  })

  # Panel Associate Model -----------------------------------------------------

  ### > Programme Table ------
  output$tableDPprog <- renderDT({
    dtOut <- result$DPProgData
    logMessage("re-rendering programme table")
    if (!is.null(dtOut)) {
      if (isolate(input$selectprogrammeID) != "") {
        rowToSelect <- which(dtOut[, 1] == isolate(input$selectprogrammeID))
      } else {
        rowToSelect <- 1
      }
      datatable(
        dtOut,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = rowToSelect,
                         target = 'row'),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    } else {
      NULL
    }
  })

  ### > Programme Actions ----

  # Ammend Programme
  onclick("buttonamendpr", {
    if (result$DPProgData_selected_rows != 0) {
      .updateDPAccountSelection()
      .updateProgrammeName()
      .updateTransformNameSelection()

      show("panelDefineProgramme")
      show("abuttonhidedefineprogpanel")
      logMessage("showing panelDefineProgramme")
    } else {
      showNotification(type = "warning", "Please select a Programme to Amend")
      hide("panelDefineProgramme")
    }
  })

  # Hide Programme Definition Panel
  onclick("abuttonhidedefineprogpanel", {
    hide("panelDefineProgramme")
    hide("abuttonhidedefineprogpanel")
  })


  # Delete Programme
  onclick("buttondeletepr",{
    if (result$DPProgData_selected_rows != 0) {
      stmt <- buildDbQuery("deleteProg", result$DPProgData[result$DPProgData_selected_rows,1])
      executeDbQuery(dbSettings, stmt)
      showNotification(type = "message", sprintf("Programme %s deleted", result$DPProgData[result$DPProgData_selected_rows,2]))
      .reloadDPProgData()
    } else {
      showNotification(type = "warning", "Please select a Programme to Delete")
    }
  })


  # > Programme Details Table-----

  ### Programme Detail Table
  observeEvent(result$progDetailsCounter, {
    if (result$DPProgData_selected_rows != 0) {
      progId <- result$DPProgData[result$DPProgData_selected_rows, 1]

      stmt <- buildDbQuery("getProgFileDetails", progId)
      progDetails <- executeDbQuery(dbSettings, stmt)

      if (!is.null(progDetails)) {
        result$progDetails  <- progDetails %>%
          mutate(Status = replace(Status, Status == "Failed", StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()
      }
      logMessage("programme details table refreshed")
    } else {
      if (active()) {
        showNotification(type = "warning", "Please select a Programme first")
      }
    }
  })


  output$tableprogdetails <- renderDT({
    datatable(
      result$progDetails,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      escape = FALSE,
      selection = "none",
      colnames = c('Row Number' = 1),
      options = .getPRTableOptions()
    )
  })

  # Show Programme Details
  observeEvent(input$buttonprogdetails, {
    show("panelProgrammeDetails")
    hide("buttonprogdetails")
    .reloadProgDetails()
  })

  observeEvent(input$buttonhideprogdetails, {
    hide("panelProgrammeDetails")
    show("buttonprogdetails")
  })

  # Manage panels to show when selected row changes
  observeEvent(result$DPProgData_selected_rows, {
    if (active()) {
      if (workflowSteps$step() == "2") {
        .defaultAssociateModel()
      }
    }
    if (result$DPProgData_selected_rows != 0) {
      if (active()) {
        if (workflowSteps$step() == "3") {
          #show("panelAssociateModel")
        }
      }
      .defaultOOKSidebar()
      if (result$DPProgData[input$tableDPprog_rows_selected, "Status"] == StatusCompleted &&
          workflowSteps$step() == "2") {
        .defaultOOKSidebar()
      } else {
        hide("panelAssociateModel")
        if (active() && workflowSteps$step() == "2") {
          showNotification(type = "warning", "Please select a completed Programme to which associate a model")
        }
      }
    } else {
      hide("panelAssociateModel")
      logMessage("hiding panelAssociateModel")
      .clearOOKSidebar()
      if (active()) {
        showNotification(type = "warning", "Please select a Programme to which associate a model")
      }
    }
  })

  # > Create Model ----

  # on click of create prog oasis button - Creates and loads the model - sends user to Configure Output panel
  onclick("abuttoncrprogoasis", {
    if (isolate(input$sinputookprogid) > 0 && isolate(input$sinputookmodelid) > 0) {
      result$progOasisId <- createProgOasis(dbSettings,
                                            isolate(input$sinputookprogid),
                                            isolate(input$sinputookmodelid),
                                            isolate(input$sinputProgModTransform))
      showNotification(type = "message",
                       paste("Prog Oasis id:", result$progOasisId,  " created."))
      .clearOOKSidebar()
      workflowSteps$update("3")
      .reloadPOData()
      if (result$POData_selected_rows != 0) {
        loadprogmodel <- loadProgrammeModel(apiSettings,
                                            progOasisId = toString(result$POData[result$POData_selected_rows,1]))
        if (loadprogmodel == 'success' || loadprogmodel == 'Success') {
          showNotification(type = "message", "Initiating load programme model.")
          .reloadProgFiles()
        } else {
          showNotification(type = "error", "Failed to load programme model.")
        }
      }
    } else{
      showNotification(type = "warning", "Please select both the fields.")
    }
  })

  # Panel Configure Output  ----------------------------------------------------

  ### > selectprogrammeID ----

  # Add choices possibilities  to selectprogrammeID
  # observe(if (active()) {
  #   if (!is.null(result$DPProgData)) {
  #     #if (input$selectprogrammeID == "") {
  #       # if (preselProcId() != -1) {
  #       #   if (!is.null(result$POData)) {
  #       #     index <- match(c(preselProcId()), result$POData[[1]])
  #       #   }
  #       #   index <- 1
  #       # } else {
  #       #   index <- 1
  #       # }
  #       # TODO-Nikki- index not being hardcoded to 1 but similar style to prgId (tableDPProg)line 586
  #       index <- tableDPProg
  #       logMessage(paste0("updating selectprogrammeID choices based on Programme Table"))
  #       updateSelectInput(session, inputId = "selectprogrammeID", choices = result$DPProgData[1:nrow(result$DPProgData),1], selected = result$DPProgData[index,1])
  #     #}
  #   }
  # })

  # Update selectprogrammeID
  observeEvent(result$DPProgData, {
    if (!is.null(result$DPProgData)) {
      logMessage(paste0("updating selectprogrammeID choices because programme table was reloaded - contains ", nrow(result$DPProgData), " rows"))
      if (input$selectprogrammeID == "")
        prgId <- 1
      else
        prgId <- input$selectprogrammeID
      updateSelectInput(session, inputId = "selectprogrammeID", choices = result$DPProgData[, 1], selected = prgId)
    }
  })

  # Preselect selectprogrammeID
  # observeEvent(result$DPProgData_selected_rows, {
  #   if (result$DPProgData_selected_rows != 0) {
  #     prgId <- result$DPProgData[result$DPProgData_selected_rows, 1]
  #     logMessage(paste0("updating selectprogrammeID because selection in programme table changed to ",  result$DPProgData_selected_rows))
  #     updateSelectInput(session, inputId = "selectprogrammeID", selected = prgId)
  #   }
  # })

  # If selectprogrammeID changes, reload programme model table and set view back to default
  observeEvent(input$selectprogrammeID, {
    if (active()) {
      show("buttonmodeldetails")
      hide("panelModelDetails")
      logMessage(paste("updating tableDPprog select because selectprogrammeID changed to", input$selectprogrammeID))
      if (!is.null(result$DPProgData)) {
         rowToSelect <- which(result$DPProgData[, 1] == input$selectprogrammeID)
         selectRows(dataTableProxy("tableDPprog"), rowToSelect)
      }
      .reloadPOData()
    }
  })

  # Output configuration: manage what to show based on  status of row selected in programme Model table
  observeEvent(result$POData_selected_rows, {
    if (result$POData_selected_rows != 0 & !is.null(result$POData)) {
      if (!is.na(result$POData[result$POData_selected_rows, "Status"])) {
        if (result$POData[result$POData_selected_rows, "Status"] == StatusCompleted) {
          if (active()) {
            if (workflowSteps$step() == "3") {
              .defaultview(session)
            }
          }
          # Show perils according to programme
          prtable <- getProcessData(dbSettings, userId(), 0, 0, 0)
          procId <- toString(prtable[result$POData_selected_rows, 1][length(prtable[result$POData_selected_rows, 1])])
          paramlist <- executeDbQuery(dbSettings,
                                      buildDbQuery("getRuntimeParamList", procId))
          hide("perilwind")
          hide("perilsurge")
          hide("perilquake")
          hide("perilflood")
          hide("demandsurge")
          hide("leakagefactor")
          if (nrow(paramlist) > 0) {
            for (i in 1:nrow(paramlist)) {
              ctrname <- gsub("_", "", paramlist[i, 1], fixed = TRUE)
              show(ctrname)
            }
          }
        } else {
          .hiddenconfigOutputView("Please select a Completed Programme Model to which associate Output Configuration")
        }
      } else {
        .hiddenconfigOutputView("Please select a Completed Programme Model to which associate Output Configuration")
      }
    }
  })

  ### > Programme Model Table (previously OOK) ----
  observeEvent(result$PODataCounter, {
    if (!is.null(input$selectprogrammeID)) {
      POData <- getProgOasisForProgdata(dbSettings, input$selectprogrammeID)
      if (!is.null(POData)) {
        result$POData <- POData %>%
          select(c("ProgOasisId", "ProgName", "ModelName", "TransformName", "SourceFileId", "FileID", "Status")) %>%
          mutate(Status = replace(Status, Status == "Failed", StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()
      }
      logMessage("programme model table refreshed")
    } else {
      if (active()) {
        showNotification(type = "warning", "Please select a Programme ID first")
      }
    }
  })


  # Manage what to show based on rows being selected in programme Model table
  observeEvent(result$POData_selected_rows, {
    if (result$POData_selected_rows != 0) {
      .reloadProgFiles()
      hide("panelModelDetails")
      show("buttonmodeldetails")
    } else {
      hide("panelModelDetails")
      hide("buttonmodeldetails")
      if (!is.null(result$POData)) {
        .hiddenconfigOutputView("Please select a Programme Model first")
      }
    }
  })

  output$tableProgOasisOOK <- renderDT(
    if (!is.null(result$POData)) {
      datatable(
        result$POData,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = rownames(result$POData)[1]),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    }
  )


  # > Model Details Table -----

  observeEvent(result$progFilesCounter, {

    if (result$POData_selected_rows != 0) {
      #result$progOasisId is updated when creating a model or y selecting a model
      result$progOasisId <- toString(result$POData[result$POData_selected_rows,1])
      stmt <- buildDbQuery("getProgOasisFileDetails", result$progOasisId)
      progFiles <- executeDbQuery(dbSettings, stmt)
      if (!is.null(progFiles)) {
        result$progFiles <-  progFiles %>%
          mutate(Status = replace(Status, Status == "Failed", StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()
      }
      logMessage("programme files table refreshed")
    } else {
      if (active()) {
        showNotification(type = "warning", "Please select a Programme Model first")
      }
    }
  })

  output$tabledisplayprogoasisfiles <- renderDT(
    if (!is.null(result$progFiles)) {
      datatable(
        result$progFiles,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = "none",
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    })

  ### Programme Model Details
  observeEvent(input$buttonmodeldetails, {
    hide("buttonmodeldetails")
    show("panelModelDetails")
    logMessage("showing panelModelDetails")
    .reloadProgFiles()
  })

  observeEvent(input$buttonhidemodeldetails, {
    show("buttonmodeldetails")
    logMessage("showing panelModelDetails")
    hide("panelModelDetails")
  })

  # > Configure Output ------

  #simplified view selection
  observe( if (active()) {
    if (length(input$chkgulprog) > 0 |  length(input$chkgulstate) > 0 |
        length(input$chkgulcounty) > 0 |  length(input$chkgulloc) > 0 |
        length(input$chkgullob) > 0 | length(input$chkgulpolicy) > 0) {
      updateCheckboxInput(session, "chkinputGUL", value = TRUE)
      disable("chkgulpolicy")
    }
    if (length(input$chkilprog) > 0 |  length(input$chkilstate) > 0 |
        length(input$chkilcounty) > 0 |  length(input$chkilloc) > 0 |
        length(input$chkillob) > 0 | length(input$chkilpolicy) > 0) {
      updateCheckboxInput(session, "chkinputIL", value = TRUE)
    }
  })

  # Select/deselect GUL
  observeEvent(input$chkinputGUL, {
    if (input$chkinputGUL == FALSE) {
      .clearchkboxGULgrp()
    }  else {
      disable("chkgulpolicy")
      gullistlength <- length(input$chkgulprog) + length(input$chkgulstate) +
        length(input$chkgulcounty) + length(input$chkgulloc) +
        length(input$chkgullob) + length(input$chkgulpolicy)
      if (gullistlength == 0) {
        for (i in checkgulgrplist) {
          updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesGUL)
        }
      }
    }
  })

  # Select/deselect IL
  observeEvent(input$chkinputIL, {
    if (input$chkinputIL == FALSE) {
      .clearchkboxILgrp()
    } else {
      illistlength <- length(input$chkilprog) + length(input$chkilstate) +
        length(input$chkilcounty) + length(input$chkilloc) +
        length(input$chkillob) + length(input$chkilpolicy)
      if (illistlength == 0) {
        for (i in checkilgrplist) {
          updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesIL)
        }
      }
    }
  })

  # reactive expression yielding the output options as a list
  outputOptionsList <- reactive(paste(collapse = ",", c(
    input$chkinputGUL, input$chkgulprog, input$chkgulpolicy,
    input$chkgulstate, input$chkgulcounty, input$chkgulloc,
    input$chkgullob,
    input$chkinputIL, input$chkilprog, input$chkilpolicy,
    input$chkilstate, input$chkilcounty, input$chkilloc,
    input$chkillob)))

  # Update button in sidebar panel to update checkboxes for pre-populated values
  observe(if (active()) {

    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp(session)
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .clearchkboxILgrp()

    if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "<Select>") {
      outputlist <- executeDbQuery(dbSettings,
                                   buildDbQuery("getOutputOptionOutputs", input$sinoutputoptions))

      if (nrow(outputlist) > 0) {
        for (i in 1:nrow(outputlist)) {
          grpid <- paste0("chk",outputlist$Group[i])
          grpinputid <- strsplit(toString(grpid), " ")[[1]]
          chkboxid <- outputlist$Parameter[i]
          selchoices <- as.list(strsplit(toString(chkboxid), ",")[[1]])
          updateCheckboxGroupInput(session, inputId = grpinputid, selected = c(selchoices))
        }
      }
    }
  })

  # Clear the checkbox groups and preset dropdown - Set back to default
  onclick("abtnclroutopt", {
    .defaultview(session)
    .clearOutputOptions()
  })

  # show advanced view
  onclick("abtnadvanced", {
    .advancedview()
  })

  # show basic view
  onclick("abtnbasic", {
    .basicview()
  })


  # Save output for later use as presets
  onclick("abuttonsaveoutput", {
    if (outputOptionsList() != "") {
      toggleModal(session, "bsmodalsaveoutput", toggle = "open")
    } else {
      toggleModal(session, "bsmodalsaveoutput", toggle = "close")
      showNotification(type = "warning", "Please select Output")
    }
  })

  # Submit output configuration (to be saved)
  onclick("abuttonsubmitoutput", {
    if (input$tinputoutputname == "") {
      showNotification(type = "warning", "Please enter Output Name")
    } else {
      stmt <- paste0("exec dbo.saveoutputoption @OutputOptionName = '",
                     input$tinputoutputname, "',@OutputOptionsList = '",
                     outputOptionsList(), "'")
      executeDbQuery(dbSettings, stmt)
      updateTextInput(session, "tinputoutputname", value = "")
      showNotification(type = "message", "Output saved.")
      toggleModal(session, "bsmodalsaveoutput", toggle = "close")
      .clearOutputOptions()
      toggleModal(session, "bsmodalrunparam", toggle = "close")
      #.defaultview(session)
    }
  })

  # A function to generate process run
  .generateRun <- function() {
    prTable <- getProcessData(dbSettings, userId(), 0, 0, 0)
    prgId <- result$POData[input$tableProgOasisOOK_rows_selected,1]
    result$progOasisID <- toString(prgId)

    processrunname <- isolate(input$tinputprocessrunname)
    nosample <- isolate(input$tinputnoofsample)
    sthreshold <- isolate(input$tinputthreshold)
    eventsetid <- isolate(input$sinputeventset)
    eventoccid <- isolate(input$sinputeventocc)

    windperil <- NULL
    surgeperil <- NULL
    quakeperil <- NULL
    floodperil <- NULL
    dmdsurge <- NULL
    leakagefactor <- NULL

    summaryreports <- tolower(isolate(input$chkinputsummaryoption))

    # functionality to handle model resource based metrics
    stmt <- buildDbQuery("getRuntimeParamList", result$progOasisID)
    runparamlist <- executeDbQuery(dbSettings, stmt)

    rows <- nrow(runparamlist)
    if (rows > 0) {
      for (i in 1:rows) {
        if (runparamlist[i, 1] == 'demand_surge') {
          dmdsurge <- tolower(isolate(input$chkinputdsurge))
          next
        }
        if (runparamlist[i, 1] == 'peril_wind') {
          windperil <- tolower(isolate(input$chkinputprwind))
          next
        }
        if (runparamlist[i, 1] == 'peril_surge') {
          surgeperil <- tolower(isolate(input$chkinputprstsurge))
          next
        }
        if (runparamlist[i, 1] == 'peril_quake') {
          quakeperil <- tolower(isolate(input$chkinputprquake))
          next
        }
        if (runparamlist[i, 1] == 'peril_flood') {
          floodperil <- tolower(isolate(input$chkinputprflood))
          next
        }
        if (runparamlist[i, 1] == 'leakage_factor') {
          leakagefactor <- isolate(input$sliderleakagefac)
        }
      }
    }

    outputsStringGUL <- paste(collapse = ", ",
                              c(input$chkgulprog, input$chkgulpolicy, input$chkgulstate,
                                input$chkgulcounty, input$chkgulloc, input$chkgullob))

    outputsStringIL <- paste(collapse = ", ",
                             c(input$chkilprog, input$chkilpolicy, input$chkilstate,
                               input$chkilcounty, input$chkilloc, input$chkillob))

    stmt <- paste0("exec dbo.WorkflowFlattener ",
                   "@ProgOasisID= ", result$progOasisID, ", ",
                   "@WorkflowID= 1", ", ",
                   "@NumberOfSamples=", nosample, ", ",
                   "@GULThreshold= ", sthreshold, ", ",
                   "@UseRandomNumberFile= 0, ",
                   "@OutputsStringGUL= '", outputsStringGUL, "', ",
                   "@OutputsStringIL= '", outputsStringIL, "', ",
                   "@EventSetID= '", eventsetid ,"', ",
                   "@EventOccurrenceID= '", eventoccid, "', ",
                   "@PerilWind = '", windperil ,"', ",
                   "@PerilSurge='", surgeperil, "', ",
                   "@PerilQuake='", quakeperil, "', ",
                   "@PerilFlood='", floodperil, "', ",
                   "@DemandSurge= '", dmdsurge, "', ",
                   "@LeakageFactor= '" , leakagefactor, "', ",
                   "@ProcessRunName= '" , processrunname, "', ",
                   "@SummaryReports='", summaryreports , "'")

    logMessage(paste("Workflow flattener query: ", stmt))
    runId <- executeDbQuery(dbSettings, stmt)
    logMessage(paste("Process Run ID: ", runId))

    return(runId)
  }

  # Execute Process run: When "Execute Run" button is clicked - switsches view to Run panel
  onclick("abuttonexecuteprrun", {
    if (outputOptionsList() == "") {
      showNotification(type = "warning", "Please select Output")
    } else {
      runId <- .generateRun()
      if (is.null(runId)) {
        showNotification(type = "error",
                         "Process Run ID could not be generated. So process run cannot be executed.")
      } else {
        status <- runProcess(apiSettings, runId)
        workflowSteps$update("4")
        if (grepl("success", status, ignore.case = TRUE)) {
          showNotification(type = "message",
                           sprintf("Created Process Run ID: %s and process run is executing.",
                                   runId))
          .reloadRunData()
        } else {
          showNotification(type = "warning",
                           sprintf("Created Process Run ID: %s. But process run executing failed.",
                                   runId))
          hide("abuttondisplayoutput")
          show("panelProcessRunLogs")
          logMessage("showing prrunlogtable")
          hide("abuttonshowlog")
        }
      }
      .defaultview(session)
    }
  })

  # Panel Run ------------------------------------------------------------------

  ### > selectprogOasisID ----

  # observeEvent(preselProcId(), {
  #   if (preselProcId() != -1) {
  #     result$POData_selected_rows <- match(c(preselProcId()), result$POData[[1]])
  #   } else {
  #     result$POData_selected_rows <- 1
  #   }
  # })

  # Add choices possibilities to selectprogOasisID
  observeEvent(result$POData, {
    if (active()) {
      if (!is.null(result$POData) & input$selectprogrammeID != "") {
        logMessage(paste("updating selectprogOasisID choices based on Programme Model Table"))
        updateSelectInput(session, inputId = "selectprogOasisID", choices = result$POData[, 1])
      }
    }
  })

  # Preselect selectprogOasisID
  # observeEvent(result$POData_selected_rows, {
  #   if (result$POData_selected_rows != 0 ) {
  #     prgId <- result$POData[input$tableProgOasisOOK_rows_selected,1]
  #     updateSelectInput(session, inputId = "selectprogOasisID", choices = result$POData[, 1], selected = prgId)
  #   }
  # })

  # If selectprogOasisID changes, reload progcess run table and set view back to default
  observeEvent(input$selectprogOasisID, {
    .reloadRunData()
    if (workflowSteps$step() == "4") {
      hide("panelDefineOutputs")
      hide("panelProcessRunLogs")
      #consider cases of programmes wtithout a run process
      if (is.na(input$selectprogOasisID) ) {
        hide("panelProcessRunTable")
        showNotification(type = "warning", "Please select a Programme with a valid Oasis Programme ID.")
      } else {
        show("panelProcessRunTable")
      }
    }
    if (!is.null(result$POData)) {
      rowToSelect <- which(result$POData[, 1] == input$selectprogOasisID)
      selectRows(dataTableProxy("tableProgOasisOOK"), rowToSelect)
    }
  })

  ### > Process Run Table -----

  .getProcessRunWithUserChoices <- function(pruser, prmodel, prprogramme,
                                            prworkflow) {
    if (active()) {
      prtable <- getProcessData(dbSettings, pruser, prmodel, prprogramme, prworkflow)
      # Rule is, for one process ID, pass that process ID in, for all
      # processes pass a null.  For processes in all states (completed,
      # created, in progress etc), pass 'All', for just in progress pass
      # 'In Progress'
      prcid <-  input$selectprogOasisID

      AllOrInProgress <- isolate(input$radioprrunsAllOrInProgress)
      if (AllOrInProgress == "In_Progress") {
        AllOrInProgress = "In Progress"
      }

      prcrundata <- getProcessRun(dbSettings, prcid, AllOrInProgress)

      if (!is.null(prcrundata)) {
        if (nrow(prcrundata) > 0 ) {
          show("tableprocessrundata")
          show("divprocessRunButtons")
          result$prcrundata <- prcrundata %>%
            mutate(ProcessRunStatus = replace(ProcessRunStatus, grepl("Failed", ProcessRunStatus, ignore.case = TRUE) | grepl("Cancelled", ProcessRunStatus, ignore.case = TRUE) | is.na(ProcessRunStatus), StatusFailed)) %>%
            mutate(ProcessRunStatus = replace(ProcessRunStatus, !grepl("Completed", ProcessRunStatus, ignore.case = TRUE) & !grepl("Failed", ProcessRunStatus, ignore.case = TRUE) & !grepl("Cancelled", ProcessRunStatus, ignore.case = TRUE) & ProcessRunStatus != StatusFailed & ProcessRunStatus != StatusCompleted, StatusProcessing)) %>%
            mutate(ProcessRunStatus = replace(ProcessRunStatus, grepl("Completed", ProcessRunStatus, ignore.case = TRUE), StatusCompleted)) %>%
            as.data.frame()

        } else {
          hide("tableprocessrundata")
          hide("divprocessRunButtons")
          if (workflowSteps$step() == "4") {
            showNotification(type = "warning", "Current Programme does not have any assocated run Process")
          }
        }
      }
    }
  }

  observeEvent(result$prcrundataCounter, {
    # reload if radio buttons for 'All' vs 'In_Progress' change
    input$radioprrunsAllOrInProgress
    if (!is.null(input$selectprogOasisID) ) {
      if (workflowSteps$step() == "4") {
        show("panelProcessRunTable")
      }
      .getProcessRunWithUserChoices(userId(), 0, 0, 0)
    } else {
      hide("panelProcessRunTable")
    }
    logMessage("process run table refreshed")
  })

  output$tableprocessrundata <- renderDT(if (!is.null(result$prcrundata)) {
    if (preselRunId() == -1) {
      index <- 1
    } else {
      index <- match(c(preselRunId()), result$prcrundata[[1]])
    }

    datatable(
      result$prcrundata,
      class = "flamingo-table display",
      rownames = TRUE,
      selection = list(mode = 'single',
                       selected = rownames(result$prcrundata)[c(as.integer(index))]),
      escape = FALSE,
      colnames = c('Row Number' = 1),
      filter = 'bottom',
      options = .getPRTableOptions()
    )
  })

  ### > Navigation ----

  #Allow display output option only if run successfull. Otherwise default view is logs
  observeEvent(result$prcrundata_selected_rows, {
    if (result$prcrundata_selected_rows != 0 & !is.null(result$prcrundata)) {
      hide("panelDefineOutputs")
      if (result$prcrundata[result$prcrundata_selected_rows, "ProcessRunStatus"] != StatusCompleted) {
        hide("abuttondisplayoutput")
        hide("abuttonshowlog")
        show("panelProcessRunLogs")
        logMessage("showing prrunlogtable")
      } else {
        show("abuttondisplayoutput")
        show("abuttonshowlog")
        hide("panelProcessRunLogs")
      }
    }
  })

  #hide process run section if DC returns empty table
  observe(
    if (!is.null(result$prcrundata)) {
      if (nrow(result$prcrundata) == 0) {
        hide("abuttondisplayoutput")
        hide("divProcessRun")
        show("divhelpProcessRun")
      } else {
        show("abuttondisplayoutput")
        show("divProcessRun")
        hide("divhelpProcessRun")
      }
    })

  # Go to browse section
  observeEvent(input$abuttondisplayoutput, {
    updateNavigation(navigation_state, "BR")
  })

  ### Hide Output Configuration panel
  observeEvent(input$abuttonehidepanelconfigureoutput, {
    hide("panelDefineOutputs")
    hide("abuttonehidepanelconfigureoutput")
  })

  ### > Rerun Process ----

  #RunId of selected row
  observeEvent(result$prcrundata_selected_rows,{
    if (!is.null(result$prcrundata_selected_rows)) {
      result$prrunid <- (result$prcrundata[result$prcrundata_selected_rows, 1][length(result$prcrundata[result$prcrundata_selected_rows, 1])])
    }
  })


  onclick("abuttonrerunpr", {
    if (result$prcrundata_selected_rows != 0) {
      outputlist <- executeDbQuery(dbSettings, paste0("exec dbo.getOutputOptionOutputs @processrunid = ", result$prrunid ))
      runparamsforpr <- executeDbQuery(dbSettings, paste0("exec dbo.getProcessRunParams ", result$prrunid ))

      show("panelDefineOutputs")
      show("abuttonehidepanelconfigureoutput")

      updateTextInput(session, "tinputprocessrunname", value = result$prcrundata[result$prcrundata_selected_rows, 2])

      if (nrow(runparamsforpr) > 0) {
        for (i in 1:nrow(runparamsforpr)) {
          if (runparamsforpr[i,1] == "number_of_samples") {
            updateTextInput(session, "tinputnoofsample", value = runparamsforpr[i,2])
            next
          }
          if (runparamsforpr[i,1] == "gul_threshold") {
            updateTextInput(session, "tinputthreshold", value = runparamsforpr[i,2])
            next
          }
          if (runparamsforpr[i,1] == "event_set") {
            updateSelectInput(session, "sinputeventset", selected = runparamsforpr[i,2])
            next
          }
          if (runparamsforpr[i,1] == "event_occurrence_id") {
            updateSelectInput(session, "sinputeventocc", selected = runparamsforpr[i,2])
            next
          }
          if (runparamsforpr[i,1] == "peril_wind") {
            updateCheckboxInput(session, "chkinputprwind", value = eval(parse(text = toString(runparamsforpr[i,2]))))
            next
          }
          if (runparamsforpr[i,1] == "peril_surge") {
            updateCheckboxInput(session, "chkinputprstsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))
            next
          }
          if (runparamsforpr[i,1] == "peril_quake") {
            updateCheckboxInput(session, "chkinputprquake", value = eval(parse(text = toString(runparamsforpr[i,2]))))
            next
          }
          if (runparamsforpr[i,1] == "peril_flood") {
            updateCheckboxInput(session, "chkinputprflood", value = eval(parse(text = toString(runparamsforpr[i,2]))))
            next
          }
          if (runparamsforpr[i,1] == "demand_surge") {
            updateCheckboxInput(session, "chkinputdsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))
            next
          }
          if (runparamsforpr[i,1] == "leakage_factor") {
            updateSliderInput(session, "sliderleakagefac", value = runparamsforpr[i,2])
            next
          }
        }
      }
      orows <- nrow(outputlist)
      if (orows > 0) {
        for (i in 1:orows) {
          grpid <- paste0("chk",outputlist$Group[i])
          grpinputid <- strsplit(toString(grpid), " ")[[1]]
          chkboxid <- outputlist$Parameter[i]
          selchoices <- as.list(strsplit(toString(chkboxid), ",")[[1]])
          updateCheckboxGroupInput(session, inputId = grpinputid, selected = c(selchoices))
        }
      }
    } else {
      showNotification(type = "warning", "Please select Process Run")
    }
  })

  ### > Logs ---------------------------------------------------------------

  observeEvent( input$abuttonshowlog, {
    if (result$prcrundata_selected_rows != 0) {
      show("panelProcessRunLogs")
      logMessage("showing prrunlogtable")
      hide("abuttonshowlog")
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
      hide("panelProcessRunLogs")
      show("abuttonshowlog")
    }
  })

  observeEvent(input$abuttonhidelog, {
    hide("panelProcessRunLogs")
    show("abuttonshowlog")
  })

  ### Log Table

  output$tablelog <- renderDT({
    if (result$prcrundata_selected_rows != 0) {
      # manual refresh button
      force(result$logsCounter)
      # reload automatically every so often
      #invalidateLater(reloadMillis)
      wfid <- result$prcrundata[result$prcrundata_selected_rows, 1][length(result$prcrundata[result$prcrundata_selected_rows, 1])]

      logMessage("log table refreshed")

      datatable(
        getProcessRunDetails(dbSettings, wfid) %>%
          mutate(Status = replace(Status, grepl("Failed", Status, ignore.case = TRUE) | grepl("Cancelled", Status, ignore.case = TRUE) , StatusFailed)) %>%
          mutate(Status = replace(Status, !grepl("Success", Status, ignore.case = TRUE) & !grepl("Failed", Status, ignore.case = TRUE) & !grepl("Cancelled", Status, ignore.case = TRUE) & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, grepl("Success", Status, ignore.case = TRUE), StatusCompleted)) %>%
          as.data.frame(),
        class = "flamingo-table display",
        rownames = TRUE,
        selection = "none",
        escape = FALSE,
        colnames = c('Row Number' = 1),
        filter = 'bottom',
        options = .getPRTableOptions()
      )
    }
  })

  # rowselected reactives including no selection -----------------------------

  # observeEvent(result$DPProgDataCounter, {
  #   if (length(input$tableDPprog_rows_selected) > 0) {
  #     result$DPProgData_selected_rows <- input$tableDPprog_rows_selected
  #   } else {
  #     result$DPProgData_selected_rows <- 0
  #   }
  # })

  observeEvent(input$tableDPprog_rows_selected, {
    if (length(input$tableDPprog_rows_selected) > 0) {
      logMessage(paste("input$tableDPprog_rows_selected is:", input$tableDPprog_rows_selected))
      result$DPProgData_selected_rows <- input$tableDPprog_rows_selected
      prgId <- result$DPProgData[result$DPProgData_selected_rows, 1]
      logMessage(paste("updating selectprogrammeID because selection in programme table changed to",  prgId))
      updateSelectInput(session, inputId = "selectprogrammeID", selected = prgId)
    } else {
      result$DPProgData_selected_rows <- 0
    }
  })

  observeEvent(input$tableProgOasisOOK_rows_selected, {
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      result$POData_selected_rows <- input$tableProgOasisOOK_rows_selected
      prgId <- result$POData[result$POData_selected_rows, 1]
      logMessage(paste("updating selectprogOasisID because selection in programme model table changed to",  prgId))
      updateSelectInput(session, inputId = "selectprogOasisID", selected = prgId)
    } else {
      result$POData_selected_rows <- 0
    }
  })

  observe({
    .reloadRunData()
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      result$prcrundata_selected_rows <- input$tableprocessrundata_rows_selected
    } else {
      result$prcrundata_selected_rows <- 0
    }
  })

  # refresh buttons  --------------------------------------------------------

  #Reload Programme table
  observeEvent(input$abuttonprgtblrfsh, {.reloadDPProgData()})

  #Reload Programme Details table
  observeEvent(input$abuttondefprogrfsh, {.reloadProgDetails()})

  #Reload Programme Model table
  observeEvent(input$abuttonookrefresh, {.reloadPOData()})

  #Reload Programme Model Details table
  observeEvent(input$abuttonprgoasisrfsh, {.reloadProgFiles()})

  #Reload Process Runs table
  observeEvent(input$abuttonrefreshprrun, {.reloadRunData()})

  #Reload Process Logs table
  observeEvent(input$abuttonrefreshprrunlogs, {.reloadLogsData()})

  # Help Functions General ----------------------------------------------------

  # hide all panels
  .hideDivs <- function(){
    hide("panelDefineProgramme")
    hide("panelProgrammeTable")
    hide("panelProgrammeDetails")
    hide("panelAssociateModel")
    hide("panelDefineIDs")
    hide("panelProgrammeModelTable")
    hide("panelModelDetails")
    hide("panelDefineOutputs")
    hide("panelDefineOutputsDetails")
    hide("panelDefineOutputConfiguration")
    hide("panelProcessRunTable")
    hide("panelProcessRunLogs")
  }

  # default view for panels
  .defaultCreateProg <- function() {
    hide("abuttonhidedefineprogpanel")
    .clearDPAccountSelection()
    .clearProgrammeName()
    .clearSourceFilesSelection()
    .clearTransformNameSelection()
    show("panelProgrammeTable")
    show("panelDefineProgramme")
    result$prog_flag <- "C"
  }

  .defaultAssociateModel <- function() {
    show("abuttonhidedefineprogpanel")
    show("buttonprogdetails")
    show("panelProgrammeTable")
    show("panelAssociateModel")
    show("buttonmodeldetails")
    hide("panelProgrammeDetails")
    hide("panelDefineProgramme")
    result$prog_flag <- "A"
  }

  .defaultConfigOutput <- function() {
    hide("divselectprogOasisID")
    show("buttonmodeldetails")
    show("abtnadvanced")
    hide("abtnbasic")
    hide("abuttonsaveoutput")
    hide("abtnclroutopt")
    hide("configureAdvancedGUL")
    hide("configureAdvancedIL")
    hide("abuttonehidepanelconfigureoutput")
    hide("configureModelParamsAdvanced")
    show("panelDefineIDs")
    if (!is.null(input$selectprogrammeID)) {
      show("panelProgrammeModelTable")
      show("panelDefineOutputs")
    }
  }

  .defaultRun <- function() {
    show("abuttonehidepanelconfigureoutput")
    show("panelDefineIDs")
    show("divselectprogOasisID")
    if (!is.null(input$selectprogrammeID) & !is.null(input$selectprogOasisID)) {
      show("panelProcessRunTable")
      show("abuttonshowlog")
    }
  }

  # table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      processing = 0,
      pageLength = 5,
      autoWidth = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    return(options)
  }


  # reload functions --------------------------------------------------------

  # Reload Programme table
  .reloadDPProgData <- function() {
    if (active()) {
      stmt <- buildDbQuery("getProgData")
      DPProgData <- executeDbQuery(dbSettings, stmt)
      if (!is.null(DPProgData)) {
        result$DPProgData <- DPProgData %>%
          mutate(Status = replace(Status, Status == "Failed" | is.na(Status), StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()
        logMessage("programme table refreshed")
      }
    }
    invisible()
    #result$DPProgData_selected_rows <- 1
  }

  #Reload Programme Details table
  .reloadProgDetails <- function() {
    result$progDetailsCounter <- isolate(result$progDetailsCounter + 1)
  }

  #Reload Programme Model table
  .reloadPOData <- function() {
    result$PODataCounter <- result$PODataCounter + 1
    message(paste("PODataCounter is:", isolate(result$PODataCounter)))
    invisible()
    #result$POData_selected_rows <- 1
  }

  #Reload Programme Model Details table
  .reloadProgFiles <- function() {
    result$progFilesCounter <- isolate(result$progFilesCounter + 1)
  }

  #Reload Process Runs table
  .reloadRunData <- function() {
    result$prcrundataCounter <- isolate(result$prcrundataCounter + 1)
    result$prcrundata_selected_rows <- 1
  }

  #Reload Process Logs table
  .reloadLogsData <- function() {
    result$logsCounter <- isolate(result$logsCounter + 1)
  }

  # Helper Functions Create Programme ---------------------------------------

  .clearDPAccountSelection <- function() {
    accounts <- getAccountName(dbSettings)
    updateSelectInput(session, "sinputDPAccountName",
                      choices = createSelectOptions(accounts, "Select Account"),
                      selected = c("Select Account" = 0))
  }

  .clearProgrammeName <- function() {
    updateTextInput(session, "tinputDPProgName", value = "")
  }

  .clearSourceFilesSelection <- function(){
    updateSelectInput(session, "sinputSLFile", selected = "")
    updateSelectInput(session, "sinputSAFile", selected = "")
    hide("divSLFileSelect")
    hide("divSLFileUpload")
    hide("divSAFileSelect")
    hide("divSAFileUpload")
  }

  .clearTransformNameSelection <- function() {
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectInput(session, "sinputTransformname",
                      choices = createSelectOptions(transforms, "Select Transform",
                                                    labelCol = 1, valueCol = 2),
                      selected = c("Select Transform" = 0))
  }

  .updateDPAccountSelection <- function() {
    accounts <- getAccountName(dbSettings)
    updateSelectInput(session, "sinputDPAccountName",
                      choices = createSelectOptions(accounts, "Select Account"),
                      selected = toString(result$DPProgData[(input$tableDPprog_rows_selected),3]))
  }

  .updateProgrammeName <- function() {
    updateTextInput(session, "tinputDPProgName",
                    value = result$DPProgData[input$tableDPprog_rows_selected,2])
  }

  .updateTransformNameSelection <- function() {
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectInput(session, "sinputTransformname",
                      choices = createSelectOptions(transforms, "Select Transform",
                                                    labelCol = 1, valueCol = 2),
                      selected = toString(result$DPProgData[(input$tableDPprog_rows_selected),5]))
  }

  # Helper Functions Create Programme Model ---------------------------------

  .clearOOKProgrammeSelection <- function() {
    programmes <- getProgrammeList(dbSettings)
    updateSelectInput(session, "sinputookprogid",
                      choices = createSelectOptions(programmes, "Select Programme"),
                      selected = c("Select Programme" = 0))
  }

  .clearOOKModelSelection <- function() {
    models <- getModelList(dbSettings)
    updateSelectInput(session, "sinputookmodelid",
                      choices = createSelectOptions(models, "Select Model"),
                      selected = c("Select Model" = 0))
  }

  .clearOOKTransformSelection <- function() {
    transforms <- getTransformNameCanModel(dbSettings)
    updateSelectInput(session, "sinputProgModTransform",
                      choices = createSelectOptions(transforms, "Select Transform",
                                                    labelCol = 1, valueCol = 2),
                      selected = c("Select Transform" = 0))
  }

  .clearOOKSidebar <- function() {
    .clearOOKProgrammeSelection()
    .clearOOKModelSelection()
    .clearOOKTransformSelection()
  }

  .defaultOOKSidebar <- function() {
    .updateOOKProgrammeSelection()
    .clearOOKModelSelection()
    .clearOOKTransformSelection()
  }

  .updateOOKProgrammeSelection <- function() {
    programmes <- getProgrammeList(dbSettings)
    updateSelectInput(session, "sinputookprogid",
                      choices = createSelectOptions(programmes, "Select Programme"),
                      selected = toString(result$DPProgData[(input$tableDPprog_rows_selected),1]))
  }

  # Helper Functions Configure Output ---------------------------------------

  # Clear checkboxgroups
  .clearchkboxGULgrp <- function() {
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
    disable("chkgulpolicy")
  }

  # Clear checkboxgroup
  .clearchkboxILgrp <- function() {
    for (i in checkilgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }

  # Clear other runtime params
  .clearotherparams <- function() {
    updateSelectInput(session, "sinoutputoptions",
                      choices = c("<Select>", getOutputOptions(dbSettings)),
                      selected = "<Select>")
    updateTextInput(session, "tinputprocessrunname", value = "")
    updateSliderInput(session, "sliderleakagefac", "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)
    if (result$progOasisId != -1) {
      updateSelectInput(session, "sinputeventset",
                        choices = getEventSet(dbSettings, result$progOasisId ))
      updateSelectInput(session, "sinputeventocc",
                        choices = getEventOccurrence(dbSettings, result$progOasisId ))
    }
    updateCheckboxInput(session, "chkinputprwind", "Peril: Wind", value = TRUE)
    updateCheckboxInput(session, "chkinputprstsurge", "Peril: Surge", value = TRUE)
    updateCheckboxInput(session, "chkinputprquake", "Peril: Quake", value = TRUE)
    updateCheckboxInput(session, "chkinputprflood", "Peril: Flood", value = TRUE)
    updateCheckboxInput(session, "chkinputdsurge", "Demand Surge", value = TRUE)
  }

  #Clear Custom Configuration option
  .clearOutputOptions <- function() {
    updateSelectInput(session, "sinoutputoptions",
                      choices = c("<Select>", getOutputOptions(dbSettings)),
                      selected = "<Select>")
  }

  #Output view
  .advancedview <- function() {
    logMessage("showing advanced view")
    show("configureAdvancedGUL")
    show("configureAdvancedIL")
    show("configureModelParamsAdvanced")
    show("abtnbasic")
    hide("abtnadvanced")
    show("abuttonsaveoutput")
    show("abtnclroutopt")
  }

  .basicview <- function() {
    logMessage("showing basic view")
    hide("configureAdvancedGUL")
    hide("configureAdvancedIL")
    hide("configureModelParamsAdvanced")
    hide("abtnbasic")
    show("abtnadvanced")
    hide("abuttonsaveoutput")
    hide("abtnclroutopt")
  }

  # Default output configuration options
  .defaultchkboxGULgrp <- function(session) {
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesGUL)
    }
  }

  .defaultchkboxILgrp <- function() {
    .clearchkboxILgrp()
  }

  .defaultview <- function(session) {
    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp(session)
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .defaultchkboxILgrp()
    .clearotherparams()
    .clearOutputOptions()
    .basicview()
    .shownconfigOutputView()
  }

  .shownconfigOutputView <- function() {
    show("panelDefineOutputs")
    disable("chkgulpolicy")
  }

  .hiddenconfigOutputView <- function(textMessage) {
    hide("panelDefineOutputs")
    if (active()) {
      if (workflowSteps$step() == "3") {
        showNotification(type = "warning", textMessage)
      }
    }
  }

  # Model Outout ------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      progOasisId = reactive(result$progOasisId),
      processRunId = reactive(result$prrunid)
    )
  )

  moduleOutput

}

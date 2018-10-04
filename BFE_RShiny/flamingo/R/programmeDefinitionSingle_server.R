#' Single Programme Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return For \code{programmeDefinitionSingle()}, list of reactives.
#' @template return-outputNavigation
#' @rdname programmeDefinitionSingle
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDT dataTableProxy selectRows DTOutput
#' @importFrom dplyr mutate select case_when
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

  stop_selProgID <- check_selProgID <- 0

  result <- reactiveValues(
    # reactive values for the programme table
    DPProgData = NULL,
    # SL file to view
    viewSLfile = NULL,
    # SA file to view
    viewSAfile = NULL,
    # reactive value for the programme details table
    progDetails = NULL,
    # reactive values for the programme model table
    POData = NULL,
    # reactive value for the programme model detail table
    progFiles = NULL,
    # reactive values for the process runs table
    prcrundata = NULL,
    #Id of the Programme Model
    progOasisId = -1,
    # Id of the Process Run
    prrunid = -1,
    # flag to know if the user is creating or amending a programme
    # TODO:
    prog_flag = "C"
  )

  workflowSteps <- callModule(singleProgrammeWorkflowSteps, "workflowsteps")

  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")

  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")

  checkrigrplist <- c("chkriprog", "chkristate", "chkricounty", "chkriloc", "chkrilob", "chkripolicy")

  # Newly created account needs to be added to choices
  observe(if (active()) {
    accounts <- getAccountName(dbSettings)
    updateSelectInput(session, "sinputDPAccountName",
                      choices = createSelectOptions(accounts, "Select Account"),
                      selected = isolate(input$sinputDPAccountName))
    # initialSelection()
  })

  # Panels switch ------------------------------------------------------------
  # Make sure the first view is reset to first panel if accessing directly and to panel 4 if coming from Browse
  observe(if (active()) {
    workflowSteps$update(preselPanel())
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
        .defaultOOKSidebar()
        #.reloadDPProgData()
      },
      "3" = {
        logMessage("showing panelDefineIDs panelProgrammeModelTable panelDefineOutputs")
        .hideDivs()
        .defaultConfigOutput()
        .defaultview(session)
        #.reloadPOData()
      },
      "4" = {
        logMessage("showing panelDefineIDs panelProcessRunTable")
        .hideDivs()
        .defaultRun()
        #.reloadRunData()
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
      if (result$prog_flag == "A" && length(input$tableDPprog_rows_selected) > 0) {
        idxSel <- input$tableDPprog_rows_selected
        query <- paste0("exec dbo.updateProg ", result$DPProgData[input$tableDPprog_rows_selected, 1],
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

    # Reload Programme Table
    .reloadDPProgData()
    logMessage(paste("updating tableDPprog select because programme table was reloaded:", idxSel))
    selectRows(dataTableProxy("tableDPprog"), idxSel)
    logMessage(paste("selected row is:", input$tableDPprog_rows_selected))
  })

  ### Clear Programme Definition panel
  onclick("abuttonProgCancel", {
    result$prog_flag <- "C"
    .clearDPAccountSelection()
    .clearProgrammeName()
    .clearSourceFilesSelection()
    .clearTransformNameSelection()
  })

  ### Load Programme Button
  onclick("buttonloadcanmodpr", {
    if (length(input$tableDPprog_rows_selected) > 0) {
      #loadprogdata <- "success"
      #progId = result$DPProgData[input$tableDPprog_rows_selected, 1]
      #logMessage(paste("loading programme - progId is:", progId))
      loadprogdata <- loadProgrammeData(
        apiSettings,
        progId = result$DPProgData[input$tableDPprog_rows_selected, 1]
      )
      if (loadprogdata == 'success' || loadprogdata == 'Success') {
        showNotification(type = "message", "Initiating load programme data...")
        workflowSteps$update("2")
      } else {
        showNotification(type = "error", "Failed to load programme data.")
      }
      .reloadDPProgData()
    } else {
      showNotification(type = "warning", "Please select a Programme to load programme data.")
    }
  })

  ### > Source Files ----


  ### Upload Location/Account File
  .uploadSourceFile <- function(inFile, recordIdString, recordIdCode){
    flc <- getFileLocationPath(dbSettings, "Exposure File")
    flcopy <- file.copy(inFile$datapath, file.path(flc, inFile[1, 1]), overwrite = TRUE)
    logMessage(file.path(flc, inFile[1, 1]))
    if (length(input$tableDPprog_rows_selected) > 0) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(
          dbSettings, inFile[1, 1], recordIdString, recordIdCode, flc, userId(),
          "Prog", result$DPProgData[input$tableDPprog_rows_selected, 1]
        )
        if (!is.null(recordId)) {
          showNotification(type = "message",
                           paste("New File record id: ", recordId, " created."))
          .reloadProgDetails()
        } else {
          showNotification(type = "error", "Could not create file record.")
        }
      } else {
        showNotification(type = "error", "File transfer failed.")
      }
    }
  }

  onclick("abuttonSLFileUpload", {
    .uploadSourceFile(inFile = input$SLFile, recordIdString = "Source Loc File", recordIdCode = 101)
  })

  onclick("abuttonSAFileUpload", {
    .uploadSourceFile(inFile = input$SAFile, recordIdString = "Source Acc File", recordIdCode = 102)
  })

  onclick("abuttonSRFileUpload", {
    .uploadSourceFile(inFile = input$SRFile, recordIdString = "Source Reinsurance File", recordIdCode = 401)
  })

  onclick("abuttonSRSFileUpload", {
    .uploadSourceFile(inFile = input$SRSFile, recordIdString = "Source Reinsurance Scope File", recordIdCode = 402)
  })

  ### Link Location/Account File

  .linkSourceFile <- function(query, inputID) {
    if (length(input$tableDPprog_rows_selected) > 0) {
      res <- executeDbQuery(dbSettings,
                            paste(query,
                                  inputID, ", ",
                                  result$DPProgData[input$tableDPprog_rows_selected, 1]))
      if (inputID != "") {
        if (is.null(res)) {
          showNotification(type = "error", "Failed to link the File!")
        } else {
          showNotification(type = "message",
                           paste("Location File linked to Programme", result$DPProgData[input$tableDPprog_rows_selected, 2]))
        }
      } else {
        showNotification(type = "warning", "Please select a file to Link")
      }
    }
  }


  onclick("abuttonSLFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceLocationFileForProg ", inputID = input$sinputselectSLFile)
  })

  onclick("abuttonSAFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceAccountFileForProg ", inputID = input$sinputselectSAFile)
  })

  # onclick("abuttonSRFileLink", {
  #   .linkSourceFile(query = "exec dbo.updateSourceAccountFileForProg ", inputID = input$sinputselectSRFile)
  # })

  # onclick("abuttonSRSFileLink", {
  #   .linkSourceFile(query = "exec dbo.updateSourceAccountFileForProg ", inputID = input$sinputselectSRSFile)
  # })

  # File upload or select from dropdown
  # Location file
  observe(if (active()) {
    if (input$sinputSLFile == "U") {
      show("divSLFileUpload")
      disable("abuttonSLFileUpload")
      hide("divSLFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SLFile
      if (!is.null(inFile)) {
        enable("abuttonSLFileUpload")
      }
    } else if (input$sinputSLFile == "S") {
      show("divSLFileSelect")
      hide("divSLFileUpload")
      SLfiles <- getFileSourceLocationFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSLFile",
        choices = createSelectOptions(SLfiles, labelCol = 1, valueCol = 2)
      )
    }
  })

  # Account file
  observe(if (active()) {
    if (input$sinputSAFile == "U") {
      show("divSAFileUpload")
      disable("abuttonSAFileUpload")
      hide("divSAFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SAFile
      if (!is.null(inFile)) {
        enable("abuttonSAFileUpload")
      }
    } else if (input$sinputSAFile == "S") {
      show("divSAFileSelect")
      hide("divSAFileUpload")
      SAfiles <- getFileSourceAccountFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSAFile",
        choices = createSelectOptions(SAfiles, labelCol = 1, valueCol = 2)
      )
    }
  })

  # SR file
  observe(if (active()) {
    if (input$sinputSRFile == "U") {
      show("divSRFileUpload")
      disable("abuttonSRFileUpload")
      hide("divSRFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SRFile
      if (!is.null(inFile)) {
        enable("abuttonSRFileUpload")
      }
    } else if (input$sinputSRFile == "S") {
      show("divSRFileSelect")
      hide("divSRFileUpload")
      #*********SRfiles <- getFileSourceAccountFile(dbSettings)
      #********* updateSelectInput(
      #*********  session, "sinputselectSRFile",
      #*********  choices = createSelectOptions(SRfiles, labelCol = 1, valueCol = 2)
      #*********)
    }
  })

  # SRS file
  observe(if (active()) {
    if (input$sinputSRSFile == "U") {
      show("divSRSFileUpload")
      disable("abuttonSRSFileUpload")
      hide("divSRSFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SRSFile
      if (!is.null(inFile)) {
        enable("abuttonSRSFileUpload")
      }
    } else if (input$sinputSRSFile == "S") {
      show("divSRSFileSelect")
      hide("divSRSFileUpload")
      #*********SRfiles <- getFileSourceAccountFile(dbSettings)
      #********* updateSelectInput(
      #*********  session, "sinputselectSRFile",
      #*********  choices = createSelectOptions(SRfiles, labelCol = 1, valueCol = 2)
      #*********)
    }
  })

  ### View source files
  .modalviewSourcefile <- function(Label, Title, InputID) {
    ns <- session$ns
    modalDialog(label = Label,
                title = Title,
                DTOutput(ns(InputID)),
                size = "l",
                easyClose = TRUE
    )
  }

  .renderDTSourceFile <- function(SourceFile){
    renderDT({
      if (!is.null(SourceFile)) {
        datatable(
          SourceFile,
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
  }

  # Source Location
  onclick("abuttonSLFileView", {
    if (input$sinputselectSLFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSLfile", Title = "Source Location File View", InputID = "tableviewSLfile"))
    } else {
      removeModal()
      showNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSLfile <- .renderDTSourceFile(SourceFile = result$viewSLfile)

  # Source Account
  onclick("abuttonSAFileView", {
    if (input$sinputselectSLFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSAfile", Title = "Source Account File View", InputID = "tableviewSAfile"))
    } else {
      removeModal()
      showNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSAfile <- .renderDTSourceFile(SourceFile = result$viewSAfile)

  # Source Reinsurance
  onclick("abuttonSRFileView", {
    if (input$sinputselectSRFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSRfile", Title = "Source Reinsurance File View", InputID = "tableviewSRfile"))
    } else {
      removeModal()
      showNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSRfile <- .renderDTSourceFile(SourceFile = result$viewSRfile)

  # Source Reinsurance Scope
  onclick("abuttonSRSFileView", {
    if (input$sinputselectSRSFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSRSfile", Title = "Source Reinsurance Scope File View", InputID = "tableviewSRSfile"))
    } else {
      removeModal()
      showNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSRSfile <-  .renderDTSourceFile(SourceFile = result$viewSRSfile)

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
  output$paneltitleAssociateModel <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, 1]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, 2]
    paste0("Associate Model to Programme", " - ", progName, " (id: ", progId, ")")
  })

  #  Programme Model Table title

  output$paneltitleProgrammeModelTable <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, 1]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, 2]
    paste0("Models Table Programme", " - ", progName," (id: ", progId, ")")
  })

  #  Details Programme Model title

  output$paneltitleProgrammeModelDetails <- renderUI({
    progName <- result$DPProgData[input$tableDPprog_rows_selected, 2]
    paste0("Details Programme Model", " - ", progName, " (id: ", result$progOasisId, ")")
  })


  ### > Programme Actions ----

  # Create/Amend programme title

  output$paneltitleDefineProgramme <- renderUI({
    if (result$prog_flag == "C" || is.null(input$tableDPprog_rows_selected)) {
      "Create Programme"
    } else if (result$prog_flag == "A") {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, 1]
      progName <- result$DPProgData[input$tableDPprog_rows_selected, 2]
      paste0("Amend Programme", "- ", progName, " (id: ", progId, ")")
    }

  })

  # Create Programme
  onclick("buttoncreatepr", {
      # updateActionButton(session = session, inputId = ns("abuttonProgSubmit"), label = "Create")
      result$prog_flag <- "C"
      .clearDPAccountSelection()
      .clearProgrammeName()
      .clearSourceFilesSelection()
      .clearTransformNameSelection()
      show("panelDefineProgramme")
      show("abuttonhidedefineprogpanel")
      logMessage("showing panelDefineProgramme")
  })

  # Show Amend Programme
  observeEvent(input$buttoncreatepr, {
    show("panelDefineProgramme")
  })

  observeEvent(input$abuttonhidecreatepr, {
    hide("panelDefineProgramme")
    show("buttoncreatepr")
  })

  # Amend Programme
  onclick("buttonamendpr", {
    if (length(input$tableDPprog_rows_selected) > 0) {
      # updateActionButton(session, ns("abuttonProgSubmit") , label = "Amend")
      # TODO: review where/when/how this should be set
      result$prog_flag <- "A"
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

  # Show Amend Programme
  observeEvent(input$buttonamendpr, {
    show("panelDefineProgramme")
  })

  observeEvent(input$abuttonhidecreatepr, {
    hide("panelDefineProgramme")
    show("buttonamendpr")
  })


  # Delete Programme
  onclick("buttondeletepr",{
    if (length(input$tableDPprog_rows_selected) > 0) {
      stmt <- buildDbQuery("deleteProg", result$DPProgData[input$tableDPprog_rows_selected, 1])
      executeDbQuery(dbSettings, stmt)
      showNotification(type = "message",
                       sprintf("Programme %s deleted", result$DPProgData[input$tableDPprog_rows_selected, 2]))
      .reloadDPProgData()
    } else {
      showNotification(type = "warning", "Please select a Programme to Delete")
    }
  })


  # > Programme Details Table-----
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

  output$paneltitleProgrammeDetails <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, 1]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, 2]
    paste0("- ", progName, " (id: ", progId, ")")
  })

  # Show Details Programme
  observeEvent(input$buttonprogdetails, {
    show("panelProgrammeDetails")
    .reloadProgDetails()
  })

  observeEvent(input$buttonhideprogdetails, {
    hide("panelProgrammeDetails")
    show("buttonprogdetails")
  })

  # Manage panels to show when selected row changes
  observeEvent(input$tableDPprog_rows_selected, {
    if (active()) {
      if (workflowSteps$step() == "2") {
        .defaultAssociateModel()
        .defaultOOKSidebar()
        .reloadProgDetails()
      }
      if (workflowSteps$step() == "1") {
        .defaultCreateProg()
        .reloadProgDetails()
      }
    }
    if (length(input$tableDPprog_rows_selected) > 0) {
      # if (active()) {
      #   if (workflowSteps$step() == "3") {
      #     #show("panelAssociateModel")
      #   }
      # }
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
  }, ignoreNULL = FALSE)

  # > Create Model ----
  # on click of create prog oasis button - Creates and loads the model - sends user to Configure Output panel
  onclick("abuttoncrprogoasis", {
    if (isolate(input$sinputookprogid) > 0 && isolate(input$sinputookmodelid) > 0) {
      result$progOasisId <- createProgOasis(dbSettings,
                                            isolate(input$sinputookprogid),
                                            isolate(input$sinputookmodelid),
                                            isolate(input$sinputProgModTransform))
      if (is.null(result$progOasisId)) {
        showNotification(type = "error",
                         paste("No Prog Oasis created"))
      } else {
        showNotification(type = "message",
                         paste("Prog Oasis id:", result$progOasisId,  " created."))
        .clearOOKSidebar()
        workflowSteps$update("3")
        .reloadPOData()
        idxSel <- match(result$progOasisId, result$POData[, 1])
        selectRows(dataTableProxy("tableProgOasisOOK"), idxSel)
        loadprogmodel <- loadProgrammeModel(
          apiSettings,
          progOasisId = toString(result$POData[input$tableProgOasisOOK_rows_selected, 1])
        )
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
  # Add choices to selectprogrammeID, update selectprogrammeID
  observeEvent(result$DPProgData, {
      logMessage(paste0("updating selectprogrammeID choices because programme table was reloaded - contains ", nrow(result$DPProgData), " rows"))
      if (input$selectprogrammeID == "")
        prgId <- 1
      else
        prgId <- input$selectprogrammeID
      updateSelectInput(session, inputId = "selectprogrammeID", choices = result$DPProgData[, 1], selected = prgId)
  })

  # If selectprogrammeID changes, reload models table programme and set view back to default
  observeEvent(input$selectprogrammeID, {
    bl_dirty <- stop_selProgID > check_selProgID
    logMessage(paste("--- stop_selProgID is:", stop_selProgID))
    if (active()) {
      show("buttonmodeldetails")
      hide("panelModelDetails")
      if (!is.null(result$DPProgData) && !bl_dirty) {
        logMessage(paste("updating tableDPprog select because selectprogrammeID changed to", input$selectprogrammeID))
        rowToSelect <- which(result$DPProgData[, 1] == input$selectprogrammeID)
        if(is.null(input$tableDPprog_rows_selected)){
          selectRows(dataTableProxy("tableDPprog"), rowToSelect)
        } else if (rowToSelect != input$tableDPprog_rows_selected)
          selectRows(dataTableProxy("tableDPprog"), rowToSelect)
          # re-selecting the same row would trigger event-observers on input$tableDPprog_rows_selected
      }
      .reloadPOData()
    }
    if (bl_dirty) check_selProgID <<- check_selProgID + 1
  })

  ### > selectprogOasisID ----
  # Add choices possibilities to selectprogOasisID
  observeEvent(result$POData, {
    if (active()) {
      if (input$selectprogrammeID != "") {
        logMessage(paste("updating selectprogOasisID choices based on Programme Model Table"))
        updateSelectInput(session, inputId = "selectprogOasisID", choices = result$POData[, 1])
      }
    }
  })

  # Output configuration: manage what to show based on  status of row selected in programme Model table
  observeEvent(input$tableProgOasisOOK_rows_selected, {
    if (length(input$tableProgOasisOOK_rows_selected) > 0 && !is.null(result$POData)) {
        if (result$POData[input$tableProgOasisOOK_rows_selected, "Status"] == StatusCompleted) {
          if (active()) {
            if (workflowSteps$step() == "3") {
              .defaultview(session)
              show("panelDefineOutputs")
            }
          }
          # Show perils according to programme
          prtable <- getProcessData(dbSettings, userId(), 0, 0, 0)
          procId <- toString(prtable[input$tableProgOasisOOK_rows_selected, 1])
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
    }
  })

  ### > Models Table Programme (previously OOK) ----
  # Manage what to show based on rows being selected in programme Model table
  observeEvent(input$tableProgOasisOOK_rows_selected, {
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
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
  }, ignoreNULL = FALSE)

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

  ### Details Programme Model
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
  # simplified view selection
  observe(if (active()) {
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
    if (length(input$chkriprog) > 0 |  length(input$chkristate) > 0 |
        length(input$chkricounty) > 0 |  length(input$chkriloc) > 0 |
        length(input$chkrilob) > 0 | length(input$chkripolicy) > 0) {
      updateCheckboxInput(session, "chkinputRI", value = TRUE)
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
      if (length(input$chkilprog) == 0 &  length(input$chkilstate) == 0 &
          length(input$chkilcounty) == 0 &  length(input$chkilloc) == 0 &
          length(input$chkillob) == 0 & length(input$chkilpolicy) == 0) {
        for (i in checkilgrplist) {
          updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesIL)
        }
      }
    }
  })

  # Select/deselect RI
  observeEvent(input$chkinputRI, {
    if (input$chkinputRI == FALSE) {
      .clearchkboxRIgrp()
    } else {
      if (length(input$chkriprog) == 0 &  length(input$chkristate) == 0 &
          length(input$chkricounty) == 0 &  length(input$chkriloc) == 0 &
          length(input$chkrilob) == 0 & length(input$chkripolicy) == 0) {
        for (i in checkrigrplist) {
          updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesRI)
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
    input$chkillob,
    input$chkinputRI, input$chkriprog, input$chkripolicy,
    input$chkristate, input$chkricounty, input$chkriloc,
    input$chkrilob
    )))

  # Update button in sidebar panel to update checkboxes for pre-populated values
  observe(if (active()) {

    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp(session)
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .clearchkboxILgrp()
    updateCheckboxInput(session, "chkinputRI", value = FALSE)
    .clearchkboxRIgrp()

    if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "<Select>") {
      outputlist <- executeDbQuery(dbSettings,
                                   buildDbQuery("getOutputOptionOutputs", input$sinoutputoptions))

      if (nrow(outputlist) > 0) {
        # print(paste0("outputlist"))
        # print(outputlist)
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
  .modalsaveoutput <- function() {
    ns <- session$ns
    modalDialog(label = "modalsaveoutput",
                title = "Save Configuration",
                textInput(ns("tinputoutputname"), label = "Configuration Name:", value = ""),
                footer = tagList(
                  actionButton(inputId = ns("abuttonsubmitoutput"),
                               label = "Submit", class = "btn btn-primary")
                ),
                size = "s",
                easyClose = TRUE
    )
  }

  onclick("abuttonsaveoutput", {
    if (outputOptionsList() != "") {
      showModal(.modalsaveoutput())
    } else {
      removeModal()
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
      removeModal()
      .clearOutputOptions()
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
        switch(runparamlist[i, 1],
               'demand_surge'   = {dmdsurge <- tolower(isolate(input$chkinputdsurge))},
               'peril_wind'     = {windperil <- tolower(isolate(input$chkinputprwind))},
               'peril_surge'    = {surgeperil <- tolower(isolate(input$chkinputprstsurge))},
               'peril_quake'    = {quakeperil <- tolower(isolate(input$chkinputprquake))},
               'peril_flood'    = {floodperil <- tolower(isolate(input$chkinputprflood))},
               'leakage_factor' = {leakagefactor <- isolate(input$sliderleakagefac)}
        )
        # if (runparamlist[i, 1] == 'demand_surge') {
        #   dmdsurge <- tolower(isolate(input$chkinputdsurge))
        #   next
        # }
        # if (runparamlist[i, 1] == 'peril_wind') {
        #   windperil <- tolower(isolate(input$chkinputprwind))
        #   next
        # }
        # if (runparamlist[i, 1] == 'peril_surge') {
        #   surgeperil <- tolower(isolate(input$chkinputprstsurge))
        #   next
        # }
        # if (runparamlist[i, 1] == 'peril_quake') {
        #   quakeperil <- tolower(isolate(input$chkinputprquake))
        #   next
        # }
        # if (runparamlist[i, 1] == 'peril_flood') {
        #   floodperil <- tolower(isolate(input$chkinputprflood))
        #   next
        # }
        # if (runparamlist[i, 1] == 'leakage_factor') {
        #   leakagefactor <- isolate(input$sliderleakagefac)
        # }
      }
    }

    outputsStringGUL <- paste(collapse = ", ",
                              c(input$chkgulprog, input$chkgulpolicy, input$chkgulstate,
                                input$chkgulcounty, input$chkgulloc, input$chkgullob))

    outputsStringIL <- paste(collapse = ", ",
                             c(input$chkilprog, input$chkilpolicy, input$chkilstate,
                               input$chkilcounty, input$chkilloc, input$chkillob))

    outputsStringRI <- paste(collapse = ", ",
                             c(input$chkriprog, input$chkripolicy, input$chkristate,
                               input$chkricounty, input$chkriloc, input$chkrilob))

    stmt <- paste0("exec dbo.WorkflowFlattener ",
                   "@ProgOasisID= ", result$progOasisID, ", ",
                   "@WorkflowID= 1", ", ",
                   "@NumberOfSamples=", nosample, ", ",
                   "@GULThreshold= ", sthreshold, ", ",
                   "@UseRandomNumberFile= 0, ",
                   "@OutputsStringGUL= '", outputsStringGUL, "', ",
                   "@OutputsStringIL= '", outputsStringIL, "', ",
                   "@OutputsStringRI= '", outputsStringRI, "', ",
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
          logMessage(paste("colnames are:", paste(colnames(result$prcrundata), collapse = ", ")))
          rowToSelect <- match(runId, result$prcrundata[, 1])
          selectRows(dataTableProxy("tableprocessrundata"), rowToSelect)
        } else {
          showNotification(type = "warning",
                           sprintf("Created Process Run ID: %s. But process run executing failed.",
                                   runId))
          show("panelProcessRunLogs")
          logMessage("showing prrunlogtable")
          # hide("abuttonshowlog")
        }
      }
      .defaultview(session)
    }
  })

  # Panel Run ------------------------------------------------------------------

  ### > selectprogOasisID ----
  # If selectprogOasisID changes, reload process run table and set view back to default
  observeEvent(input$selectprogOasisID, {
    # reload set of runs
    .reloadRunData()
    if (is.null(result$prcrundata)) {
      if (!is.null(input$tableprocessrundata_rows_selected))
        logWarning(paste("*** STOPPP - input$tableprocessrundata_rows_selected should be NULL"))
    } else {
      selectRows(dataTableProxy("tableprocessrundata"), 1)
    }

    if (workflowSteps$step() == "4") {
      hide("panelDefineOutputs")
      hide("panelProcessRunLogs")
      # consider cases of programmes without a run process
      if (is.na(input$selectprogOasisID)) {
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
  # reload if radio buttons for 'All' vs 'In_Progress' change

  output$paneltitlepanelProcessRunTable <- renderUI({
    progOasisId <- result$POData[input$tableProgOasisOOK_rows_selected, 1]
    progOasisName <- result$POData[input$tableProgOasisOOK_rows_selected, 2]
    paste0("Process Runs", " - ", progOasisName," (id: ", progOasisId, ")")
  })

  observeEvent(input$radioprrunsAllOrInProgress, {
    if (active()) .reloadRunData()
  })

  .getProcessRunWithUserChoices <- function(pruser, prmodel, prprogramme,
                                            prworkflow) {
    if (active()) {
      prtable <- getProcessData(dbSettings, pruser, prmodel, prprogramme, prworkflow)
      # Rule is, for one process ID, pass that process ID in, for all
      # processes pass a null. For processes in all states (completed,
      # created, in progress etc), pass 'All', for just in progress pass
      # 'In Progress'
      prcid <- input$selectprogOasisID

      AllOrInProgress <- isolate(input$radioprrunsAllOrInProgress)
      if (AllOrInProgress == "In_Progress") {
        AllOrInProgress = "In Progress"
      }

      prcrundata <- getProcessRun(dbSettings, prcid, AllOrInProgress)

      # case_when
      if (input$radioprrunsAllOrInProgress == "In_Progress") {
        prcrundata <- prcrundata %>% filter(ProcessRunStatus == StatusProcessing)
      }

      StatusGood <- "Completed"
      StatusBad <- c("Failed", "Cancelled", NA_character_)
      '%notin%' <- Negate('%in%')

      # RSc TODO: should probably allow NULL to clear connections when selecting
      # a ProgOasisID that has no runs
      if (!is.null(prcrundata)) {
        if (nrow(prcrundata) > 0 ) {
          show("tableprocessrundata")
          show("divprocessRunButtons")
          result$prcrundata <- prcrundata %>%
            mutate(ProcessRunStatus =
                     case_when(ProcessRunStatus %in% StatusGood ~ StatusCompleted,
                             ProcessRunStatus %in% StatusBad ~ StatusFailed,
                             ProcessRunStatus %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
          # result$prcrundata <- prcrundata %>%
          #   mutate(ProcessRunStatus = replace(ProcessRunStatus, grepl("Failed", ProcessRunStatus, ignore.case = TRUE) | grepl("Cancelled", ProcessRunStatus, ignore.case = TRUE) | is.na(ProcessRunStatus), StatusFailed)) %>%
          #   mutate(ProcessRunStatus = replace(ProcessRunStatus, !grepl("Completed", ProcessRunStatus, ignore.case = TRUE) & !grepl("Failed", ProcessRunStatus, ignore.case = TRUE) & !grepl("Cancelled", ProcessRunStatus, ignore.case = TRUE) & ProcessRunStatus != StatusFailed & ProcessRunStatus != StatusCompleted, StatusProcessing)) %>%
          #   mutate(ProcessRunStatus = replace(ProcessRunStatus, grepl("Completed", ProcessRunStatus, ignore.case = TRUE), StatusCompleted)) %>%
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

  # Allow display output option only if run successful. Otherwise default view is logs
  observeEvent(input$tableprocessrundata_rows_selected, {
    logMessage(paste("*** input$tableprocessrundata_rows_selected is:", input$tableprocessrundata_rows_selected))
    if (length(input$tableprocessrundata_rows_selected) > 0 && !is.null(result$prcrundata)) {
      if (workflowSteps$step() == "4") {
        hide("panelDefineOutputs")
        if (result$prcrundata[input$tableprocessrundata_rows_selected, "ProcessRunStatus"] != StatusCompleted) {
          # hide("abuttondisplayoutput")
          # hide("abuttonshowlog")
          show("panelProcessRunLogs")
          logMessage("showing prrunlogtable")
        } else {
          show("abuttondisplayoutput")
          show("abuttonshowlog")
          hide("panelProcessRunLogs")
        }
      }
    }
  }) #, ignoreNULL = FALSE)


  # hide process run section if DC returns empty table
  observeEvent(result$prcrundata, {
    #if (!is.null(result$prcrundata)) {
      if (nrow(result$prcrundata) == 0) {
        # hide("abuttondisplayoutput")
        hide("divProcessRun")
        show("divhelpProcessRun")
      } else {
        show("abuttondisplayoutput")
        show("divProcessRun")
        hide("divhelpProcessRun")
      }
    #}
  })

  # Go to browse section
  observeEvent(input$abuttondisplayoutput, {
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      updateNavigation(navigation_state, "SBR")
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
    }
  })

  # go to New configuration
  onclick("abuttonconfigoutput", {
    .defaultview(session)
    show("panelDefineOutputs")
    show("abuttonhidepanelconfigureoutput")
    selectRows(dataTableProxy("tableprocessrundata"), selected = NULL)
  })

#   # Go to browse section
#   observeEvent(input$abuttondisplayoutput, {
#     updateNavigation(navigation_state, "SBR")
#   })

  # configuration title
  output$paneltitleReDefineProgramme <- renderUI({
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, 1]
      processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, 2]
      paste0("Re-Define Programme Output", " - ", processRunName, " (id: ", processRunId, ")")
    } else {
      "New Output Configuration"
    }
  })

  # Hide or show Output Configuration panel
  observeEvent(input$abuttonhidepanelconfigureoutput, {
    hide("panelDefineOutputs")
    hide("abuttonhidepanelconfigureoutput")
    show("abuttonconfigoutput")
  })

  # Hide or show Re-Define Output Configuration
  observeEvent(input$abuttonhidepanelconfigureoutput, {
    hide("panelDefineOutputs")
    hide("abuttonhidepanelconfigureoutput")
    show("abuttonrerunpr")
  })

  ### > Re-Define Output Configuration ----
  # RunId of selected row
  observeEvent(input$tableprocessrundata_rows_selected, {
    logMessage(paste("***2 input$tableprocessrundata_rows_selected is:", input$tableprocessrundata_rows_selected))
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      logMessage(paste("input$tableprocessrundata_rows_selected is", input$tableprocessrundata_rows_selected))
      result$prrunid <- result$prcrundata[input$tableprocessrundata_rows_selected, 1]
    } else {
      result$prrunid <- -1
    }
  }, ignoreNULL = FALSE)


  onclick("abuttonrerunpr", {
    if (length(input$tableprocessrundata_rows_selected) > 0) {

      outputlist <- executeDbQuery(dbSettings, paste0("exec dbo.getOutputOptionOutputs @processrunid = ", result$prrunid))
      runparamsforpr <- executeDbQuery(dbSettings, paste0("exec dbo.getProcessRunParams ", result$prrunid))

      show("panelDefineOutputs")
      show("abuttonhidepanelconfigureoutput")

      updateTextInput(session, "tinputprocessrunname", value = result$prcrundata[input$tableprocessrundata_rows_selected, 2])

      if (nrow(runparamsforpr) > 0) {
        for (i in 1:nrow(runparamsforpr)) {
          switch(runparamsforpr[i,1],
                 "number_of_samples" = {updateTextInput(session, "tinputnoofsample", value = runparamsforpr[i,2])},
                 "gul_threshold" = {updateTextInput(session, "tinputthreshold", value = runparamsforpr[i,2])},
                 "event_set" = {updateSelectInput(session, "sinputeventocc", selected = runparamsforpr[i,2])},
                 "event_occurrence_id" = {updateSelectInput(session, "sinputeventocc", selected = runparamsforpr[i,2])},
                 "peril_wind" = {updateCheckboxInput(session, "chkinputprwind", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
                 "peril_surge" = {updateCheckboxInput(session, "chkinputprstsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
                 "peril_quake" = {updateCheckboxInput(session, "chkinputprquake", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
                 "peril_flood" = {updateCheckboxInput(session, "chkinputprflood", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
                 "demand_surge" = {updateCheckboxInput(session, "chkinputdsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
                 "leakage_factor" = {updateSliderInput(session, "sliderleakagefac", value = runparamsforpr[i,2])}
                 )
#
#           if (runparamsforpr[i,1] == "number_of_samples") {
#             updateTextInput(session, "tinputnoofsample", value = runparamsforpr[i,2])
#             next
#           }
#           if (runparamsforpr[i,1] == "gul_threshold") {
#             updateTextInput(session, "tinputthreshold", value = runparamsforpr[i,2])
#             next
#           }
#           if (runparamsforpr[i,1] == "event_set") {
#             updateSelectInput(session, "sinputeventset", selected = runparamsforpr[i,2])
#             next
#           }
#           if (runparamsforpr[i,1] == "event_occurrence_id") {
#             updateSelectInput(session, "sinputeventocc", selected = runparamsforpr[i,2])
#             next
#           }
#           if (runparamsforpr[i,1] == "peril_wind") {
#             updateCheckboxInput(session, "chkinputprwind", value = eval(parse(text = toString(runparamsforpr[i,2]))))
#             next
#           }
#           if (runparamsforpr[i,1] == "peril_surge") {
#             updateCheckboxInput(session, "chkinputprstsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))
#             next
#           }
#           if (runparamsforpr[i,1] == "peril_quake") {
#             updateCheckboxInput(session, "chkinputprquake", value = eval(parse(text = toString(runparamsforpr[i,2]))))
#             next
#           }
#           if (runparamsforpr[i,1] == "peril_flood") {
#             updateCheckboxInput(session, "chkinputprflood", value = eval(parse(text = toString(runparamsforpr[i,2]))))
#             next
#           }
#           if (runparamsforpr[i,1] == "demand_surge") {
#             updateCheckboxInput(session, "chkinputdsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))
#             next
#           }
#           if (runparamsforpr[i,1] == "leakage_factor") {
#             updateSliderInput(session, "sliderleakagefac", value = runparamsforpr[i,2])
#             next
#           }
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
      .clearOutputOptions()
    } else {
      .defaultview(session)
      showNotification(type = "warning", "Please select Process Run")
    }
  })

  ### > Logs ---------------------------------------------------------------
  observeEvent(input$abuttonshowlog, {
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      show("panelProcessRunLogs")
      logMessage("showing prrunlogtable")
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

  # process run logs title
  output$paneltitleProcessRunLogs <- renderUI({
    processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, 1]
    processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, 2]
    paste0("Logs Process Run", " - ", processRunName, " (id: ", processRunId, ")")
  })

  ### Log Table
  output$tablelog <- renderDT({
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      # manual refresh button
      invisible(input$abuttonrefreshprrunlogs)
      # reload automatically every so often
      #invalidateLater(reloadMillis)
      wfid <- result$prcrundata[input$tableprocessrundata_rows_selected, 1]

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
  observeEvent(input$tableDPprog_rows_selected, {
    if (length(input$tableDPprog_rows_selected) > 0) {
      logMessage(paste("input$tableDPprog_rows_selected is:", input$tableDPprog_rows_selected))
      # note that tableDPprog allows single row selection only
      prgId <- result$DPProgData[input$tableDPprog_rows_selected, 1]
      if (prgId != input$selectprogrammeID) {
        # re-selecting the same programme ID in the drop-down would not re-trigger
        # any of the observers of the drop-down, however we then also want to be
        # sure not to increase stop_selProgID!
        logMessage(paste("updating selectprogrammeID because selection in programme table changed to",  prgId))
        stop_selProgID <<- stop_selProgID + 1
        updateSelectInput(session, inputId = "selectprogrammeID", selected = prgId)
      }
    }
  })

  observeEvent(input$tableProgOasisOOK_rows_selected, {
      # note that tableProgOasisOOK has selection-mode "single"
      prgId <- result$POData[input$tableProgOasisOOK_rows_selected, 1]
      logMessage(paste("updating selectprogOasisID because selection in  models table programme changed to",  prgId))
      updateSelectInput(session, inputId = "selectprogOasisID", selected = prgId)
  })

  # refresh buttons  --------------------------------------------------------
  # Reload Programme table
  observeEvent(input$abuttonprgtblrfsh, {.reloadDPProgData()})

  # Reload Details Programme table
  observeEvent(input$abuttondefprogrfsh, {.reloadProgDetails()})

  # Reload Programme Model table
  observeEvent(input$abuttonookrefresh, {.reloadPOData()})

  # Reload Details Programme Model table
  observeEvent(input$abuttonprgoasisrfsh, {.reloadProgFiles()})

  # Reload Process Runs table
  observeEvent(input$abuttonrefreshprrun, {.reloadRunData()})

  # Help Functions General ----------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
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
    show("buttonprogdetails")
    show("buttonamendpr")
    show("buttoncreatepr")
    hide("panelProgrammeDetails")
    show("panelDefineProgramme")
    # result$prog_flag <- "C"
  }

  .defaultAssociateModel <- function() {
    show("abuttonhidedefineprogpanel")
    show("buttonprogdetails")
    show("buttonamendpr")
    show("buttoncreatepr")
    show("panelProgrammeTable")
    show("panelAssociateModel")
    show("buttonmodeldetails")
    hide("panelProgrammeDetails")
    hide("panelDefineProgramme")
    # result$prog_flag <- "A"
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
    hide("configureAdvancedRI")
    hide("abuttonhidepanelconfigureoutput")
    hide("configureModelParamsAdvanced")
    show("panelDefineIDs")
    if (!is.null(input$selectprogrammeID)) {
      show("panelProgrammeModelTable")
      show("panelDefineOutputs")
    }
  }

  .defaultRun <- function() {
    # show("abuttonhidepanelconfigureoutput")
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
  }

  # Reload table of Deatils of Programme
  .reloadProgDetails <- function() {
    if (length(input$tableDPprog_rows_selected) > 0) {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, 1]

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
        result$progDetails  <- NULL
      }
    }
    invisible()
  }

  # Reload table of Programme Model
  .reloadPOData <- function() {
    if (!is.null(input$selectprogrammeID)) {
      POData <- getProgOasisForProgdata(dbSettings, input$selectprogrammeID)
      if (!is.null(POData)) {
        result$POData <- POData %>%
          select(c("ProgOasisId", "ProgName", "ModelName", "TransformName", "SourceFileId", "FileID", "Status")) %>%
          mutate(Status = replace(Status, Status == "Failed" | is.na(Status), StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()
      }
      logMessage("models table programme refreshed")
    } else {
      if (active()) {
        showNotification(type = "warning", "Please select a Programme ID first")
      }
    }
    logMessage(".reloadPOData called")
    invisible()
  }

  # Reload Details Programme Model table
  .reloadProgFiles <- function() {
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      # result$progOasisId is updated when creating a model or when selecting a model
      result$progOasisId <- toString(result$POData[input$tableProgOasisOOK_rows_selected, 1])
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
    invisible()
  }

  # Reload Process Runs table
  .reloadRunData <- function() {
    if (!is.null(input$selectprogOasisID) ) {
      if (workflowSteps$step() == "4") {
        show("panelProcessRunTable")
      }
      .getProcessRunWithUserChoices(userId(), 0, 0, 0)
      logMessage("process run table refreshed")
    } else {
      hide("panelProcessRunTable")
    }
    invisible()
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

  .clearSourceFilesSelection <- function() {
    updateSelectInput(session, "sinputSLFile", selected = "")
    updateSelectInput(session, "sinputSAFile", selected = "")
    updateSelectInput(session, "sinputSRFile", selected = "")
    updateSelectInput(session, "sinputSRSFile", selected = "")
    hide("divSLFileSelect")
    hide("divSLFileUpload")
    hide("divSAFileSelect")
    hide("divSAFileUpload")
    hide("divSRFileSelect")
    hide("divSRFileUpload")
    hide("divSRSFileSelect")
    hide("divSRSFileUpload")
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
                      selected = toString(result$DPProgData[input$tableDPprog_rows_selected, 3]))
  }

  .updateProgrammeName <- function() {
    updateTextInput(session, "tinputDPProgName",
                    value = result$DPProgData[input$tableDPprog_rows_selected, 2])
  }

  .updateTransformNameSelection <- function() {
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectInput(session, "sinputTransformname",
                      choices = createSelectOptions(transforms, "Select Transform",
                                                    labelCol = 1, valueCol = 2),
                      selected = toString(result$DPProgData[input$tableDPprog_rows_selected, 5]))
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
                      selected = toString(result$DPProgData[input$tableDPprog_rows_selected, 1]))
  }

  # Helper Functions Configure Output ---------------------------------------
  # Clear checkboxgroups GUL
  .clearchkboxGULgrp <- function() {
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
    disable("chkgulpolicy")
  }

  # Clear checkboxgroup IL
  .clearchkboxILgrp <- function() {
    for (i in checkilgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }

  # Clear checkboxgroup RI
  .clearchkboxRIgrp <- function() {
    for (i in checkrigrplist) {
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

  # Clear Custom Configuration option
  .clearOutputOptions <- function() {
    updateSelectInput(session, "sinoutputoptions",
                      choices = c("<Select>", getOutputOptions(dbSettings)),
                      selected = "<Select>")
  }

  # Output view
  .advancedview <- function() {
    logMessage("showing advanced view")
    show("configureAdvancedGUL")
    show("configureAdvancedIL")
    show("configureAdvancedRI")
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
    hide("configureAdvancedRI")
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

  .defaultchkboxRIgrp <- function() {
    .clearchkboxRIgrp()
  }


  .defaultview <- function(session) {
    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp(session)
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .defaultchkboxILgrp()
    updateCheckboxInput(session, "chkinputRI", value = FALSE)
    .defaultchkboxRIgrp()
    .clearotherparams()
    .clearOutputOptions()
    .basicview()
    .shownconfigOutputView()
  }

  .shownconfigOutputView <- function() {
    #show("panelDefineOutputs")
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

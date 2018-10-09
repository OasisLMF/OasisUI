#' Single Programme Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return For \code{programmeDefinitionSingle()}, list of reactives.
#' @template return-outputNavigation
#' @rdname programmeDefinitionSingle
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDT dataTableProxy selectRows DTOutput selectPage
#' @importFrom dplyr mutate select case_when
#' @importFrom shinyjs onclick js removeClass addClass
#' @export
programmeDefinitionSingle <- function(input, output, session, dbSettings,
                                      apiSettings, userId, active = reactive(TRUE), logMessage = message,
                                      preselRunId = reactive(-1),
                                      preselProcId = reactive(-1),
                                      preselPanel = "1",
                                      reloadMillis = 10000) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ------------------------------------------
  # Navigation State
  navigation_state <- reactiveNavigation()
  
  #values to stop ping pong effect
  stop_selProgID <- check_selProgID <- 0
  stop_selProgOasisID <- check_selProgOasisID <- 0
  
  #number of Rows per Page in a dataable
  pageLength <- 5
  
  # Help function
  '%notin%' <- Negate('%in%')
  
  # Default checkgroup for  GUL, IL and RI
  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")
  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")
  checkrigrplist <- c("chkriprog", "chkristate", "chkricounty", "chkriloc", "chkrilob", "chkripolicy")
  
  # > Variables for cols positions -----------------------------------------------
  ### Creating Variables for col names of Programme Table
  #result$DPProgData
  #"Programme ID", "Programme Name", "Account ID", "Account Name", "Transform ID", "Transform", "Status"
  DPProgData.ProgrammeID <- "Programme ID" #reactive(names(result$DPProgData)[1])
  DPProgData.ProgrammeName <- "Programme Name" #reactive(names(result$DPProgData)[2])
  DPProgData.AccountID <- "Account ID" #reactive(names(result$DPProgData)[3])
  DPProgData.AccountName <- "Account Name" #reactive(names(result$DPProgData)[4])
  DPProgData.TranformID <- "Transform ID" #reactive(names(result$DPProgData)[5])
  DPProgData.Tranform <- "Transform" #reactive(names(result$DPProgData)[6])
  DPProgData.Status <-  "Status" #reactive(names(result$DPProgData)[7])
  
  ### Creating Variables for col names of Programme Model Table
  # result$POData
  #"ProgOasisId", "ProgName", "ModelName", "TransformName", "SourceFileId", "FileID", "Status"
  POData.ProgOasisId <- "ProgOasisId" #reactive(names(result$POData)[1])
  POData.ProgName <- "ProgName" #reactive(names(result$POData)[2])
  POData.ModelName <- "ModelName" #reactive(names(result$POData)[3])
  POData.TransformName <- "TransformName" #reactive(names(result$POData)[4])
  POData.SourceFileId <- "SourceFileId" #reactive(names(result$POData)[5])
  POData.FileID <- "FileID" #reactive(names(result$POData)[6])
  POData.Status <- "Status" #reactive(names(result$POData)[7])
  
  ### Creating Variables for col names of Process Runs Table
  # result$prcrundata
  #"ProcessRunID", "ProcessRunName", "ProgOasisID", "ProcessRunStatus"
  prcrundata.ProcessRunID <- "ProcessRunID" #reactive(names(result$prcrundata)[1])
  prcrundata.ProcessRunName <- "ProcessRunName" #reactive(names(result$prcrundata)[2])
  prcrundata.ProgOasisID <- "ProgOasisID" #reactive(names(result$prcrundata)[3])
  prcrundata.ProcessRunStatus <- "ProcessRunStatus" #reactive(names(result$prcrundata)[4])
  
  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # reactive values for the programme table
    DPProgData = NULL,
    # SL file to view
    viewSLfile = NULL,
    # SA file to view
    viewSAfile = NULL,
    # reactive value for details of programme table
    progDetails = NULL,
    # reactive values for model table
    POData = NULL,
    # reactive value for detail of model table
    progFiles = NULL,
    # reactive values for process runs table
    prcrundata = NULL,
    #Id of the Programme Model
    #progOasisId = -1,
    # Id of the Process Run
    prrunid = -1,
    # flag to know if the user is creating or amending a programme
    prog_flag = "C"
  )
  
  # Panels switch ------------------------------------------------------------
  # Module to control colors of radio buttons in the singleProgrammeWorkflowSteps
  workflowSteps <- callModule(singleProgrammeWorkflowSteps, "workflowsteps")
  
  # Make sure the first view is reset to first panel if accessing directly and to panel 4 if coming from Browse
  observe(if (active()) {
    workflowSteps$update(preselPanel())
    .reloadDPProgData()
  })
  
  observeEvent(workflowSteps$step(), ignoreInit = TRUE, {
    switch(
      workflowSteps$step(),
      "1" = {
        logMessage("showing Section 'Choose Programme' = '1'")
        .hideDivs()
        .defaultCreateProg()
      },
      "2" = {
        logMessage("showing Section 'Choose Model' = '2'")
        .hideDivs()
        .defaultAssociateModel()
      },
      "3" = {
        logMessage("showing Section 'Configure Output & Run' = '3'")
        .hideDivs()
        .defaultRun()
      }
    )
  })
  
  # Section 'Choose Programme' = '1' ------------------------------------------
  ### > Programme Table ------
  output$tableDPprog <- renderDT({
    # manual refresh button
    invisible(input$abuttonprgtblrfsh)
    
    logMessage("re-rendering programme table")
    if (!is.null(result$DPProgData)) {
      if (isolate(input$selectprogrammeID) != "") {
        rowToSelect <- match(isolate(input$selectprogrammeID), result$DPProgData[, DPProgData.ProgrammeID])
      } else {
        rowToSelect <- 1
      }
      datatable(
        result$DPProgData %>% select(-c(DPProgData.TranformID, DPProgData.AccountID)),
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
      .nothingToShowTable(contentMessage = "No Programme Available")
    }
  })
  
  # > Programme Details Table-----
  output$tableprogdetails <- renderDT({
    if (!is.null(result$progDetails) && nrow(result$progDetails) > 0) {
      
      # manual refresh button
      invisible(input$abuttondefprogrfsh)
      logMessage("re-rendering programme details table")
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
    } else {
      .nothingToShowTable(contentMessage = paste0("No files available for Programme ID ", input$selectprogrammeID))
    }
  })
  
  # Title Programme Details Panel
  output$paneltitleProgrammeDetails <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
    paste0("- ", progName, " (id: ", progId, ")")
  })
  
  # Show Programme Details
  onclick("buttonprogdetails", {
    if (length(input$tableDPprog_rows_selected) > 0) {
      show("panelProgrammeDetails")
      hide("buttonprogdetails")
      logMessage("showing panelProgrammeDetails")
      .reloadProgDetails()
    } else {
      showNotification(type = "warning", "Please select a Programme first")
    }
  })
  
  # Hide Programme Details
  onclick("buttonhideprogdetails", {
    hide("panelProgrammeDetails")
    show("buttonprogdetails")
    logMessage("hiding panelProgrammeDetails")
  })
  
  # > Create / Amend Programme sub-panel -----------------------------------------------
  # Create/Amend programme title
  output$paneltitleDefineProgramme <- renderUI({
    if (result$prog_flag == "C" || is.null(input$tableDPprog_rows_selected)) {
      "Create Programme"
    } else if (result$prog_flag == "A") {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
      progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
      paste0("Amend Programme", "- ", progName, " (id: ", progId, ")")
    }
  })
  
  # Hide Programme Definition Panel
  onclick("abuttonhidedefineprogpanel", {
    hide("panelDefineProgramme")
  })
  
  # Create Programme
  onclick("buttoncreatepr", {
    result$prog_flag <- "C"
    #clear fields
    .clearDPAccountSelection()
    .clearProgrammeName()
    .clearSourceFilesSelection()
    .clearTransformNameSelection()
    show("panelDefineProgramme")
    logMessage("showing panelDefineProgramme")
  })
  
  ### Amend Programme
  onclick("buttonamendpr", {
    if (length(input$tableDPprog_rows_selected) > 0) {
      # TODO: review where/when/how this should be set
      result$prog_flag <- "A"
      #clear fields
      .updateDPAccountSelection()
      .updateProgrammeName()
      .clearSourceFilesSelection()
      .updateTransformNameSelection()
      show("panelDefineProgramme")
      logMessage("showing panelDefineProgramme")
    } else {
      showNotification(type = "warning", "Please select a Programme to Amend")
    }
  })
  
  ### Submit Button
  onclick("abuttonProgSubmit", {
    idxSel <- 1
    pageSel <- 1
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
    } else if (result$prog_flag == "A") { # && length(input$tableDPprog_rows_selected) > 0
      idxSel <- input$tableDPprog_rows_selected
      pageSel <- ceiling(input$tableDPprog_rows_selected/pageLength)
      query <- paste0("exec dbo.updateProg ", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID],
                      ",[", input$tinputDPProgName,"],", input$sinputDPAccountName,
                      ", [", input$sinputTransformname, "]")
      res <- executeDbQuery(dbSettings, query)
      message(paste("A res is:", res))
      if (is.null(res)) {
        showNotification(type = "error",
                         paste("Failed to amend a Programme - ", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]))
      } else {
        showNotification(type = "message",
                         paste("Programme ", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName], " amended."))
      }
    } 
    
    # Reload Programme Table
    .reloadDPProgData()
    logMessage(paste("updating tableDPprog select because programme table was reloaded:", idxSel))
    selectRows(dataTableProxy("tableDPprog"), idxSel)
    selectPage(dataTableProxy("tableDPprog"), pageSel)
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
    progId = result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    logMessage(paste("loading programme - progId is:", progId))
    loadprogdata <- loadProgrammeData(
      apiSettings,
      progId = progId
    )
    if (loadprogdata == 'success' || loadprogdata == 'Success') {
      showNotification(type = "message", "Initiating load programme data...")
      # Going to next step when programme load is successful (but not completed)
      workflowSteps$update("2")
    } else {
      showNotification(type = "error", "Failed to load programme data")
    }
    .reloadDPProgData()
  })
  
  # Delete Programme
  onclick("buttondeletepr",{
    if (length(input$tableDPprog_rows_selected) > 0) {
      stmt <- buildDbQuery("deleteProg", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID])
      executeDbQuery(dbSettings, stmt)
      showNotification(type = "message",
                       sprintf("Programme %s deleted", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]))
      .reloadDPProgData()
    } else {
      showNotification(type = "warning", "Please select a Programme to Delete")
    }
  })
  
  ### > Source Files ----------------------------------------------------------
  ### Upload Location/Account File
  .uploadSourceFile <- function(inFile, recordIdString, recordIdCode){
    flc <- getFileLocationPath(dbSettings, "Exposure File")
    flcopy <- file.copy(inFile$datapath, file.path(flc, inFile[1, 1]), overwrite = TRUE)
    logMessage(file.path(flc, inFile[1, 1]))
    if (length(input$tableDPprog_rows_selected) > 0) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(
          dbSettings, inFile[1, 1], recordIdString, recordIdCode, flc, userId(),
          "Prog", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
        )
        if (!is.null(recordId)) {
          showNotification(type = "message",
                           paste("New File record id: ", recordId, " created"))
          #.reloadProgDetails()
        } else {
          showNotification(type = "error", "Could not create file record")
        }
      } else {
        showNotification(type = "error", "File transfer failed")
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
                                  result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]))
      if (inputID != "") {
        if (is.null(res)) {
          showNotification(type = "error", "Failed to link the File")
        } else {
          showNotification(type = "message",
                           paste("Location File linked to Programme", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]))
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
        logMessage("re-rendering source file")
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
      } else {
        .nothingToShowTable(contentMessage = "nothing to show")
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
  
  
  # > Updates dependent on changed: tableDPprog_rows_selected -------------------
  observe({print("input$tableDPprog_rows_selected")
    print(input$tableDPprog_rows_selected)})
  
  observe({print("input$tableProgOasisOOK_rows_selected")
    print(input$tableProgOasisOOK_rows_selected)})
  
  observe({print("input$tableprocessrundata_rows_selected")
    print(input$tableprocessrundata_rows_selected)})
  
  observe({print("input$selectprogrammeID")
    print(input$selectprogrammeID)})
  
  observe({print("input$selectprogOasisID")
    print(input$selectprogOasisID)})
  
  observe({print("result$prrunid")
    print(result$prrunid)
  })
  
  observeEvent(input$tableDPprog_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    logMessage(paste("input$tableDPprog_rows_selected is changed to:", input$tableDPprog_rows_selected))
    # Update Programme Detail table if row selected changes
    #.reloadProgDetails()
    # Update Associate Model Panel
    .updateOOKProgrammeSelection()
    .clearOOKModelSelection()
    .clearOOKTransformSelection()
    hide("panelProgrammeDetails")
    show("buttonprogdetails")
    hide("panelDefineProgramme")
    
    
    if (length(input$tableDPprog_rows_selected) > 0) {
      # note that tableDPprog allows single row selection only
      prgId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
      logMessage(paste("updating selectprogrammeID because selection in programme table changed to", prgId))
      if (prgId != input$selectprogrammeID) {
        # re-selecting the same programme ID in the drop-down would not re-trigger
        # any of the observers of the drop-down, however we then also want to be
        # sure not to increase stop_selProgID!
        stop_selProgID <<- stop_selProgID + 1
        updateSelectizeInput(session, inputId = "selectprogrammeID", selected = prgId)
      }
    } else {
      updateSelectizeInput(session, inputId = "selectprogrammeID", selected = character(0))
    }
  })
  
  # Section 'Choose Model' = '2' ----------------------------------------------
  ### > Define selectprogrammeID ----
  # Add choices to selectprogrammeID, update selectprogrammeID
  observeEvent(result$DPProgData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    logMessage(paste0("updating selectprogrammeID choices because programme table was reloaded - contains ", nrow(result$DPProgData), " rows"))
    if (input$selectprogrammeID == "") {
      # initial selection last programme created
      prgId <- result$DPProgData[1, DPProgData.ProgrammeID]
    } else {
      # keep current selection
      prgId <- input$selectprogrammeID
    }
    updateSelectizeInput(session, inputId = "selectprogrammeID", choices = c(result$DPProgData[, DPProgData.ProgrammeID]), selected = prgId)
  })
  
  observeEvent( input$sinputookprogid, {
    if (input$selectprogrammeID != input$sinputookprogid){
      #Making sure the two selectize inputs are the same
      updateSelectizeInput(session, inputId = "selectprogrammeID", selected = input$sinputookprogid) 
      # freezeReactiveValue(input, "sinputookprogid")
      # freezeReactiveValue(input, "selectprogrammeID")
    }
  }
  )
  
  # If selectprogrammeID changes, reload programme model table and set view back to default
  observeEvent(input$selectprogrammeID, ignoreInit = TRUE, {
    #Making sure the two selectize inputs are the same
    if (input$selectprogrammeID != input$sinputookprogid) {
      updateSelectizeInput(session, inputId = "sinputookprogid", selected = input$selectprogrammeID)
    }
    bl_dirty <- stop_selProgID > check_selProgID
    logMessage(paste("--- stop_selProgID is:", stop_selProgID))
    logMessage(paste("updating tableDPprog select because selectprogrammeID changed to", input$selectprogrammeID))
    if (input$selectprogrammeID != "") {
      if (!is.null(result$DPProgData) && nrow(result$DPProgData) > 0 && !bl_dirty ) {
        rowToSelect <- match(input$selectprogrammeID, result$DPProgData[, DPProgData.ProgrammeID])
        pageSel <- ceiling(rowToSelect/pageLength)
        #backward propagation
        if (is.null(input$tableDPprog_rows_selected)) {
          if (workflowSteps$step() == '2'){
            selectRows(dataTableProxy("tableDPprog"), rowToSelect)
            selectPage(dataTableProxy("tableDPprog"), pageSel)
            logMessage(paste("selected row is:", input$tableDPprog_rows_selected))
          }
        } else if (rowToSelect != input$tableDPprog_rows_selected) {
          # re-selecting the same row would trigger event-observers on input$tableDPprog_rows_selected
          selectRows(dataTableProxy("tableDPprog"), rowToSelect)
          selectPage(dataTableProxy("tableDPprog"), pageSel)
          logMessage(paste("selected row is:", input$tableDPprog_rows_selected))
        }
      }
    } else {
      selectRows(dataTableProxy("tableDPprog"), NULL)
      selectPage(dataTableProxy("tableDPprog"), 1)
      logMessage(paste("selected row is:", input$tableDPprog_rows_selected))
    }
    .reloadPOData()
    if (bl_dirty) check_selProgID <<- check_selProgID + 1
  })
  
  # > Programme Model Table --------------------------
  output$tableProgOasisOOK <- renderDT(
    if (!is.null(result$POData) && nrow(result$POData) > 0 ) {
      
      # manual refresh button
      invisible(input$abuttonookrefresh)
      
      if (isolate(input$selectprogOasisID) != "") {
        rowToSelect <- match(isolate(input$selectprogOasisID), result$POData[,POData.ProgOasisId])
      } else {
        rowToSelect <- 1
      }
      logMessage("re-rendering programme model table")
      datatable(
        result$POData %>% select(-c(POData.SourceFileId, POData.FileID)),
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = rownames(result$POData)[rowToSelect]),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    } else {
      .nothingToShowTable(contentMessage = paste0("no Models associated with Programme ID ", input$selectprogrammeID))
    }
  )
  
  #  Programme Model Table title
  output$paneltitleProgrammeModelTable <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
    if (input$selectprogrammeID != "") {
      paste0("Models Table for Programme", " - ", progName," (id: ", progId, ")")
    } else {
      paste0("Models Table")
    }
  })
  
  # Associate Model Table Title
  output$paneltitleAssociateModel <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
    if (input$selectprogrammeID != "") {
      paste0("Associate Model to Programme", " - ", progName, " (id: ", progId, ")")
    } else {
      paste0("Associate Model to Programme")
    }
    
  })
  
  # > Model Details Table -----
  output$tabledisplayprogoasisfiles <- renderDT(
    if (!is.null(result$progFiles) && nrow(result$progFiles) > 0 ) {
      
      # manual refresh button
      invisible(input$abuttonprgoasisrfsh)
      logMessage("re-rendering programme model details table")
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
    } else {
      .nothingToShowTable(contentMessage = paste0("no files associated with Model ID ", input$selectprogOasisID ))
    })
  
  # Details Model title
  output$paneltitleProgrammeModelDetails <- renderUI({
    progId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
    paste0("Details Programme Model", " - ", progName, " (id: ", progId, ")")
  })
  
  ### Show/hide Programme Model Details Panel
  onclick("buttonmodeldetails", {
    logMessage("showing panelModelDetails")
    .reloadProgFiles()
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      hide("buttonmodeldetails")
      show("panelModelDetails")
      logMessage("showing panelModelDetails")
    } else {
      showNotification(type = "warning", "Please select a Programme Model first")
    }
  })
  
  onclick("buttonhidemodeldetails", {
    show("buttonmodeldetails")
    hide("panelModelDetails")
    logMessage("hiding panelModelDetails")
  })
  
  ### > Create Model ------
  onclick("abuttoncrprogoasis", {
    if (isolate(input$sinputookprogid) > 0 && isolate(input$sinputookmodelid) > 0) {
      prgId <- createProgOasis(dbSettings,
                               isolate(input$sinputookprogid),
                               isolate(input$sinputookmodelid),
                               isolate(input$sinputProgModTransform))
      prgId <- ifelse(is.null(prgId), -1, prgId)
      if (prgId == -1) {
        showNotification(type = "error", paste("No Prog Oasis created"))
      } else {
        showNotification(type = "message", paste("Prog Oasis id:",prgId, " created"))
        .clearOOKSidebar()
        .reloadPOData()
        logMessage(paste("updating tableProgOasisOOK select because programme model table was reloaded:", idxSel))
        idxSel <- match(prgId, result$POData[, POData.ProgOasisId])
        pageSel <- ceiling(idxSel/pageLength)
        selectRows(dataTableProxy("tableProgOasisOOK"), idxSel)
        selectPage(dataTableProxy("tableProgOasisOOK"), pageSel)
        logMessage(paste("selected row is:", input$tableProgOasisOOK_rows_selected))
        loadprogmodel <- loadProgrammeModel(
          apiSettings,
          progOasisId = toString(result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId])
        )
        if (loadprogmodel == 'success' || loadprogmodel == 'Success') {
          showNotification(type = "message", "Initiating load programme model...")
          #.reloadProgFiles()
          # Going to next step when model load is successful (but not completed)
          workflowSteps$update("3")
        } else {
          showNotification(type = "error", "Failed to load programme model")
        }
      }
    } else{
      showNotification(type = "warning", "Please select both the fields")
    }
  })
  
  # > Updates dependent on changed: tableProgOasisOOK_rows_selected ------------
  observeEvent(input$tableProgOasisOOK_rows_selected, ignoreInit = TRUE, {
    #force collapsed state of Associate Model flamingo panel
    removeClass(id = paste0("progmodel-body"), class = "in")
    removeClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
    addClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
  })
  
  # Output configuration: manage what to show based on  status of row selected in programme Model table
  observeEvent(input$tableProgOasisOOK_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    #.reloadProgFiles()
    show("buttonmodeldetails")
    hide("panelModelDetails")
    
    # Show perils according to programme model
    if (length(input$tableProgOasisOOK_rows_selected) > 0 ) {
      prgId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
      procId <- toString(prgId)
      
      logMessage(paste("updating selectprogOasisID because selection in programme model table changed to",  prgId))
      updateSelectizeInput(session, inputId = "selectprogOasisID", selected = prgId)
      
      if (result$POData[input$tableProgOasisOOK_rows_selected, POData.Status] == StatusCompleted) {
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
      }
      
    } else {
      updateSelectizeInput(session, inputId = "selectprogOasisID", selected = character(0))
    }
  })
  
  # Section 'Configure Output & Run' = '3' ------------------------------------
  ### > Define selectprogOasisID ----
  # Add choices possibilities to selectprogOasisID
  observeEvent(result$POData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (!is.null(result$POData) && nrow(result$POData) > 0) {
      updateSelectizeInput(session, inputId = "selectprogOasisID", choices = c(result$POData[, POData.ProgOasisId]))
      if (input$selectprogrammeID != "") {
        logMessage(paste("updating selectprogOasisID choices based on Programme Model Table"))
        updateSelectizeInput(session, inputId = "selectprogOasisID", selected = result$POData[1, POData.ProgOasisId])
      } else {
        updateSelectizeInput(session, inputId = "selectprogOasisID", selected = character(0))
      }
    } else {
      updateSelectizeInput(session, inputId = "selectprogOasisID", selected = character(0)) 
    }
  })
  
  # If selectprogOasisID changes, reload process run table and set view back to default
  observeEvent(input$selectprogOasisID, ignoreInit = TRUE, {
    bl_dirty1 <- stop_selProgOasisID > check_selProgOasisID
    #.defaultview(session)
    show("buttonmodeldetails")
    hide("panelModelDetails")
    hide("panelDefineOutputs")
    hide("panelProcessRunLogs")
    .reloadRunData()
    logMessage(paste("updating prcrundata select because selectprogOasisID changed to", input$selectprogOasisID))
    if (input$selectprogOasisID != "") {
      if (!is.null(result$POData) && nrow(result$POData) > 0   && !bl_dirty1 ) {
        rowToSelect <- match(input$selectprogOasisID, result$POData[, POData.ProgOasisId])
        pageSel <- ceiling(rowToSelect/pageLength)
        if (!is.null(input$tableProgOasisOOK_rows_selected) && rowToSelect != input$tableProgOasisOOK_rows_selected) {
          # re-selecting the same row would trigger event-observers on input$tableprocessrundata_rows_selected
          selectRows(dataTableProxy("tableProgOasisOOK"), rowToSelect)
          selectPage(dataTableProxy("tableProgOasisOOK"), pageSel)
          logMessage(paste("selected row is:", input$tableProgOasisOOK_rows_selected))
        }
      } 
    } else {
      result$prcrundata <- NULL
      selectRows(dataTableProxy("tableProgOasisOOK"), NULL)
      selectPage(dataTableProxy("tableProgOasisOOK"), 1)
      logMessage(paste("selected row is:", input$tableProgOasisOOK_rows_selected))
    }
    if (bl_dirty1) check_selProgOasisID <<- check_selProgOasisID + 1
  })
  
  
  ### > Process Run Table -----
  # reload if radio buttons for 'All' vs 'In_Progress' change
  observeEvent(input$radioprrunsAllOrInProgress, ignoreInit = TRUE, {
    .reloadRunData()
  })
  
  #Content of the process run table
  .getProcessRunWithUserChoices <- function(pruser, prmodel, prprogramme,
                                            prworkflow) {
    logMessage(".getProcessRunWithUserChoices called")
    #prtable <- getProcessData(dbSettings, pruser, prmodel, prprogramme, prworkflow)
    prcid <- input$selectprogOasisID
    # For processes in all states (completed, created, in progress etc), pass 'All', for just in progress pass
    # 'In Progress' (not handled by stored procedure in the DB due to bug!)
    prcrundata <- getProcessRun(dbSettings, prcid, input$radioprrunsAllOrInProgress)
    StatusGood <- "Completed"
    StatusBad <- c("Failed", "Cancelled", NA_character_)
    # RSc TODO: should probably allow NULL to clear connections when selecting
    # a ProgOasisID that has no runs
    if (!is.null(prcrundata) && nrow(prcrundata) > 0 ) {
      show("tableprocessrundata")
      show("divprocessRunButtons")
      result$prcrundata <- prcrundata %>%
        mutate(ProcessRunStatus = case_when(ProcessRunStatus %in% StatusGood ~ StatusCompleted,
                                            ProcessRunStatus %in% StatusBad ~ StatusFailed,
                                            ProcessRunStatus %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
        as.data.frame()
      #Handling bug for 'In Progress' 
      if (input$radioprrunsAllOrInProgress == "In_Progress") {
        result$prcrundata <- result$prcrundata %>% filter(ProcessRunStatus == StatusProcessing)
      }
    } else {
      result$prcrundata <- NULL
    }
  }
  
  output$tableprocessrundata <- renderDT(
    
    if (!is.null(result$prcrundata) && nrow(result$prcrundata) > 0) {
      
      # manual refresh button
      invisible(input$abuttonrefreshprrun)
      
      # if (preselRunId() == -1) {
      #   index <- 1
      # } else {
      #   index <- match(c(preselRunId()), result$prcrundata[,prcrundata.ProcessRunID])
      # }
      index <- 1
      logMessage("re-rendering process run table")
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
    } else {
      .nothingToShowTable(contentMessage = paste0("no runs available for Model ID ", input$selectprogOasisID))
    })
  
  # Process Run Table Title
  output$paneltitlepanelProcessRunTable <- renderUI({
    # initialize so that the title would look nice even if rowToSelect is NA
    progOasisId <- NULL
    progOasisName <- NULL
    rowToSelect <- match(input$selectprogOasisID, result$POData[, POData.ProgOasisId])
    #rowToSelect can be temporarly NA if selectprogrammeID changes but selectprogOasisID is not yet updated
    if (!is.na(rowToSelect)) {
      progOasisId <- result$POData[rowToSelect, POData.ProgOasisId]
      progOasisName <- result$POData[rowToSelect, POData.ProgName]
      paste0("Process Runs for Model", " - ", progOasisName," (id: ", progOasisId, ")")
    } else {
      paste0("Process Runs")
    }
    
  })
  
  #Not allow any actions if the process run table is empty
  observeEvent(result$prcrundata, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (!is.null(result$prcrundata) && nrow(result$prcrundata) > 0) {
      show("divprocessRunButtons")
    } else {
      hide("divprocessRunButtons")
    }
  })
  
  # > Configure Output --------------------------------------------
  # configuration title
  output$paneltitleReDefineProgramme <- renderUI({
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunID]
      processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunName]
      paste0("Re-Define Output Configuration for Process", " - ", processRunName, " (id: ", processRunId, ")")
    } else {
      "New Output Configuration"
    }
  })
  
  #Show Output Configuration Panel
  onclick("abuttonconfigoutput", {
    if (input$selectprogOasisID != "") {
      .defaultview(session)
      show("panelDefineOutputs")
      logMessage("showing panelDefineOutputs")
      logMessage(paste("updating tableprocessrundataa select because defining new output configuration"))
      selectRows(dataTableProxy("tableprocessrundata"), selected = NULL)
      selectPage(dataTableProxy("tableprocessrundata"), 1)
      logMessage(paste("selected row is:", input$tableprocessrundata_rows_selected))
    } else {
      showNotification(type = "error", "Please select a Programme Model first")
    }
    
  })
  
  onclick("abuttonrerunpr", {
    .defaultview(session)
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      show("panelDefineOutputs")
      logMessage("showing panelDefineOutputs")
      .updateOutputConfig()
    } else {
      showNotification(type = "warning", "Please select Process Run first")
    }
  })
  
  ### Hide Output Configuration panel
  onclick("abuttonehidepanelconfigureoutput", {
    hide("panelDefineOutputs")
  })
  
  # simplified view selection
  observe({
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
  
  # Update button in sidebar panel to update checkboxes for pre-populated values
  observeEvent(input$sinoutputoptions, {
    if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "") {
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
  })
  
  # show advanced view
  onclick("abtnadvanced", {
    .advancedview()
  })
  
  # show basic view
  onclick("abtnbasic", {
    .basicview()
  })
  
  # reactive expression yielding the output options as a list
  outputOptionsList <- reactive({paste(collapse = ",", c(
    input$chkinputGUL, input$chkgulprog, input$chkgulpolicy,
    input$chkgulstate, input$chkgulcounty, input$chkgulloc,
    input$chkgullob,
    input$chkinputIL, input$chkilprog, input$chkilpolicy,
    input$chkilstate, input$chkilcounty, input$chkilloc,
    input$chkillob,
    input$chkinputRI, input$chkriprog, input$chkripolicy,
    input$chkristate, input$chkricounty, input$chkriloc,
    input$chkrilob
  ))})
  
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
      showNotification(type = "warning", "Please select Output Configuration")
    }
  })
  
  # Submit output configuration (to be saved)
  onclick("abuttonsubmitoutput", {
    if (input$tinputoutputname == "") {
      showNotification(type = "warning", "Please enter Output Configuration Name")
    } else {
      stmt <- paste0("exec dbo.saveoutputoption @OutputOptionName = '",
                     input$tinputoutputname, "',@OutputOptionsList = '",
                     outputOptionsList(), "'")
      executeDbQuery(dbSettings, stmt)
      showNotification(type = "message", paste0("Output Configuration ", input$tinputoutputname ," saved"))
      updateTextInput(session, "tinputoutputname", value = "")
      removeModal()
      .clearOutputOptions()
      #.defaultview(session)
    }
  })
  
  
  ### > Run Process --------------------------------------------------------
  # A function to generate process run
  .generateRun <- function() {
    # prTable <- getProcessData(dbSettings, userId(), 0, 0, 0) # Never used
    # prgId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
    # if (is.null(prgId)) { 
    #   result$progOasisID <- -1
    # } else {
    #   result$progOasisID <- toString(prgId)
    # }
    
    processrunname <- isolate(input$tinputprocessrunname)
    nosample <- isolate(input$tinputnoofsample)
    sthreshold <- isolate(input$tinputthreshold)
    eventsetid <- isolate(input$sinputeventset)
    eventoccid <- isolate(input$sinputeventocc)
    
    # windperil <- NULL
    # surgeperil <- NULL
    # quakeperil <- NULL
    # floodperil <- NULL
    # dmdsurge <- NULL
    # leakagefactor <- NULL
    
    summaryreports <- tolower(isolate(input$chkinputsummaryoption))
    
    # functionality to handle model resource based metrics
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      prgId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
      prgId <- ifelse(is.null(prgId), toString(prgId), -1)
    } else {
      prgId <- -1
    }
    stmt <- buildDbQuery("getRuntimeParamList", prgId)
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
                   "@ProgOasisID= ", prgId, ", ",
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
      showNotification(type = "warning", "Please select Output Configuration")
    } else {
      runId <- .generateRun()
      if (is.null(runId)) {
        showNotification(type = "error",
                         "Process Run ID could not be generated. So process run cannot be executed")
      } else {
        status <- runProcess(apiSettings, runId)
        if (grepl("success", status, ignore.case = TRUE)) {
          showNotification(type = "message",
                           sprintf("Created Process Run ID: %s and process run is executing",
                                   runId))
          .reloadRunData()
          #logMessage(paste("colnames are:", paste(colnames(result$prcrundata), collapse = ", ")))
          logMessage(paste("updating tableprocessrundataa select because executing a new run"))
          rowToSelect <- match(runId, result$prcrundata[, prcrundata.ProcessRunID])
          pageSel <- ceiling(rowToSelect/pageLength)
          selectRows(dataTableProxy("tableprocessrundata"), rowToSelect)
          selectPage(dataTableProxy("tableprocessrundata"), pageSel)
          logMessage(paste("selected row is:", input$tableprocessrundata_rows_selected))
        } else {
          showNotification(type = "warning",
                           sprintf("Created Process Run ID: %s. But process run executing failed",
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
  
  ### > Logs ---------------------------------------------------------------
  onclick("abuttonshowlog", {
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      show("panelProcessRunLogs")
      logMessage("showing prrunlogtable")
      hide("abuttonshowlog")
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
    }
  })
  
  onclick("abuttonhidelog", {
    hide("panelProcessRunLogs")
    show("abuttonshowlog")
  })
  
  ### Log Table
  output$tablelog <- renderDT({
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      # manual refresh button
      invisible(input$abuttonrefreshprrunlogs)
      
      wfid <- result$prrunid
      
      StatusGood <- "Success"
      StatusBad <- c("Cancelled", "Failed",  NA_character_)
      logdata <- getProcessRunDetails(dbSettings, wfid) %>%
        mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                  Status %in% StatusBad ~ StatusFailed,
                                  Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
        as.data.frame()
      logMessage("re-rendering process run log table")
      if (!is.null(logdata)) {
        datatable(
          logdata,
          class = "flamingo-table display",
          rownames = TRUE,
          selection = "none",
          escape = FALSE,
          colnames = c('Row Number' = 1),
          filter = 'bottom',
          options = .getPRTableOptions()
        )
      } else {
        .nothingToShowTable(contentMessage = paste0("no log files associated with Process Run ID ", ifelse(!is.null(result$prrunid), result$prrunid, "NULL")))
      }
    }
  })
  
  # run logs title
  output$paneltitleProcessRunLogs <- renderUI({
    processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunID]
    processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunName]
    paste0("Logs", " - ", processRunName, " (id: ", processRunId, ")")
  })
  
  # > Navigation --------------------------------------------------------------
  # hide process run section if DC returns empty table
  observeEvent(result$prcrundata, {
    if (nrow(result$prcrundata) == 0) {
      hide("abuttondisplayoutput")
    } else {
      show("abuttondisplayoutput")
    }
  })
  
  # Go to browse section
  onclick("abuttondisplayoutput", {
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      updateNavigation(navigation_state, "SBR")
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
    }
  })
  
  # > Updates dependent on changed: tableprocessrundata_rows_selected ---------
  # Allow display output option only if run successful. Otherwise default view is logs
  observeEvent(input$tableprocessrundata_rows_selected, ignoreNULL = FALSE, {
    logMessage(paste("input$tableprocessrundata_rows_selected is changed to:", input$tableprocessrundata_rows_selected))
    hide("panelDefineOutputs")
    hide("panelProcessRunLogs")
    show("abuttondisplayoutput")
    show("abuttonshowlog")
    ##### TODO: Do I need the second check in this if????
    if (length(input$tableprocessrundata_rows_selected) > 0 && !is.null(result$prcrundata)) {
      result$prrunid <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunID]
      if (result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunStatus] != StatusCompleted) {
        hide("abuttondisplayoutput")
        hide("abuttonshowlog")
        # This occurs only by changing process run, which is only possible in panel 3
        show("panelProcessRunLogs")
        logMessage("showing prrunlogtable")
      } else {
        show("abuttondisplayoutput")
        show("abuttonshowlog")
      }
    } else {
      result$prrunid <- -1
    }
  }) 
  
  
  # reload functions --------------------------------------------------------
  # Reload Programme table
  .reloadDPProgData <- function() {
    stmt <- buildDbQuery("getProgData")
    DPProgData <- executeDbQuery(dbSettings, stmt)
    StatusGood <- "Loaded"
    StatusBad <- c("Failed", "Cancelled", NA_character_)
    if (!is.null(DPProgData)) {
      result$DPProgData <- DPProgData %>%
        mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                  Status %in% StatusBad ~ StatusFailed,
                                  Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
        as.data.frame()
      logMessage("programme table refreshed")
    }
    logMessage(".reloadDPProgData called")
    invisible()
  }
  
  # Reload Programme Details table
  .reloadProgDetails <- function() {
    if (length(input$tableDPprog_rows_selected) > 0) {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
      
      stmt <- buildDbQuery("getProgFileDetails", progId)
      progDetails <- executeDbQuery(dbSettings, stmt)
      StatusGood <- "Loaded"
      StatusBad <- c("Failed", "Cancelled", NA_character_)
      if (!is.null(progDetails)) {
        result$progDetails  <- progDetails %>%
          mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                    Status %in% StatusBad ~ StatusFailed,
                                    Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
          as.data.frame()
      }
      logMessage("programme details table refreshed")
    } else {
      result$progDetails  <- NULL
    }
    logMessage(".reloadProgDetails called")
    invisible()
  }
  
  # Reload Programme Model table
  .reloadPOData <- function() {
    logMessage(".reloadPOData called")
    if (input$selectprogrammeID != "") {
      POData <- getProgOasisForProgdata(dbSettings, input$selectprogrammeID)
      StatusGood <- "Loaded"
      StatusBad <- c("Failed", "Cancelled", NA_character_)
      if (!is.null(POData)) {
        result$POData <- POData %>%
          select(c(POData.ProgOasisId, POData.ProgName, POData.ModelName, POData.TransformName, POData.SourceFileId, POData.FileID, POData.Status)) %>%
          mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                    Status %in% StatusBad ~ StatusFailed,
                                    Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
          as.data.frame()
      }
      logMessage("programme model table refreshed")
    } else {
      result$POData <- NULL
    }
    invisible()
  }
  
  # Reload Programme Model Details table
  .reloadProgFiles <- function() {
    logMessage(".reloadProgFiles called")
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      prgId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
      stmt <- buildDbQuery("getProgOasisFileDetails", prgId)
      progFiles <- executeDbQuery(dbSettings, stmt)
      StatusGood <- "Loaded"
      StatusBad <- c("Failed", "Cancelled", NA_character_)
      if (!is.null(progFiles)) {
        result$progFiles <-  progFiles %>%
          mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                    Status %in% StatusBad ~ StatusFailed,
                                    Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
          as.data.frame()
      }
      logMessage("files table refreshed")
    } else {
      result$progFiles <- NULL
    }
    invisible()
  }
  
  # Reload Process Runs table
  .reloadRunData <- function() {
    logMessage(".reloadRunData called")
    if (input$selectprogOasisID != "") {
      .getProcessRunWithUserChoices(userId(), 0, 0, 0)
      logMessage("process run table refreshed")
    }  else {
      result$prcrundata <- NULL
    }
    invisible()
  }
  
  # table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      processing = 0,
      pageLength = pageLength,
      #width = "100%",
      #autoWidth = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    return(options)
  }
  
  #empty table
  .nothingToShowTable <- function(contentMessage){
    datatable(
      data.frame(content = contentMessage),
      class = "flamingo-table display",
      selection = "none",
      rownames = FALSE,
      #filter = 'bottom',
      colnames = c(""),
      escape = FALSE,
      options = list(searchHighlight = TRUE)
    )
  }
  
  # Help Functions General ----------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs called")
    # Section "Choose Programme" = "1"
    hide("panelProgrammeTable")
    hide("panelProgrammeDetails")
    hide("panelDefineProgramme")
    #Section "Choose Model" = "2"
    hide("panelDefineIDs")
    hide("panelProgrammeModelTable")
    hide("panelModelDetails")
    hide("panelAssociateModel")
    #Section "Configure Output & Run" = "3"
    hide("panelProcessRunTable")
    hide("panelDefineOutputs")
    hide("panelProcessRunLogs")
  }
  
  #show default view for Section "Choose Programme" = "1"
  .defaultCreateProg <- function(){
    logMessage(".defaultCreateProg called")
    show("panelProgrammeTable")
    show("buttonprogdetails")
  }
  
  #show default view for Section "Choose Model" = "2"
  .defaultAssociateModel <- function(){
    logMessage(".defaultAssociateModel called")
    hide("divselectprogOasisID")
    show("panelDefineIDs")
    show("panelProgrammeModelTable")
    show("buttonmodeldetails")
    show("panelAssociateModel")
    #force collapsed state of Associate Model flamingo panel
    removeClass(id = paste0("progmodel-body"), class = "in")
    removeClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
    addClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
  }
  
  #show default view for Section "Configure Output & Run" = "3"
  .defaultRun <- function(){
    logMessage(".defaultRun called")
    show("panelDefineIDs")
    show("panelProcessRunTable")
    show("divselectprogOasisID")
    disable("chkgulpolicy")
  }
  
  # Helper Functions 'Choose Programme' = '1' ---------------------------------------
  .clearDPAccountSelection <- function() {
    logMessage(".clearDPAccountSelection called")
    accounts <- getAccountName(dbSettings)
    updateSelectizeInput(session, "sinputDPAccountName",
                         choices = createSelectOptions(accounts),
                         selected = character(0))
  }
  
  .clearProgrammeName <- function() {
    logMessage(".clearProgrammeName called")
    updateTextInput(session, "tinputDPProgName", value = "")
  }
  
  .clearSourceFilesSelection <- function() {
    logMessage(".clearSourceFilesSelection called")
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
    logMessage(".clearTransformNameSelection called")
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectizeInput(session, "sinputTransformname",
                         choices = createSelectOptions(transforms,
                                                       labelCol = 1, valueCol = 2),
                         selected = character(0))
  }
  
  .updateDPAccountSelection <- function() {
    logMessage(".updateDPAccountSelection called")
    accounts <- getAccountName(dbSettings)
    updateSelectizeInput(session, "sinputDPAccountName",
                         choices = createSelectOptions(accounts),
                         selected = toString(result$DPProgData[input$tableDPprog_rows_selected, DPProgData.AccountID]))
  }
  
  .updateProgrammeName <- function() {
    logMessage(".updateProgrammeName called")
    updateTextInput(session, "tinputDPProgName",
                    value = result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName])
  }
  
  .updateTransformNameSelection <- function() {
    logMessage(".updateTransformNameSelection called")
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectizeInput(session, "sinputTransformname",
                         choices = createSelectOptions(transforms,
                                                       labelCol = 1, valueCol = 2),
                         selected = toString(result$DPProgData[input$tableDPprog_rows_selected, DPProgData.TranformID]))
  }
  
  
  # Helper Functions 'Choose Model' = '2' ---------------------------------
  .clearOOKProgrammeSelection <- function() {
    logMessage(".clearOOKProgrammeSelection called")
    programmes <- getProgrammeList(dbSettings)
    updateSelectizeInput(session, "sinputookprogid",
                         choices = createSelectOptions(programmes),
                         selected = character(0))
  }
  
  .clearOOKModelSelection <- function() {
    logMessage(".clearOOKModelSelection called")
    models <- getModelList(dbSettings)
    updateSelectizeInput(session, "sinputookmodelid",
                         choices = c(models),
                         selected = character(0))
  }
  
  .clearOOKTransformSelection <- function() {
    logMessage(".clearOOKTransformSelection called")
    transforms <- getTransformNameCanModel(dbSettings)
    updateSelectizeInput(session, "sinputProgModTransform",
                         choices = createSelectOptions(transforms),
                         selected = character(0))
  }
  
  .updateOOKProgrammeSelection <- function() {
    logMessage(".updateOOKProgrammeSelection called")
    programmes <- getProgrammeList(dbSettings)
    if (length(input$tableDPprog_rows_selected) > 0 ) {
      selection <- toString(result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID])
    } else {
      selection <- character(0)
    }
    updateSelectizeInput(session, "sinputookprogid",
                         choices = createSelectOptions(programmes),
                         selected = selection)
  }
  
  # Helper Functions 'Configure Output & Run' = '3' ---------------------------------
  # Clear checkboxgroups GUL
  .clearchkboxGULgrp <- function() {
    logMessage(".clearchkboxGULgrp called")
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }
  
  # Clear checkboxgroup IL
  .clearchkboxILgrp <- function() {
    logMessage(".clearchkboxILgrp called")
    for (i in checkilgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }
  
  # Clear checkboxgroup RI
  .clearchkboxRIgrp <- function() {
    logMessage(".clearchkboxRIgrp called")
    for (i in checkrigrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }
  
  # Clear other runtime params
  .clearotherparams <- function() {
    logMessage(".clearotherparams called")
    updateSelectInput(session, "sinoutputoptions",
                      choices = c(getOutputOptions(dbSettings)),
                      selected = character(0))
    updateTextInput(session, "tinputprocessrunname", value = "")
    updateSliderInput(session, "sliderleakagefac", "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      prgId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
      prgId <- ifelse(is.null(prgId), toString(prgId), -1)
    } else {
      prgId <- -1
    }
    if (prgId != -1) {
      updateSelectInput(session, "sinputeventset",
                        choices = getEventSet(dbSettings, prgId ))
      updateSelectInput(session, "sinputeventocc",
                        choices = getEventOccurrence(dbSettings, prgId ))
    }
    updateCheckboxInput(session, "chkinputprwind", "Peril: Wind", value = TRUE)
    updateCheckboxInput(session, "chkinputprstsurge", "Peril: Surge", value = TRUE)
    updateCheckboxInput(session, "chkinputprquake", "Peril: Quake", value = TRUE)
    updateCheckboxInput(session, "chkinputprflood", "Peril: Flood", value = TRUE)
    updateCheckboxInput(session, "chkinputdsurge", "Demand Surge", value = TRUE)
  }
  
  # Clear Custom Configuration option
  .clearOutputOptions <- function() {
    logMessage(".clearOutputOptions called")
    updateSelectInput(session, "sinoutputoptions",
                      choices = c(getOutputOptions(dbSettings)),
                      selected = character(0))
  }
  
  # Update output configuration for rerun
  .updateOutputConfig <- function() {
    logMessage(".updateOutputConfig called")
    outputlist <- executeDbQuery(dbSettings, paste0("exec dbo.getOutputOptionOutputs @processrunid = ", result$prrunid ))
    runparamsforpr <- executeDbQuery(dbSettings, paste0("exec dbo.getProcessRunParams ", result$prrunid ))
    
    updateTextInput(session, "tinputprocessrunname", value = result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunName])
    
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
    invisible()
  }
  
  # Output view
  .advancedview <- function() {
    logMessage(".advancedview called")
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
    logMessage(".basicview called")
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
    logMessage(".defaultchkboxGULgrp called")
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesGUL)
    }
  }
  
  .defaultview <- function(session) {
    logMessage(".defaultview called")
    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp(session)
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .clearchkboxILgrp()
    updateCheckboxInput(session, "chkinputRI", value = FALSE)
    .clearchkboxRIgrp()
    .clearotherparams()
    .clearOutputOptions()
    .basicview()
  }
  
  # Model Outout ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      #Not used
      #progOasisId = reactive(result$progOasisId),
      processRunId = reactive(result$prrunid)
    )
  )
  
  moduleOutput
  
}

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
  navigation_state <- reactiveNavigation()
  
  stop_selProgID <- check_selProgID <- 0
  
  stop_selProgOasisID <- check_selProgOasisID <- 0
  
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
    progOasisId = -1,
    # Id of the Process Run
    prrunid = -1,
    # flag to know if the user is creating or amending a programme
    # TODO:
    prog_flag = "C"
  )
  
  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")
  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")
  checkrigrplist <- c("chkriprog", "chkristate", "chkricounty", "chkriloc", "chkrilob", "chkripolicy")
  
  # Module to control colors of radio buttons in the singleProgrammeWorkflowSteps
  workflowSteps <- callModule(singleProgrammeWorkflowSteps, "workflowsteps")
  
  # Panels switch ------------------------------------------------------------
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
  
  ### Creating reactive for selectors of Programme Table
  #result$DPProgData
  #"Programme ID", "Programme Name", "Account ID", "Account Name", "Transform ID", "Transform", "Status"
  ProgrammeID <- reactive(names(result$DPProgData)[1])
  ProgrammeName <- reactive(names(result$DPProgData)[2])
  ProgrammeAccountID <- reactive(names(result$DPProgData)[3])
  ProgrammeAccountName <- reactive(names(result$DPProgData)[4])
  ProgrammeTranformID <- reactive(names(result$DPProgData)[5])
  ProgrammeTranform <- reactive(names(result$DPProgData)[6])
  ProgrammeStatus <- reactive(names(result$DPProgData)[7])
  
  ### > Programme Table ------
  output$tableDPprog <- renderDT({
    print(names(result$DPProgData))
    
    # manual refresh button
    invisible(input$abuttonprgtblrfsh)

    logMessage("re-rendering programme table")
    if (!is.null(result$DPProgData)) {
      if (isolate(input$selectprogrammeID) != "<Select>") {
        rowToSelect <- match(isolate(input$selectprogrammeID), result$DPProgData[, ProgrammeID()])
      } else {
        rowToSelect <- 1
      }
      datatable(
        result$DPProgData,
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
    progId <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]
    paste0("- ", progName, " (id: ", progId, ")")
  })
  
  # Show Programme Details
  onclick("buttonprogdetails", {
    logMessage("showing panelProgrammeDetails")
    .reloadProgDetails()
    if (length(input$tableDPprog_rows_selected) > 0) {
      show("panelProgrammeDetails")
      hide("buttonprogdetails")
    } else {
      showNotification(type = "warning", "Please select a Programme first")
    }
  })
  
  # Hide Programme Details
  onclick("buttonhideprogdetails", {
    hide("panelProgrammeDetails")
    show("buttonprogdetails")
  })
  
  # > Create / Amend Programme sub-panel -----------------------------------------------

  # Create/Amend programme title
  output$paneltitleDefineProgramme <- renderUI({
    if (result$prog_flag == "C" || is.null(input$tableDPprog_rows_selected)) {
      "Create Programme"
    } else if (result$prog_flag == "A") {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
      progName <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]
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
      hide("panelDefineProgramme")
    }
  })
  
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
    } else if (result$prog_flag == "A") { # && length(input$tableDPprog_rows_selected) > 0
      idxSel <- input$tableDPprog_rows_selected
      query <- paste0("exec dbo.updateProg ", result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()],
                      ",[", input$tinputDPProgName,"],", input$sinputDPAccountName,
                      ", [", input$sinputTransformname, "]")
      res <- executeDbQuery(dbSettings, query)
      message(paste("A res is:", res))
      if (is.null(res)) {
        showNotification(type = "error",
                         paste("Failed to amend a Programme - ", result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]))
      } else {
        showNotification(type = "message",
                         paste("Programme ", result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()], " amended."))
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
    # if (length(input$tableDPprog_rows_selected) > 0) {
    #loadprogdata <- "success"
    #progId = result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
    #logMessage(paste("loading programme - progId is:", progId))
    loadprogdata <- loadProgrammeData(
      apiSettings,
      progId = result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
    )
    if (loadprogdata == 'success' || loadprogdata == 'Success') {
      showNotification(type = "message", "Initiating load programme data...")
      workflowSteps$update("2")
    } else {
      showNotification(type = "error", "Failed to load programme data.")
    }
    .reloadDPProgData()
    # } else {
    #   showNotification(type = "warning", "Please select a Programme to load programme data.")
    # }
  })
  
  # Delete Programme
  onclick("buttondeletepr",{
    if (length(input$tableDPprog_rows_selected) > 0) {
      stmt <- buildDbQuery("deleteProg", result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()])
      executeDbQuery(dbSettings, stmt)
      showNotification(type = "message",
                       sprintf("Programme %s deleted", result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]))
      .reloadDPProgData()
    } else {
      showNotification(type = "warning", "Please select a Programme to Delete")
    }
  })
  
  ### >> Source Files ----------------------------------------------------------
  
  ### Upload Location/Account File
  .uploadSourceFile <- function(inFile, recordIdString, recordIdCode){
    flc <- getFileLocationPath(dbSettings, "Exposure File")
    flcopy <- file.copy(inFile$datapath, file.path(flc, inFile[1, 1]), overwrite = TRUE)
    logMessage(file.path(flc, inFile[1, 1]))
    if (length(input$tableDPprog_rows_selected) > 0) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(
          dbSettings, inFile[1, 1], recordIdString, recordIdCode, flc, userId(),
          "Prog", result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
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
                                  result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]))
      if (inputID != "") {
        if (is.null(res)) {
          showNotification(type = "error", "Failed to link the File!")
        } else {
          showNotification(type = "message",
                           paste("Location File linked to Programme", result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]))
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
    logMessage("Updating many things because input$tableDPprog_rows_selected changed")
    # Update Programme Detail table if row selected changes
    .reloadProgDetails()
    # Update Associate Model Panel
    .updateOOKProgrammeSelection()
    .clearOOKModelSelection()
    .clearOOKTransformSelection()
    hide("panelProgrammeDetails")
    show("buttonprogdetails")
    hide("panelDefineProgramme")
    
    
    if (length(input$tableDPprog_rows_selected) > 0) {
      logMessage(paste("input$tableDPprog_rows_selected is:", input$tableDPprog_rows_selected))
      # note that tableDPprog allows single row selection only
      prgId <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
      if (prgId != input$selectprogrammeID) {
        # re-selecting the same programme ID in the drop-down would not re-trigger
        # any of the observers of the drop-down, however we then also want to be
        # sure not to increase stop_selProgID!
        logMessage(paste("updating selectprogrammeID because selection in programme table changed to",  prgId))
        stop_selProgID <<- stop_selProgID + 1
        updateSelectInput(session, inputId = "selectprogrammeID", selected = prgId)
      }
    } else {
      updateSelectInput(session, inputId = "selectprogrammeID", selected = "<Select>")
    }
  })
  
  # Section 'Choose Model' = '2' ----------------------------------------------
  
  ### > Define selectprogrammeID ----
  # Add choices to selectprogrammeID, update selectprogrammeID
  observeEvent(result$DPProgData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    logMessage(paste0("updating selectprogrammeID choices because programme table was reloaded - contains ", nrow(result$DPProgData), " rows"))
    if (input$selectprogrammeID == "<Select>") {
      # initial selection last programme created
      prgId <- result$DPProgData[1, ProgrammeID()]
    } else {
      # keep current selection
      prgId <- input$selectprogrammeID
    }
    updateSelectInput(session, inputId = "selectprogrammeID", choices = c("<Select>", result$DPProgData[, ProgrammeID()]), selected = prgId)
  })
  
  # If selectprogrammeID changes, reload programme model table and set view back to default
  observeEvent(input$selectprogrammeID, ignoreInit = TRUE, {
    bl_dirty <- stop_selProgID > check_selProgID
    logMessage(paste("--- stop_selProgID is:", stop_selProgID))
    if (active()) {
    if (input$selectprogrammeID != "<Select ProgID>") {
      if (!is.null(result$DPProgData) && nrow(result$DPProgData) > 0 && !bl_dirty ) {
        logMessage(paste("updating tableDPprog select because selectprogrammeID changed to", input$selectprogrammeID))
        rowToSelect <- match(input$selectprogrammeID, result$DPProgData[, ProgrammeID()])
        #backward propagation
        # if (is.null(input$tableDPprog_rows_selected)) {
        #   selectRows(dataTableProxy("tableDPprog"), rowToSelect)
        # } else
        if (!is.null(input$tableDPprog_rows_selected) && rowToSelect != input$tableDPprog_rows_selected) {
          # re-selecting the same row would trigger event-observers on input$tableDPprog_rows_selected
          selectRows(dataTableProxy("tableDPprog"), rowToSelect)
        }
      }
    } else {
      selectRows(dataTableProxy("tableDPprog"), NULL) 
    }
    .reloadPOData()
    }
    if (bl_dirty) check_selProgID <<- check_selProgID + 1
  })
  
  ### Creating reactive for selectors of Programme Model Table
  # result$POData
  #"ProgOasisId", ProgName", "ModelName", "TransformName", "SourceFileId", "FileID", "Status"
  ProgOasisId <- reactive(names(result$POData)[1])
  ProgOasisName <- reactive(names(result$POData)[2])
  ProgOasisModelName <- reactive(names(result$POData)[3])
  ProgOasisTransformName <- reactive(names(result$POData)[4])
  ProgOasisSourceFileId <- reactive(names(result$POData)[5])
  ProgOasisFileID <- reactive(names(result$POData)[6])
  ProgOasisStatus <- reactive(names(result$POData)[7])
  
  # > Programme Model Table --------------------------
  
  output$tableProgOasisOOK <- renderDT(
    if (!is.null(result$POData) && nrow(result$POData) > 0 ) {
      
      # manual refresh button
      invisible(input$abuttonookrefresh)
      
      print(names(result$POData))
      
      if (isolate(input$selectprogOasisID) != "<Select>") {
        rowToSelect <- match(isolate(input$selectprogOasisID), result$POData[,ProgOasisId()])
      } else {
        rowToSelect <- 1
      }
      
      datatable(
        result$POData,
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
    progId <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]
    paste0("Models Table for Programme", " - ", progName," (id: ", progId, ")")
  })
  
  # Associate Model Table Title
  output$paneltitleAssociateModel <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]
    paste0("Associate Model to Programme", " - ", progName, " (id: ", progId, ")")
  })
  
  # > Model Details Table -----
  output$tabledisplayprogoasisfiles <- renderDT(
    if (!is.null(result$progFiles) && nrow(result$progFiles) > 0 ) {
      
      # manual refresh button
      invisible(input$abuttonprgoasisrfsh)
      
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
    progName <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()]
    paste0("Details Programme Model", " - ", progName, " (id: ", result$progOasisId, ")")
  })
  
  ### Show/hide Programme Model Details Panel
  onclick("buttonmodeldetails", {
    logMessage("showing panelModelDetails")
    .reloadProgFiles()
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      hide("buttonmodeldetails")
      show("panelModelDetails")
    } else {
      showNotification(type = "warning", "Please select a Programme Model first")
    }
  })
  
  onclick("buttonhidemodeldetails", {
    show("buttonmodeldetails")
    hide("panelModelDetails")
  })
  
  ### > Create Model ------
  onclick("abuttoncrprogoasis", {
    if (isolate(input$sinputookprogid) > 0 && isolate(input$sinputookmodelid) > 0) {
      progOasisId <- createProgOasis(dbSettings,
                                            isolate(input$sinputookprogid),
                                            isolate(input$sinputookmodelid),
                                            isolate(input$sinputProgModTransform))
      ifelse(is.null(progOasisId), -1, result$progOasisId)
      if (result$progOasisId == -1) {
        showNotification(type = "error",
                         paste("No Prog Oasis created"))
      } else {
        showNotification(type = "message",
                         paste("Prog Oasis id:", result$progOasisId,  " created."))
        .clearOOKSidebar()
        workflowSteps$update("3")
        .reloadPOData()
        idxSel <- match(result$progOasisId, result$POData[, ProgOasisId()])
        selectRows(dataTableProxy("tableProgOasisOOK"), idxSel)
        loadprogmodel <- loadProgrammeModel(
          apiSettings,
          progOasisId = toString(result$POData[input$tableProgOasisOOK_rows_selected, ProgOasisId()])
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
  
  # > Updates dependent on changed: tableProgOasisOOK_rows_selected ------------
  
  observeEvent(input$tableProgOasisOOK_rows_selected, ignoreInit = TRUE, {
    #force collapsed state of Associate Model flamingo panel
    removeClass(id = paste0("progmodel-body"), class = "in")
    removeClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
    addClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
  })
  
  # Output configuration: manage what to show based on  status of row selected in programme Model table
  observeEvent(input$tableProgOasisOOK_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    .reloadRunData()
    .reloadProgFiles()
    show("buttonmodeldetails")
    hide("panelModelDetails")
    .defaultview(session)
    hide("panelDefineOutputs")
    
    # Show perils according to programme model
    if (length(input$tableProgOasisOOK_rows_selected) > 0 ) {
      prgId <- result$POData[input$tableProgOasisOOK_rows_selected, ProgOasisId()]
      procId <- toString(prgId)
      
      logMessage(paste("updating selectprogOasisID because selection in programme model table changed to",  prgId))
      updateSelectInput(session, inputId = "selectprogOasisID", selected = prgId)
      
      if (result$POData[input$tableProgOasisOOK_rows_selected, ProgOasisStatus()] == StatusCompleted) {
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
      updateSelectInput(session, inputId = "selectprogOasisID", selected = "<Select>")
    }
  })
  
  # Section 'Configure Output & Run' = '3' ------------------------------------
  
  ### > Define selectprogOasisID ----
  # Add choices possibilities to selectprogOasisID
  observeEvent(result$POData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (!is.null(result$POData) && nrow(result$POData) > 0) {
      updateSelectInput(session, inputId = "selectprogOasisID", choices =  c("<Select>",result$POData[, ProgOasisId()]))
      if (input$selectprogrammeID != "<Select>") {
        logMessage(paste("updating selectprogOasisID choices based on Programme Model Table"))
        print(" result$POData[1, ProgOasisId()]")
        print( result$POData[1, ProgOasisId()])
        updateSelectInput(session, inputId = "selectprogOasisID", selected = result$POData[1, ProgOasisId()])
      } else {
        updateSelectInput(session, inputId = "selectprogOasisID", selected = "<Select>")
      }
    } else {
      updateSelectInput(session, inputId = "selectprogOasisID", choices =  c("<Select>"), selected = "<Select>") 
    }
  })
  
  # If selectprogOasisID changes, reload process run table and set view back to default
  observeEvent(input$selectprogOasisID, ignoreInit = TRUE, {
    logMessage(paste0(" #### selectprogOasisID changed to ", input$selectprogOasisID))
    bl_dirty1 <- stop_selProgOasisID > check_selProgOasisID
    if (active()) {
      show("buttonmodeldetails")
      hide("panelModelDetails")
      hide("panelDefineOutputs")
      hide("panelProcessRunLogs")
      if (input$selectprogOasisID != "<Select>") {
        if (!is.null(result$POData) && nrow(result$POData) > 0   && !bl_dirty1 ) {
          logMessage(paste("updating prcrundata select because selectprogOasisID changed to", input$selectprogOasisID))
          rowToSelect <- match(input$selectprogOasisID, result$POData[, ProgOasisId()])
          if (!is.null(input$tableProgOasisOOK_rows_selected) && rowToSelect != input$tableProgOasisOOK_rows_selected) {
            # re-selecting the same row would trigger event-observers on input$tableprocessrundata_rows_selected
          selectRows(dataTableProxy("tableProgOasisOOK"), rowToSelect)
          }
        } 
      } else {
        result$prcrundata <- NULL
        selectRows(dataTableProxy("tableProgOasisOOK"), NULL)
      }
      # reload set of runs
      .reloadRunData()
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
    if (active()) {
      prtable <- getProcessData(dbSettings, pruser, prmodel, prprogramme, prworkflow)
      prcid <- input$selectprogOasisID
      # For processes in all states (completed, created, in progress etc), pass 'All', for just in progress pass
      # 'In Progress' (not handled by stored procedure in the DB due to bug!)
      prcrundata <- getProcessRun(dbSettings, prcid, input$radioprrunsAllOrInProgress)
      StatusGood <- "Completed"
      StatusBad <- c("Failed", "Cancelled", NA_character_)
      '%notin%' <- Negate('%in%')
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
  }
  
  ### Creating reactive for selectors of Process Runs Table
  # result$prcrundata
  #"ProcessRunID", "ProcessRunName", "ProgOasisID", "ProcessRunStatus"
  ProcessRunID <- reactive(names(result$prcrundata)[1])
  ProcessRunName <- reactive(names(result$prcrundata)[2])
  ProcessRunProgOasisID <- reactive(names(result$prcrundata)[3])
  ProcessRunStatus <- reactive(names(result$prcrundata)[4])
  
  output$tableprocessrundata <- renderDT(
    
    if (!is.null(result$prcrundata) && nrow(result$prcrundata) > 0) {
      
      # manual refresh button
      invisible(input$abuttonrefreshprrun)
      
      # if (preselRunId() == -1) {
      #   index <- 1
      # } else {
      #   index <- match(c(preselRunId()), result$prcrundata[,ProcessRunID()])
      # }
      index <- 1
      
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
    progOasisId <- result$POData[input$tableProgOasisOOK_rows_selected, ProgOasisId()]
    progOasisName <- result$POData[input$tableProgOasisOOK_rows_selected, ProgOasisName()]
    paste0("Process Runs for Model", " - ", progOasisName," (id: ", progOasisId, ")")
  })
  
  #Not allow any actions if the process run table is empty
  observeEvent(result$prcrundata, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (!is.null(result$prcrundata) && nrow(result$prcrundata) > 0) {
      show("divprocessRunButtons")
    } else {
      hide("divprocessRunButtons")
    }
  })
  
  # > Configure Output ------
  
  # configuration title
  output$paneltitleReDefineProgramme <- renderUI({
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, ProcessRunID()]
      processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, ProcessRunName()]
      paste0("Re-Define Output Configuration for Process", " - ", processRunName, " (id: ", processRunId, ")")
    } else {
      "New Output Configuration"
    }
  })
  
  #Show Output Configuration Panel
  onclick("abuttonconfigoutput", {
    .defaultview(session)
    show("panelDefineOutputs")
    selectRows(dataTableProxy("tableprocessrundata"), selected = NULL)
  })
  
  onclick("abuttonrerunpr", {
    .defaultview(session)
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      show("panelDefineOutputs")
      .updateOutputConfig()
    } else {
      hide("panelDefineOutputs")
      showNotification(type = "warning", "Please select Process Run")
    }
  })
  
  ### Hide Output Configuration panel
  onclick("abuttonehidepanelconfigureoutput", {
    hide("panelDefineOutputs")
  })
  
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
  
  # Update button in sidebar panel to update checkboxes for pre-populated values
  observeEvent(input$sinoutputoptions, {
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
  
  
  ### > Run Process --------------------------------------------------------
  
  # A function to generate process run
  .generateRun <- function() {
    prTable <- getProcessData(dbSettings, userId(), 0, 0, 0)
    prgId <- result$POData[input$tableProgOasisOOK_rows_selected, ProgOasisId()]
    if (is.null(prgId)) { 
      result$progOasisID <- -1
    } else {
      result$progOasisID <- toString(prgId)
    }
    
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
          rowToSelect <- match(runId, result$prcrundata[, ProcessRunID()])
          selectRows(dataTableProxy("tableprocessrundata"), rowToSelect)
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
  
  ### > Logs ---------------------------------------------------------------
  onclick("abuttonshowlog", {
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      show("panelProcessRunLogs")
      logMessage("showing prrunlogtable")
      hide("abuttonshowlog")
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
      hide("panelProcessRunLogs")
      show("abuttonshowlog")
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
      
      logMessage("log table refreshed")
      logdata <- getProcessRunDetails(dbSettings, wfid) %>%
        mutate(Status = replace(Status, grepl("Failed", Status, ignore.case = TRUE) | grepl("Cancelled", Status, ignore.case = TRUE) , StatusFailed)) %>%
        mutate(Status = replace(Status, !grepl("Success", Status, ignore.case = TRUE) & !grepl("Failed", Status, ignore.case = TRUE) & !grepl("Cancelled", Status, ignore.case = TRUE) & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
        mutate(Status = replace(Status, grepl("Success", Status, ignore.case = TRUE), StatusCompleted)) %>%
        as.data.frame()
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
    processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, ProcessRunID()]
    processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, ProcessRunName()]
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
    logMessage(paste("*** input$tableprocessrundata_rows_selected is:", input$tableprocessrundata_rows_selected))
    hide("panelDefineOutputs")
    hide("panelProcessRunLogs")
    show("abuttondisplayoutput")
    show("abuttonshowlog")
    ##### TODO: Do I need the second check in this if????
    if (length(input$tableprocessrundata_rows_selected) > 0 && !is.null(result$prcrundata)) {
      result$prrunid <- result$prcrundata[input$tableprocessrundata_rows_selected, ProcessRunID()]
      if (result$prcrundata[input$tableprocessrundata_rows_selected, ProcessRunStatus()] != StatusCompleted) {
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
    if (!is.null(DPProgData)) {
      result$DPProgData <- DPProgData %>%
        mutate(Status = replace(Status, Status == "Failed" | is.na(Status), StatusFailed)) %>%
        mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
        mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
        as.data.frame()
      logMessage("programme table refreshed")
    }
    logMessage(".reloadDPProgData called")
    invisible()
  }
  
  # Reload Programme Details table
  .reloadProgDetails <- function() {
    if (length(input$tableDPprog_rows_selected) > 0) {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()]
      
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
      result$progDetails  <- NULL
    }
    logMessage(".reloadProgDetails called")
    invisible()
  }
  
  # Reload Programme Model table
  .reloadPOData <- function() {
    if (input$selectprogrammeID != "<Select>") {
      POData <- getProgOasisForProgdata(dbSettings, input$selectprogrammeID)
      if (!is.null(POData)) {
        result$POData <- POData %>%
          select(c("ProgOasisId", "ProgName", "ModelName", "TransformName", "SourceFileId", "FileID", "Status")) %>%
          mutate(Status = replace(Status, Status == "Failed" | is.na(Status), StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()
      }
      logMessage("programme model table refreshed")
    } else {
      result$POData <- NULL
    }
    logMessage(".reloadPOData called")
    invisible()
  }
  
  # Reload Programme Model Details table
  .reloadProgFiles <- function() {
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      # result$progOasisId is updated when creating a model or when selecting a model
      result$progOasisId <- toString(result$POData[input$tableProgOasisOOK_rows_selected, ProgOasisId()])
      stmt <- buildDbQuery("getProgOasisFileDetails", result$progOasisId)
      progFiles <- executeDbQuery(dbSettings, stmt)
      if (!is.null(progFiles)) {
        result$progFiles <-  progFiles %>%
          mutate(Status = replace(Status, Status == "Failed", StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()
      }
      logMessage("files table refreshed")
    } else {
      result$progFiles <- NULL
      # if (active()) {
      #   showNotification(type = "warning", "Please select a Programme Model first")
      # }
    }
    logMessage(".reloadProgFiles called")
    invisible()
  }
  
  # Reload Process Runs table
  .reloadRunData <- function() {
    if (input$selectprogOasisID != "<Select>") {
      .getProcessRunWithUserChoices(userId(), 0, 0, 0)
      logMessage("process run table refreshed")
    }  else {
      result$prcrundata <- NULL
    }
    logMessage(".reloadRunData called")
    invisible()
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
    show("panelProgrammeTable")
    show("buttonprogdetails")
  }
  
  #show default view for Section "Choose Model" = "2"
  .defaultAssociateModel <- function(){
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
    show("panelDefineIDs")
    show("panelProcessRunTable")
    show("divselectprogOasisID")
    disable("chkgulpolicy")
  }
  
  # Helper Functions 'Choose Programme' = '1' ---------------------------------------
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
                      selected = toString(result$DPProgData[input$tableDPprog_rows_selected, ProgrammeAccountID()]))
  }
  
  .updateProgrammeName <- function() {
    updateTextInput(session, "tinputDPProgName",
                    value = result$DPProgData[input$tableDPprog_rows_selected, ProgrammeName()])
  }
  
  .updateTransformNameSelection <- function() {
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectInput(session, "sinputTransformname",
                      choices = createSelectOptions(transforms, "Select Transform",
                                                    labelCol = 1, valueCol = 2),
                      selected = toString(result$DPProgData[input$tableDPprog_rows_selected, ProgrammeTranformID()]))
  }
  
  
  # Helper Functions 'Choose Model' = '2' ---------------------------------
  
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
  
  .updateOOKProgrammeSelection <- function() {
    programmes <- getProgrammeList(dbSettings)
    if (length(input$tableDPprog_rows_selected) > 0 ) {
      selection <- toString(result$DPProgData[input$tableDPprog_rows_selected, ProgrammeID()])
    } else {
      selection <- "Select Programme"
    }
    updateSelectInput(session, "sinputookprogid",
                      choices = createSelectOptions(programmes, "Select Programme"),
                      selected = selection)
  }
  
  # Helper Functions 'Configure Output & Run' = '3' ---------------------------------
  
  # Clear checkboxgroups GUL
  .clearchkboxGULgrp <- function() {
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
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
  
  # Update output configuration for rerun
  .updateOutputConfig <- function() {
    outputlist <- executeDbQuery(dbSettings, paste0("exec dbo.getOutputOptionOutputs @processrunid = ", result$prrunid ))
    runparamsforpr <- executeDbQuery(dbSettings, paste0("exec dbo.getProcessRunParams ", result$prrunid ))
    
    updateTextInput(session, "tinputprocessrunname", value = result$prcrundata[input$tableprocessrundata_rows_selected, ProcessRunName()])
    
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
  
  .defaultview <- function(session) {
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
      progOasisId = reactive(result$progOasisId),
      processRunId = reactive(result$prrunid)
    )
  )
  
  moduleOutput
  
}

#' Single Programme Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater}; 
#' @return list of reactives:
#' @rdname programmeDefinitionSingle
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDataTable
#' @importFrom dplyr mutate
#' @export
programmeDefinitionSingle <- function(input, output, session, dbSettings,
                                      apiSettings, userId, active = reactive(TRUE), logMessage = message,
                                      preselRunId = reactive(-1),
                                      preselProcId = reactive(-1),
                                      reloadMillis = 10000) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ------------------------------------------
  
  result <- reactiveValues(
    #reactive values for the programme table
    DPProgData = NULL,
    DPProgDataCounter = 0,
    DPprogID = NULL,
    #flag to know if the user is creating or amending a programme
    prog_flag = "",
    #Id of the Programme Model
    progOasisId = -1,
    #Id of the Process Run
    prrunid = -1,
    #reactive values for the programme details table
    progDetails = NULL,
    progDetailsCounter = 0,
    #reactive values for the programme model table
    POData = NULL,
    PODataCounter = 0,
    PODataID = NULL,
    #reactive values for the programme model detail table
    progFiles = NULL,
    progFilesCounter = 0,
    #reactive values for the process runs table
    prcrundata = NULL,
    prcrundataCounter = 0,
    #length of the tables
    listprogrammes = 0,
    listprogOasis = 0,
    #flag to navigate to different pages
    navigate = NULL
  )
  
  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")
  
  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")
  
  # Panel switch ------------------------------------------------------------
  
  #Make sure the view is reset to first panel
  observe( if (active()) {
    updateSliderTextInput(session, inputId = "sliderdefprogsteps", selected = "Create Programme" )
  })
  
  observeEvent(input$sliderdefprogsteps, {
    
    if (input$sliderdefprogsteps == "Create Programme") {
      .hideDivs()
      show("panelcreateprogramme")
      logMessage("showing panelcreateprogramme")
      result$prog_flag <- "C"
      .clearDPAccountSelection()
      updateTextInput(session, "tinputDPProgName", value = "")
      updateSelectInput(session, "sinputSLFile", selected = "")
      updateSelectInput(session, "sinputSAFile", selected = "")
      .clearTransformNameSelection()
    }
    
    if (input$sliderdefprogsteps == "Select Programme & Associate Model") {
      .hideDivs()
      .reloadDPProgData()
      show("panelamendprogramme")
      logMessage("showing panelamendprogramme")
      result$prog_flag <- "A"
    }
    
    if (input$sliderdefprogsteps == "Configure Workflow Output") {
      .hideDivs()
      show("paneldefineids")
      logMessage("showing paneldefineids")
      show("divselectprogrammeID")
      hide("divselectprogOasisID")
      show("panelamendmodel")
      logMessage("showing panelamendmodel")
      .reloadPOData()
    }
    
    if (input$sliderdefprogsteps == "Browse & re-run") {
      .hideDivs()
      show("paneldefineids")
      logMessage("showing paneldefineids")
      show("divselectprogrammeID")
      show("divselectprogOasisID")
      show("panelrunprogramme")
      logMessage("showing panelrunprogramme")
      .reloadRunData()
    }
  })
  
  
  # Main Tables --------------------------------------------------------------

  ### Programme Table 
  
  observe(if (active()) {

    # reload after pressing refresh
    force(input$abuttonprgtblrfsh)
    
    # reload automatically every so often
    invalidateLater(reloadMillis)
    
    # reload after reloadDPProgData is called 
    force(result$DPProgDataCounter)
    
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
      
  })
  
  
  output$tableDPprog <- renderDataTable({
    if (!is.null(result$DPProgData)) {
      if (input$selectprogrammeID != "") {
        rowToSelect <- which(result$DPProgData[1:nrow(result$DPProgData),1] == input$selectprogrammeID)
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
                         selected = rownames(result$DPProgData)[rowToSelect]),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      ) 
    }
  })
  
  
  ### Programme Model Table (previously OOK)
  
  observeEvent(result$PODataCounter,{
    
    if (!is.null(input$selectprogrammeID)) {
      
      show("divprogmodeltable")
      logMessage("showing divprogmodeltable")
      
      POData <- getProgOasisForProgdata(dbSettings, input$selectprogrammeID) 
      
      if (!is.null(POData)) {
        result$POData <- POData %>% 
          mutate(Status = replace(Status, Status == "Failed", StatusFailed)) %>%
          mutate(Status = replace(Status, Status != "Loaded" & Status != "Failed" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
          mutate(Status = replace(Status, Status == "Loaded", StatusCompleted)) %>%
          as.data.frame()  
      }

     logMessage("programme model table refreshed")
      
      programmes <- getProgrammeList(dbSettings)
      updateSelectInput(session, "sinputookprogid",
                        choices = createSelectOptions(programmes, "Select Programme"),
                        selected = result$DPProgData[input$tableDPprog_rows_selected,1])
    } else {
      
      hide("divprogmodeltable")
      hide("divprogoasisfiles")
      hide("panelconfigureoutput")
    }
  })
  
  
  output$tableProgOasisOOK <- renderDataTable(
    if (!is.null(result$POData)) {
      
      datatable(
        result$POData,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = rownames( result$POData)[1]),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
      
    })
  
  ### Process Run Table
  
  getProcessRunWithUserChoices <- function(pruser, prmodel, prprogramme,
                                           prworkflow) {
    
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
      result$prcrundata <- prcrundata %>% 
        mutate(ProcessRunStatus = replace(ProcessRunStatus, grepl("Failed", ProcessRunStatus, ignore.case = TRUE) | grepl("Cancelled", ProcessRunStatus, ignore.case = TRUE) | is.na(ProcessRunStatus), StatusFailed)) %>%
        mutate(ProcessRunStatus = replace(ProcessRunStatus, !grepl("Completed", ProcessRunStatus, ignore.case = TRUE) & !grepl("Failed", ProcessRunStatus, ignore.case = TRUE) & !grepl("Cancelled", ProcessRunStatus, ignore.case = TRUE) & ProcessRunStatus != StatusFailed & ProcessRunStatus != StatusCompleted, StatusProcessing)) %>%
        mutate(ProcessRunStatus = replace(ProcessRunStatus, grepl("Completed", ProcessRunStatus, ignore.case = TRUE), StatusCompleted)) %>%
        as.data.frame()
    }

  }
  
  observeEvent( result$prcrundataCounter, {  
    #reload once reloadunData()
    force(result$prcrundataCounter)
    
    # reload after pressing refresh
    force(input$abuttonrefreshprrun)
    
    # reload automatically every so often
    #invalidateLater(reloadMillis)
    
    # reload if radio buttons for 'All' vs 'In_Progress' change
    force(input$radioprrunsAllOrInProgress)
    
    if (!is.null(input$selectprogOasisID) ) {
      
      show("prruntable")
      logMessage("showing prruntable")
      getProcessRunWithUserChoices(userId(), 0, 0, 0)
      
    } else {
      
      hide("prruntable")
      
    }
    
    logMessage("process run table refreshed")
    
  })
  
  output$processrundata <- renderDataTable(if (!is.null(result$prcrundata)) {
    
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
  
  
  # Create Programme --------------------------------------------------------
  
  ### DPprogID
  
  observe(if (active()) {
    if (length(input$tableDPprog_rows_selected) > 0) {
      result$DPprogID <- input$tableDPprog_rows_selected
    }else{
      .reloadDPProgData()
      result$DPprogID <- 1
    }
  })
  
  ### Submit Button
  
  onclick("abuttonProgSubmit", {
    
    if (result$prog_flag == "C") {
      
      query <- paste0("exec dbo.createProg [",input$tinputDPProgName,
                      "],",input$sinputDPAccountName,", [",input$sinputTransformname,"]")
      res <- executeDbQuery(dbSettings, query)
      if (is.null(res)) {
        showNotification(type = "error",
                         paste("Failed to create a Programme - ", input$tinputDPProgName))
      } else{
        showNotification(type = "message",
                         paste("Programme ", input$tinputDPProgName, " created."))
      }
      
    } else {
      
      if (result$prog_flag == "A") {
        query <- paste0("exec dbo.updateProg ", result$DPProgData[input$tableDPprog_rows_selected,1],",[",input$tinputDPProgName,"],", input$sinputDPAccountName,", [",input$sinputTransformname,"]")
        res <- executeDbQuery(dbSettings, query)
        if (is.null(res)) {
          showNotification(type = "error",
                           paste("Failed to amend a Programme - ", result$DPProgData[input$tableDPprog_rows_selected,2]))
        } else{
          showNotification(type = "message",
                           paste("Programme ", result$DPProgData[input$tableDPprog_rows_selected,2], " amended."))
        }
      }
    }
    
    .reloadDPProgData()
    selectRows(dataTableProxy("tableDPprog"), c(1))
    
  })
  
  ### Cancel Programme
  
  onclick("abuttonProgCancel",{
    updateTextInput(session, "tinputDPProgName", value = "")
    updateSelectInput(session, "sinputSLFile", selected = "")    
    updateSelectInput(session, "sinputSAFile", selected = "")      
  })
  
  ### Load Programme Button
  
  onclick("buttonloadcanmodpr",{

    if (!is.null(result$DPprogID)) {
      loadprogdata <- loadProgrammeData(apiSettings,
                                        progId = result$DPProgData[result$DPprogID,1])
      if (loadprogdata == 'success' || loadprogdata == 'Success') {
        showNotification(type = "message", "Initiating load programme data...")
        updateSliderTextInput(session, inputId = "sliderdefprogsteps", selected = "Select Programme & Associate Model")
      } else {
        showNotification(type = "error", "Failed to load programme data.")
      }
    } else {
      showNotification(type = "warning", "Please select a Programme to load programme data.")
    }
  })
  
  ### Upload Location/Account File
  
  onclick("abuttonSLFileUpload", {
    .reloadDPProgData()
    inFile <- input$SLFile
    flc <- getFileLocationPath(dbSettings, "Exposure File")
    flcopy <- file.copy(inFile$datapath,
                        file.path(flc,inFile[1,1]), overwrite = TRUE)
    logMessage(file.path(flc,inFile[1,1]))
    if (!is.null(result$DPprogID)) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(dbSettings,
                                     inFile[1,1], "Source Loc File", 101, flc, userId(),
                                     "Prog", result$DPProgData[result$DPprogID,1])
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
    if (!is.null(result$DPprogID)) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(dbSettings,
                                     inFile[1,1], "Source Acc File", 102, flc, userId(),
                                     "Prog", result$DPProgData[result$DPprogID,1])
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
    if (!is.null(result$DPprogID)) {
      res <- executeDbQuery(dbSettings,
                            paste("exec dbo.updateSourceLocationFileForProg ",
                                  input$sinputselectSLFile, ", ", result$DPProgData[result$DPprogID,1]))
      if (input$sinputselectSLFile != "") {
        if (is.null(res)) {
          showNotification(type = "error", "Failed to link the File!")
        } else {
          showNotification(type = "message",
                           paste("Location File linked to Programme", result$DPProgData[result$DPprogID,2]))
        }
      } else {
        showNotification(type = "warning", "Please select a file to Link")
      }
    }
    
  })
  
  onclick("abuttonSAFileLink",{
    if (!is.null(result$DPprogID)) {
      res <- executeDbQuery(dbSettings,
                            paste("exec dbo.updateSourceAccountFileForProg ",
                                  input$sinputselectSAFile, ", ", result$DPProgData[result$DPprogID,1]))
      if (input$sinputselectSAFile != "") {
        if (is.null(res)) {
          showNotification(type = "error", "Failed to link the File!")
        } else {
          showNotification(type = "message", 
                           paste("Location File linked to Programme", result$DPProgData[result$DPprogID,2]))
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
                          choices = createSelectOptions(SLfiles, labelCol = 1,
                                                        valueCol = 2))
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
                          choices = createSelectOptions(SAfiles, labelCol = 1,
                                                        valueCol = 2))
      }
    }
  })
  

  # Programme Details -------------------------------------------------------

  observeEvent(input$buttonhideprogdetails, {
    hide("divdefprogdetails")
    hide("buttonhideprogdetails")
    show("buttonprogdetails")
  })
  
  ### Programme Detail Table
  
  observeEvent(input$buttonprogdetails, {
    if (length(input$tableDPprog_rows_selected) > 0) {
      
      show("divdefprogdetails")
      show("buttonhideprogdetails")
      hide("buttonprogdetails")
      
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
      
      hide("divdefprogdetails")
      
    }
  })
  

  output$tableprogdetails <- renderDataTable({
    datatable(
      result$progDetails,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      escape = FALSE,
      selection = "none",
      colnames = c('Row Number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),
        scrollX = TRUE
      )
    )
  })
  
  
  # Ammend Programme --------------------------------------------------------

  ### Amend button 
  
  onclick("buttonamendpr", {
    
    if (length(input$tableDPprog_rows_selected) > 0) {

      .updateDPAccountSelection()
      
      updateTextInput(session, "tinputDPProgName",
                      value = result$DPProgData[result$DPprogID,2])
      
      .updateTransformNameSelection()
      
      show("panelcreateprogramme")
      logMessage("showing panelcreateprogramme")
      
    } else {
      
      showNotification(type = "warning", "Please select a Programme to Amend")
      hide("panelcreateprogramme")
    }
  })
  
  
  ### Delete Programme 
  
  onclick("buttondeletepr",{
    if (length(input$tableDPprog_rows_selected) > 0) {
      
      stmt <- buildDbQuery("deleteProg", result$DPProgData[result$DPprogID,1])
      executeDbQuery(dbSettings, stmt)
      
      showNotification(type = "message", sprintf("Programme %s deleted",
                                                 result$DPProgData[result$DPprogID,2]))
      
      .reloadDPProgData()
      
    } else {
      
      showNotification(type = "warning", "Please select a Programme to Delete")
      
    }
  })
  
  # Create Model ------------------------------------------------------------

  observe( if (active()) {
    force(result$DPProgDataCounter)
    if (length(input$tableDPprog_rows_selected) > 0) {
      if (result$DPProgData[input$tableDPprog_rows_selected, "Status"] == StatusCompleted & input$sliderdefprogsteps == "Select Programme & Associate Model") {
        show("panelassociatemodel")
        logMessage("showing panelassociatemodel")
        .updateOOKProgrammeSelection()
        .clearOOKModelSelection()
        .clearOOKTransformSelection()
      } else {
        hide("panelassociatemodel")
      }
    }
  })
  
  ### DPprogID
  
  observe(if (active()) {
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      result$PODataID <- input$tableProgOasisOOK_rows_selected
    }else{
      .reloadDPProgData()
      result$PODataID <- 1
    }
  })
  
  
  # on click of create prog oasis button - Creates and loads the model - sends user to Configure Output panel
  onclick("abuttoncrprogoasis", {
    if (isolate(input$sinputookprogid) > 0 &&
        isolate(input$sinputookmodelid) > 0) {
      
      result$progOasisId <- createProgOasis(dbSettings,
                                            isolate(input$sinputookprogid),
                                            isolate(input$sinputookmodelid),
                                            isolate(input$sinputProgModTransform))
      
      showNotification(type = "message",
                       paste("Prog Oasis id:", result$progOasisId,  " created."))
      
      .clearOOKSidebar()
      
      updateSliderTextInput(session, inputId = "sliderdefprogsteps", selected = "Configure Workflow Output")
      
      .reloadPOData()
      
      if (!is.null(result$PODataID)) {
        
        loadprogmodel <- loadProgrammeModel(apiSettings,
                                            progOasisId = toString(result$POData[result$PODataID,1]))
        
        if (loadprogmodel == 'success' || loadprogmodel == 'Success') {
          showNotification(type = "message", "Initiating load programme model..")
          .reloadProgFiles()
        } else {
          showNotification(type = "error", "Failed to load programme model.")
        }
      }else{
        showNotification(type = "warning",
                         "Please select a Prog Oasis to load Programme model.")
      }
      
    } else{
      
      showNotification(type = "warning", "Please select both the fields.")
      
    }
  })

  
  # Model Details  ------------------------------------------------------------
  
  
  ### Programme Model detail
  
  observeEvent(input$buttonmodeldetails, {
    hide("buttonmodeldetails")
    show("buttonhidemodeldetails")
    show("divprogoasisfiles")
    logMessage("showing divprogoasisfiles")
  })
  
  observeEvent(input$buttonhidemodeldetails, {
    show("buttonmodeldetails")
    logMessage("showing divprogoasisfiles")
    hide("buttonhidemodeldetails")
    hide("divprogoasisfiles")
  })
  
  ### Programme Oasis File Table / Programme Model Details
  
  # observer to display prog oasis files
  observe(if (active()) {
    
    # on click of prog oasis refresh table
    force(input$abuttonprgoasisrfsh)
    
    force(result$progFilesCounter)
    
    # reload automatically every so often
    invalidateLater(reloadMillis)
    
    if (length(input$tableProgOasisOOK_rows_selected) > 0) {
      
      result$progOasisId <- toString(result$POData[input$tableProgOasisOOK_rows_selected,1])
      
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
      
    } 
  })
  
  output$tabledisplayprogoasisfiles <- renderDataTable(
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

  
  # Configure Workflow Output --------------------------------------------------------
  
  ### selectprogrammeID
  observe( if (active()) {
    if ( !is.null(result$DPProgData)) {
      result$listprogrammes <- nrow(result$DPProgData)
    }
    
    if (result$listprogrammes > 0 ) {
      if (input$sliderdefprogsteps == "Configure Workflow Output" | input$sliderdefprogsteps == "Browse & re-run") {
        if (input$selectprogrammeID == "") {
          logMessage(paste0("updating selectprogrammeID because active and of result$listprogrammes "))
          updateSelectInput(session, inputId = "selectprogrammeID", choices = result$DPProgData[1:result$listprogrammes,1])
        }
        .reloadPOData()
      }
    }
    
  })
  
  observeEvent(input$tableDPprog_rows_selected, {
    if (length(input$tableDPprog_rows_selected) > 0 ) {
      prgId <- result$DPProgData[input$tableDPprog_rows_selected,1]
      logMessage(paste0("updating selectprogrammeID because of input$tableDPprog_rows_selected changing to ", input$tableDPprog_rows_selected))
      updateSelectInput(session, inputId = "selectprogrammeID", choices = result$DPProgData[1:result$listprogrammes,1], selected = prgId)
      .reloadPOData()
    }
  })
  
  observeEvent( input$selectprogrammeID, {
    logMessage(paste0("updating selectprogrammeID because of reloadPOData "))
    if (length(row <- input$tableDPprog_rows_selected) > 0 & input$selectprogrammeID != "") {
      logMessage(paste0("row is ", row))
      if (!is.null(result$DPProgData)) {
        if (result$DPProgData[row, 1] !=  input$selectprogrammeID) {
          rowToSelect <- which(result$DPProgData[1:result$listprogrammes,1] == input$selectprogrammeID)
          selectRows(dataTableProxy("tableDPprog"), rowToSelect) 
        } 
      }
    }
    .reloadPOData() 
  })
  
  
  #show panel configure output
  
  observeEvent(input$tableProgOasisOOK_rows_selected, {
    .reloadPOData()
  })
  
  observeEvent(result$PODataCounter, {
    
    if (length(row <- input$tableProgOasisOOK_rows_selected) > 0) {

      if (!is.na(result$POData[row, "Status"])) {
        if (result$POData[row, "Status"] == StatusCompleted & (input$sliderdefprogsteps == "Configure Workflow Output" | input$sliderdefprogsteps == "Browse & re-run" )) {
          
          if (input$sliderdefprogsteps == "Configure Workflow Output") {
            show("panelconfigureoutput")
            logMessage("showing panelconfigureoutput")
            .defaultview(session)
          }
          
          
          prtable <- getProcessData(dbSettings, userId(), 0, 0, 0)
          procId <- toString(prtable[row, 1][length(prtable[row, 1])])
          
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
          hide("panelconfigureoutput")
        }
      }
    }
  })
  
  #simplified view selection
  observe( if (active()) {
    if (length(input$chkgulprog) > 0 |  length(input$chkgulstate) > 0 |  
        length(input$chkgulcounty) > 0 |  length(input$chkgulloc) > 0 |
        length(input$chkgullob) > 0 | length(input$chkgulpolicy) > 0) {
      updateCheckboxInput(session, "chkinputGUL", value = TRUE)
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
  
  # show advanced / basic views
  
  onclick("abtnadvanced", {
    .advancedview()
  })
  
  onclick("abtnbasic", {
    .basicview()
  })
  
  
  # A function to generate process run
  generateRun <- function() {
    
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
      
      runId <- generateRun()
      .defaultview(session)
      if (is.null(runId)) {
        showNotification(type = "error",
                         "Process Run ID could not be generated. So process run cannot be executed.")
      } else {      
        status <- runProcess(apiSettings, runId)
        logMessage(sprintf("runProcess status: %s", status))
        if (grepl("success", status, ignore.case = TRUE)) {
          showNotification(type = "message", 
                           sprintf("Created Process Run ID: %s and process run is executing.",
                                   runId))
          .reloadRunData()
        } else {
          showNotification(type = "warning",
                           sprintf("Created Process Run ID: %s. But process run executing failed.",
                                   runId))
          hide("prrunoutput")
          hide("abuttondisplayoutput")
          show("prrunlogtable")
          logMessage("showing prrunlogtable")
          hide("abuttonhidelog")
          hide("abuttonshowlog")
          
        }
        updateSliderTextInput(session, inputId = "sliderdefprogsteps", selected = "Browse & re-run")
      }
    }
  })
  
  #Default Advanced view
  observe(if (active()) {
    
    show("perilwind")
    show("perilsurge")
    show("perilquake")
    show("perilflood")
    show("demandsurge")
    show("leakagefactor")
    
    disable("chkgulpolicy")
    
    disable(selector = "#chkgulcounty input[value='gulcountyFullUncAEP']")
    disable(selector = "#chkgulcounty input[value='gulcountyFullUncOEP']")
    disable(selector = "#chkgulcounty input[value='gulcountyAEPWheatsheaf']")
    disable(selector = "#chkgulcounty input[value='gulcountyOEPWheatsheaf']")
    
    disable(selector = "#chkgulloc input[value='gullocFullUncAEP']")
    disable(selector = "#chkgulloc input[value='gullocFullUncOEP']")
    disable(selector = "#chkgulloc input[value='gullocAEPWheatsheaf']")
    disable(selector = "#chkgulloc input[value='gullocOEPWheatsheaf']")
    
    disable(selector = "#chkilcounty input[value='ilcountyFullUncAEP']")
    disable(selector = "#chkilcounty input[value='ilcountyFullUncOEP']")
    disable(selector = "#chkilcounty input[value='ilcountyAEPWheatsheaf']")
    disable(selector = "#chkilcounty input[value='ilcountyOEPWheatsheaf']")
    
    disable(selector = "#chkilloc input[value='illocFullUncAEP']")
    disable(selector = "#chkilloc input[value='illocFullUncOEP']")
    disable(selector = "#chkilloc input[value='illocAEPWheatsheaf']")
    disable(selector = "#chkilloc input[value='illocOEPWheatsheaf']")
    
  })
    
  # Run Process -------------------------------------------------------------
  
  ### selectprogOasisID

  observe( if (active()) {
    if ( !is.null(result$POData)) {
      result$listprogOasis <- nrow(result$POData)
      if (input$sliderdefprogsteps == "Browse & re-run") {
        updateSelectInput(session, inputId = "selectprogOasisID", choices = result$POData[1:result$listprogOasis,1])
      }
    }
  })
  

  observeEvent(input$tableProgOasisOOK_rows_selected, {
    if (length(input$tableProgOasisOOK_rows_selected) > 0 ) {
      prgId <- result$POData[input$tableProgOasisOOK_rows_selected,1]
      updateSelectInput(session, inputId = "selectprogOasisID", choices = result$POData[1:result$listprogOasis,1], selected = prgId)
      .reloadRunData()
    }
  })
  
  observeEvent( input$selectprogOasisID, {
    .reloadRunData() 
  })

  
  ### Navigation
  
  observeEvent(input$processrundata_rows_selected, {
    if (length(input$processrundata_rows_selected) > 0) {
      if (result$prcrundata[c(input$processrundata_rows_selected), "ProcessRunStatus"] != StatusCompleted) {
        hide("abuttondisplayoutput")
      } else {
        show("abuttondisplayoutput")
      }
    }
  })
  
  observe(
    if (!is.null(result$prcrundata)) {
      if (nrow(result$prcrundata) == 0) {
        hide("abuttondisplayoutput")
      } 
    })
  
  observeEvent(input$abuttondisplayoutput, {
    result$navigate <- structure("BR", count = input$abuttondisplayoutput)
  })  
  
  ### Rerun Process 
  
  onclick("abuttonrerunpr", {
    if (length(input$processrundata_rows_selected) > 0) {
      result$prrunid <- (result$prcrundata[c(input$processrundata_rows_selected), 1][length(result$prcrundata[c(input$processrundata_rows_selected), 1])])
      outputlist <- executeDbQuery(dbSettings, paste0("exec dbo.getOutputOptionOutputs @processrunid = ", result$prrunid ))
      runparamsforpr <- executeDbQuery(dbSettings, paste0("exec dbo.getProcessRunParams ", result$prrunid ))

      show("panelconfigureoutput")
      show("hidepanelconfigureoutput")
      
      updateTextInput(session, "tinputprocessrunname", value = result$prcrundata[c(input$processrundata_rows_selected), 2])
      
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
  
  ### Hide Output Configuration panel
  observeEvent(input$abuttonehidepanelconfigureoutput, {
    hide("panelconfigureoutput")
    hide("hidepanelconfigureoutput")
  })
  
  ### Log Button
  
  observeEvent( input$abuttonshowlog, {
    
    if (length(input$processrundata_rows_selected) > 0) {
      
      show("prrunlogtable")
      logMessage("showing prrunlogtable")
      show("abuttonhidelog")
      hide("abuttonshowlog")
      
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
      
      hide("prrunlogtable")
      hide("abuttonhidelog")
      show("abuttonshowlog")
    }
    
  })
  
  observeEvent(input$abuttonhidelog, {
    
    hide("prrunlogtable")
    hide("abuttonhidelog")
    show("abuttonshowlog")
    
  })
  
  ### Log Table
  
  output$log <- renderDataTable({
    
    if (length(row <- input$processrundata_rows_selected) > 0 ) {
      
      # manual refresh button
      force(input$abuttonrefreshprrunlogs)
      
      # reload automatically every so often
      #invalidateLater(reloadMillis)
      
      wfid <- result$prcrundata[row, 1][length(result$prcrundata[row, 1])]
      
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
  
  # reload functions --------------------------------------------------------
  
  #Reload Programme table
  .reloadDPProgData <- function() {
    result$DPProgDataCounter <- isolate(result$DPProgDataCounter + 1)
  }
  
  #Reload Programme Details table
  .reloadProgDetails <- function() {
    result$progDetailsCounter <- isolate(result$progDetailsCounter + 1)
  }
  
  #Reload Programme Model table
  .reloadPOData <- function() {
    result$PODataCounter <- isolate(result$PODataCounter + 1)
  }
  
  #Reload Programme Model Details table
  .reloadProgFiles <- function() {
    result$progFilesCounter <- isolate(result$progFilesCounter + 1)
  }
  
  #Reload Process Runs table
  .reloadRunData <- function() {
    result$prcrundataCounter <- isolate(result$prcrundataCounter + 1)
  } 
  
  # Help Functions General ----------------------------------------------------
  
  # hide all panels
  .hideDivs <- function(){
    hide("panelamendprogramme")
    hide("panelcreateprogramme")
    hide("paneldefineids")
    hide("panelamendmodel")
    hide("panelassociatemodel")
    hide("panelconfigureoutput")
    hide("panelrunprogramme")
    hide("divdefprogdetails")
    hide("divprogmodeltable")
    hide("divprogoasisfiles")
    hide("prruntable")
    hide("prrunlogtable")
    hide("divselectprogrammeID")
    hide("divselectprogOasisID")
    hide("hidepanelconfigureoutput")
  }  
  
  # table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function() {
    
    options <- list(
      search = list(caseInsensitive = TRUE), 
      searchHighlight = TRUE,
      processing = 0,
      scrollX = TRUE,
      pageLength = 5,
      autoWidth = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    
    return(options)
  }
  
# Helper Functions Create Programme ---------------------------------------

  .clearDPAccountSelection <- function() {
    accounts <- getAccountName(dbSettings)
    updateSelectInput(session, "sinputDPAccountName",
                      choices = createSelectOptions(accounts, "Select Account"),
                      selected = c("Select Account" = 0))
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
                      selected = toString(result$DPProgData[(result$DPprogID),3]))
  }
  
  .updateTransformNameSelection <- function() {
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectInput(session, "sinputTransformname",
                      choices = createSelectOptions(transforms, "Select Transform",
                                                    labelCol = 1, valueCol = 2),
                      selected = toString(result$DPProgData[(result$DPprogID),5]))
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
  
  .updateOOKProgrammeSelection <- function() {
    programmes <- getProgrammeList(dbSettings)
    updateSelectInput(session, "sinputookprogid",
                      choices = createSelectOptions(programmes, "Select Programme"),
                      selected = toString(result$DPProgData[(result$DPprogID),1]))
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
    
    ####
    
    updateSliderInput(session, "sliderleakagefac", "Leakage factor:",
                      min = 0, max = 100, value = 0.5, step = 0.5)
    if (result$progOasisId != -1) {
      updateSelectInput(session, "sinputeventset",
                        choices = getEventSet(dbSettings, result$progOasisId ))
      updateSelectInput(session, "sinputeventocc",
                        choices = getEventOccurrence(dbSettings, result$progOasisId )) 
    }
    updateCheckboxInput(session, "chkinputprwind", "Peril: Wind",
                        value = TRUE)
    updateCheckboxInput(session, "chkinputprstsurge", "Peril: Surge",
                        value = TRUE)
    updateCheckboxInput(session, "chkinputprquake", "Peril: Quake",
                        value = TRUE)
    updateCheckboxInput(session, "chkinputprflood", "Peril: Flood",
                        value = TRUE)
    updateCheckboxInput(session, "chkinputdsurge", "Demand Surge",
                        value = TRUE)
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
  
  .advancedview <- function() {
    logMessage("showing advanced view")
    show("configureOutputAdvancedGULUIOutput")
    show("configureOutputAdvancedILUIOutput")
    show("configureOutputAdvancedUIOutput")
    show("configureOutputPerilAdvancedUIOutput")
    show("basic")
    hide("advanced")
    show("saveoutput")
    show("clroutopt")
  }
  
  .basicview <- function() {
    logMessage("showing basic view")
    hide("configureOutputAdvancedGULUIOutput")
    hide("configureOutputAdvancedILUIOutput")
    hide("configureOutputAdvancedUIOutput")
    hide("configureOutputPerilAdvancedUIOutput")
    hide("basic")
    show("advanced")
    hide("saveoutput")
    hide("clroutopt")
  }
  
  .defaultview <- function(session) {
    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp(session)
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .defaultchkboxILgrp()
    
    .clearotherparams()
    .basicview()
  }
  
  .clearOutputOptions <- function() {
    updateSelectInput(session, "sinoutputoptions",
                      choices = c("<Select>", getOutputOptions(dbSettings)),
                      selected = "<Select>")
  }
  
  
  # Model Outout ------------------------------------------------------------

  moduleOutput <- list(
    navigate = reactive(result$navigate),
    progOasisId = reactive(result$progOasisId),
    processRunId = reactive(result$prrunid) 
  )
  
  return(moduleOutput)
  
}
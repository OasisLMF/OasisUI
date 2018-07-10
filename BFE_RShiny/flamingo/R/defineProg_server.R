
#' Programme Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater}; 
#' @return list of reactives:
#' \itemize{
#' 		\item{\code{navigate}}{reactive yielding navigation}
#' 		\item{\code{progOasisId}}{selected ProgOasis Id}
#' }
#' @rdname programmeDefinition
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDataTable
#' @export
programmeDefinition <- function(input, output, session, dbSettings,
    apiSettings, userId, active = reactive(TRUE), logMessage = message,
    reloadMillis = 10000) {
  
  ns <- session$ns
  
  result <- reactiveValues(
      DPProgData = NULL,
      DPProgDataCounter = 0,
      
      prog_flag = "",
      
      progOasisId = -1,
      
      progDetails = NULL,
      progDetailsCounter = 0,
      
      POData = NULL,
      PODataCounter = 0,
      
      progFiles = NULL,
      progFilesCounter = 0,
      
      navigate = NULL
  )
  
  reloadDPProgData <- function() {
    result$DPProgDataCounter <- isolate(result$DPProgDataCounter + 1)
  }
  
  reloadProgDetails <- function() {
    result$progDetailsCounter <- isolate(result$progDetailsCounter + 1)
  }
  
  reloadPOData <- function() {
    result$PODataCounter <- isolate(result$PODataCounter + 1)
  }
  
  reloadProgFiles <- function() {
    result$progFilesCounter <- isolate(result$progFilesCounter + 1)
  }
  
  ### When Activated (e.g. tab is openened)
  
  observe(if (active()) {
        
        result$prog_flag <- ""
        reloadProgDetails()
        hide("obtainoasiscrtprgdiv")
        hide("divprogmodeltable")
        hide("divSLFileUpload")
        hide("divSLFileSelect")
        hide("divSAFileUpload")
        hide("divSAFileSelect")
        hide("divSRFileUpload")
        hide("divSRFileSelect")
        hide("divSRSFileUpload")
        hide("divSRSFileSelect")
        hide("divdefprogdetails")
        hide("divprogoasisfiles")
        updateSelectInput(session, "sinputSLFile", selected="")
        updateSelectInput(session, "sinputSAFile", selected="")
        updateSelectInput(session, "sinputSRFile", selected="")
        updateSelectInput(session, "sinputSRSFile", selected="")
        
      })
  
  ### Programmme Table
  
  observe(if (active()) {
        
        # reload after pressing refresh
        force(input$abuttonprgtblrfsh)
        
        # reload automatically every so often
        invalidateLater(reloadMillis)
        
        # reload after reloadDPProgData is called 
        force(result$DPProgDataCounter)
        
        stmt <- buildDbQuery("getProgData")
        result$DPProgData <- executeDbQuery(dbSettings, stmt)
        
        logMessage("programme table refreshed")
        
      })
  
  output$tableDPprog <- renderDataTable({
        datatable(
            result$DPProgData,
            class = "flamingo-table display",
            rownames = TRUE,
            filter = "none",
            selection = "single",
            colnames = c('Row Number' = 1),
            options = list(
                columnDefs = list(list(visible = FALSE, targets = 0)),
                scrollX = TRUE,
                pageLength = 5
            )
        )
      })
  
  
  
  ### Programme Details Table
  
  observe(if (active()) {
        
        # reload after pressing refresh
        force(input$abuttondefprogrfsh)
        
        # reload automatically every so often
        invalidateLater(reloadMillis)
        
        # reload after reloadProgDetails is called
        force(result$reloadProgDetails)
        
        if(length(input$tableDPprog_rows_selected) > 0) {
          
          show("divdefprogdetails")
          
          progId <- result$DPProgData[input$tableDPprog_rows_selected, 1]
          
          stmt <- buildDbQuery("getProgFileDetails", progId)
          result$progDetails <- executeDbQuery(dbSettings, stmt)
          
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
            selection = "none",
            colnames = c('Row Number' = 1),
            options = list(
                columnDefs = list(list(visible = FALSE, targets = 0)),
                scrollX = TRUE
            )
        )
      })
  
  
  
  ### Helper Functions
  
  clearDPAccountSelection <- function() {
    accounts <- getAccountName(dbSettings)
    updateSelectInput(session, "sinputDPAccountName",
        choices = createSelectOptions(accounts, "Select Account"),
        selected = c("Select Account" = 0))
  }
  
  clearTransformNameSelection <- function() {
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectInput(session, "sinputTransformname",
        choices = createSelectOptions(transforms, "Select Transform",
            labelCol = 1, valueCol = 2),
        selected=c("Select Transform" = 0))
  }
  
  updateDPAccountSelection <- function() {
    accounts <- getAccountName(dbSettings) 
    updateSelectInput(session, "sinputDPAccountName",
        choices = createSelectOptions(accounts, "Select Account"),
        selected = toString(result$DPProgData[(input$tableDPprog_rows_selected),3]))
  }
  
  updateTransformNameSelection <- function() {
    transforms <- getTransformNameSourceCan(dbSettings)
    updateSelectInput(session, "sinputTransformname",
        choices = createSelectOptions(transforms, "Select Transform",
            labelCol = 1, valueCol = 2),
        selected = toString(result$DPProgData[(input$tableDPprog_rows_selected),5]))
  }
  
  
  
  ### Create/Amend/Delete - Programme
  
  onclick("buttoncreatepr", {
        
        result$prog_flag <- "C"
        
        hide("divFileUpload")
        toggleModal(session, "prog-crtupModal", toggle = "open")
        
        clearDPAccountSelection()
        
        updateTextInput(session, "tinputDPProgName", value = "")
        
        clearTransformNameSelection()
        
      })
  
  onclick("buttonamendpr", {
        if (length(input$tableDPprog_rows_selected) > 0) {
          
          result$prog_flag <- "A"
          
          show("divFileUpload") 
          toggleModal(session, "prog-crtupModal", toggle = "open")
          
          updateDPAccountSelection()
          
          updateTextInput(session, "tinputDPProgName",
              value = result$DPProgData[input$tableDPprog_rows_selected,2])
          
          updateTransformNameSelection()
          
        } else {
          showNotification(type = "warning", "Please select a Programme to Amend")
        }
      })
  
  
  onclick("buttondeletepr",{
        if (length(input$tableDPprog_rows_selected) > 0) {
          
          stmt <- buildDbQuery("deleteProg", result$DPProgData[input$tableDPprog_rows_selected,1])
          executeDbQuery(dbSettings, stmt)
          
          showNotification(type = "message", sprintf("Programme %s deleted",
                  result$DPProgData[input$tableDPprog_rows_selected,2]))
          
          reloadDPProgData()
          
        } else {
          
          showNotification(type = "warning", "Please select a Programme to Delete")
          
        }
      })
  
  
  
  ### Submit - Programme
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
          
          if (result$prog_flag == "A"){
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
        
        reloadDPProgData()

        toggleModal(session, "prog-crtupModal", toggle = "close")
        hide("divFileUpload")
        updateTextInput(session, "tinputDPProgName", value="")
        shinyjs::enable("buttoncreatepr")
        shinyjs::enable("buttonamendpr")
        result$prog_flag <- ""
      })
  
  onclick("abuttonProgCancel",{
        toggleModal(session, "prog-crtupModal", toggle = "close")
        hide("divFileUpload")
        hide("divSLFileUpload")
        hide("divSLFileSelect")
        hide("divSAFileUpload")
        hide("divSAFileSelect")
        updateTextInput(session, "tinputDPProgName", value="")
        updateSelectInput(session, "sinputSLFile", selected="")    
        updateSelectInput(session, "sinputSAFile", selected="")      
        result$prog_flag <- ""
      })
  
  
  
  ### Upload Location/Account/Reinsurance Files
  
  onclick("abuttonSLFileUpload", {
        inFile <- input$SLFile
        flc <- getFileLocationPath(dbSettings, "Exposure File")
        flcopy <- file.copy(inFile$datapath,
            file.path(flc,inFile[1,1]), overwrite = TRUE)
        logMessage(file.path(flc,inFile[1,1]))
        if (flcopy == TRUE){
          recordId <- createFileRecord(dbSettings,
              inFile[1,1], "Source Loc File", 101, flc, userId(),
              "Prog", result$DPProgData[input$tableDPprog_rows_selected,1])
          if (!is.null(recordId)) {
            showNotification(type = "message",
                paste("New File record id: ", recordId, " created."))
            reloadProgDetails()
          } else {
            showNotification(type = "error", "Could not create file record.")
          }
        } else{
          showNotification(type = "error", "File transfer failed.")
        }
      })
  
  onclick("abuttonSAFileUpload", {
        inFile <- input$SAFile
        flc <- getFileLocationPath(dbSettings, "Exposure File")
        flcopy <- file.copy(inFile$datapath,
            file.path(flc, inFile[1,1]), overwrite = TRUE)
        logMessage(file.path(flc,inFile[1,1]))
        if (flcopy == TRUE){
          recordId <- createFileRecord(dbSettings,
              inFile[1,1], "Source Acc File", 102, flc, userId(),
              "Prog", result$DPProgData[input$tableDPprog_rows_selected,1])
          if (!is.null(recordId)) {
            showNotification(type = "message",
                paste("New File record id: ", recordId, " created."))
            reloadProgDetails()
          } else {
            showNotification(type = "error", "Could not create file record.")
          }
        } else{
          showNotification(type = "error", "File transfer failed.")
        }
      })
    
  onclick("abuttonSRFileUpload", {
        inFile <- input$SRFile
        flc <- getFileLocationPath(dbSettings, "Exposure File")
        flcopy <- file.copy(inFile$datapath,
            file.path(flc, inFile[1,1]), overwrite = TRUE)
        logMessage(file.path(flc,inFile[1,1]))
        if (flcopy == TRUE){
          recordId <- createFileRecord(dbSettings,
              inFile[1,1], "Source Reinsurance File", 401, flc, userId(),
              "Prog", result$DPProgData[input$tableDPprog_rows_selected,1])
          if (!is.null(recordId)) {
            showNotification(type = "message",
                paste("New File record id: ", recordId, " created."))
            reloadProgDetails()
          } else {
            showNotification(type = "error", "Could not create file record.")
          }
        } else{
          showNotification(type = "error", "File transfer failed.")
        }
      })
  
  onclick("abuttonSRSFileUpload", {
        inFile <- input$SRSFile
        flc <- getFileLocationPath(dbSettings, "Exposure File")
        flcopy <- file.copy(inFile$datapath,
            file.path(flc, inFile[1,1]), overwrite = TRUE)
        logMessage(file.path(flc,inFile[1,1]))
        if (flcopy == TRUE){
          recordId <- createFileRecord(dbSettings,
              inFile[1,1], "Source Reinsurance Scope File", 402, flc, userId(),
              "Prog", result$DPProgData[input$tableDPprog_rows_selected,1])
          if (!is.null(recordId)) {
            showNotification(type = "message",
                paste("New File record id: ", recordId, " created."))
            reloadProgDetails()
          } else {
            showNotification(type = "error", "Could not create file record.")
          }
        } else{
          showNotification(type = "error", "File transfer failed.")
        }
      })

  
  
  ### Link Location/Account File
  
  onclick("abuttonSLFileLink",{
        res <- executeDbQuery(dbSettings,
            paste("exec dbo.updateSourceLocationFileForProg ",
                input$sinputselectSLFile, ", ",result$DPProgData[input$tableDPprog_rows_selected,1]))
        if(input$sinputselectSLFile != ""){
          if (is.null(res)){
            showNotification(type = "error", "Failed to link the File!")
          }else {
            showNotification(type = "message",
                paste("Location File linked to Programme", result$DPProgData[input$tableDPprog_rows_selected,2]))
          }
        }else{
          showNotification(type = "warning", "Please select a file to Link")
        }
      })
  
  onclick("abuttonSAFileLink",{
        res <- executeDbQuery(dbSettings,
            paste("exec dbo.updateSourceAccountFileForProg ",
                input$sinputselectSAFile, ", ", result$DPProgData[input$tableDPprog_rows_selected,1]))
        if(input$sinputselectSAFile != ""){
          if (is.null(res)){
            showNotification(type = "error", "Failed to link the File!")
          }else {
            showNotification(type = "message", 
                paste("Location File linked to Programme", result$DPProgData[input$tableDPprog_rows_selected,2]))
          }
        }else{
          showNotification(type = "warning", "Please select a file to Link")
        }
      })
  
  
  
  ### Load Programme data
  
  onclick("buttonloadcanmodpr",{
        if(length(input$tableDPprog_rows_selected) > 0) {

          loadprogdata <- loadProgrammeData(apiSettings,
              progId = toString(result$DPProgData[input$tableDPprog_rows_selected,1]))
          
          if(loadprogdata == 'success' || loadprogdata == 'Success') {
            showNotification(type = "message", "Initiating load programme data...")
          } else {
            showNotification(type = "error", "Failed to load programme data.")
          }
        } else {
          showNotification(type = "warning", "Please select a Programme to load programme data.")
        }
        
      })

  
  
  ### Display File Upload/link options
  observe(if (active()) {
        
        if(input$sinputSLFile == "U") {
          show("divSLFileUpload")
          disable("abuttonSLFileUpload")
          hide("divSLFileSelect")
        } else if(input$sinputSLFile == "S") {
          show("divSLFileSelect")
          hide("divSLFileUpload")
        }
        
      })
  
  observe(if (active()) {
        
        if(input$sinputSAFile == "U") {
          show("divSAFileUpload")
          disable("abuttonSAFileUpload")
          hide("divSAFileSelect")
        } else if(input$sinputSAFile == "S") {
          show("divSAFileSelect")
          hide("divSAFileUpload")
        }
        
      })
  
  observe(if (active()) {

        if(input$sinputSRFile == "U") {
          show("divSRFileUpload")
          disable("abuttonSRFileUpload")
          hide("divSRFileSelect")
        } else if(input$sinputSRFile == "S") {
          show("divSRFileSelect")
          hide("divSRFileUpload")
        }

      })
  
  observe(if (active()) {

        if(input$sinputSRSFile == "U") {
          show("divSRSFileUpload")
          disable("abuttonSRSFileUpload")
          hide("divSRSFileSelect")
        } else if(input$sinputSRSFile == "S") {
          show("divSRSFileSelect")
          hide("divSRSFileUpload")
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
          if (input$sinputSLFile == "S"){
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
          if (input$sinputSAFile == "S"){
            SAfiles <- getFileSourceAccountFile(dbSettings)
            updateSelectInput(session, "sinputselectSAFile",
                choices = createSelectOptions(SAfiles, labelCol = 1,
                    valueCol = 2))
          }
        }
      })
  
  observe(if (active()) {
        if (input$sinputSRFile == "U") {
          options(shiny.maxRequestSize = 1024*1024^2)
          inFile <- input$SRFile
          if (!is.null(inFile)) {
            enable("abuttonSRFileUpload")
          }
        } else {
          if (input$sinputSRFile == "S"){
            SRfiles <- getFileSourceAccountFile(dbSettings)
            updateSelectInput(session, "sinputselectSRFile",
                choices = createSelectOptions(SRfiles, labelCol = 1,
                    valueCol = 2))
          }
        }
      })

  observe(if (active()) {
        if (input$sinputSRSFile == "U") {
          options(shiny.maxRequestSize = 1024*1024^2)
          inFile <- input$SRSFile
          if (!is.null(inFile)) {
            enable("abuttonSRSFileUpload")
          }
        } else {
          if (input$sinputSRSFile == "S"){
            SRSfiles <- getFileSourceAccountFile(dbSettings)
            updateSelectInput(session, "sinputselectSRSFile",
                choices = createSelectOptions(SRSfiles, labelCol = 1,
                    valueCol = 2))
          }
        }
      })
  
  
  ### Programme Model Table (previously OOK)
  
  observe(if (active()) {
        
        # refresh button
        force(input$abuttonookrefresh)
        
        force(result$PODataCounter)
        
        # reload automatically every so often
        invalidateLater(reloadMillis)
        
        if(length(input$tableDPprog_rows_selected) > 0) {
          
          show("divprogmodeltable")
        
          prgId <- result$DPProgData[input$tableDPprog_rows_selected,1]
          
          result$POData <- getProgOasisForProgdata(dbSettings, prgId)
          
          logMessage("programme model table refreshed")
          
          programmes <- getProgrammeList(dbSettings)
          updateSelectInput(session, "sinputookprogid",
              choices = createSelectOptions(programmes, "Select Programme"),
              selected = result$DPProgData[input$tableDPprog_rows_selected,1])
        
        } else {
          
          hide("divprogmodeltable")
          hide("divprogoasisfiles")
          hide("obtainoasiscrtprgdiv")
          
        }
      
      })
  
  output$tableProgOasisOOK <- renderDataTable(
      if (!is.null(result$POData)) {
        
        datatable(
            result$POData,
            class = "flamingo-table display",
            rownames = TRUE,
            filter = "none",
            selection = "single",
            colnames = c('Row Number' = 1),
            options = list(
                columnDefs = list(list(visible = FALSE, targets = 0)),
                scrollX = TRUE,
                pageLength = 5
            )
        )
      
      })
  
  
  
  ### Programme Oasis File Table / Programme Model Details
  
  # observer to display prog oasis files
  observe(if (active()) {
        
        # on click of prog oasis refresh table
        force(input$abuttonprgoasisrfsh)
      
        force(result$progFilesCounter)
        
        # reload automatically every so often
        invalidateLater(reloadMillis)
        
        if(length(input$tableProgOasisOOK_rows_selected) > 0) {
          
          show("divprogoasisfiles")
          
          progOasisId <- toString(result$POData[input$tableProgOasisOOK_rows_selected,1])
          
          stmt <- buildDbQuery("getProgOasisFileDetails", progOasisId)
          result$progFiles <- executeDbQuery(dbSettings, stmt)
          
          logMessage("programme files table refreshed")
          
        } else{
          hide("divprogoasisfiles")
        }
      })
  
  output$tabledisplayprogoasisfiles <- renderDataTable(
      if (!is.null(result$progFiles)){
        datatable(
            result$progFiles,
            class = "flamingo-table display",
            rownames = TRUE,
            filter = "none",
            selection = "none",
            colnames = c('Row Number' = 1),
            options = list(
                columnDefs = list(list(visible = FALSE, targets = 0)),
                pageLength = 20,
                scrollX = TRUE
            )
        )
      })
  
  
  
  ### Helper Functions
  
  clearOOKProgrammeSelection <- function() {
    programmes <- getProgrammeList(dbSettings)
    updateSelectInput(session, "sinputookprogid",
        choices = createSelectOptions(programmes, "Select Programme"),
        selected = c("Select Programme"= 0))
  }
  
  clearOOKModelSelection <- function() {
    models <- getModelList(dbSettings)
    updateSelectInput(session, "sinputookmodelid",
        choices = createSelectOptions(models, "Select Model"),
        selected = c("Select Model"= 0))
  }
  
  clearOOKTransformSelection <- function() {
    transforms <- getTransformNameCanModel(dbSettings)
    updateSelectInput(session, "sinputProgModTransform",
        choices = createSelectOptions(transforms, "Select Transform",
            labelCol = 1, valueCol = 2),
        selected = c("Select Transform" = 0))
  }
  
  clearOOKSidebar <- function() {
    clearOOKProgrammeSelection()
    clearOOKModelSelection()
    clearOOKTransformSelection()
  }
  
  updateOOKProgrammeSelection <- function() {
    programmes <- getProgrammeList(dbSettings)
    updateSelectInput(session, "sinputookprogid",
        choices = createSelectOptions(programmes, "Select Programme"),
        selected = result$DPProgData[input$tableDPprog_rows_selected,1])
  }
  
  
  
  ### Create/Amend/Delete - Programme Model
  
  # onclick of create prog oasis in main panel
  onclick("abuttonmaincreateprog", {
        
        show("obtainoasiscrtprgdiv")
        hide("divFileUpload")
        
        updateOOKProgrammeSelection()
        clearOOKModelSelection()
        clearOOKTransformSelection()
        
        toggleModal(session, "progModel-crtModal", toggle = "open")
        
      })
  
  # onclick of cancel create prog oasis
  onclick("abuttoncancreateprog", {
        
        toggleModal(session, "progModel-crtModal", toggle = "close")
        
        hide("divamdprog")
        hide("obtainoasiscrtprgdiv")
        
        clearOOKSidebar()
        
      })
  
  
  # on click of create prog oasis button
  onclick("abuttoncrprogoasis",{
        if(isolate(input$sinputookprogid) > 0 &&
            isolate(input$sinputookmodelid) > 0) {
          
          progOasisId <- createProgOasis(dbSettings,
              isolate(input$sinputookprogid),
              isolate(input$sinputookmodelid),
              isolate(input$sinputProgModTransform))
          
          showNotification(type = "message",
              paste("Prog Oasis id:", progOasisId,  " created."))
          
          clearOOKSidebar()
          
          toggleModal(session, "progModel-crtModal", toggle = "close")
          hide("obtainoasiscrtprgdiv")
          
          reloadPOData()
          
        } else{
          
          showNotification(type = "warning", "Please select both the fields.")
          
        }
      }
  )
  
  # on click of load programme model
  onclick("abuttonloadprogmodel",{
        if(length(input$tableProgOasisOOK_rows_selected) > 0){

          loadprogmodel <- loadProgrammeModel(apiSettings,
              progOasisId = toString(result$POData[input$tableProgOasisOOK_rows_selected,1]))
          
          if(loadprogmodel == 'success' || loadprogmodel == 'Success'){
            showNotification(type = "message", "Initiating load programme model..")
            reloadProgFiles()
          } else {
            showNotification(type = "error", "Failed to load programme model.")
          }
        }else{
          showNotification(type = "warning",
              "Please select a Prog Oasis to load Programme model.")
        }
        
      })
  
  
  ### Navigation to Process run screen
  
  # navigation to Define process
  onclick("abuttongotoprocessrun", {
        if (length(rows <- input$tableProgOasisOOK_rows_selected) > 0){
          
          result$progOasisId <- toString(result$POData[rows, 1])
          
          result$navigate <- structure("WF", count = input$abuttongotoprocessrun)
          
        } else {
          showNotification(type = "warning",
              "Please select a ProgOasis Id to navigate to Process Run.")
        }
      })
  
  
  
  ### Export to Excel
  
  output$DPPdownloadexcel <- downloadHandler(
      filename ="programmelist.csv",
      content = function(file) {
        write.csv(result$DPProgData, file)}
  )
  
  
  ### Module Output
  
  moduleOutput <- list(
      navigate = reactive(result$navigate),
      progOasisId = reactive(result$progOasisId)
  )
  
  return(moduleOutput)
  
}





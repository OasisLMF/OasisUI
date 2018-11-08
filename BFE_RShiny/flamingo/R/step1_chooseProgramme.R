# step1_chooseProgramme Module -------------------------------------------------

# UI ---------------------------------------------------------------------------
#' step1_chooseProgrammeUI
#'
#' @rdname step1_chooseProgramme
#'
#' @description UI/View for the step1_chooseProgramme.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
step1_chooseProgrammeUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Section "Choose Programme" = "1"
    div(id = ns("panelProgrammeTable"), panelProgrammeTable(id)),
    hidden(div(id = ns("panelProgrammeDetails"), panelProgrammeDetails(id))),
    hidden(div(id = ns("panelDefineProgramme"), panelDefineProgramme(id))),
    hidden(div(id = ns("panelLinkFiles"), panelLinkFiles(id)))
  )
}

#' panelProgrammeTable
#'
#' @rdname panelProgrammeTable
#'
#' @description Function wrapping panel to show created programmes table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelProgrammeTable <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("progtbl"),
    heading = tagAppendChildren(
      h4("Programme Table"),
      actionButton(inputId = ns("abuttonprgtblrfsh"), label = "Refresh", style = "float: right;")
    ),
    DTOutput(ns("tableDPprog")),
    flamingoButton(ns("buttoncreatepr"), "Create Programme", align = "centre"),
    flamingoButton(ns("buttonamendpr"), "Amend Programme", align = "centre") %>%
      bs_embed_tooltip(title = programme_Definition_Single$buttonamendpr, placement = "right"),
    flamingoButton(ns("buttondeletepr"), "Delete Programme", align = "centre") %>%
      bs_embed_tooltip(title = programme_Definition_Single$buttondeletepr, placement = "right"),
    flamingoButton(ns("buttonprogdetails"), "Show Details", align = "centre") %>%
      bs_embed_tooltip(title = programme_Definition_Single$buttonprogdetails, placement = "right"),
    actionButton(ns("buttonpgotonextstep"), "Proceed to Choose Model", style = "float:right")
  )
}

#' panelProgrammeDetails
#'
#' @rdname panelProgrammeDetails
#'
#' @description Function wrapping panel to show details of programme.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
panelProgrammeDetails <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("progdtl"),
    heading = tagAppendChildren(
      h4("Details of Programme"),
      uiOutput(ns("paneltitleProgrammeDetails"), inline = TRUE),
      actionButton(inputId = ns("abuttondefprogrfsh"), label = "Refresh", style = "float: right;"),
      actionButton(inputId = ns("buttonhideprogdetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("tableprogdetails"))
  )
}

#' panelDefineProgramme
#'
#' @rdname panelDefineProgramme
#'
#' @description Function wrapping panel to create/amend programme.
#' 
#' @template params-module-ui
#'
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelDefineProgramme <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("progdef"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleDefineProgramme"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidedefineprogpanel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
      column(12, h4("Programme metadata"))),
    fluidRow(
      column(4,
             selectizeInput(ns("sinputDPAccountName"), "Account Name", choices = c(""), selected = character(0),
                            options = list(
                              allowEmptyOption = TRUE,
                              placeholder = 'Select',
                              onInitialize = I('function() { this.setValue(""); }'))
             )),
      column(4,
             textInput(inputId = ns("tinputDPProgName"), label = "Programme Name")),
      column(4,
             selectizeInput(ns("sinputTransformname"), "Transform Name", choices = c(""), selected = character(0),
                            options = list(
                              allowEmptyOption = TRUE,
                              placeholder = 'Select',
                              onInitialize = I('function() { this.setValue(""); }'))
             ) %>%
               bs_embed_tooltip(title = programme_Definition_Single$sinputTransformname,
                                placement = "right"))),
    fluidRow(column(4,
                    flamingoButton(ns("abuttonProgSubmit"), "Submit")), style = "float:right") %>%
      bs_embed_tooltip(title = programme_Definition_Single$abuttonProgSubmit, placement = "right")
  )
}


#' panelLinkFiles
#'
#' @rdname panelLinkFiles
#'
#' @description Function wrapping panel to link files to a programme.
#' 
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelLinkFiles <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("proglink"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleLinkFiles"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelinkfilespanel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),

    fluidRow(
      column(12, h4("Link input files to programme"))),
    fluidRow(
      # Source Location File
      column(4,
             selectInput(inputId = ns("sinputSLFile"), label = "Source Location File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSLFileUpload"),
                        fileInput(inputId = ns("SLFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSLFileUpload"), label = "Upload File", align = "left", enable = FALSE))),
             hidden(div(id = ns("divSLFileSelect"),
                        selectInput(inputId = ns("sinputselectSLFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSLFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSLFileView"), label = "View", align = "left")))),
      ## Source Account File
      column(4,
             selectInput(inputId = ns("sinputSAFile"), label = "Source Account File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSAFileUpload"),
                        fileInput(inputId = ns("SAFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSAFileUpload"), label = "Upload File", align = "left"))),
             hidden(div(id = ns("divSAFileSelect"),
                        selectInput(inputId = ns("sinputselectSAFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSAFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSAFileView"), label = "View", align = "left"))))),
    fluidRow(
      column(12, h4("Link reinsurance input files to programme"))),
    fluidRow(
      ## Source Reinsurance File
      column(4,
             selectInput(inputId = ns("sinputSRFile"), label = "Source Reinsurance File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSRFileUpload"),
                        fileInput(inputId = ns("SRFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSRFileUpload"), label = "Upload File", align = "left"))),
             hidden(div(id = ns("divSRFileSelect"),
                        selectInput(inputId = ns("sinputselectSRFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSRFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSRFileView"), label = "View", align = "left")))),
      ## Source Reinsurance Scope File
      column(4,
             selectInput(inputId = ns("sinputSRSFile"), label = "Source Reinsurance Scope File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSRSFileUpload"),
                        fileInput(inputId = ns("SRSFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSRSFileUpload"), label = "Upload File", align = "left"))),
             hidden(div(id = ns("divSRSFileSelect"),
                        selectInput(inputId = ns("sinputselectSRSFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSRSFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSRSFileView"), label = "View", align = "left"))))
    ),
    fluidRow(
      column(12,
             actionButton(inputId = ns("abuttonProgCancel"), label = "Clear", style = "inline: true;float:right;"),
             flamingoButton(inputId = ns("buttonloadcanmodpr"), label = "Load Programme", style = "inline: true;float:right;margin-right: 10px;")))
  )
}

# Server -----------------------------------------------------------------------
#' step1_chooseProgramme
#'
#' @rdname step1_chooseProgramme
#'
#' @description Server logic to step1_chooseProgramme.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param currstep current selected step.
#' @param selectprogrammeID selected programme ID.
#'
#' @return selectprogrammeID Id of selected programmeID
#' @return DPProgData POData model association table.
#' @return newstep navigation step

#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom shinyjs onclick
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT DTOutput
#' @importFrom DT selectPage
#' @importFrom dplyr select
#'
#' @export
step1_chooseProgramme <- function(input, output, session,
                                  dbSettings,apiSettings, userId,
                                  active = reactive(TRUE),
                                  logMessage = message,
                                  currstep = reactive(-1),
                                  selectprogrammeID = reactive("")

) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  #values to stop ping pong effect
  stop_selProgID <- check_selProgID <- 0

  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # reactive for selectprogrammeID
    selectprogrammeID = "",
    # reactive value for the programme table
    DPProgData = NULL,
    # SL file to view
    viewSLfile = NULL,
    # SA file to view
    viewSAfile = NULL,
    # reactive value for details of programme table
    progDetails = NULL,
    # flag to know if the user is creating or amending a programme
    prog_flag = "C"
  )

  #Set Params
  observe( if (active()) {
    result$selectprogrammeID <- isolate(selectprogrammeID())
  })

  # Panels Visualization -------------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 1 ){
      .defaultCreateProg()
      .reloadDPProgData()
    }
  })

  ### Programme Table ----------------------------------------------------------
  output$tableDPprog <- renderDT({
    logMessage("re-rendering programme table")
    if (!is.null(result$DPProgData)) {
      if (isolate(result$selectprogrammeID) != "") {
        rowToSelect <- match(isolate(result$selectprogrammeID), result$DPProgData[, DPProgData.ProgrammeID])
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

  # Programme Details Table ----------------------------------------------------
  output$tableprogdetails <- renderDT({
    if (!is.null(result$progDetails) && nrow(result$progDetails) > 0) {
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
      .nothingToShowTable(contentMessage = paste0("No files available for Programme ID ", result$selectprogrammeID))
    }
  })

  # Title Programme Details Panel
  output$paneltitleProgrammeDetails <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
    progName <- ifelse(progName == " ", "", paste0('"', progName, '"'))
    paste0('Details of Programme id ', progId, ' ', progName)
  })

  # Enable and disable buttons
  observeEvent({
    result$DPProgData
    input$tableDPprog_rows_selected
    }, ignoreNULL = FALSE, ignoreInit = TRUE, {
      disable("buttonprogdetails")
      disable("buttondeletepr")
      disable("buttonamendpr")
      disable("buttonpgotonextstep")
    if (length(input$tableDPprog_rows_selected) > 0) {
      enable("buttonprogdetails")
      enable("buttondeletepr")
      enable("buttonamendpr")
      if (result$DPProgData[input$tableDPprog_rows_selected, DPProgData.Status] == StatusCompleted) {
        enable("buttonpgotonextstep")
      }
    }
  })

  # Show Programme Details
  onclick("buttonprogdetails", {
    hide("panelDefineProgramme")
    hide("panelLinkFiles")
    show("panelProgrammeDetails")
    logMessage("showing panelProgrammeDetails")
    .reloadProgDetails()
  })

  # Hide Programme Details
  onclick("buttonhideprogdetails", {
    hide("panelProgrammeDetails")
    logMessage("hiding panelProgrammeDetails")
  })

  # Create / Amend Programme sub-panel -----------------------------------------
  # Create/Amend programme title
  output$paneltitleDefineProgramme <- renderUI({
    if (result$prog_flag == "C" || is.null(input$tableDPprog_rows_selected)) {
      "Create Programme"
    } else if (result$prog_flag == "A") {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
      progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
      progName <- ifelse(progName == " ", "", paste0('"', progName, '"'))
      paste0('Amend Programme id ', progId, ' ', progName)
    }
  })

  # Hide Programme Definition Panel
  onclick("abuttonhidedefineprogpanel", {
    hide("panelDefineProgramme")
  })

  # Create Programme
  onclick("buttoncreatepr", {
    hide("panelProgrammeDetails")
    hide("panelLinkFiles")
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
    hide("panelProgrammeDetails")
    .defaultCreateProg()
    if (length(input$tableDPprog_rows_selected) > 0) {
      # TODO: review where/when/how this should be set
      result$prog_flag <- "A"
      #clear fields
      .updateDPAccountSelection()
      .updateProgrammeName()
      .clearSourceFilesSelection()
      .updateTransformNameSelection()
      show("panelDefineProgramme")
      show("panelLinkFiles")
      logMessage("showing panelDefineProgramme")
    }
  })

  # to enable and disable submit button for create programme
  observeEvent({
    input$tinputDPProgName
    input$sinputDPAccountName
    input$sinputTransformname
  }, ignoreInit = TRUE, {
    if (input$tinputDPProgName == "" || input$sinputDPAccountName == "" || input$sinputTransformname == "") {
      disable("abuttonProgSubmit")
    } else {
      enable("abuttonProgSubmit")
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

      } else {
        flamingoNotification(type = "message",
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
        flamingoNotification(type = "error",
                             paste("Failed to amend a Programme - ", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]))
      } else {
        flamingoNotification(type = "message",
                             paste("Programme ", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName], " amended."))
      }
    }

    # Reload Programme Table
    .reloadDPProgData()
    logMessage(paste("updating tableDPprog select because programme table was reloaded"))
    result$selectprogrammeID <- result$DPProgData[1, DPProgData.ProgrammeID]
    hide("panelDefineProgramme")
    show("panelLinkFiles")
  })

  ### Clear Programme Definition panel
  onclick("abuttonProgCancel", {
    result$prog_flag <- "C"
    .clearDPAccountSelection()
    .clearProgrammeName()
    .clearSourceFilesSelection()
    .clearTransformNameSelection()
  })

  # > Link files to Programme --------------------------------------------------

  # Link files to programme title
  output$paneltitleLinkFiles <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
    progName <- ifelse(progName == " ", "", paste0('"', progName, '"'))
    if (result$prog_flag == "C") {
      paste0('Provide Inputs to Programme id ', progId, ' ', progName)
    } else {
      paste0('Amend Inputs to Programme id ', progId, ' ', progName)
    }

  })

  # Hide Link files Panel
  onclick("abuttonhidelinkfilespanel", {
    hide("panelLinkFiles")
  })

  ### Load Programme Button
  onclick("buttonloadcanmodpr", {
    progId = result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    logMessage(paste("loading programme - progId is:", progId))
    loadprogdata <- loadProgrammeData(apiSettings,
                                      progId = toString(progId))
    if (loadprogdata == 'success' || loadprogdata == 'Success') {
      flamingoNotification(type = "message", "Initiating load programme data...")
    } else {
      flamingoNotification(type = "error", "Failed to load programme data")
    }
    .reloadDPProgData()
    .hideDivs()
    .defaultCreateProg()
  })

  # title for delete button
  output$progdelmodal <- renderUI({
    progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
    progName <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]
    paste0('Delete Programme id ', progId, ' "', progName,'"')
    })

  # Modal dialog of delete button
  .progdelmodal <- function() {
    ns <- session$ns
    modalDialog(label = "progdelmodal",
                title = uiOutput(ns("progdelmodal"), inline = TRUE),
                paste0("Are you sure you want to delete?"),
                footer = tagList(
                  flamingoButton(ns("abuttonuconfirmdel"),
                                 label = "Confirm", align = "center"),
                  actionButton(ns("abuttonucanceldel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # Delete Programme
  onclick("buttondeletepr",{
    showModal(.progdelmodal())
  })

  # onclick of confirm delete button
  onclick("abuttonuconfirmdel",{
    hide("panelProgrammeDetails")
    hide("panelDefineProgramme")
    hide("panelLinkFiles")
    stmt <- buildDbQuery("deleteProg", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID])
    executeDbQuery(dbSettings, stmt)
    flamingoNotification(type = "message",
                         sprintf("Programme %s deleted", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]))
    .reloadDPProgData()
    removeModal()
  })

  # onclick of cancel delete button
  onclick("abuttonucanceldel", {
    removeModal()
  })

  # > Source Files -------------------------------------------------------------
  # Upload Location/Account File
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
          flamingoNotification(type = "message",
                               paste("New File record id: ", recordId, " created"))
          #.reloadProgDetails()
        } else {
          flamingoNotification(type = "error", "Could not create file record")
        }
      } else {
        flamingoNotification(type = "error", "File transfer failed")
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
          flamingoNotification(type = "error", "Failed to link the File")
        } else {
          flamingoNotification(type = "message",
                               paste("File linked to Programme", result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeName]))
        }
      } else {
        flamingoNotification(type = "warning", "Please select a file to Link")
      }
    }
  }


  onclick("abuttonSLFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceLocationFileForProg ", inputID = input$sinputselectSLFile)
  })

  onclick("abuttonSAFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceAccountFileForProg ", inputID = input$sinputselectSAFile)
  })

  onclick("abuttonSRFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceReinsuranceFileForProg ", inputID = input$sinputselectSRFile)
  })

  onclick("abuttonSRSFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceReinsuranceScopeFileForProg ", inputID = input$sinputselectSRSFile)
  })

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
      SRfiles <- getFileSourceReinsuranceFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSRFile",
        choices = createSelectOptions(SRfiles, labelCol = 1, valueCol = 2)
      )
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
      SRSfiles <- getFileSourceReinsuranceScopeFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSRSFile",
        choices = createSelectOptions(SRSfiles, labelCol = 1, valueCol = 2)
      )
    }
  })

  # View source files
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
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSLfile <- .renderDTSourceFile(SourceFile = result$viewSLfile)

  # Source Account
  onclick("abuttonSAFileView", {
    if (input$sinputselectSLFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSAfile", Title = "Source Account File View", InputID = "tableviewSAfile"))
    } else {
      removeModal()
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSAfile <- .renderDTSourceFile(SourceFile = result$viewSAfile)

  # Source Reinsurance
  onclick("abuttonSRFileView", {
    if (input$sinputselectSRFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSRfile", Title = "Source Reinsurance File View", InputID = "tableviewSRfile"))
    } else {
      removeModal()
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSRfile <- .renderDTSourceFile(SourceFile = result$viewSRfile)

  # Source Reinsurance Scope
  onclick("abuttonSRSFileView", {
    if (input$sinputselectSRSFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSRSfile", Title = "Source Reinsurance Scope File View", InputID = "tableviewSRSfile"))
    } else {
      removeModal()
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSRSfile <-  .renderDTSourceFile(SourceFile = result$viewSRSfile)

  # Define selectprogrammeID ---------------------------------------------------
  # Add choices to selectprogrammeID, update selectprogrammeID
  observeEvent(result$DPProgData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste0("updating selectprogrammeID choices because programme table was reloaded - contains ", nrow(result$DPProgData), " rows"))
      if (result$selectprogrammeID == "") {
        # initial selection last programme created
        prgId <- result$DPProgData[1, DPProgData.ProgrammeID]
      } else {
        # keep current selection
        prgId <- result$selectprogrammeID
      }
      result$selectprogrammeID <- prgId
    }
  })

  # If selectprogrammeID changes, reload programme model table and set view back to default
  observeEvent(result$selectprogrammeID, ignoreInit = TRUE, {
    if (active()) {
      prgId <- ""
      if (!is.null(input$tableDPprog_rows_selected)) {
        prgId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
      }
      if (result$selectprogrammeID != prgId) {
        bl_dirty <- stop_selProgID > check_selProgID
        logMessage(paste("--- stop_selProgID is:", stop_selProgID))
        logMessage(paste("updating tableDPprog select because selectprogrammeID changed to", result$selectprogrammeID))
        if (result$selectprogrammeID != "") {
          if (!is.null(result$DPProgData) && nrow(result$DPProgData) > 0 && !bl_dirty ) {
            rowToSelect <- match(result$selectprogrammeID, result$DPProgData[, DPProgData.ProgrammeID])
            pageSel <- ceiling(rowToSelect/pageLength)
            #backward propagation
            if (is.null(input$tableDPprog_rows_selected)) {
                selectRows(dataTableProxy("tableDPprog"), rowToSelect)
                selectPage(dataTableProxy("tableDPprog"), pageSel)
                logMessage(paste("selected row is:", input$tableDPprog_rows_selected))
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
        if (bl_dirty) check_selProgID <<- check_selProgID + 1
      }
    }
  })

  # Refresh Buttons ------------------------------------------------------------
  onclick("abuttonprgtblrfsh", {
    .reloadDPProgData()
  } )

  onclick("abuttondefprogrfsh", {
    .reloadProgDetails()
  } )

  # Updates dependent on changed: tableDPprog_rows_selected --------------------
  observeEvent(input$tableDPprog_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste("input$tableDPprog_rows_selected is changed to:", input$tableDPprog_rows_selected))
      hide("panelProgrammeDetails")
      hide("panelDefineProgramme")
      hide("panelLinkFiles")

      if (length(input$tableDPprog_rows_selected) > 0) {
        # note that tableDPprog allows single row selection only
        prgId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]
        # #If incomplete show panel to link files
        currStatus <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.Status]
        if (!is.na(currStatus) && currStatus == StatusProcessing) {
          show("panelLinkFiles")
        }
        if (prgId != result$selectprogrammeID) {
          logMessage(paste("updating selectprogrammeID because selection in programme table changed to", prgId))
          # re-selecting the same programme ID in the drop-down would not re-trigger
          # any of the observers of the drop-down, however we then also want to be
          # sure not to increase stop_selProgID!
          stop_selProgID <<- stop_selProgID + 1
          result$selectprogrammeID <- prgId
        }
      } else {
        result$selectprogrammeID <- ""
      }
    }
  })

  # Help Functions -------------------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs called")
    # Section "Choose Programme" = "1"
    hide("panelProgrammeTable")
    hide("panelProgrammeDetails")
    hide("panelDefineProgramme")
    hide("panelLinkFiles")
  }

  #show default view for Section "Choose Programme" = "1"
  .defaultCreateProg <- function(){
    logMessage(".defaultCreateProg called")
    show("panelProgrammeTable")
  }


  # Reload Programme table
  .reloadDPProgData <- function() {
    # manual refresh button
    logMessage(".reloadDPProgData called")
    stmt <- buildDbQuery("getProgData")
    DPProgData <- executeDbQuery(dbSettings, stmt)
    if (!is.null(DPProgData)) {
      result$DPProgData <- DPProgData %>%
        replaceWithIcons()
      logMessage("programme table refreshed")
    }
    invisible()
  }

  # Reload Programme Details table
  .reloadProgDetails <- function() {
    logMessage(".reloadProgDetails called")
    if (length(input$tableDPprog_rows_selected) > 0) {
      progId <- result$DPProgData[input$tableDPprog_rows_selected, DPProgData.ProgrammeID]

      stmt <- buildDbQuery("getProgFileDetails", progId)
      progDetails <- executeDbQuery(dbSettings, stmt)
      if (!is.null(progDetails)) {
        result$progDetails  <- progDetails %>%
          replaceWithIcons()
      }
      logMessage("programme details table refreshed")
    } else {
      result$progDetails  <- NULL
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

  # Model Outout ---------------------------------------------------------------

  moduleOutput <- c(
    list(
      selectprogrammeID = reactive(result$selectprogrammeID),
      DPProgData = reactive({result$DPProgData}),
      newstep = reactive({input$buttonpgotonextstep})
    )
  )

  moduleOutput

}

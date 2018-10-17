# step2_chooseModel  Module ----------------

# UI ------------------------------------------
#' step2_chooseModel UI
#' @rdname step2_chooseModel
#' @description UI/View for the step2_chooseModel
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#' @export
step2_chooseModelUI <- function(id) {

  ns <- NS(id)

  tagList(
    hidden(div(id = ns("panelProgrammeModelTable"), panelProgrammeModelTable(id))),
    hidden(div(id = ns("panelModelDetails"), panelModelDetails(id))),
    hidden(div(id = ns("panelAssociateModel"), panelAssociateModel(id)))
  )
}

#' Function wrapping panel to show created programme model table
#' @rdname panelProgrammeModelTable
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
panelProgrammeModelTable <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("progmodeltbl"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleProgrammeModelTable"), inline = TRUE),
      actionButton(inputId = ns("abuttonookrefresh"), label = "Refresh", style = "float: right;")
    ),
    DTOutput(ns("tableProgOasisOOK")),
    fluidRow(column(12, flamingoButton(ns("buttonmodeldetails"), "Show Details"))) #, align = "right"
  )
}

#' Function wrapping panel to show details of programme table
#' @rdname panelModelDetails
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
panelModelDetails <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("progmodeldtl"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleProgrammeModelDetails"), inline = TRUE),
      actionButton(inputId = ns("abuttonprgoasisrfsh"), label = "Refresh", style = "float: right;"),
      actionButton(inputId = ns("buttonhidemodeldetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("tabledisplayprogoasisfiles"))
  )
}

#' Function wrapping panel to associate model
#' @inheritParams flamingoModuleUI
#' @importFrom bsplus bs_embed_tooltip
#' @export
panelAssociateModel <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = FALSE,
    ns("progmodel"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleAssociateModel"), inline = TRUE)
    ),
    fluidRow(
      column(4,
             selectizeInput(ns("sinputookprogid"), "Programme", choices = c(""), selected = character(0),
                            options = list(
                              allowEmptyOption = TRUE,
                              placeholder = 'Select',
                              onInitialize = I('function() { this.setValue(""); }'))
             )),
      column(4,
             selectizeInput(ns("sinputookmodelid"), "Model", choices = c(""), selected = character(0),
                            options = list(
                              allowEmptyOption = TRUE,
                              placeholder = 'Select',
                              onInitialize = I('function() { this.setValue(""); }'))
             )),
      column(4,
             selectizeInput(ns("sinputProgModTransform"), "Transform Name", choices = c("") , selected = character(0),
                            options = list(
                              allowEmptyOption = TRUE,
                              placeholder = 'Select',
                              onInitialize = I('function() { this.setValue(""); }'))
             )) %>%
        bs_embed_tooltip(title = programme_Definition_Single$sinputProgModTransform,
                         placement = "right")),

    div(flamingoButton(inputId = ns("abuttoncrprogoasis"), label = "Create"), style="float:right;")
  )
}


# Server --------------------------------------
#' step2_chooseModel Server
#' @rdname step2_chooseModel
#' @description Server logic to step2_chooseModel
#' @inheritParams flamingoModule
#' @return For \code{programmeDefinitionSingle()}, list of reactives.
#' @template return-outputNavigation
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDT dataTableProxy selectRows DTOutput selectPage
#' @importFrom dplyr mutate select case_when
#' @importFrom shinyjs onclick js removeClass addClass
#' @export
step2_chooseModel <- function(input, output, session,
                              dbSettings,apiSettings, userId,
                              active = reactive(TRUE),
                              logMessage = message,
                              currstep = reactive(-1),
                              selectprogrammeID = reactive({""}),
                              selectprogOasisID = reactive({""}),
                              progName = reactive({""}),
                              progStatus = reactive({""}),
                              DPProgData = reactive(NULL)

) {

  ns <- session$ns

  # Reactive Values and parameters -------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  #values to stop ping pong effect
  stop_selProgOasisID <- check_selProgOasisID <- 0

  # Help function
  '%notin%' <- Negate('%in%')

  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # reactive for selectprogrammeID
    selectprogrammeID = "",
    # reactive for selectprogOasisID
    selectprogOasisID = "",
    # reactive value for model table
    POData = NULL,
    # reactive value for detail of model table
    progFiles = NULL
  )

  #Set Params
  observeEvent(selectprogrammeID(), {
    if (!is.null(selectprogrammeID())) {
      result$selectprogrammeID <- selectprogrammeID()
    } else {
      result$selectprogrammeID <- ""
    }

  })

  observe(if (active()) {
    result$selectprogOasisID <- selectprogOasisID()
  })

  # Panels Visualization -----------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 2 ) {
      .defaultAssociateModel()
      .reloadPOData()
    }
  })


  ### > Define selectprogrammeID ----
  observeEvent( input$sinputookprogid, {
    rowtoSelect <- match( input$sinputookprogid, DPProgData()[,  DPProgData.ProgrammeName])
    sinputookprogid <- DPProgData()[rowtoSelect,  DPProgData.ProgrammeID]
    if (result$selectprogrammeID != sinputookprogid && !is.null(sinputookprogid)) {
      #Making sure the two selectize inputs are the same
      result$selectprogrammeID <- sinputookprogid
    }
  }
  )

  # If selectprogrammeID changes, reload programme model table and set view back to default
  observeEvent(result$selectprogrammeID, ignoreInit = TRUE, {
    logMessage(paste0("updating Programme Model Table because result$selectprogrammeID changed to ", result$selectprogrammeID))
    if (active()) {
      .reloadPOData()
      # Update Associate Model Panel
      .updateOOKProgrammeSelection()
      .clearOOKModelSelection()
      .clearOOKTransformSelection()
    }
  })

  ### > Define selectprogOasisID ----
  observeEvent(result$POData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      if (result$selectprogOasisID != "") {
        logMessage(paste0("updating selectprogOasisID choices because Programme Model Table was reloaded - contains ", nrow(result$POData), " rows"))
        result$selectprogOasisID <- result$POData[1, POData.ProgOasisId]
      } else {
        result$selectprogOasisID <- ""
      }
    }
  })

  # If selectprogOasisID changes, reload process run table and set view back to default
  observeEvent(result$selectprogOasisID, ignoreInit = TRUE, {
    if (active()) {
      bl_dirty1 <- stop_selProgOasisID > check_selProgOasisID
      #.defaultview(session)
      show("buttonmodeldetails")
      hide("panelModelDetails")
      if (result$selectprogOasisID != "") {
        if (!is.null(result$POData) && nrow(result$POData) > 0   && !bl_dirty1 ) {
          rowToSelect <- match(result$selectprogOasisID, result$POData[, POData.ProgOasisId])
          pageSel <- ceiling(rowToSelect/pageLength)
          if (!is.null(input$tableProgOasisOOK_rows_selected) && rowToSelect != input$tableProgOasisOOK_rows_selected) {
            # re-selecting the same row would trigger event-observers on input$tableprocessrundata_rows_selected
            selectRows(dataTableProxy("tableProgOasisOOK"), rowToSelect)
            selectPage(dataTableProxy("tableProgOasisOOK"), pageSel)
            logMessage(paste("selected row is:", input$tableProgOasisOOK_rows_selected))
          }
        }
      } else {
        selectRows(dataTableProxy("tableProgOasisOOK"), NULL)
        selectPage(dataTableProxy("tableProgOasisOOK"), 1)
        logMessage(paste("selected row is:", input$tableProgOasisOOK_rows_selected))
      }
      if (bl_dirty1) check_selProgOasisID <<- check_selProgOasisID + 1
    }
  })


  # > Programme Model Table --------------------------
  output$tableProgOasisOOK <- renderDT(
    if (!is.null(result$POData) && nrow(result$POData) > 0 ) {

      # manual refresh button
      invisible(input$abuttonookrefresh)

      if (isolate(result$selectprogOasisID) != "") {
        rowToSelect <- match(isolate(result$selectprogOasisID), result$POData[,POData.ProgOasisId])
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
      .nothingToShowTable(contentMessage = paste0("no Models associated with Programme ID ", result$selectprogrammeID))
    }
  )

  #  Programme Model Table title
  output$paneltitleProgrammeModelTable <- renderUI({
    if (result$selectprogrammeID != "") {
      progName <- ifelse(toString(progName()) == " " | toString(progName()) == "" | toString(progName()) == "NA", "", paste0('"', toString(progName()), '"'))
      paste0('Models for Programme ', progName,' (id: ', toString(result$selectprogrammeID), ') ', toString(progStatus()))
    } else {
      paste0("Models")
    }
  })

  # Associate Model Table Title
  output$paneltitleAssociateModel <- renderUI({
    if (result$selectprogrammeID != "") {
      progName <- ifelse(toString(progName()) == " " | toString(progName()) == "" | toString(progName()) == "NA", "", paste0('"', toString(progName()), '"'))
      paste0('Associate Model to Programme ', progName, ' (id: ', toString(result$selectprogrammeID), ') ', toString(progStatus()))
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
      .nothingToShowTable(contentMessage = paste0("no files associated with Model ID ", result$selectprogOasisID ))
    })

  # Details Model title
  output$paneltitleProgrammeModelDetails <- renderUI({
    progOasisId <- result$POData[ input$tableProgOasisOOK_rows_selected,POData.ProgOasisId]
    progOasisName <- result$POData[ input$tableProgOasisOOK_rows_selected,POData.ProgName]
    progOasisName <- ifelse(progOasisName == " " | progOasisName == "", "", paste0('"', progOasisName, '"'))
    paste0('Details of Model ', progOasisName, ' (id: ', progOasisId, ')')
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
      flamingoNotification(type = "warning", "Please select a Programme Model first")
    }
  })

  onclick("buttonhidemodeldetails", {
    show("buttonmodeldetails")
    hide("panelModelDetails")
    logMessage("hiding panelModelDetails")
  })

  ### > Create Model ------
  onclick("abuttoncrprogoasis", {
    if (progStatus() == "- Status: Completed") {
      if (isolate(input$sinputookprogid) > 0 && isolate(input$sinputookmodelid) > 0) {
        prgId <- createProgOasis(dbSettings,
                                 isolate(input$sinputookprogid),
                                 isolate(input$sinputookmodelid),
                                 isolate(input$sinputProgModTransform))
        prgId <- ifelse(is.null(prgId), -1, prgId)
        if (prgId == -1) {
          flamingoNotification(type = "error", paste("No Prog Oasis created"))
        } else {
          flamingoNotification(type = "message", paste("Prog Oasis id:",prgId, " created"))
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
            flamingoNotification(type = "message", "Initiating load programme model...")
            #.reloadProgFiles()
            # Going to next step when model load is successful (but not completed)
            workflowSteps$update("3")
          } else {
            flamingoNotification(type = "error", "Failed to load programme model")
          }
        }
      } else{
        flamingoNotification(type = "warning", "Please select both the fields")
      }
    } else {
      flamingoNotification(type = "error", "Please select a completed Programme first")
    }
  })

  # > Updates dependent on changed: tableProgOasisOOK_rows_selected ------------
  #collapse associate panel when row selected changes
  observeEvent({
    input$tableProgOasisOOK_rows_selected
  }, ignoreInit = TRUE, {
    #force collapsed state of Associate Model flamingo panel
    removeClass(id = paste0("progmodel-body"), class = "in")
    removeClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
    addClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
  })

  # Output configuration: manage what to show based on  status of row selected in programme Model table
  observeEvent(input$tableProgOasisOOK_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      #.reloadProgFiles()
      show("buttonmodeldetails")
      hide("panelModelDetails")

      # Show perils according to programme model
      if (length(input$tableProgOasisOOK_rows_selected) > 0 ) {
        prgId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
        procId <- toString(prgId)

        logMessage(paste("updating selectprogOasisID because selection in programme model table changed to",  prgId))
        result$selectprogOasisID <- prgId

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
        result$selectprogOasisID <- ""
      }
    }
  })


  # Help Functions -----------------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs called")
    #Section "Choose Model" = "2"
    hide("panelProgrammeModelTable")
    hide("panelModelDetails")
    hide("panelAssociateModel")
  }

  #show default view for Section "Choose Model" = "2"
  .defaultAssociateModel <- function(){
    logMessage(".defaultAssociateModel called")
    show("panelDefineIDs")
    show("panelProgrammeModelTable")
    show("buttonmodeldetails")
    show("panelAssociateModel")
    # #force collapsed state of Associate Model flamingo panel
    removeClass(id = paste0("progmodel-body"), class = "in")
    removeClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
    addClass(id = paste0("progmodel-collapse-button"), class = "collapsed")
  }


  # Reload Programme Model table
  .reloadPOData <- function() {
    logMessage(".reloadPOData called")
    if (result$selectprogrammeID != "") {
      POData <- getProgOasisForProgdata(dbSettings, result$selectprogrammeID)
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

  .clearOOKProgrammeSelection <- function() {
    logMessage(".clearOOKProgrammeSelection called")
    #Trying to avoid colling the stored procedure if possible
    programmes <- getProgrammeList(dbSettings)
    updateSelectizeInput(session, "sinputookprogid",
                         choices = createSelectOptions(programmes, "Select Programme"),
                         selected = character(0))
  }

  .clearOOKModelSelection <- function() {
    logMessage(".clearOOKModelSelection called")
    models <- getModelList(dbSettings)
    updateSelectizeInput(session, "sinputookmodelid",
                         choices = createSelectOptions(models, "Select Model"),
                         selected = character(0))
  }

  .clearOOKTransformSelection <- function() {
    logMessage(".clearOOKTransformSelection called")
    transforms <- getTransformNameCanModel(dbSettings)
    updateSelectizeInput(session, "sinputProgModTransform",
                         choices = createSelectOptions(transforms, "Select Transform", labelCol = 1, valueCol = 2),
                         selected = character(0))
  }

  .updateOOKProgrammeSelection <- function() {
    logMessage(".updateOOKProgrammeSelection called")
    programmes <- c("")
    if (!is.null(DPProgData())) {
      programmes <- DPProgData()[,  DPProgData.ProgrammeName]
      rowSelected <- match(result$selectprogrammeID, DPProgData()[, DPProgData.ProgrammeID])
      # Remove selection if input$tableDPprog_rows_selected is null
      if (length(rowSelected) > 0 ) {
        selection <- toString(DPProgData()[rowSelected, DPProgData.ProgrammeName])
      } else {
        selection <- character(0)
      }
      updateSelectizeInput(session, "sinputookprogid",
                           choices = programmes, #createSelectOptions(programmes),
                           selected = selection)
    }
  }

  # Model Outout ------------------------------------------------------------

  progOasisStatus <- reactive({
    if (result$POData[input$tableProgOasisOOK_rows_selected, POData.Status] == StatusCompleted) {
      progOasisStatus <- "- Status: Completed"
    } else if (result$POData[input$tableProgOasisOOK_rows_selected, POData.Status] == StatusProcessing) {
      progOasisStatus <- "- Status: in Progress"
    } else if (result$POData[input$tableProgOasisOOK_rows_selected, POData.Status] == StatusFailed) {
      progOasisStatus <- "- Status: Failed"
    }
    progOasisStatus
  })

  moduleOutput <- c(
    list(
      selectprogrammeID = reactive({result$selectprogrammeID}),
      selectprogOasisID = reactive({result$selectprogOasisID}),
      POData = reactive({result$POData})
    )
  )

  moduleOutput

}

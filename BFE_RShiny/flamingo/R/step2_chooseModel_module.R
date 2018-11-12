# step2_chooseModel  Module ----------------------------------------------------

# UI ---------------------------------------------------------------------------
#' step2_chooseModel UI
#'
#' @rdname step2_chooseModel
#'
#' @description UI/View for the step2_chooseModel.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
step2_chooseModelUI <- function(id) {

  ns <- NS(id)

  tagList(
    hidden(div(id = ns("panelProgrammeModelTable"), panelProgrammeModelTable(id))),
    hidden(div(id = ns("panelModelDetails"), panelModelDetails(id))),
    hidden(div(id = ns("panelAssociateModel"), panelAssociateModel(id)))
  )
}

#' panelProgrammeModelTable
#'
#' @rdname panelProgrammeModelTable
#'
#' @description Function wrapping panel to show created programme model table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
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
    flamingoButton(ns("abuttonmodeldetails"), "Show Details", align = "centre") %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonmodeldetails, placement = "right"),
    flamingoButton(ns("abuttonassociatemodel"), "Create Model Association", align = "centre"),
    actionButton(ns("abuttonpgotonextstep"), "Proceed to Configure Output & Run", style = "float:right")
  )
}

#' panelModelDetails
#'
#' @rdname panelModelDetails
#'
#' @description Function wrapping panel to show details of programme table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
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

#' panelAssociateModel
#'
#' @rdname panelAssociateModel
#'
#' @description Function wrapping panel to associate model.
#'
#' @template params-module-ui
#'
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelAssociateModel <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("progmodel"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleAssociateModel"), inline = TRUE),
      actionButton(inputId = ns("abuttonhideassociatemodel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
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
        bs_embed_tooltip(title = defineSingleAna$sinputProgModTransform,
                         placement = "right")),

    div(flamingoButton(inputId = ns("abuttoncrprogoasis"), label = "Create") %>%
          bs_embed_tooltip(title = defineSingleAna$abuttoncrprogoasis, placement = "right"),
        style = "float:right;")
  )
}


# Server -----------------------------------------------------------------------
#' step2_chooseModel Server
#'
#' @rdname step2_chooseModel
#'
#' @description Server logic to step2_chooseModel.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#' 
#' @param currstep current selected step.
#' @param portfolioID selected programme ID.
#' @param modelID selected ProgOasis ID.
#' @param pfName Name of selected programme.
#' @param progStatus Status of selected programme.
#'
#' @return modelID Id of selected progOasis.
#' @return POData model association table.
#' @return newstep navigation step
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs onclick
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT selectPage
#' @importFrom dplyr select
#'
#' @export
step2_chooseModel <- function(input, output, session,
                              dbSettings,apiSettings,
                              active = reactive(TRUE),
                              logMessage = message,
                              currstep = reactive(-1),
                              portfolioID = reactive({""}),
                              modelID = reactive({""}),
                              pfName = reactive({""}),
                              progStatus = reactive({""})

) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  #values to stop ping pong effect
  stop_selProgOasisID <- check_selProgOasisID <- 0

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # reactive for portfolioID
    portfolioID = "",
    # reactive for modelID
    modelID = "",
    # reactive value for model table
    POData = NULL,
    # reactive value for detail of model table
    progFiles = NULL
  )

  #Set Params
  observeEvent(portfolioID(), {
    if (!is.null(portfolioID())) {
      result$portfolioID <- portfolioID()
    } else {
      result$portfolioID <- ""
    }

  })

  observe(if (active()) {
    result$modelID <- modelID()
  })

  # Panels Visualization -------------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 2 ) {
      .defaultAssociateModel()
      .reloadPOData()
    }
  })


  # Define portfolioID ---------------------------------------------------

  # If portfolioID changes, reload programme model table and set view back to default
  observeEvent(result$portfolioID, ignoreInit = TRUE, {
    logMessage(paste0("updating Programme Model Table because result$portfolioID changed to ", result$portfolioID))
    if (active()) {
      .reloadPOData()
      # Update Associate Model Panel
      .clearOOKModelSelection()
      .clearOOKTransformSelection()
    }
  })

  # Define modelID ---------------------------------------------------
  observeEvent(result$POData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      if (result$modelID != "" && !is.null(result$POData) && nrow(result$POData) > 0) {
        logMessage(paste0("updating modelID choices because Programme Model Table was reloaded - contains ", nrow(result$POData), " rows"))
        result$modelID <- result$POData[1, POData.ProgOasisId]
      }
    }
  })

  # If modelID changes, reload process run table and set view back to default
  observeEvent(result$modelID, ignoreInit = TRUE, {
    if (active()) {
      bl_dirty1 <- stop_selProgOasisID > check_selProgOasisID
      #.defaultview(session)
      hide("panelModelDetails")
      if (result$modelID != "") {
        if (!is.null(result$POData) && nrow(result$POData) > 0   && !bl_dirty1 ) {
          rowToSelect <- match(result$modelID, result$POData[, POData.ProgOasisId])
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

  # Programme Model Table ------------------------------------------------------
  output$tableProgOasisOOK <- renderDT(
    if (!is.null(result$POData) && nrow(result$POData) > 0 ) {
      if (isolate(result$modelID) != "") {
        rowToSelect <- match(isolate(result$modelID), result$POData[,POData.ProgOasisId])
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
      .nothingToShowTable(contentMessage = paste0("no Models associated with Programme ID ", result$portfolioID))
    }
  )

  #  Programme Model Table title
  output$paneltitleProgrammeModelTable <- renderUI({
    if (result$portfolioID != "") {
      pfName <- ifelse(toString(pfName()) == " " | toString(pfName()) == "" | toString(pfName()) == "NA", "", paste0('"', toString(pfName()), '"'))
      paste0('Model Associations for Programme id ', toString(result$portfolioID), ' ', pfName,' ', toString(progStatus()))
    } else {
      paste0("Models")
    }
  })

  # Associate Model Table Title
  output$paneltitleAssociateModel <- renderUI({
    if (result$portfolioID != "") {
      pfName <- ifelse(toString(pfName()) == " " | toString(pfName()) == "" | toString(pfName()) == "NA", "", paste0('"', toString(pfName()), '"'))
      paste0('Create Model Association to Programme id ', toString(result$portfolioID), ' ', pfName,' ', toString(progStatus()))
    } else {
      paste0("Create Model Association")
    }
  })

  # Model Details Table --------------------------------------------------------
  output$tabledisplayprogoasisfiles <- renderDT(
    if (!is.null(result$progFiles) && nrow(result$progFiles) > 0 ) {
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
      .nothingToShowTable(contentMessage = paste0("no files associated with Model ID ", result$modelID ))
    })

  # Details Model title
  output$paneltitleProgrammeModelDetails <- renderUI({
    progOasisId <- result$POData[ input$tableProgOasisOOK_rows_selected,POData.ProgOasisId]
    progOasisName <- result$POData[ input$tableProgOasisOOK_rows_selected,POData.ProgName]
    progOasisName <- ifelse(progOasisName == " " | progOasisName == "", "", paste0('"', progOasisName, '"'))
    paste0('Details of Model Association id ', progOasisId, ' ', progOasisName)
  })

  # Enable and disable buttons
  observeEvent({
    result$POData
    input$tableProgOasisOOK_rows_selected}, ignoreNULL = FALSE, ignoreInit = TRUE, {
      disable("abuttonmodeldetails")
      disable("abuttonpgotonextstep")
      if (length(input$tableProgOasisOOK_rows_selected) > 0) {
        enable("abuttonmodeldetails")
        currStatus <- result$POData[input$tableProgOasisOOK_rows_selected, POData.Status]
        if (!is.na(currStatus) && currStatus == StatusCompleted) {
          enable("abuttonpgotonextstep")
        }
      }
    })

  # Show/hide Programme Model Details Panel
  onclick("abuttonmodeldetails", {
    logMessage("showing panelModelDetails")
    .reloadProgFiles()
    show("panelModelDetails")
    logMessage("showing panelModelDetails")
  })

  onclick("buttonhidemodeldetails", {
    hide("panelModelDetails")
    logMessage("hiding panelModelDetails")
  })

  # Create Model ----------------------------------------------------------------
  onclick("abuttonassociatemodel", {
    show("panelAssociateModel")
  })

  # Enable and Disable associate Model button
  observeEvent(progStatus(), {
    if (progStatus() == "- Status: Completed") {
      enable("abuttonassociatemodel")
    } else {
      disable("abuttonassociatemodel")
    }
  })

  # Enable and disable create button
  observeEvent({
    input$sinputookmodelid
    input$sinputProgModTransform
  }, ignoreInit = TRUE, {
    if (input$sinputookmodelid > 0 && input$sinputProgModTransform > 0 && result$portfolioID != "") {
      enable("abuttoncrprogoasis")
    } else {
      disable("abuttoncrprogoasis")
    }

  })

  onclick("abuttoncrprogoasis", {
    if (progStatus() == "- Status: Completed") {
      prgId <- createProgOasis(dbSettings,
                               result$portfolioID,
                               isolate(input$sinputookmodelid),
                               isolate(input$sinputProgModTransform))
      prgId <- ifelse(is.null(prgId), -1, prgId)
      if (prgId == -1) {
        flamingoNotification(type = "error", paste("No Prog Oasis created"))
      } else {
        flamingoNotification(type = "message", paste("Prog Oasis id:",prgId, " created"))
        .clearOOKTransformSelection()
        .clearOOKModelSelection()
        .defaultAssociateModel()
        loadprogmodel <- loadProgrammeModel(
          apiSettings,
          progOasisId = toString(prgId)
        )
        if (loadprogmodel == 'success' || loadprogmodel == 'Success') {
          flamingoNotification(type = "message", "Initiating load programme model...")
          #.reloadProgFiles()
        } else {
          flamingoNotification(type = "error", "Failed to load programme model")
        }
        .reloadPOData()
        idxSel <- match(prgId, result$POData[, POData.ProgOasisId])
        pageSel <- ceiling(idxSel/pageLength)
        selectRows(dataTableProxy("tableProgOasisOOK"), idxSel)
        selectPage(dataTableProxy("tableProgOasisOOK"), pageSel)
        logMessage(paste("updating tableProgOasisOOK select because programme model table was reloaded"))
        logMessage(paste("selected row is:", input$tableProgOasisOOK_rows_selected))
      }
    } else {
      flamingoNotification(type = "error", "Please select a completed Programme first")
    }
  })

  # Hide Programme Definition Panel
  onclick("abuttonhideassociatemodel", {
    hide("panelAssociateModel")
  })

  # Refresh Buttons ------------------------------------------------------------
  onclick("abuttonookrefresh", {
    .reloadPOData()
  } )

  onclick("abuttonprgoasisrfsh", {
    .reloadProgFiles()
  } )

  # Updates dependent on changed: tableProgOasisOOK_rows_selected --------------
  # Output configuration: manage what to show based on  status of row selected in programme Model table
  observeEvent(input$tableProgOasisOOK_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      #.reloadProgFiles()
      hide("panelModelDetails")
      hide("panelDefineProgramme")

      # Show perils according to programme model
      if (length(input$tableProgOasisOOK_rows_selected) > 0 ) {
        prgId <- result$POData[input$tableProgOasisOOK_rows_selected, POData.ProgOasisId]
        procId <- toString(prgId)

        logMessage(paste("updating modelID because selection in programme model table changed to",  prgId))
        result$modelID <- prgId

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
        result$modelID <- ""
      }
    }
  })


  # Help Functions -------------------------------------------------------------
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
    show("panelProgrammeModelTable")
    hide("panelAssociateModel")
  }


  # Reload Programme Model table
  .reloadPOData <- function() {
    logMessage(".reloadPOData called")
    if (result$portfolioID != "") {
      POData <- getProgOasisForProgdata(dbSettings, result$portfolioID)
      if (!is.null(POData)) {
        result$POData <- POData %>%
          select(c(POData.ProgOasisId, POData.ProgName, POData.ModelName, POData.TransformName, POData.SourceFileId, POData.FileID, POData.Status)) %>%
          replaceWithIcons()
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
      if (!is.null(progFiles)) {
        result$progFiles <-  progFiles %>%
          replaceWithIcons()
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

  # Model Outout ---------------------------------------------------------------

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
      modelID = reactive({result$modelID}),
      POData = reactive({result$POData}),
      newstep = reactive({input$abuttonpgotonextstep})
    )
  )

  moduleOutput

}

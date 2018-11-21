#' modelSupplierPage
#'
#' @rdname modelSupplierPage
#'
#' @description Server logic for the model supplier page.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @return Empty list.
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
modelSupplierPage <- function(input, output, session, dbSettings,
                              logMessage = message, active = reactive(TRUE)) {

  result <- reactiveValues(
    tbl_MData = NULL,
    tbl_MDataCounter = 0,
    MID = -1,
    tbl_MRData = NULL,
    tbl_MRDataCounter = 0,
    crtAmFlag = "" # either "", "C" for create or "A" for amend
  )

  .reloadtbl_MData <- function() {
    result$tbl_MDataCounter <- result$tbl_MDataCounter + 1
    invisible()
  }

  .reloadtbl_MRData <- function() {
    result$tbl_MRDataCounter <- result$tbl_MRDataCounter + 1
    invisible()
  }

  # Model List Table -----------------------------------------------------------

  # when navigated to Model tab, model table should be updated
  observe(if (active()) {
    # reload if .reloadtbl_MData is called
    force(result$tbl_MDataCounter)

    hide("divmr")
    hide("cramndelmodres")
    hide("submitmodrescreate")
    hide("ConfirmModResDel")

    result$tbl_MData <- getModelList(dbSettings)
  })

  output$dt_model <- renderDT({
    datatable(
      result$tbl_MData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      selection = "single",
      colnames = c('Row Number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),
        scrollX = TRUE
      )
    )
  })

  output$Modeldownloadexcel <- downloadHandler(
    filename = "model.csv",
    content = function(file) {
      write.csv(result$tbl_MData, file)
    }
  )

  # Model Resource Table -------------------------------------------------------

  # Model resource table to be displayed at the click a row of Model table
  observe(if (active() && length(input$dt_model_rows_selected) > 0) {

    # reload if .reloadtbl_MRData is called
    force(result$tbl_MRDataCounter)

    show("divmr")
    MID <- result$tbl_MData[(input$dt_model_rows_selected),1]

    stmt <- buildDbQuery("getmodelresource", result$MID)
    result$tbl_MRData <- executeDbQuery(dbSettings, stmt)

    result$MID <- MID

  } else {
    hide("divmr")
    hide("cramndelmodres")
    hide("submitmodrescreate")
    hide("ConfirmModResDel")
  })

  output$dt_model_resource <- renderDT({

    datatable(
      result$tbl_MRData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      selection = "single",
      colnames = c('Row Number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),
        scrollX = TRUE
      )
    )
  })

  output$MRdownloadexcel <- downloadHandler(
    filename = "modelresource.csv",
    content = function(file) {
      write.csv(result$tbl_MRData, file)
    }
  )

  # Model Resource CRUD --------------------------------------------------------

  ## create/amend/delete buttons - open/initialize modal dialog

  .crtAmModal <- function() {
    ns <- session$ns
    modalDialog(label = "crtAmModal",
                title = "Create/Amend Model Resource",
                textInput(ns("tinmodelresname"), label = "Model Resource Name:",
                          value = ""),
                selectInput(ns("sinresrctype"), label = "Resource Type:",
                            choices = c("")),
                selectInput(ns("sinoasissysname"), label = "Oasis System Name:",
                            choices = c("")),
                textInput(ns("tinmodelresvalue"), label = "Model Resource Value:",
                          value = ""),
                footer = tagList(
                  flamingoButton(ns("abuttonSubmitCrtAm"),
                                 label = "Submit", align = "left")  %>%
                    bs_embed_tooltip(title = sys_conf$abuttonSubmitCrtAm, placement = "right"),
                  actionButton(ns("btnCancelCrtAm"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  observeEvent(input$abuttoncreate, {
    result$crtAmFlag <- "C"
    showModal(.crtAmModal())
    .clearCrtAm()
  })

  # Enable and disable buttons
  observeEvent({
    result$tbl_MRData
    input$dt_model_resource_rows_selected}, ignoreNULL = FALSE, ignoreInit = TRUE, {
      if (length(input$dt_model_resource_rows_selected) > 0) {
        enable("abuttonamend")
        enable("abuttondelete")
        enable("abuttonConfirmDel")
      } else {
        disable("abuttonamend")
        disable("abuttondelete")
        disable("abuttonConfirmDel")
      }
    })

  observeEvent(input$abuttonamend, {
    result$crtAmFlag <- "A"
    showModal(.crtAmModal())
    .autoFillCrtAm(row)
  })

  # title for delete button
  output$delModal <- renderUI({
    modelId <- result$tbl_MRData[input$dt_model_resource_rows_selected, 1]
    modelName <- result$tbl_MRData[input$dt_model_resource_rows_selected, 2]
    paste0('Delete ', modelId, ' ', modelName)
  })

  .delModal <- function(){
    ns <- session$ns
    modalDialog(label = "delModal",
                title = uiOutput(ns("delModal"), inline = TRUE),
                paste0("Are you sure you want to delete?"),
                footer = tagList(
                  flamingoButton(ns("abuttonConfirmDel"),
                                 label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = sys_conf$abuttonConfirmDel, placement = "right"),
                  actionButton(ns("btnCancelDel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  observeEvent(input$abuttondelete, {
    showModal(.delModal())
  })

  # submit/cancel buttons
  observeEvent(input$abuttonSubmitCrtAm, {

    if (result$crtAmFlag == "C") {

      if (input$tinmodelresname > 0 &&
         (isolate(input$sinresrctype) > 0) &&
         (isolate(input$sinoasissysname) > 0) &&
         input$tinmodelresvalue > 0){

        crtmodres <- createModelResource(dbSettings,
                                         input$tinmodelresname,
                                         isolate(input$sinresrctype),
                                         isolate(input$sinoasissysname),
                                         isolate(result$MID),
                                         input$tinmodelresvalue)

        flamingoNotification(sprintf("Model Resource %s created.", crtmodres),
                             type = "message")

        .reloadtbl_MRData()

      }

    } else if (result$crtAmFlag == "A") {

      updtmodres <- updateModelResource(dbSettings,
                                        result$tbl_MRData[row, 1],
                                        input$tinmodelresname,
                                        isolate(input$sinresrctype),
                                        isolate(input$sinoasissysname),
                                        isolate(result$MID),
                                        input$tinmodelresvalue)

      flamingoNotification(sprintf("Model Resource %s updated.", updtmodres),
                           type = "message")

      .reloadtbl_MRData()
    }
    removeModal()
  })

  observeEvent(input$btnCancelCrtAm, {
    removeModal()
  })

  observeEvent(input$abuttonConfirmDel, {
    modResId <- deleteModelResource(dbSettings, result$tbl_MRData[input$dt_model_resource_rows_selected,1])

    if (!is.null(modResId)) {
      flamingoNotification(sprintf("Model Resource %s deleted.", modResId),
                           type = "message")

    } else {
      flamingoNotification(sprintf("Model Resource could not be deleted."))
    }

    .reloadtbl_MRData()
    removeModal()
  })

  observeEvent(input$btnCancelDel, {
    removeModal()
  })

  # helper functions -----------------------------------------------------------
  .clearCrtAm <- function() {

    updateTextInput(session, "tinmodelresname", value = "")

    resourceType <- getResourceType(dbSettings)
    updateSelectInput(session, "sinresrctype",
                      choices = createSelectOptions(resourceType, "Select Resource Type"),
                      selected = c("Select Resource Type" = 0))

    oasisSys <- getOasisSystemId(dbSettings)
    updateSelectInput(session, "sinoasissysname",
                      choices = createSelectOptions(oasisSys, "Select Oasis System"),
                      selected = c("Select Oasis System" = 0))

    updateTextInput(session, "tinmodelresvalue", value = "")

  }


  .autoFillCrtAm <- function(row) {

    updateTextInput(session, "tinmodelresname",
                    value = result$tbl_MRData[row, 2])

    resourceType <- getResourceType(dbSettings)
    updateSelectInput(session, "sinresrctype",
                      choices = createSelectOptions(resourceType, "Select Resource Type"),
                      selected = result$tbl_MRData[row, 3])

    oasisSys <- getOasisSystemId(dbSettings)
    updateSelectInput(session, "sinoasissysname",
                      choices = createSelectOptions(oasisSys, "Select Oasis System"),
                      selected = result$tbl_MRData[row, 4])

    updateTextInput(session, "tinmodelresvalue",
                    value = result$tbl_MRData[row, 6])

  }

  ### Module Output
  moduleOutput <- list()

  return(moduleOutput)

}

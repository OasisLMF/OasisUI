#' Module For The Model Supplier Page
#' @rdname modelSupplierPage
#' @description Server logic for the model supplier page
#' @inheritParams flamingoModule
#' @inheritParams accountDefinitionUI
#' @return empty list
#' @importFrom DT renderDT
#' @importFrom shinyjs hide show onclick
#' @export
modelSupplierPage <- function(input, output, session, dbSettings,
                              logMessage = message, active = reactive(TRUE)) {

  result <- reactiveValues(
    MData = NULL,
    MDataCounter = 0,
    MID = -1,
    MRData = NULL,
    MRDataCounter = 0,
    crtAmFlag = "" # either "", "C" for create or "A" for amend
  )

  .reloadMData <- function() {
    result$MDataCounter <- result$MDataCounter + 1
    invisible()
  }

  .reloadMRData <- function() {
    result$MRDataCounter <- result$MRDataCounter + 1
    invisible()
  }

  ### Model List Table

  # when navigated to Model tab, model table should be updated
  observe(if (active()) {
    # reload if .reloadMData is called
    force(result$MDataCounter)

    hide("divmr")
    hide("cramndelmodres")
    hide("submitmodrescreate")
    hide("ConfirmModResDel")

    result$MData <- getModelList(dbSettings)
  })

  output$tablemodel <- renderDT({
    datatable(
      result$MData,
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
    filename ="model.csv",
    content = function(file) {
      write.csv(result$MData, file)
    }
  )

  ### Model Resource Table

  # Model resource table to be displayed at the click a row of Model table
  observe(if(active() && length(input$tablemodel_rows_selected) > 0) {

    # reload if .reloadMRData is called
    force(result$MRDataCounter)

    show("divmr")
    MID <- result$MData[(input$tablemodel_rows_selected),1]

    stmt <- buildDbQuery("getmodelresource", result$MID)
    result$MRData <- executeDbQuery(dbSettings, stmt)

    result$MID <- MID

  } else {
    hide("divmr")
    hide("cramndelmodres")
    hide("submitmodrescreate")
    hide("ConfirmModResDel")
  })

  output$mrtable <- renderDT({

    datatable(
      result$MRData,
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
      write.csv(result$MRData, file)
    }
  )

  ### Model Resource CRUD

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
                  flamingoButton(ns("btnSubmitCrtAm"),
                                 label = "Submit", align = "left")  %>%
                    bs_embed_tooltip(title = sys_conf$btnSubmitCrtAm, placement = "right"),
                  actionButton(ns("btnCancelCrtAm"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  observeEvent(input$btnCreate, {
    result$crtAmFlag <- "C"
    showModal(.crtAmModal())
    .clearCrtAm()
  })

  # Enable and disable buttons
  observeEvent ({
    result$MRData
    input$mrtable_rows_selected}, ignoreNULL = FALSE, ignoreInit = TRUE, {
      if (length(input$mrtable_rows_selected) > 0) {
        shinyjs::enable("btnAmend")
        shinyjs::enable("btnDelete")
        shinyjs::enable("btnConfirmDel")
      } else {
        shinyjs::disable("btnAmend")
        shinyjs::disable("btnDelete")
        shinyjs::disable("btnConfirmDel")
      }
    })

  observeEvent(input$btnAmend, {
    result$crtAmFlag <- "A"
    showModal(.crtAmModal())
    .autoFillCrtAm(row)
  })

  .delModal <- function(){
    ns <- session$ns
    modalDialog(label = "delModal",
                title = "Delete Selection",
                paste0("Are you sure you want to delete?"),
                footer = tagList(
                  flamingoButton(ns("btnConfirmDel"),
                                 label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = sys_conf$btnConfirmDel, placement = "right"),
                  actionButton(ns("btnCancelDel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  observeEvent(input$btnDelete, {
    showModal(.delModal())
  })

  ## submit/cancel buttons
  observeEvent(input$btnSubmitCrtAm, {

    if (result$crtAmFlag == "C") {

      if(input$tinmodelresname >0 &&
         (isolate(input$sinresrctype)>0) &&
         (isolate(input$sinoasissysname)>0) &&
         input$tinmodelresvalue >0){

        crtmodres <- createModelResource(dbSettings,
                                         input$tinmodelresname,
                                         isolate(input$sinresrctype),
                                         isolate(input$sinoasissysname),
                                         isolate(result$MID),
                                         input$tinmodelresvalue)

        flamingoNotification(sprintf("Model Resource %s created.", crtmodres),
                             type = "message")

        .reloadMRData()

      }

    } else if (result$crtAmFlag == "A") {

      updtmodres <- updateModelResource(dbSettings,
                                        result$MRData[row, 1],
                                        input$tinmodelresname,
                                        isolate(input$sinresrctype),
                                        isolate(input$sinoasissysname),
                                        isolate(result$MID),
                                        input$tinmodelresvalue)

      flamingoNotification(sprintf("Model Resource %s updated.", updtmodres),
                           type = "message")

      .reloadMRData()
    }
    removeModal()
  })

  observeEvent(input$btnCancelCrtAm, {
    removeModal()
  })

  observeEvent(input$btnConfirmDel, {
    modResId <- deleteModelResource(dbSettings, result$MRData[input$mrtable_rows_selected,1])

    if (!is.null(modResId)) {
      flamingoNotification(sprintf("Model Resource %s deleted.", modResId),
                           type = "message")

    } else {
      flamingoNotification(sprintf("Model Resource could not be deleted."))
    }

    .reloadMRData()
    removeModal()
  })

  observeEvent(input$btnCancelDel, {
    removeModal()
  })

  ## helper functions
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
                    value = result$MRData[row, 2])

    resourceType <- getResourceType(dbSettings)
    updateSelectInput(session, "sinresrctype",
                      choices = createSelectOptions(resourceType, "Select Resource Type"),
                      selected = result$MRData[row, 3])

    oasisSys <- getOasisSystemId(dbSettings)
    updateSelectInput(session, "sinoasissysname",
                      choices = createSelectOptions(oasisSys, "Select Oasis System"),
                      selected = result$MRData[row, 4])

    updateTextInput(session, "tinmodelresvalue",
                    value = result$MRData[row, 6])

  }

  ### Module Output
  moduleOutput <- list()

  return(moduleOutput)

}

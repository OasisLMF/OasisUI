#' companyDefinition
#'
#' @rdname companyDefinition
#'
#' @description Server logic to define a company.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-logMessage
#' @template params-active
#' @param dbSettings Setting object as returned by e.g. [flamingoDB()].
#'
#' @return Empty list.
#
#' @importFrom DT datatable
#' @importFrom DT renderDT
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom shinyjs onclick
#'
#' @export
companyDefinition <- function(input, output, session, dbSettings,
                              active = reactive(TRUE), logMessage = message) {

  ns <- session$ns

  result <- reactiveValues(
    # company table data
    tbl_compData = 0,

    # counter to increase to trigger refresh after CRUD
    tbl_compDataCounter = 0,

    # stores current edit mode
    compFlag = c("", "U", "C")[1]
  )

  .reloadtbl_compData <- function() {
    result$tbl_compDataCounter <- result$tbl_compDataCounter + 1
    invisible()
  }


  # Company Table --------------------------------------------------------------

  # update company table when:
  # - module activated (e.g. when switching to tab)
  # - .reloadtbl_compData() called
  observe(if (active()) {
    force(result$tbl_compDataCounter)
    result$tbl_compData <- getCompanyList(dbSettings)
  })

  # draw company table with custom format options, queries the database every
  # time to update its dataset
  output$dt_companylist <- renderDT({
    datatable(
      result$tbl_compData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      selection = "single",
      colnames = c('row number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),
        autoWidth = TRUE
      )
    )
  })

  # Modal dialog and buttons in main panel -------------------------------------
  # Modal dialog of create button in main panel
  .compcrtupmodal <- function() {
    ns <- session$ns
    modalDialog(label = "compcrtupmodal",
                title = "Company Details",
                textInput(ns("tinputCompName"), "Company Name"),
                textInput(ns("tinputCompDom"), "Company Domicile"),
                textInput(ns("tinputCompLegName"), "Company Legal Name"),
                textInput(ns("tinputCompRegNo"), "Company Registration Number"),
                footer = tagList(
                  flamingoButton(ns("abuttonsubcomp"),
                               label = "Submit", align = "left"),
                  actionButton(ns("abuttonccancel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of cancel button in pop-up
  onclick("abuttonccancel", {
    removeModal()
    .reloadtbl_compData()
  })

  # onclick of create button in main panel
  onclick("abuttoncompcrt", {
    result$compFlag <- "C"
    showModal(.compcrtupmodal())
  })

  # Enable and disable buttons
  observeEvent(input$dt_companylist_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
      if (length(input$dt_companylist_rows_selected) > 0) {
        enable("abuttoncompupdate")
        enable("abuttoncompdel")
      } else {
        disable("abuttoncompupdate")
        disable("abuttoncompdel")
      }
    })

  # on click of update button in main panel
  onclick("abuttoncompupdate", {
      showModal(.compcrtupmodal())
      result$compFlag <- "U"
      updateTextInput(session, "tinputCompName",
                      value = result$tbl_compData[input$dt_companylist_rows_selected, 2])
      updateTextInput(session, "tinputCompDom",
                      value = result$tbl_compData[input$dt_companylist_rows_selected, 3])
      updateTextInput(session, "tinputCompLegName",
                      value = result$tbl_compData[input$dt_companylist_rows_selected, 4])
      updateTextInput(session, "tinputCompRegNo",
                      value = result$tbl_compData[input$dt_companylist_rows_selected, 5])
  })

  # title for delete button
  output$compdelmodaltitle <- renderUI({
    companyId <- result$tbl_compData[input$dt_companylist_rows_selected, 1]
    companyName <- result$tbl_compData[input$dt_companylist_rows_selected, 2]
    paste0('Delete company id ', companyId, ' "', companyName, '"')
  })

  # modalDialog of delete button in main panel
  .compdelmodal <- function() {
    ns <- session$ns
    modalDialog(label = "compdelmodal",
                title = uiOutput(ns("compdelmodaltitle"), inline = TRUE),
                paste0("Are you sure you want to delete?"),
                footer = tagList(
                  flamingoButton(ns("abuttoncconfirmdel"),
                               label = "Confirm", align = "center"),
                  actionButton(ns("abuttonccanceldel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # on click of delete button in main panel
  onclick("abuttoncompdel", {
      showModal(.compdelmodal())
  })

  # on click of cancel button in delete modal
  onclick("abuttonccanceldel", {
    removeModal()
  })


  # onclick of submit button in pop-up
  onclick("abuttonsubcomp", {
    res <- NULL
    if (result$compFlag == "C") {

      stmt <- buildDbQuery("createCompany",
                           input$tinputCompName,
                           input$tinputCompDom,
                           input$tinputCompLegName,
                           input$tinputCompRegNo)

      res <- executeDbQuery(dbSettings, stmt)
      if (is.null(res)) {
        flamingoNotification(type = "error",
                         paste("Failed to create company - ", input$tinputCompName))
      } else {
        flamingoNotification(type = "message",
                         paste("Company ", input$tinputCompName, " created."))
      }

    } else {
      if (result$compFlag == "U") {

        stmt <- buildDbQuery("updateCompany",
                             result$tbl_compData[input$dt_companylist_rows_selected, 1],
                             input$tinputCompName,
                             input$tinputCompDom,
                             input$tinputCompLegName,
                             input$tinputCompRegNo)

        res <- executeDbQuery(dbSettings, stmt)

        if (is.null(res)) {
          flamingoNotification(type = "error",
                           sprintf("Failed to update company - %s",
                                   result$tbl_compData[input$dt_companylist_rows_selected, 2]))
        } else {
          flamingoNotification(type = "message",
                           sprintf("Company - %s updated.",
                                   result$tbl_compData[input$dt_companylist_rows_selected, 2]))
        }
      }}
    result$compFlag <- ""
    removeModal()
    .reloadtbl_compData()
  })

  # confirm delete
  onclick("abuttoncconfirmdel", {
    removeModal()

      stmt <- buildDbQuery("deleteCompany",
                           result$tbl_compData[input$dt_companylist_rows_selected, 1])
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        flamingoNotification(type = "error",
                         paste("Failed to delete company - ",
                               result$tbl_compData[input$dt_companylist_rows_selected, 2]))
      } else {
        flamingoNotification(type = "message",
                         sprintf("Company - %s deleted.",
                                 result$tbl_compData[input$dt_companylist_rows_selected, 2]))
      }
      .reloadtbl_compData()
  })


  # Module Output --------------------------------------------------------------

  moduleOutput <- list()

  return(moduleOutput)

}

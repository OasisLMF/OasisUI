#' Company Definition Module
#' @rdname companyDefinition
#' @description Server logic to define a company
#' @inheritParams flamingoModule
#' @param userId reactive expression yielding user id
#' @return empty list
#' @importFrom DT renderDT
#' @export
companyDefinition <- function(input, output, session, dbSettings, userId,
                              active = reactive(TRUE), logMessage = message) {

  ns <- session$ns

  result <- reactiveValues(
    # company table data
    compData = 0,

    # counter to increase to trigger refresh after CRUD
    compDataCounter = 0,

    # stores current edit mode
    compFlag = c("", "U", "C")[1]
  )

  .reloadCompData <- function() {
    result$compDataCounter <- result$compDataCounter + 1
    invisible()
  }


  ### Company Table ###

  # update company table when:
  # - module activated (e.g. when switching to tab)
  # - .reloadCompData() called
  observe(if (active()) {
    force(result$compDataCounter)
    result$compData <- getCompanyList(dbSettings)
  })

  # draw company table with custom format options, queries the database every
  # time to update its dataset
  output$tablecompanylist <- renderDT({
    datatable(
      result$compData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      selection = "single",
      colnames = c('Row Number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),
        autoWidth=TRUE
      )
    )
  })

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
    .reloadCompData()
  })

  # onclick of create button in main panel
  onclick("abuttoncompcrt", {
    result$compFlag <- "C"
    showModal(.compcrtupmodal())
  })

  # Enable and disable buttons
  observeEvent(input$tablecompanylist_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
      if (length(input$tablecompanylist_rows_selected) > 0) {
        shinyjs::enable("abuttoncompupdate")
        shinyjs::enable("abuttoncompdel")
      } else {
        shinyjs::disable("abuttoncompupdate")
        shinyjs::disable("abuttoncompdel")
      }
    })

  # on click of update button in main panel
  onclick("abuttoncompupdate", {
      showModal(.compcrtupmodal())
      result$compFlag <- "U"
      updateTextInput(session, "tinputCompName",
                      value = result$compData[input$tablecompanylist_rows_selected, 2])
      updateTextInput(session, "tinputCompDom",
                      value = result$compData[input$tablecompanylist_rows_selected, 3])
      updateTextInput(session, "tinputCompLegName",
                      value = result$compData[input$tablecompanylist_rows_selected, 4])
      updateTextInput(session, "tinputCompRegNo",
                      value = result$compData[input$tablecompanylist_rows_selected, 5])
  })

  # title for delete button
  output$compdelmodal <- renderUI({
    companyId <- result$compData[input$tablecompanylist_rows_selected, 1]
    companyName <- result$compData[input$tablecompanylist_rows_selected, 2]
    paste0('Delete Company id ', companyId, ' "', companyName, '"')
  })

  # modalDialog of delete button in main panel
  .compdelmodal <- function() {
    ns <- session$ns
    modalDialog(label = "compdelmodal",
                title = uiOutput(ns("compdelmodal"), inline = TRUE),
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
                             result$compData[input$tablecompanylist_rows_selected, 1],
                             input$tinputCompName,
                             input$tinputCompDom,
                             input$tinputCompLegName,
                             input$tinputCompRegNo)

        res <- executeDbQuery(dbSettings, stmt)

        if (is.null(res)) {
          flamingoNotification(type = "error",
                           sprintf("Failed to update company - %s",
                                   result$compData[input$tablecompanylist_rows_selected, 2]))
        } else {
          flamingoNotification(type = "message",
                           sprintf("Company - %s updated.",
                                   result$compData[input$tablecompanylist_rows_selected, 2]))
        }
      }}
    result$compFlag <- ""
    removeModal()
    .reloadCompData()
  })

  # confirm delete
  onclick("abuttoncconfirmdel", {
    removeModal()
    
      stmt <- buildDbQuery("deleteCompany",
                           result$compData[input$tablecompanylist_rows_selected, 1])
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        flamingoNotification(type = "error",
                         paste("Failed to delete company - ",
                               result$compData[input$tablecompanylist_rows_selected, 2]))
      } else {
        flamingoNotification(type = "message",
                         sprintf("Company - %s deleted.",
                                 result$compData[input$tablecompanylist_rows_selected, 2]))
      }
      .reloadCompData()
  })


  ### Module Output ###########################################################

  moduleOutput <- list()

  return(moduleOutput)

}

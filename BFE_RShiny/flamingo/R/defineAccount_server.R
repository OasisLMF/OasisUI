#' Account Definition Module
#'
#' @rdname accountDefinition
#'
#' @description Server logic to define an account.
#'
#' @inheritParams executeDbQuery
#' @inheritParams flamingoModule
#'
#' @return Empty list.
#'
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom shinyjs onclick
#' @importFrom DT renderDT
#' @importFrom DT datatable
#'
#' @export
accountDefinition <- function(input, output, session, dbSettings,
                              active = reactive(TRUE)) {

  ns <- session$ns

  result <- reactiveValues(
    # either "", "C" for create or "A" for amend
    accFlag = "",
    # data for account table
    DAAccountData = NULL,
    DAAccountDataCounter = 0
  )

  .reloadDAAccountData <- function() {
    result$DAAccountDataCounter <- result$DAAccountDataCounter + 1
    invisible()
  }

  ### Account Table ###
  observe(if (active()) {

    force(result$DAAccountDataCounter)

    stmt <- paste0("exec dbo.getAccount")
    result$DAAccountData <- executeDbQuery(dbSettings, stmt)

  })

  output$tableDAAccount <- renderDT(if (!is.null(result$DAAccountData)) {
    datatable(
      result$DAAccountData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      selection = "single",
      colnames = c('Row Number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0)),
        pageLength = 5
      )
    )
  })

  output$DAAdownloadexcel <- downloadHandler(
    filename = "accounts.csv",
    content = function(file) {
      write.csv(result$DAAccountData, file)
    }
  )

  ### Create/Amend Account

  .crtupModal <- function() {
    ns <- session$ns
    modalDialog(label = "crtupModal",
                title = "Create/Amend Account",
                textInput(ns("tinputDAAccountName"), "Account Name"),
                footer = tagList(
                  flamingoButton(ns("abuttonAccSubmit"),
                                 label = "Submit", align = "left"),
                  actionButton(ns("abuttonAccCancel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  onclick("buttoncreateac", {
    # "C" for create
    result$accFlag <- "C"
    showModal(.crtupModal())
    updateTextInput(session, "tinputDAAccountName", value = "")
  })

  # Enable and disable buttons
  observeEvent(input$tableDAAccount_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (length(input$tableDAAccount_rows_selected) > 0) {
      shinyjs::enable("buttonamendac")
      shinyjs::enable("buttondeleteac")
    } else {
      shinyjs::disable("buttonamendac")
      shinyjs::disable("buttondeleteac")
    }
  })

  onclick("buttonamendac", {
    result$accFlag <- "A"

    showModal(.crtupModal())
    updateTextInput(session, "tinputDAAccountName",
                    value = result$DAAccountData[row, 2])
  })

  onclick("abuttonAccSubmit", {

    if (result$accFlag == "C") {

      stmt <- buildDbQuery("createAccount", input$tinputDAAccountName,
                           squareBrackets = FALSE)
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        flamingoNotification(type = "error",
                             sprintf("Failed to create an account - %s", input$tinputDAAccountName))
      } else {
        flamingoNotification(type = "message",
                             sprintf("Account %s created.", input$tinputDAAccountName))
      }

    } else if (result$accFlag == "A") {

      if (length(row <- input$tableDAAccount_rows_selected) > 0) {

        stmt <- buildDbQuery("updateAccount ",
                             result$DAAccountData[row, 1], input$tinputDAAccountName)
        res <- executeDbQuery(dbSettings, stmt)

        if (is.null(res)) {
          flamingoNotification(type = "error",
                               paste("Failed to amend an account - ", result$DAAccountData[row, 2]))

        } else {
          flamingoNotification(type = "message",
                               paste("Account ", result$DAAccountData[row, 2], " amended."))
        }

      }

    }

    result$accFlag <- ""
    removeModal()
    .reloadDAAccountData()

  })

  onclick("abuttonAccCancel",{

    result$accFlag <- ""

    removeModal()

  })

  ### Delete Account

  .delModal <- function() {
    ns <- session$ns
    modalDialog(label = "delModal",
                title = "Delete Account",
                paste0("Are you sure you want to delete?"),
                footer = tagList(
                  flamingoButton(ns("btnConfirmDel"),
                                 label = "Confirm", align = "center"),
                  actionButton(ns("btnCancelDel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  onclick("buttondeleteac", {

    if (length(input$tableDAAccount_rows_selected) > 0) {
      showModal(.delModal())
    } else {
      flamingoNotification(type = "warning",
                           "Please select an Account to Delete")
    }

  })

  observeEvent(input$btnCancelDel, {
    removeModal()
  })

  observeEvent(input$btnConfirmDel, {
    removeModal()

    if (length(row <- input$tableDAAccount_rows_selected) > 0) {

      stmt <- buildDbQuery("deleteAccount", result$DAAccountData[row ,1])
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        flamingoNotification(type = "message",
                             sprintf("Failed to delete account %s",
                                     result$DAAccountData[row, 2]))
      } else {

        flamingoNotification(type = "message",
                             sprintf("Account %s deleted",
                                     result$DAAccountData[row, 2]))

      }
    }
    .reloadDAAccountData()

  })

  ### When Module Activated
  observe(if (active()) {
    result$accFlag <- ""
  })

  ### Module Output ###
  moduleOutput <- list()

  return(moduleOutput)

}

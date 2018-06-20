
#' Account Definition Module
#' @description Server logic to define an account.
#' @inheritParams executeDbQuery
#' @inheritParams flamingoModule
#' @return empty list
#' @import shiny
#' @importFrom shinyjs onclick show disable enable hide
#' @importFrom DT renderDataTable datatable
#' @rdname accountDefinition
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
  
  reloadDAAccountData <- function() {
    result$DAAccountDataCounter <- isolate(result$DAAccountDataCounter) + 1
  }
  
  
  
  ### Account Table ###

  observe(if (active()) {
        
        force(result$DAAccountDataCounter)
        
        stmt <- paste0("exec dbo.getAccount")
        result$DAAccountData <- executeDbQuery(dbSettings, stmt)
        
      })
  
  output$tableDAAccount <- renderDataTable(if (!is.null(result$DAAccountData)) {
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
      filename ="accounts.csv",
      content = function(file) {
        write.csv(result$DAAccountData, file)}
  )
  
  
  
  ### Create/Amend Account
  
  onclick("buttoncreateac", {
        
        result$accFlag <- "C"
        updateTextInput(session, "tinputDAAccountName", value = "")
        
        toggleModal(session, "crtupModal", toggle = "open")
        
      })
  
  onclick("buttonamendac", {
        if (length(row <- input$tableDAAccount_rows_selected) > 0) {
          
          result$accFlag <- "A"
          updateTextInput(session, "tinputDAAccountName",
              value = result$DAAccountData[row, 2])
          
          toggleModal(session, "crtupModal", toggle = "open")
          
        } else {
          
          showNotification(type = "warning",
              "Please select an Account to Amend")
          
        }
      })
  
  onclick("abuttonAccSubmit", {
        
        if (result$accFlag == "C"){
          
          stmt <- buildDbQuery("createAccount", input$tinputDAAccountName,
              squareBrackets = FALSE)
          res <- executeDbQuery(dbSettings, stmt)
          
          if (is.null(res)) {
            showNotification(type = "error",
                sprintf("Failed to create an account - %s",
                    input$tinputDAAccountName))
            
          } else{
            showNotification(type = "message",
                sprintf("Account %s created.",
                    input$tinputDAAccountName))
          }
          
        } else if (result$accFlag == "A") {
          
          if (length(row <- input$tableDAAccount_rows_selected) > 0) {
            
            stmt <- buildDbQuery("updateAccount ",
                result$DAAccountData[row, 1], input$tinputDAAccountName)
            res <- executeDbQuery(dbSettings, stmt)
            
            if (is.null(res)) {
              showNotification(type = "error",
                  paste("Failed to amend an account - ",
                      result$DAAccountData[row, 2]))
              
            } else {
              showNotification(type = "message",
                  paste("Account ",
                      result$DAAccountData[row, 2],
                      " amended."))
            }
            
          }
          
        }
        
        toggleModal(session, "crtupModal", toggle = "close")
        
        updateTextInput(session, "tinputDAAccountName", value="")
        result$accFlag <- ""
        
        reloadDAAccountData()
        
      })
  
  onclick("abuttonAccCancel",{
        
        result$accFlag <- ""
        updateTextInput(session, "tinputDAAccountName", value="")
        
        toggleModal(session, "crtupModal", toggle = "close")
        
      })
  
  
  
  ### Delete Account
  
  onclick("buttondeleteac", {
        
        if (length(row <- input$tableDAAccount_rows_selected) > 0) {
          
          toggleModal(session, "delModal", toggle = "open")
          
        } else {
          
          showNotification(type = "warning",
              "Please select an Account to Delete")
        
        }
        
      })
  
  observeEvent(input$btnCancelDel, {
        toggleModal(session, "delModal", toggle = "close")
      })
  
  observeEvent(input$btnConfirmDel, {
        toggleModal(session, "delModal", toggle = "close")
        
        if (length(row <- input$tableDAAccount_rows_selected) > 0) {
          
          stmt <- buildDbQuery("deleteAccount", result$DAAccountData[row ,1])
          res <- executeDbQuery(dbSettings, stmt)
          
          if (is.null(res)) {
            
            showNotification(type = "message",
                sprintf("Failed to delete account %s",
                    result$DAAccountData[row, 2]))
            
          } else {
            
            showNotification(type = "message",
                sprintf("Account %s deleted",
                    result$DAAccountData[row, 2]))
            
          }
          
        }
        
        reloadDAAccountData()
        
      })
  
  
  
  ### When Module Activated
  
  observe(if (active()) {
        
          result$accFlag <- ""
        
      })
  
  
  
  ### Module Output ###
  
  moduleOutput <- list()
  
  return(moduleOutput)
  
}


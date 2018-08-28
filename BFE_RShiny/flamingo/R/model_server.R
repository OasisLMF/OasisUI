
#' Module For The Model Supplier Page
#' @description Server logic for the model supplier page
#' @inheritParams flamingoModule
#' @return empty list
#' @rdname modelSupplierPage
#' @importFrom DT renderDataTable
#' @importFrom shinyjs hide show onclick
#' @importFrom shinyBS toggleModal
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
  
  reloadMData <- function() {
    result$MDataCounter <- isolate(result$MDataCounter + 1)
  }
  
  reloadMRData <- function() {
    result$MRDataCounter <- isolate(result$MRDataCounter + 1)
  }
  
  
  ### Model List Table
  
  # when navigated to Model tab, model table should be updated
  observe(if (active()) {
        
        # reload if reloadMData is called
        force(result$MDataCounter)
        
        hide("divmr")
        hide("cramndelmodres")
        hide("submitmodrescreate")
        hide("ConfirmModResDel")
        
        result$MData <- getModelList(dbSettings)
        
      })
  
  output$tablemodel <- renderDataTable({
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
        
        # reload if reloadMRData is called
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
  
  output$mrtable <- renderDataTable({
        
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
  
  observeEvent(input$btnCreate, {
        
        clearCrtAm()
        
        result$crtAmFlag <- "C"
        toggleModal(session, "crtAmModal", toggle = "open")
        
      })
  
  observeEvent(input$btnAmend, {
        
        if (length(row <- input$mrtable_rows_selected) > 0) {
          
          autoFillCrtAm(row)
          
          result$crtAmFlag <- "A"
          toggleModal(session, "crtAmModal", toggle = "open")
          
        } else {
          
          showNotification("Please select a Model Resource to amend.",
              type = "warning")
          
        }
        
      })
  
  observeEvent(input$btnDelete, {
        
        if (length(row <- input$mrtable_rows_selected) > 0) {
          
          toggleModal(session, "delModal", toggle = "open")
          
        } else {
          
          showNotification("Please select a Model Resource to delete.",
              type = "warning")
          
        }
        
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
            
            showNotification(sprintf("Model Resource %s created.", crtmodres),
                type = "message")
            
            reloadMRData()
            
          } else {
            
            showNotification("Please fill all the fields.", type = "error")
            
          }
          
        } else if (result$crtAmFlag == "A") {
          
          if(length(row <- input$mrtable_rows_selected) > 0) {
            
            updtmodres <- updateModelResource(dbSettings,
                result$MRData[row, 1],
                input$tinmodelresname,
                isolate(input$sinresrctype),
                isolate(input$sinoasissysname),
                isolate(result$MID),
                input$tinmodelresvalue)
            
            showNotification(sprintf("Model Resource %s updated.", updtmodres),
                type = "message")
            
            reloadMRData()
            
          } else {
            
            showNotification("No Model Resource selected.", type = "error")
            
          }
          
        }
        
        toggleModal(session, "crtAmModal", toggle = "close")
        result$crtAmFlag <- ""
        
      })
  
  observeEvent(input$btnCancelCrtAm, {
        
        clearCrtAm()
        
        toggleModal(session, "crtAmModal", toggle = "close")
        result$crtAmFlag <- ""
        
      })
  
  observeEvent(input$btnConfirmDel, {
        
        if (length(row <- input$mrtable_rows_selected) > 0) {
          
          modResId <- deleteModelResource(dbSettings, result$MRData[row,1])
          
          if (!is.null(modResId)) {
            showNotification(sprintf("Model Resource %s deleted.", modResId),
                type = "message")
          } else {
            showNotification(sprintf("Model Resource could not be deleted."))
          }
          
          reloadMRData()
          
        }
        
        toggleModal(session, "delModal", toggle = "close")
        
      })
  
  observeEvent(input$btnCancelDel, {
        
        toggleModal(session, "delModal", toggle = "close")
        
      })
  
  
  ## helper functions
  
  clearCrtAm <- function() {
    
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
  
  
  autoFillCrtAm <- function(row) {
    
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
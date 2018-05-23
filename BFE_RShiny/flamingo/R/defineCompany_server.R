
#' Company Definition Module
#' @description Server logic to define a company
#' @inheritParams flamingoModule
#' @param userId reactive expression yielding user id
#' @return empty list
#' @importFrom DT renderDataTable
#' @rdname companyDefinition
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
  
  reloadCompData <- function() {
    result$compDataCounter <- isolate(result$compDataCounter + 1)
  }
  
  
  ### Company Table ###
  
  # update company table when: 
  # - module activated (e.g. when switching to tab)
  # - reloadCompData() called
  observe(if (active()) {
        
        force(result$compDataCounter)
        
        result$compData <- getCompanyList(dbSettings)
        
      })
  
  # draw company table with custom format options, queries the database every
  # time to update its dataset
  output$tablecompanylist <- renderDataTable({
          datatable(
              result$compData,
              class = "flamingo-table display",
              rownames = TRUE,
              filter = "none",
              selection = "single",
              colnames = c('Row Number' = 1),
              options = list(
                  columnDefs = list(list(visible = FALSE, targets = 0)),
                  autoWidth=TRUE
              )
          )
        })
    
    
  ### Company Create / Update / Delete ###
  
  # Clear Side Bar Panel
  clearCmpnySideBar <- function() {
    updateTextInput(session, "tinputCompName", value = "")
    updateTextInput(session, "tinputCompDom", value = "")
    updateTextInput(session, "tinputCompLegName", value = "")
    updateTextInput(session, "tinputCompRegNo", value = "")
  }
  
  # onclick of cancel button in pop-up
  onclick("abuttonccancel", {
        toggleModal(session, "compcrtupmodal", toggle = "close")
        clearCmpnySideBar()
        reloadCompData()
      })
  
  # onclick of create button in main panel
  onclick("abuttoncompcrt", {
        
        result$compFlag <- "C"
        clearCmpnySideBar()
        
        toggleModal(session, "compcrtupmodal", toggle = "open")
        
      })
  
  # on click of update button in main panel
  onclick("abuttoncompupdate", {
        if(length(input$tablecompanylist_rows_selected) > 0){
          result$compFlag <- "U"
          updateTextInput(session, "tinputCompName",
              value = result$compData[input$tablecompanylist_rows_selected, 2])
          updateTextInput(session, "tinputCompDom",
              value = result$compData[input$tablecompanylist_rows_selected, 3])
          updateTextInput(session, "tinputCompLegName",
              value = result$compData[input$tablecompanylist_rows_selected, 4])
          updateTextInput(session, "tinputCompRegNo",
              value = result$compData[input$tablecompanylist_rows_selected, 5])  
          toggleModal(session, "compcrtupmodal", toggle = "open")
        } else{
          showNotification(type = "warning",
              "Please select the company to update.")
        }
      })
  
  # on click of delete button in main panel
  onclick("abuttoncompdel", {
        if(length(input$tablecompanylist_rows_selected) > 0){
          toggleModal(session, "compdelmodal", toggle = "open")
        } else{
          showNotification(type = "warning",
              "Please select the company to delete")
        }
      })
  
  # on click of cancel button in delete modal
  onclick("abuttonccanceldel", {
        toggleModal(session, "compdelmodal", toggle = "close")
        reloadCompData()
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
            showNotification(type = "error",
                paste("Failed to create company - ", input$tinputCompName))
          } else {
            showNotification(type = "message",
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
              showNotification(type = "error",
                  sprintf("Failed to update company - %s",
                      result$compData[input$tablecompanylist_rows_selected, 2]))
            } else {
              showNotification(type = "message",
                  sprintf("Company - %s updated.",
                      result$compData[input$tablecompanylist_rows_selected, 2]))
            }
          }}
        result$compFlag <- ""
        toggleModal(session, "compcrtupmodal", toggle = "close")
        reloadCompData()
      })
    
  # confirm delete
  onclick("abuttoncconfirmdel", {
        toggleModal(session, "compdelmodal", toggle = "close")
        if(length(input$tablecompanylist_rows_selected) > 0){
          
          stmt <- buildDbQuery("deleteCompany",
              result$compData[input$tablecompanylist_rows_selected, 1])
          res <- executeDbQuery(dbSettings, stmt)
          
          if (is.null(res)) {
            showNotification(type = "error",
                paste("Failed to delete company - ",
                    result$compData[input$tablecompanylist_rows_selected, 2]))
          } else {
            showNotification(type = "message",
                sprintf("Company - %s deleted.",
                    result$compData[input$tablecompanylist_rows_selected, 2]))
          }
          clearCmpnySideBar()
          reloadCompData() 
        }
      })
    
  
  ### Module Output ###########################################################
  
  moduleOutput <- list()
  
  return(moduleOutput)
  
}












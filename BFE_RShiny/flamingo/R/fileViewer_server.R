#' File Viewer Module
#' @description Server logic to view files
#' @inheritParams flamingoModule
#' @param userId user id
#' @param preselRunId preselected run id
#' @return empty list
#' @importFrom DT renderDataTable dataTableProxy selectRows
#' @importFrom shinyjs show hide hidden
#' @importFrom utils read.csv zip
#' @rdname fileViewer
#' @export
fileViewer <- function(
  input,
  output,
  session,
  dbSettings,
  userId,
  preselRunId = reactive(-1),
  active = reactive(TRUE),
  logMessage = message) {
  
  # Reactive Values -----
  
  result <- reactiveValues(
    FLdata = NULL,               #table of files
    FVid = -1,                   #needed to get the operations valid on file id
    fileData = NULL,             #file to download
    currentFile = NULL,          #Current filename
    currentrows = 0           #indices of current rows selected
  )
  
  ns <- session$ns
  
  ### File List Table ----
  
  # Load Company user list data when the page is loaded
  # queries the database every time to update its dataset
  observe(if (active()) {
    stmt <- buildDbQuery("getFileViewerTable")
    result$FLdata <- executeDbQuery(dbSettings, stmt)
  })
  
  # Pre-select the correct runId
  initialSelection <- reactive({
    if (preselRunId() == -1) {
      index <- 1 
      initialSelection <- NULL
    } else {
      index <- match(c(paste0("Process:", preselRunId())), result$FLdata[[7]])
      initialSelection <- rownames(result$FLdata)[c(as.integer(index))]
    }
    return(initialSelection)
  })
  
  # Add buttons to table
  FLdata <- reactive({
    cbind(
      data.frame(Selected = .shinyInput(checkboxInput,"srows_", nrow(result$FLdata), value = FALSE, width = 1)),
      result$FLdata,
      data.frame(View = .shinyInput(actionButton, "vrows_", nrow(result$FLdata), Label = "View", hidden = TRUE, onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')), #,style = "color: #8b2129; background-color: transparent; border-color: #8b2129")), 
      data.frame(Map = .shinyInput(actionButton, "mrows_", nrow(result$FLdata), Label = "Map",  hidden = TRUE, onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;'))   #, style = "color: #8b2129; background-color: transparent; border-color: #8b2129"))
    )
  })
  
  # draw company user list table with custom format options
  output$tableFVfileList <- renderDataTable( if (!is.null(result$FLdata) ) {
    
    datatable(
      #result$FLdata,
      FLdata(),
      class = "flamingo-table display",
      rownames = TRUE,
      selection = "multiple", #list(mode = "multiple",
                    #"none"
                       #selected = initialSelection()
      #),
      colnames = c("Row Number" = 1),
      filter = 'bottom',
      escape = FALSE,
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = c(0,5,6))),
        pageLength = 10,
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        autoWidth = TRUE)
    )
  })
  

  # # Download table of files to csv 
  # output$FVFLdownloadexcel <- downloadHandler(
  #   filename = "filelist.csv",
  #   content = function(file) {
  #     write.csv(result$FLdata, file)}
  # )
  
  ### Checkboxes & Enabling/show buttons ----

  
  observe({
    currentrows <- input$tableFVfileList_rows_selected
    if (is.null(currentrows)) {
      result$currentrows <- 0
    } else {
      result$currentrows <- currentrows
    }
  })
  
  #if one row is selected/unselected, update checkbox and sow/hide buttons
  observeEvent( result$currentrows, {
    if (all(result$currentrows != 0 )) {
      lapply(result$currentrows, function(i){
        updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = TRUE)
        .enableButton(i)})
      lapply(setdiff(input$tableFVfileList_rows_current, result$currentrows), function(i) {
        updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = FALSE)
        .hideButtons(i)})
    } else {
      lapply(input$tableFVfileList_rows_current, function(i){
        updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = FALSE)
        .hideButtons(i)})
    }
  })
  
  
  observe(if (active()) {
    if (!is.null(input$tableFVfileListSelectall) && input$tableFVfileListSelectall) {
      lapply(input$tableFVfileList_rows_current, function(i){updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = input$tableFVfileListSelectall)})
      selectRows(dataTableProxy("tableFVfileList"), input$tableFVfileList_rows_current)
    }
  })
  
  # Select all functionality
  #update checkboxes according to selectAll button
  observeEvent(input$tableFVfileListSelectall, {
    lapply(input$tableFVfileList_rows_current, function(i){updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = input$tableFVfileListSelectall)})
    #to update the selection if page is changed
    if (input$tableFVfileListSelectall == TRUE) {
      selectRows(dataTableProxy("tableFVfileList"), input$tableFVfileList_rows_current)
    } else {
      selectRows(dataTableProxy("tableFVfileList"), NULL)
      lapply(input$tableFVfileList_rows_current, function(i){.hideButtons(i)} )
    }
  })
  
  # Download Files ----
  
  # Files to download in zip bundle
  fs <- reactive({
    files <- c()
    if (all(result$currentrows != 0)) {
      files <- file.path(result$FLdata[result$currentrows, 5], result$FLdata[result$currentrows, 2])
    }
    files
  })
  
  # Download zip button
  output$FVfileListdownloadzip <- downloadHandler(
    filename = "files.zip",
    content = function(fname){
      zip(zipfile = fname, files = fs())
      if (file.exists(paste0(fname, "./"))) {file.rename(paste0(fname, ".zip"), fname)}
    }
  )
  
  
  ### FV Exposure / File Contents ----
  
  # Exposure table
  output$tableFVExposureSelected <- renderDataTable(
    if (!is.null(result$fileData)) {
      datatable(
        result$fileData,
        class = "flamingo-table display",
        rownames = TRUE,
        selection = "none",
        filter = 'bottom',
        colnames = c("Row Number" = 1),
        width = "100%",
        options = list(searchHighlight = TRUE,
                       scrollX = TRUE))
    } else {
      datatable(
        data.frame(content = "nothing to show"),
        class = "flamingo-table display",
        selection = "none",
        rownames = FALSE,
        filter = 'bottom',
        colnames = c(""),
        width = "100%",
        options = list(searchHighlight = TRUE,
                       scrollX = TRUE))
    }
  )
  
  # Export to .csv
  output$FVEdownloadexcel <- downloadHandler(
    filename = result$currentFile, 
    content = function(file) {
      write.csv(result$fileData, file)}
  )
  
  # Modal Panel
  FileContent <- modalDialog(
    easyClose = TRUE,
    size = "l",
    fluidPage(
      h4("File Contents", class = "flamingo-table-title"),
      htmlOutput(ns("tableFVExposureSelectedInfo")),
      dataTableOutput(ns("tableFVExposureSelected")),
      br(),
      downloadButton(ns("FVEdownloadexcel"), label = "Export to csv")
    )
  )
  
  # # Length table
  # observe({if (!is.null(result$FLdata) ) {
  #   result$lenFLdata <- nrow(result$FLdata)
  # }
  # })
  
  #Show content in Modal
  observe({
    if (!is.null(result$FLdata)) {
      lapply(
        X = 1:nrow(result$FLdata),
        FUN = function(i){
          observeEvent(input[[paste0("vrows_", i)]], {
            if (input[[paste0("vrows_", i)]] > 0) {
              result$currentFile <-  paste0(result$FLdata[i, 2])
              showModal(FileContent)
              # Extra info table
              output$tableFVExposureSelectedInfo <- renderUI({
                str1 <- paste("File Name: ", result$FLdata[i,2])
                str2 <- paste("Resource Key ", result$FLdata[i,10])
                HTML(paste(str1, str2, sep = '<br/>'))
              })
              # get data to show in modal table
              fileName <- file.path(result$FLdata[i, 5], result$FLdata[i, 2])
              tryCatch({
                result$fileData <- read.csv(fileName, header = TRUE, sep = ",",
                                            quote = "\"", dec = ".", fill = TRUE, comment.char = "")
              }, error = function(e) {
                showNotification(type = "error",
                                 paste("Could not read file:", e$message))
                result$fileData <- NULL
              }) # end try catch
            } # end check on input vrous_i
          }) # End observer
        }) # end lapply
    } #end if
  })
  
  
  ### Plain Map ----
  
  Map <- modalDialog(
    easyClose = TRUE,
    size = "l",
    fluidPage(
      h4("Map", class = "flamingo-table-title"),
      htmlOutput(ns("tableFVExposureSelectedInfo")),
      leafletOutput(ns("plainmap"))
    )
  )
  
  observe({
    if (!is.null(result$FLdata)) {
      lapply(
        X = 1:nrow(result$FLdata),
        FUN = function(i){
          observeEvent(input[[paste0("mrows_", i)]], {
            if (input[[paste0("mrows_", i)]] > 0) {
              showModal(Map)
              fileName <- file.path(result$FLdata[i, 5], result$FLdata[i, 2])
              # Extra info table
              output$tableFVExposureSelectedInfo <- renderUI({
                str1 <- paste("File Name: ", result$FLdata[i,2])
                str2 <- paste("Resource Key ", result$FLdata[i,10])
                HTML(paste(str1, str2, sep = '<br/>'))
              })
              output$plainmap <- renderLeaflet({createPlainMap(fileName)})
            }
          })
        })
    }
  })
  
  # Helper functions ------------------------
  
  # Check permission row by row
  .enableButton <- function(i) {
    result$FVid <- result$FLdata[i, 1]
    validButtons <- executeDbQuery(dbSettings,
                                   buildDbQuery("TellOperationsValidOnFileID", result$FVid))
    manageButtons <- c("FO_btn_show_raw_content" = paste0("vrows_", i),
                       "FO_btn_show_map" = paste0("mrows_", i))
    # lapply(t(validButtons), function(btnIDs){enable(manageButtons[btnIDs])})
    lapply(t(validButtons), function(btnIDs){shinyjs::show(manageButtons[btnIDs])})
  }
  
  #hide buttons in a row
  .hideButtons <- function(i=NULL){
    shinyjs::hide(paste0("vrows_", i))
    shinyjs::hide(paste0("mrows_", i))
  }
  
  # utility function to add input Id to buttons in table
  .shinyInput <- function(FUN, id, num, Label = NULL, hidden = FALSE,  ...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      if (hidden) {
        inputs[i] <- as.character(shinyjs::hidden(FUN(inputId = ns(paste0(id,i)), label = Label, ...)))
      } else {
        inputs[i] <- as.character(FUN(inputId = ns(paste0(id,i)), label = Label, ...))
      }
    }
    inputs
  }
  
  invisible()
  
}
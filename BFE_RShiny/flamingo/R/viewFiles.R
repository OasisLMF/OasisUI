# ViewFilesModule Module -----------------------
#' Module to View Files
# UI ---------------
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
ViewFilesModuleUI <-  function(id, includechkbox = FALSE){
  ns <- NS(id)
  tagList(
    if (includechkbox) {
      checkboxInput(inputId = ns("chkboxselectall"), label = "Select all", value = FALSE)
    },
    DTOutput(ns("outputFLtable")),
    if (includechkbox) {
      downloadButton(ns("FLdownloadzip"), label = "Export to zip")
      bsTooltip(ns("FLdownloadzip"),
                file_Viewer$FLdownloadzip,
                placement = "right",
                options   = list(container = "body"))
    } else {
      downloadButton(ns("FLdownloadexcel"), label = "Export to csv")
    }
  )
}


# Server -----------
#' @description Server logic to view  files
#' @inheritParams flamingoModule
#' @param filesListData table of output files for a given runID
#' @return list of reactives:
#' @rdname panelViewOutputFilesModule
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom dplyr mutate select contains filter
#' @export
ViewFilesModule <- function(input, output, session, logMessage = message, filesListData, includemrows = FALSE, includechkbox = FALSE) {
  
  ns <- session$ns
  
  
  # Reactive values & parameters --------------------------------------------
  
  result <- reactiveValues(
    #reactive of the input list of files
    filesListData = NULL,
    #df to show in table
    filesListDataButtons = NULL,
    #current status of vrows buttons
    currentv_rows = NULL,
    #previous status of vrows buttons
    previousv_rows = NULL,
    #current status of mrows buttons
    currentm_rows = NULL,
    #previous status of mrows buttons
    previousm_rows = NULL,
    #View output file content
    currentFile = NULL,
    #content of curr file
    fileData = NULL
  )
  # 
  # observe({
  #   print("names")
  #   print(names(input))
  # })
  
  
  # Add buttons -------------------------------------
  observeEvent(filesListData(), ignoreNULL = FALSE, {
    #result$filesListData <- NULL
    filesListData <- filesListData()
    if (length(filesListData) > 0) {
      result$filesListData <- filesListData
      if (includechkbox) {
        filesListData <- cbind(data.frame(Selected = .shinyInput(checkboxInput,"srows_", nrow(filesListData), value = FALSE, width = 1)), filesListData)
      }
      filesListData <- cbind(filesListData, data.frame(View = .shinyInput(actionButton, "vrows_", nrow(filesListData), Label = "View", hidden = TRUE, onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
      result$previousv_rows <- data.frame("vrows" = rep(0, nrow(filesListData)))
      result$currentv_rows <- data.frame("vrows" = rep(0, nrow(filesListData)))
      if (includemrows) {
        filesListData <- cbind(filesListData, data.frame(Map = .shinyInput(actionButton, "mrows_", nrow(filesListData), Label = "Map", hidden = TRUE, onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
        result$previousm_rows <- data.frame("mrows" = rep(0, nrow(filesListData)))
        result$currentm_rows <- data.frame("mrows" = rep(0, nrow(filesListData)))
      }
      result$filesListDataButtons <- filesListData %>% select(-contains("Location") )
    } else {
      result$filesListData <- NULL
    }
    #clean up reactives
    result$currentFile <- NULL
    result$fileData <- NULL
  })
  
  
  output$outputFLtable <- renderDT(
    if (!is.null(result$filesListData) && nrow(result$filesListData) > 0) {
      if (includechkbox) {
        selectionUsed <- "multiple"
      } else {
        selectionUsed <- "single"
      }
      datatable(
        result$filesListDataButtons,
        class = "flamingo-table display",
        rownames = TRUE,
        escape = FALSE,
        selection =  selectionUsed,
        colnames = c('Row Number' = 1),
        options = .getFLTableOptions()
      )
    } else {
      datatable(
        data.frame(content = "nothing to show"),
        class = "flamingo-table display",
        selection = "none",
        rownames = FALSE,
        filter = 'bottom',
        colnames = c(""),
        width = "100%",
        options = list(searchHighlight = TRUE))
    }
  )
  
  
  # Download Files ----
  
  # Files to download in zip bundle
  fs <- reactive({
    files <- c()
    if (!is.null(input$outputFLtable_rows_selected)) {
      files <- file.path(result$filesListData[input$outputFLtable_rows_selected, 5], result$filesListData[input$outputFLtable_rows_selected, 2])
    }
    files
  })
  
  # Download zip button
  output$FLdownloadzip <- downloadHandler(
    filename = "files.zip",
    content = function(fname){
      zip(zipfile = fname, files = fs())
      if (file.exists(paste0(fname, "./"))) {file.rename(paste0(fname, ".zip"), fname)}
    }
  )
  
  output$FLTdownloadexcel <- downloadHandler(
    filename = "outputFLtable.csv",
    content = function(file) {
      write.csv(result$filesListData, file)
    }
  )
  
  # Selected Row --------------------------------------------------------
  
  #identify selected rows
  observeEvent({
    input$outputFLtable_rows_selected
    input$outputFLtable_rows_current
    lapply(input$outputFLtable_rows_current, function(i){input[[paste0("vrows_", i)]]})
    lapply(input$outputFLtable_rows_current, function(i){input[[paste0("mrows_", i)]]})
  }, {
    if (!is.null(input$outputFLtable_rows_current)) {
      if (!is.null(result$currentv_rows)) {
        for (i in 1:nrow(result$currentv_rows)) {
          result$currentv_rows$vrows[i] <- ifelse(!is.null(input[[paste0("vrows_", i)]]), input[[paste0("vrows_", i)]], 0)
        }
      }
      if (!is.null(result$currentm_rows)) {
        for (i in 1:nrow(result$currentm_rows)) {
          result$currentm_rows$mrows[i] <- ifelse(!is.null(input[[paste0("mrows_", i)]]), input[[paste0("mrows_", i)]], 0)
        }
      }
    }
  })
  
  
  #if one row is selected/unselected, update checkbox and sow/hide buttons
  observeEvent( input$outputFLtable_rows_selected, ignoreNULL = FALSE, {
    if (length( input$outputFLtable_rows_selected) > 0) {
      lapply(input$outputFLtable_rows_selected, function(i){
        updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = TRUE) 
        .enableButton(i)})
      lapply(setdiff(input$outputFLtable_rows_current, input$outputFLtable_rows_selected), function(i) {
        updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = FALSE)
        .hideButtons(i)})
    } else {
      lapply(input$outputFLtable_rows_current, function(i){
        updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = FALSE)
        .hideButtons(i)})
    }
  })
  
  # Select All Functionality --------------------------------------------
  
  #If page in table is changed, update rows selection based on select all value
  observe({
    if (!is.null(input$chkboxselectall) && input$chkboxselectall) {
      lapply(input$outputFLtable_rows_current, function(i){updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = input$chkboxselectall)})
      selectRows(dataTableProxy("outputFLtable"), input$outputFLtable_rows_current)
    }
  })
  
  #update checkboxes according to selectAll button
  observeEvent(input$chkboxselectall, ignoreNULL = FALSE, {
    lapply(input$outputFLtable_rows_current, function(i){updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = input$chkboxselectall)})
    #to update the selection if page is changed
    if (!is.null(input$chkboxselectall)  && input$chkboxselectall) {
      selectRows(dataTableProxy("outputFLtable"), input$outputFLtable_rows_current)
    } else {
      selectRows(dataTableProxy("outputFLtable"), NULL)
      lapply(input$outputFLtable_rows_current, function(i){.hideButtons(i)} )
    }
  })
  
  
  # File content view ---------------------------------------------------
  
  # Exposure table
  output$tableFVExposureSelected <- renderDT(
    if (!is.null(result$fileData) && nrow(result$fileData) > 0 ) {
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
      DTOutput(ns("tableFVExposureSelected")),
      br(),
      downloadButton(ns("FVEdownloadexcel"), label = "Export to csv")
    )
  )
  
  observeEvent(result$currentv_rows, {
    if (!is.null(result$currentv_rows) && nrow(result$currentv_rows) > 0) {
      #idx is the index of the button clicked
      # print("result$currentv_rows")
      # print(result$currentv_rows)
      # print("result$previousv_rows")
      # print(result$previousv_rows)
      idx <- seq(nrow(result$filesListData))[result$currentv_rows != result$previousv_rows]
      if (length(idx) > 1) {
        #this happens when changing run. the inputs are not reset, wherease the result$previousv_rows is all 0.  Can be avoided with better initialization of result$previousv_rows
        result$previousv_rows <- result$currentv_rows
      } else  if ( length(idx) ==  1) {
        #this is the case when a button has been clicked
        result$previousv_rows <- result$currentv_rows
        showModal(FileContent)
        # Extra info table
        output$tableFVExposureSelectedInfo <- renderUI({
          str1 <- paste("File Name: ", result$filesListData[idx,2])
          str2 <- paste("Resource Key ", result$filesListData[idx,10])
          HTML(paste(str1, str2, sep = '<br/>'))
        })
        # get data to show in modal table
        fileName <- file.path(result$filesListData[idx, 5], result$filesListData[idx, 2])
        tryCatch({
          result$fileData <- read.csv(fileName, header = TRUE, sep = ",",
                                      quote = "\"", dec = ".", fill = TRUE, comment.char = "")
        }, error = function(e) {
          showNotification(type = "error",
                           paste("Could not read file:", e$message))
          result$fileData <- NULL
        }) # end try catch
      }
    }
  })
  
  # File content map -------------------------
  
  Map <- modalDialog(
    easyClose = TRUE,
    size = "l",
    fluidPage(
      h4("Map", class = "flamingo-table-title"),
      htmlOutput(ns("tableFVExposureSelectedInfo")),
      leafletOutput(ns("plainmap"))
    )
  )
  
  observeEvent(result$currentm_rows, {
    if (!is.null(result$currentm_rows) && nrow(result$currentm_rows) > 0) {
      #idx is the index of the button clicked
      idx <- seq(nrow(result$filesListData))[result$currentm_rows != result$previousm_rows]
      if (length(idx) > 1) {
        #this happens when changing run. the inputs are not reset, wherease the result$previousv_rows is all 0.  Can be avoided with better initialization of result$previousv_rows
        result$previousm_rows <- result$currentm_rows
      } else  if ( length(idx) ==  1) {
        #this is the case when a button has been clicked
        result$previousm_rows <- result$currentm_rows
        showModal(Map)
        # Extra info table
        output$tableFVExposureSelectedInfo <- renderUI({
          str1 <- paste("File Name: ", result$filesListData[idx,2])
          str2 <- paste("Resource Key ", result$filesListData[idx,10])
          HTML(paste(str1, str2, sep = '<br/>'))
        })
        # get data to show in modal table
        fileName <- file.path(result$filesListData[idx, 5], result$filesListData[idx, 2])
        tryCatch({
          output$plainmap <- renderLeaflet({createPlainMap(fileName)})
        }, error = function(e) {
          showNotification(type = "error",
                           paste("Could not read file:", e$message))
        }) # end try catch
      }
    }
  })
  
  # Helper functions -------------------------
  
  .getFLTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      columnDefs = list(list(visible = FALSE, targets = c(0,5,6))),
      processing = 0,
      scrollX = TRUE,
      pageLength = 10,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
      autoWidth = TRUE)
    return(options)
  }
  
  # Check permission row by row
  .enableButton <- function(i) {
    FVid <- result$filesListData[i, 1]
    validButtons <- executeDbQuery(dbSettings,
                                   buildDbQuery("TellOperationsValidOnFileID", FVid))
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
  
  # Module Output -----------------------
  invisible()
  
}

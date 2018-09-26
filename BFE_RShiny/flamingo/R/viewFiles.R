# ViewFilesModule Module -----------------------
#' Module to View Files
# UI ---------------
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @importFrom shinyBS bsTooltip
#' @export
ViewFilesModuleUI <-  function(id, includechkbox = FALSE){
  ns <- NS(id)
  tagList(
    tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
    if (includechkbox) {
      checkboxInput(inputId = ns("chkboxselectall"), label = "Select all", value = FALSE)
    },
    DTOutput(ns("outputFLtable")),
    if (includechkbox) {
      downloadButton(ns("FLdownloadzip"), label = "Export to zip")
    },
    if (includechkbox) {
      bsTooltip(ns("FLdownloadzip"),
                file_Viewer$FLdownloadzip,
                placement = "right",
                options   = list(container = "body"))
    }, 
    if (!includechkbox) {
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
#' @importFrom leaflet renderLeaflet leafletOutput
#' @export
ViewFilesModule <- function(input, output, session, logMessage = message, filesListData, includemrows = FALSE, includechkbox = FALSE) {
  
  ns <- session$ns
  
  
  # Reactive values & parameters --------------------------------------------
  
  maxrowsperpage <- 10
  
  result <- reactiveValues(
    #reactive of the input list of files
    filesListData = NULL,
    #df to show in table
    filesListDataButtons = NULL,
    #View output file content
    currentFile = NULL,
    #content of curr file
    fileData = NULL
  )
  
  
  # observe({
  #   filesListData()
  #   print("names")
  #   print(names(input))
  # })
  # 
  # observeEvent(input$vrows_1, {
  #   print("vrows_1")
  #   print(input$vrows_1) 
  # })
  # 
  observe({
    print("input$outputFLtable_rows_selected")
    print(input$outputFLtable_rows_selected)
  })

  # observe({
  #   print("input$outputFLtable_rows_current")
  #   print(input$outputFLtable_rows_current)
  # })
  # 
  
  # Add buttons -------------------------------------
  observeEvent(filesListData(), ignoreNULL = FALSE, {
    #result$filesListData <- NULL
    filesListData <- filesListData()
    # print("removeUI")
    # removeUI(selector = "#vrows_1", immediate = TRUE)
    # Sys.sleep(5)
    if (length(filesListData) > 0) {
      result$filesListData <- filesListData
      if (includechkbox) {
        filesListData <- cbind(data.frame(Selected = .shinyInput(checkboxInput,"srows_", nrow(filesListData), value = FALSE, width = 1)), filesListData)
      }
      filesListData <- cbind(filesListData,data.frame(View = .shinyInput(shiny::actionButton, "vrows_", nrow(filesListData), Label = "View", hidden = TRUE, onclick = 'Shiny.onInputChange(\"select_button\",  this.id)', onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
      if (includemrows) {
        filesListData <- cbind(filesListData,data.frame(Map = .shinyInput(shiny::actionButton, "mrows_", nrow(filesListData), Label = "Map", hidden = TRUE, onclick = 'Shiny.onInputChange(\"select_button\",  this.id)', onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
      }
      result$filesListDataButtons <- filesListData %>% select(-contains("Location") )
    } else {
      result$filesListData <- NULL
    }
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
  
  lapply(seq(maxrowsperpage), function(idx){
    observeEvent({input[[paste0("vrows_", idx)]]},{
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
    })#end observeEvent
  })
  

  # File content map -------------------------
  
  Map <- modalDialog(
    easyClose = TRUE,
    size = "l",
    fluidPage(
      h4("Map", class = "flamingo-table-title"),
      htmlOutput(ns("tableFVMapSelectedInfo")),
      leafletOutput(ns("plainmap"))
    )
  )
  
  lapply(seq(maxrowsperpage), function(idx){
    observeEvent({input[[paste0("mrows_", idx)]]},{
      showModal(FileContent)
      # Extra info table
      output$tableFVMapSelectedInfo <- renderUI({
        str1 <- paste("File Name: ", result$filesListData[idx,2])
        str2 <- paste("Resource Key ", result$filesListData[idx,10])
        HTML(paste(str1, str2, sep = '<br/>'))
      }) 
      # get data to show in modal table
      fileName <- file.path(result$filesListData[idx, 5], result$filesListData[idx, 2])
      tryCatch({
        routput$plainmap <- renderLeaflet({createPlainMap(fileName)})
      }, error = function(e) {
        showNotification(type = "error",
                         paste("Could not read file:", e$message))
        result$fileData <- NULL
      }) # end try catch
    })#end observeEvent
  })

  
  # Helper functions -------------------------
  
  .getFLTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      columnDefs = list(list(visible = FALSE, targets = c(0,5,6))),
      processing = 0,
      scrollX = FALSE,
      pageLength = maxrowsperpage,
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

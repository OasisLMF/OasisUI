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
  
  result <- reactiveValues(
    FLdata = NULL,               #table of files
    FVid = -1,                   #needed to get the operations valid on file id
    fileData = NULL,             #file to download
    lastViewBtn = character(),   #last button View to be clicked
    lastMapBtn  = character(),   #last button Map to be clicked
    validButtons = NULL,         #buttons for which the user has permission. Depends on current list of files
    lenFLdata = 400              #length of the table fileData
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
      selection = list(mode = "multiple",#"none"
                       selected = initialSelection()
      ),
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
  
  ### Checkboxes ----
  
  #if one row is selected, update checkbox
  observe(if (active()) {
    lapply(input$tableFVfileList_rows_selected, function(i){
      updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = TRUE  )})
    lapply(setdiff(input$tableFVfileList_rows_current, input$tableFVfileList_rows_selected), function(i) {
      updateCheckboxInput(session = session, inputId = paste0("srows_", i), value = FALSE  )})
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
      lapply(input$tableFVfileList_rows_current, function(i) {
        shinyjs::hide(paste0("vrows_", i))
        shinyjs::hide(paste0("mrows_", i))
      })
    }
  })
  
  # Download Files ----
  
  # Files to download in zip bundle
  fs <- reactive({
    files <- c()
    if (length( input$tableFVfileList_rows_selected) > 0) {
      files <- file.path(result$FLdata[input$tableFVfileList_rows_selected, 5], result$FLdata[input$tableFVfileList_rows_selected, 2])
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
  
  
  ### Enabling/show buttons ----

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

  observeEvent(input$tableFVfileList_rows_selected, {
    lapply(input$tableFVfileList_rows_selected,  function(i){.enableButton(i)})
  })
  

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
    filename = paste0(result$FLdata[result$lastViewClicked, 3]),
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

  # Length table
  observe({if (!is.null(result$FLdata) ) {
    result$lenFLdata <- nrow(result$FLdata)
  }
  })
  
  observe({lapply(
    X = 1:result$lenFLdata,
    FUN = function(i){
      observeEvent(input[[paste0("vrows_", i)]], {
        if (input[[paste0("vrows_", i)]] > 0) {
          result$lastViewBtn <- paste0(i)   
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
          })
        }
      })
    }
  )
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
  
  observe({lapply(
    X = 1:result$lenFLdata,
    FUN = function(i){
      observeEvent(input[[paste0("mrows_", i)]], {
        if (input[[paste0("mrows_", i)]] > 0) {
          result$lastMapBtn <- paste0(i)   
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
    }
  )
  })

  
}
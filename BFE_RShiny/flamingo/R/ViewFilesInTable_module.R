# View Files in table Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' ViewFilesInTableUI
#'
#' @rdname ViewFilesInTable
#'
#' @description UI/View to view  files.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
ViewFilesInTableUI <-  function(id, includechkbox = FALSE){
  ns <- NS(id)
  tagList(
    tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);});"),
    tags$script("Shiny.addCustomMessageHandler('resetcolorWhite', function(variableName){
                document.getElementById(variableName).style.color = '#ffffff';});"),
    tags$script("Shiny.addCustomMessageHandler('resetcolorOasis', function(variableName){
                document.getElementById(variableName).style.color = '#8b2129';});"),
    if (includechkbox) {
      checkboxInput(inputId = ns("chkboxselectall"), label = "Select all", value = FALSE)
    },
    DTOutput(ns("dt_outputFL")),
    if (includechkbox) {
      downloadButton(ns("FLdownloadzip"), label = "Export to zip") %>%
        bs_embed_tooltip(title = file_Viewer$FLdownloadzip, placement = "right")
    },
    if (!includechkbox) {
      downloadButton(ns("FLdownloadexcel"), label = "Export to csv")
    }
  )
}

# Server -----------------------------------------------------------------------
#' ViewFilesInTable
#'
#' @rdname ViewFilesInTable
#'
#' @description Server logic to view files.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param tbl_filesListData dataframe of output files.
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs hidden
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT DTOutput
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom htmlwidgets JS
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet leafletOutput
#' @importFrom utils write.csv
#'
#' @export
ViewFilesInTable <- function(input, output, session, 
                             logMessage = message, 
                             tbl_filesListData, 
                             param = NULL,
                             includechkbox = FALSE) {
  
  ns <- session$ns
  
  
  # Reactive values & parameters -----------------------------------------------
  
  maxrowsperpage <- 10
  
  result <- reactiveValues(
    #df to show in table
    tbl_filesListData_wButtons = NULL,
    #View output file content
    currentFile = NULL,
    #content of curr file
    tbl_fileData = NULL
  )
  
  # Add buttons ----------------------------------------------------------------
  observeEvent(tbl_filesListData(), ignoreNULL = FALSE, {
    filesListData <- tbl_filesListData()
    if (length(filesListData) > 0) {
      if (includechkbox) {
        filesListData <- cbind(data.frame(Selected = .shinyInput(flamingoCheckboxButton,"srows_", nrow(filesListData), Label = NULL,
                                                                 hidden = FALSE,
                                                                 style = "background-color: white;
                                                                          border-color: black;
                                                                          color: white;
                                                                          font-size: 10px;
                                                                          text-shadow: none;
                                                                          padding: 0px;
                                                                          height: 16px;
                                                                          width: 16px;",
                                                                 icon = icon("check"),
                                                                 onclick = paste0('Shiny.onInputChange(\"',ns("select_sbutton"),'\",  this.id)')
        )),
        filesListData)
      }
      filesListData <- cbind(filesListData,data.frame(Explore = .shinyInput(actionButton, "vrows_", nrow(filesListData), Label = "Explore", hidden = TRUE, onclick = paste0('Shiny.onInputChange(\"',ns("select_vbutton"),'\",  this.id)'), onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
      result$tbl_filesListData_wButtons <- filesListData
    } else {
      result$tbl_filesListData_wButtons <- NULL
    }
  })
  
  # Render Table ---------------------------------------------------------------
  output$dt_outputFL <- renderDT(
    if (!is.null(result$tbl_filesListData_wButtons) && nrow(result$tbl_filesListData_wButtons) > 0) {
      if (includechkbox) {
        selectionUsed <- "multiple"
      } else {
        selectionUsed <- "single"
      }
      datatable(
        result$tbl_filesListData_wButtons,
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
  
  # Download zip Files ----------------------------------------------------------
  
  # Download zip button
  output$FLdownloadzip <- downloadHandler(
    filename = "files.zip",
    content = function(fname){
      #path of files to download in Zip bundle
      fs <- c()
      for (f in 1:nrow(result$tbl_filesListData_wButtons)) {
        filename <- result$tbl_filesListData_wButtons[f, "fields"]
        func <- get(paste0("return_", filename, "_df"))
        return_df <- func(param())
        if (nrow(return_df) > 0) {
          fpath <- file.path(".", paste0(filename, ".csv"))
          write.csv(x = return_df, file = fpath)
          fs <- c(fs, fpath)
        }
      }
      zip(zipfile = fname, files = fs)
      if (file.exists(paste0(fname, "./"))) {file.rename(paste0(fname, ".zip"), fname)}
    }#,
    #contentType = "application/zip"
  )
  
  # Selected Row ---------------------------------------------------------------
  observeEvent( input$dt_outputFL_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (length( input$dt_outputFL_rows_selected) > 0) {
      lapply(input$dt_outputFL_rows_selected, function(i) {
        session$sendCustomMessage(type = 'resetcolorOasis', message =  session$ns( paste0("srows_", i)))
        show(paste0("vrows_", i))})
      lapply(setdiff(input$dt_outputFL_rows_current, input$dt_outputFL_rows_selected), function(i) {
        session$sendCustomMessage(type = 'resetcolorWhite', message = session$ns(paste0("srows_", i)))
        hide(paste0("vrows_", i))})
    }else {
      lapply(input$dt_outputFL_rows_current, function(i){
        session$sendCustomMessage(type = 'resetcolorWhite', message = session$ns(paste0("srows_", i)))
        hide(paste0("vrows_", i))})
    }
  })
  
  # Select All Functionality ---------------------------------------------------
  
  #If page in table is changed, update rows selection based on select all value
  observeEvent(input$dt_outputFL_rows_current, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (!is.null(input$chkboxselectall) && input$chkboxselectall) {
      lapply(input$dt_outputFL_rows_current, function(i){
        if (input$chkboxselectall) {
          session$sendCustomMessage(type = 'resetcolorOasis', message = session$ns(paste0("srows_", i)))
        } else {
          session$sendCustomMessage(type = 'resetcolorWhite', message = session$ns(paste0("srows_", i)))
        }
      })
      selectRows(dataTableProxy("dt_outputFL"), input$dt_outputFL_rows_current)
    }
  })
  
  #update checkboxes according to selectAll button
  observeEvent(input$chkboxselectall, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (!is.null(result$tbl_filesListData_wButtons)) {
      if (!is.null(input$chkboxselectall) && input$chkboxselectall) {
        lapply(input$dt_outputFL_rows_current, function(i){
          if (input$chkboxselectall) {
            session$sendCustomMessage(type = 'resetcolorOasis', message = session$ns(paste0("srows_", i)))
          } else {
            session$sendCustomMessage(type = 'resetcolorWhite', message = session$ns(paste0("srows_", i)))
          }
        })
        selectRows(dataTableProxy("dt_outputFL"), input$dt_outputFL_rows_current)
      } else {
        lapply(input$dt_outputFL_rows_current, function(i){
          session$sendCustomMessage(type = 'resetcolorWhite', message =  session$ns( paste0("srows_", i)))
        })
        selectRows(dataTableProxy("dt_outputFL"), NULL)
      }
    }
  })
  
  # File content view ----------------------------------------------------------
  # Modal Panel
  FileContent <- modalDialog(
    easyClose = TRUE,
    size = "l",
    fluidPage(
      fluidRow(
               h4("File Contents", class = "flamingo-table-title")),
      fluidRow(
               htmlOutput(ns("FVExposureStatisticInfo"))),
      br(),
      fluidRow(
        flamingoButton(inputId = ns("abuttonview"), label = "View", icon = icon("file")),
        hidden(flamingoButton(inputId = ns("abuttonmap"), label = "Map", icon = icon("map"))),
        downloadButton(ns("FVEdownloadexcel"), label = "Export to csv"),
        style = "inline:true"),
      
      hidden(flamingoPanel(
        id = ns("flamingoPanelFVExposureSelected"),
        collapsible = FALSE,
        heading =  tagAppendChildren(
          h4("File Content"),
          actionButton(inputId = ns("abuttonhidedFVExposureSelected"), label = NULL, icon = icon("times"), style = "float: right;")),
        DTOutput(ns("dt_FVExposureSelected")))),
      
      hidden(flamingoPanel(
        id = ns("flamingoPanelmapFVExposureSelected"),
        collapsible = FALSE,
        heading = tagAppendChildren(
          h4("Map "),
          actionButton(inputId = ns("abuttonhidemapFVExposureSelected"), label = NULL, icon = icon("times"), style = "float: right;")),
        leafletOutput(ns("plainmap"))))
    )
  )
  
  # Panel View Content
  observeEvent(input$abuttonhidedFVExposureSelected, {
    hide("flamingoPanelFVExposureSelected")
  })
  
  onclick("abuttonview", {
    show("flamingoPanelFVExposureSelected")
  })
  
  # Exposure table
  output$dt_FVExposureSelected <- renderDT(
    if (!is.null(result$tbl_fileData) && nrow(result$tbl_fileData) > 0 ) {
      datatable(
        result$tbl_fileData,
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
      write.csv(result$tbl_fileData, file)}
  )
  
  # Panel Map
  observeEvent(input$abuttonhidemapFVExposureSelected, {
    hide("flamingoPanelmapFVExposureSelected")
  })
  
  onclick("abuttonmap", {
    show("flamingoPanelmapFVExposureSelected")
  })
  
  observeEvent({input[["select_vbutton"]]},{
    splitidx <- strsplit(input[["select_vbutton"]], "_")
    idx <- as.numeric(splitidx[[1]][length(splitidx[[1]])])
    showModal(FileContent)
    session$sendCustomMessage(type = 'resetInputValue', message =  session$ns("select_vbutton"))
    #Get dataframe
    result$currentFile <- result$tbl_filesListData_wButtons[idx, "fields"]
    func <- get(paste0("return_", result$currentFile, "_df"))
    result$tbl_fileData <- func(param())
    #Show buttons
    if ("LATITUDE" %in% names(result$currentFile)) {
      show("abuttonmap")
    } else {
      hide("abuttonmap")
    }
    # Extra info table
    output$FVExposureStatisticInfo <- renderUI({
      column(12,
             p(paste0("File Name: ", result$currentFile)),
             p(paste0("Number of Rows", nrow(result$tbl_fileData))),
             p("Column names"),
             p(paste(names(result$tbl_fileData), sep = " "))
      )
    })
    
    if (!is.null(result$tbl_fileData)) {
      output$plainmap <- renderLeaflet({createPlainMap(result$currentFile)})
    }
  })#end observeEvent
  
  # Helper functions -----------------------------------------------------------
  
  # default table options
  .getFLTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      # columnDefs = list(list(visible = FALSE, targets = c(0,5,6))),
      processing = 0,
      scrollX = FALSE,
      pageLength = maxrowsperpage,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )
    return(options)
  }
  
  # utility function to add to buttons in table
  .shinyInput <- function(FUN, id, num, Label = NULL, hidden = FALSE,  ...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      if (hidden) {
        inputs[i] <- as.character(hidden(FUN(inputId = ns(paste0(id,i)), label = Label, ...)))
      } else {
        inputs[i] <- as.character(FUN(inputId = ns(paste0(id,i)), label = Label, ...))
      }
    }
    inputs
  }
  
}
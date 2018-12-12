# ViewFilesModule Module -------------------------------------------------------
# UI ---------------------------------------------------------------------------
#' ViewFilesModuleUI
#'
#' @rdname ViewFilesModule
#'
#' @description UI/View to view  files.
#'
#' @return List of tags.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
ViewFilesModuleUI <-  function(id, includechkbox = FALSE){
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
#' ViewFilesModule
#'
#' @rdname ViewFilesModule
#'
#' @description Server logic to view files.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-logMessage
#'
#' @param filesListData Table of output files for a given anaID.
#' @param dbSettings Setting object as returned by e.g. [flamingoDB()].
#' @param includechkbox logical indicating the presence of checkboxes. Default FALSE.
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
#' @importFrom utils zip
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#'
#' @export
ViewFilesModule <- function(input, output, session, 
                            dbSettings,
                            logMessage = message, filesListData, includechkbox = FALSE) {

  ns <- session$ns


  # Reactive values & parameters -----------------------------------------------

  maxrowsperpage <- 10

  result <- reactiveValues(
    #reactive of the input list of files
    filesListData = NULL,
    #df to show in table
    filesListDataButtons = NULL,
    #View output file content
    currentFile = NULL,
    #content of curr file
    tbl_fileData = NULL
  )

  # Add buttons ----------------------------------------------------------------
  observeEvent(filesListData(), ignoreNULL = FALSE, {
    filesListData <- filesListData()
    if (length(filesListData) > 0) {
      result$filesListData <- filesListData
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
      result$filesListDataButtons <- filesListData %>% select(-contains("Location") )
    } else {
      result$filesListData <- NULL
    }
  })


  output$dt_outputFL <- renderDT(
    if (!is.null(result$filesListData) && nrow(result$filesListData) > 0) {
      if (includechkbox) {
        selectionUsed <- "multiple"
      } else {
        selectionUsed <- "single"
      }
      if ("FileID" %in% names(result$filesListDataButtons)) {
        tableToShow <- result$filesListDataButtons %>% select(-c(FileID))
      } else {
        tableToShow <- result$filesListDataButtons
      }
      datatable(
        tableToShow,
        class = "flamingo-table display",
        rownames = TRUE,
        escape = FALSE,
        selection =  selectionUsed,
        colnames = c('row number' = 1),
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


  # Download Files -------------------------------------------------------------

  # Files to download in zip bundle
  fs <- reactive({
    files <- c()
    if (!is.null(input$dt_outputFL_rows_selected)) {
      files <- file.path(result$filesListData[input$dt_outputFL_rows_selected, filesListData.path], result$filesListData[input$outputFLtable_rows_selected, filesListData.fileName])
    }
    files
  })


  # Download zip button
  output$FLdownloadzip <- downloadHandler(
    filename = "files.zip",
    content = function(fname){
      fs <- file.path(result$filesListData[, filesListData.path], result$filesListData[ filesListData.fileName])
      zip(zipfile = fname, files = fs)
      if (file.exists(paste0(fname, "./"))) {file.rename(paste0(fname, ".zip"), fname)}
    }
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
    if (!is.null(result$filesListDataButtons)) {
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
        column(4,
               h4("File Contents", class = "flamingo-table-title")),
        column(4,
               htmlOutput(ns("dt_FVExposureSelected")))),
      fluidRow(
        column(12,
               h5("Metadata"),
               htmlOutput(ns("tableFVExposureStatisticInfo")))),
      br(),
      fluidRow(
        hidden(flamingoButton(inputId = ns("abuttonview"), label = "View", icon = icon("file"))),
        hidden(flamingoButton(inputId = ns("abuttonmap"), label = "Map", icon = icon("map"))),
        downloadButton(ns("FVEdownloadexcel"), label = "Export to csv"),
        style = "inline:true"),
      hidden(flamingoPanel(
        id = ns("flamingoPaneltableFVExposureSelected"),
        collapsible = FALSE,
        heading =  tagAppendChildren(
          h4("File Content"),
          #uiOutput(ns("paneltitledt_FVExposureSelected"), inline = TRUE),
          actionButton(inputId = ns("buttonhidedt_FVExposureSelected"), label = NULL, icon = icon("times"), style = "float: right;")),
        DTOutput(ns("dt_FVExposureSelected")))),
      hidden(flamingoPanel(
        id = ns("flamingoPanelmapFVExposureSelected"),
        collapsible = FALSE,
        heading = tagAppendChildren(
          h4("Map "),
          #uiOutput(ns("paneltitlemapFVExposureSelected"), inline = TRUE),
          actionButton(inputId = ns("buttonhidemapFVExposureSelected"), label = NULL, icon = icon("times"), style = "float: right;")),
        leafletOutput(ns("plainmap"))))
    )
  )

  # Panel View Content
  observeEvent(input$buttonhidedt_FVExposureSelected, {
    hide("flamingoPaneltableFVExposureSelected")
  })

  onclick("abuttonview", {
    show("flamingoPaneltableFVExposureSelected")
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
        colnames = c("row number" = 1),
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

  # Panel Map
  observeEvent(input$buttonhidemapFVExposureSelected, {
    hide("flamingoPanelmapFVExposureSelected")
  })

  onclick("abuttonmap", {
    show("flamingoPanelmapFVExposureSelected")
  })

  # Export to .csv
  output$FVEdownloadexcel <- downloadHandler(
    filename = result$currentFile,
    content = function(file) {
      write.csv(result$tbl_fileData, file)}
  )

  observeEvent({input[["select_vbutton"]]},{
    idx <- as.numeric(strsplit(input$select_vbutton, "_")[[1]][2])
    showModal(FileContent)
    .enableButton(idx)
    session$sendCustomMessage(type = 'resetInputValue', message =  session$ns("select_vbutton"))
    # Extra info table
    output$dt_FVExposureSelected <- renderUI({.getDetailsFile(idx)})
    output$tableFVExposureStatisticInfo <- renderUI({
      column(12,
             p(paste0("Number of Rows", nrow(result$tbl_fileData))),
             p("Column names"),
             p(paste(names(result$tbl_fileData), sep = " "))
      )
    })
    # get data to show in modal table
    fileName <- file.path(result$filesListData[idx, filesListData.path], result$filesListData[idx, filesListData.fileName])
    tryCatch({
      result$tbl_fileData <- read.csv(fileName, header = TRUE, sep = ",",
                                  quote = "\"", dec = ".", fill = TRUE, comment.char = "")
    }, error = function(e) {
      flamingoNotification(type = "error",
                           paste("Could not read file:", e$message))
      result$tbl_fileData <- NULL
    }) # end try catch

    if (!is.null(result$tbl_fileData)) {
      output$plainmap <- renderLeaflet({createPlainMap(fileName)})
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

  # Check permission row by row and show buttons
  .enableButton <- function(i) {
    FVid <- result$filesListData[i, filesListData.fileID]
    validButtons <- executeDbQuery(dbSettings,
                                   buildDbQuery("TellOperationsValidOnFileID", FVid))
    manageButtons <- c("FO_btn_show_raw_content" = "abuttonview",
                       "FO_btn_show_map" = "abuttonmap")
    # lapply(t(validButtons), function(btnIDs){enable(manageButtons[btnIDs])})
    lapply(t(validButtons), function(btnIDs){show(manageButtons[btnIDs])})
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

  .getDetailsFile <- function(idx){
    str1 <- paste("File Name: ", result$filesListData[idx, filesListData.fileName])
    str2 <- paste("Resource Key ", result$filesListData[idx, filesListData.key])
    HTML(paste(str1, str2, sep = '<br/>'))
  }

  # Module Output --------------------------------------------------------------
  invisible()

}

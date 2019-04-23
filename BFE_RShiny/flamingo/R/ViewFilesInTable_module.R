# View Files in table Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' ViewFilesInTableUI
#'
#' @rdname ViewFilesInTable
#'
#' @description UI/View to view  files.
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
#' @template params-logMessage
#'
#' @param tbl_filesListData dataframe of output files.
#' @param param id to be used
#' @param file_column name of the column containing filename. Default "files"
#' @param folderpath  path to files. Can be "_output/output/" or "_inputs/"; default output path.
#' @param includechkbox logical indicating the presence of checkboxes. Default FALSE.
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs hidden
#' @importFrom shinyjs disable
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
#' @importFrom data.table fwrite
#' @importFrom data.table fread
#' @importFrom utils count.fields
#' @importFrom utils zip
#'
#' @export
ViewFilesInTable <- function(input, output, session,
                             logMessage = message,
                             tbl_filesListData,
                             param = NULL,
                             file_column = "files",
                             folderpath = "_outputs/output",
                             includechkbox = FALSE) {

  ns <- session$ns


  # Reactive values & parameters -----------------------------------------------

  maxrowsperpage <- 10

  currfolder <- getOption("flamingo.settings.api.share_filepath")

  result <- reactiveValues(
    #df to show in table
    tbl_filesListData_wButtons = NULL,
    #View output file content
    currentFile = NULL,
    # Filepath of file to view
    currfilepath = NULL,
    #content of curr file
    tbl_fileData = NULL
  )

  # Add buttons ----------------------------------------------------------------
  observeEvent(tbl_filesListData(), ignoreNULL = FALSE, {
    filesListData <- tbl_filesListData()
    if (length(filesListData) > 0) {
      names(filesListData) <- tolower(names(filesListData))
      if (includechkbox) {
        filesListData <- cbind(data.frame(selected = .shinyInput(flamingoCheckboxButton,"srows_", nrow(filesListData), Label = NULL,
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
      filesListData <- cbind(filesListData,data.frame(view = .shinyInput(actionButton, "vrows_", nrow(filesListData), Label = "View", hidden = TRUE, onclick = paste0('Shiny.onInputChange(\"',ns("select_vbutton"),'\",  this.id)'), onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
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
        result$tbl_filesListData_wButtons %>% capitalize_names_df(),
        class = "flamingo-table display",
        rownames = TRUE,
        escape = FALSE,
        selection =  selectionUsed,
        colnames = c('Row Number' = 1),
        options = getTableOptions(maxrowsperpage = maxrowsperpage, escape = FALSE)
      )
    } else {
      nothingToShowTable("nothing to show")
    }
  )

  # Download zip Files ----------------------------------------------------------

  # Download zip button
  output$FLdownloadzip <- downloadHandler(
    filename = "files.zip",
    content = function(fname) {
      # path of files to download in Zip bundle
      fs <- c()
      g <- input$dt_outputFL_rows_selected
      for (f in g) {
        filename <- result$tbl_filesListData_wButtons[f, file_column]

        # Get dataframe
        currNamespace <- ls("package:flamingo")
        func_wpattern <- currNamespace[grepl(filename, currNamespace)]
        returnfunc <- func_wpattern[grepl("api_get", func_wpattern)]
        if (length(returnfunc) != 0) {
          func <- get(returnfunc)
          fileData <- return_file_df(func, param())
        } else {
          extractFolder <- set_extractFolder(id = param(), label = folderpath)
          currfilepath <- set_extractFilePath(extractFolder, filename)
          fileData <- fread(currfilepath)
        }

        if (nrow(fileData) > 0) {
          fpath <- file.path(currfolder, filename)
          fwrite(x = fileData, file = fpath, row.names = TRUE, quote = TRUE)
          fs <- c(fs, fpath)
        }
      }
      zip(zipfile = fname, files = fs)
      if (file.exists(paste0(fname, currfolder))) file.rename(paste0(fname, ".zip"), fname)
    }
  )

  # Download to csv ------------------------------------------------------------
  # Export to .csv
  output$FLdownloadexcel <- downloadHandler(
    filename = "file.csv",
    content = function(file) {
      fwrite(result$tbl_filesListData_wButtons, file, row.names = TRUE, quote = TRUE)}
  )

  # Selected Row ---------------------------------------------------------------
  observeEvent(input$dt_outputFL_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (length(input$dt_outputFL_rows_selected) > 0) {
      enable("FLdownloadzip")
      lapply(input$dt_outputFL_rows_selected, function(i) {
        session$sendCustomMessage(type = 'resetcolorOasis', message =  session$ns( paste0("srows_", i)))
        show(paste0("vrows_", i))
        if (!is.null(result$tbl_filesListData_wButtons[i, "name"]) && result$tbl_filesListData_wButtons[i, "name"] == "Not Available") {
          disable(paste0("vrows_", i))
        }
      })
      lapply(setdiff(input$dt_outputFL_rows_current, input$dt_outputFL_rows_selected), function(i) {
        session$sendCustomMessage(type = 'resetcolorWhite', message = session$ns(paste0("srows_", i)))
        hide(paste0("vrows_", i))})
    }else {
      disable("FLdownloadzip")
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
        h4("File Summary", class = "flamingo-table-title")),
      fluidRow(
        htmlOutput(ns("FVExposureStatisticInfo"))),
      br(),
      fluidRow(
        flamingoButton(inputId = ns("abuttonview"), label = "Content", icon = icon("file")),
        hidden(flamingoButton(inputId = ns("abuttonmap"), label = "Map", icon = icon("map"))),
        downloadButton(ns("FVEdownloadexcel"), label = "Export to csv"),
        style = "display: inline"),

      hidden(flamingoPanel(
        id = ns("flamingoPanelFVExposureSelected"),
        collapsible = FALSE,
        heading =  tagAppendChildren(
          h4("File content"),
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
        result$tbl_fileData %>% capitalize_names_df(),
        class = "flamingo-table display",
        rownames = TRUE,
        selection = "none",
        filter = 'bottom',
        colnames = c("Row Number" = 1),
        width = "100%",
        options = getTableOptions()
      )
    } else {
      nothingToShowTable("nothing to show")
    }
  )

  # Export to .csv
  output$FVEdownloadexcel <- downloadHandler(
    filename = function(){result$currentFile},
    content = function(file) {
      fwrite(result$tbl_fileData, file, row.names = TRUE, quote = TRUE)}
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
    result$currentFile <- result$tbl_filesListData_wButtons[idx, file_column] %>% as.character()
    currNamespace <- ls("package:flamingo")
    func_wpattern <- currNamespace[grepl(result$currentFile, currNamespace)]
    returnfunc <- func_wpattern[grepl("api_get",func_wpattern)]
    filerows <- NULL
    filecolumns <- NULL
    if (length(returnfunc) != 0) {
      func <- get(returnfunc)
      result$tbl_fileData <- return_file_df(func,param())
      if (!is.null(result$tbl_fileData)) {
        names(result$tbl_fileData) <- tolower(names(result$tbl_fileData))
        filecolumns <- paste(names(result$tbl_fileData), collapse = ", ")
        filerows <- nrow(result$tbl_fileData)
      }
    } else {
      extractFolder <- set_extractFolder(id = param(), label = folderpath)
      result$currfilepath <- set_extractFilePath(extractFolder, result$currentFile)
      if (dir.exists(result$currfilepath)) {
        result$tbl_fileData <- list.files(result$currfilepath, recursive = TRUE) %>%
          as.data.frame() %>%
          setNames("files")
        filecolumns <- ""
        filerows <- length(result$tbl_fileData)
        hide("abuttonmap")
        output$FVExposureStatisticInfo <- renderUI({
          tagList(
            fluidRow(
              column(2,
                     h5("Dir Name: ")
              ),
              column(10,
                     p(result$currentFile, style = "margin-top: 10px;")
              )
            ),
            fluidRow(
              column(2,
                     h5("Number of Files ")
              ),
              column(10,
                     p(filerows, style = "margin-top: 10px;")
              )
            )
          )
        })
      } else {
        result$tbl_fileData <- fread(result$currfilepath)
        if (!is.null(result$tbl_fileData)) {
          names(result$tbl_fileData) <- tolower(names(result$tbl_fileData))
        }
        filecolumns <- paste(tolower(unlist(strsplit(readLines(result$currfilepath, n = 1), ","))), collapse = ", ")
        filerows <- length(count.fields(result$currfilepath, skip = 1))

        #Show buttons
        if ("latitude" %in% names(result$tbl_fileData)) {
          if (!is.null(result$tbl_fileData)) {
            output$plainmap <- renderLeaflet({createPlainMap(result$tbl_fileData)})
          }
          show("abuttonmap")
        } else {
          hide("abuttonmap")
        }
        # Extra info table
        output$FVExposureStatisticInfo <- renderUI({
          tagList(
            fluidRow(
              column(2,
                     h5("File Name: ")
              ),
              column(10,
                     p(result$currentFile, style = "margin-top: 10px;")
              )
            ),
            fluidRow(
              column(2,
                     h5("Number of Rows ")
              ),
              column(10,
                     p(filerows, style = "margin-top: 10px;")
              )
            ),
            fluidRow(
              column(2,
                     h5("Column names")
              ),
              column(10,
                     p(filecolumns, style = "margin-top: 10px;")
              )
            )
          )
        })
      }
    }
  })#end observeEvent


  # Helper functions -----------------------------------------------------------

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

  invisible()
}

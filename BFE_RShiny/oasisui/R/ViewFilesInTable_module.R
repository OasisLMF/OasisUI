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
ViewFilesInTableUI <- function(id, includechkbox = FALSE) {
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
        bs_embed_tooltip(title = file_Viewer_tooltips$FLdownloadzip, placement = "right")
    },
    if (!includechkbox) {
      downloadButton(ns("FLdownloadexcel"), label = "Export")
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
#' @importFrom utils tail
#' @importFrom jsonlite toJSON
#'
#' @export
ViewFilesInTable <- function(input, output, session,
                             tbl_filesListData,
                             param = NULL,
                             file_column = "files",
                             folderpath = "output",
                             includechkbox = FALSE) {

  ns <- session$ns


  # Reactive values & parameters -----------------------------------------------

  maxrowsperpage <- 10

  currfolder <- getOption("oasisui.settings.api.share_filepath")

  result <- reactiveValues(
    #df to show in table
    tbl_filesListData_wButtons = NULL,
    #View output file content csv
    currentFile = NULL,
    #View output file content parquet
    currentFileP = NULL,
    # Display file category in file name
    fielCategory = NULL,
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
        filesListData <- cbind(data.frame(selected = .shinyInput(oasisuiCheckboxButton,"srows_", nrow(filesListData), Label = NULL,
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
      filesListData <- cbind(filesListData,
                             data.frame(view = .shinyInput(actionButton,
                                                           "vrows_", nrow(filesListData),
                                                           Label = "View", hidden = TRUE,
                                                           onclick = paste0('Shiny.onInputChange(\"',ns("select_vbutton"),'\",  this.id)'),
                                                           onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
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
        class = "oasisui-table display",
        rownames = FALSE,
        escape = FALSE,
        selection =  selectionUsed,
        options = getTableOptions(maxrowsperpage = maxrowsperpage, escape = FALSE)
      )
    } else {
      nothingToShowTable("Nothing to show")
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
        filename <- result$tbl_filesListData_wButtons[f, file_column] %>% as.character()
        if (filename %in% c("location_file", "accounts_file", "reinsurance_info_file", "reinsurance_scope_file")) {
          fileData <- session$userData$data_hub$get_pf_dataset_content(id = param(), dataset_identifier = filename)
          filename <- paste0(filename, ".csv")
        } else {
          fileData <- session$userData$data_hub$get_ana_dataset_content(id = param(), dataset_identifier = filename, type = folderpath)
        }
        if (!is.null(fileData)) {
          fpath <- session$userData$data_hub$write_file(data = fileData, dataset_identifier = filename)
          fs <- c(fs, fpath)
        }

      }
      zip(zipfile = fname, files = fs)
      # if (file.exists(paste0(fname, currfolder))) file.rename(paste0(fname, ".zip"), fname)
    }
  )

  # Download file-- ------------------------------------------------------------
  # Export to .csv
  output$FLdownloadexcel <- downloadHandler(
    filename = "file.csv",
    content = function(file) {
      session$userData$data_hub$write_file(data = result$tbl_filesListData_wButton, dataset_identifier = filename,
                                           file_towrite = file)
    }
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
        hide(paste0("vrows_", i))
      })
    } else {
      disable("FLdownloadzip")
      lapply(input$dt_outputFL_rows_current, function(i) {
        session$sendCustomMessage(type = 'resetcolorWhite', message = session$ns(paste0("srows_", i)))
        hide(paste0("vrows_", i))
      })
    }
  })

  # Select All Functionality ---------------------------------------------------

  # If page in table is changed, update rows selection based on select all value
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
        lapply(input$dt_outputFL_rows_current, function(i) {
          if (input$chkboxselectall) {
            session$sendCustomMessage(type = 'resetcolorOasis', message = session$ns(paste0("srows_", i)))
          } else {
            session$sendCustomMessage(type = 'resetcolorWhite', message = session$ns(paste0("srows_", i)))
          }
        })
        selectRows(dataTableProxy("dt_outputFL"), input$dt_outputFL_rows_current)
      } else {
        lapply(input$dt_outputFL_rows_current, function(i) {
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
        h4("File Summary", class = "oasisui-table-title")),
      fluidRow(
        htmlOutput(ns("FVExposureStatisticInfo"))),
      br(),
      fluidRow(
        oasisuiButton(inputId = ns("abuttonview"), label = "Content", icon = icon("file")),
        hidden(oasisuiButton(inputId = ns("abuttonmap"), label = "Map", icon = icon("map"))),
        downloadButton(ns("FVEdownloadexcel"), label = "Export"),
        downloadButton(ns("FVEdownloadparquet"), label = "Export Parquet"),
        style = "display: inline"),

      hidden(oasisuiPanel(
        id = ns("oasisuiPanelFVExposureSelected"),
        collapsible = FALSE,
        heading =  tagAppendChildren(
          h4("File content"),
          actionButton(inputId = ns("abuttonhidedFVExposureSelected"), label = NULL, icon = icon("times"), style = "float: right;")),
        uiOutput(ns("FVExposureSelected")))),

      hidden(oasisuiPanel(
        id = ns("oasisuiPanelmapFVExposureSelected"),
        collapsible = FALSE,
        heading = tagAppendChildren(
          h4("Map"),
          actionButton(inputId = ns("abuttonhidemapFVExposureSelected"), label = NULL, icon = icon("times"), style = "float: right;")),
        leafletOutput(ns("plainmap"))))
    )
  )

  # Panel View Content
  observeEvent(input$abuttonhidedFVExposureSelected, {
    hide("oasisuiPanelFVExposureSelected")
  })

  observeEvent(input$abuttonview, {
    show("oasisuiPanelFVExposureSelected")
  })

  # Exposure table
  output$FVExposureSelected <- renderUI({
    extension <-  strsplit(result$currentFile, split = "\\.") %>% unlist() %>% tail(n = 1)
    if (extension == "csv") {
      DTOutput(ns("dt_FVExposureSelected"))
    } else if (extension == "json") {
      verbatimTextOutput(ns("json_FVExposureSelected"))
    } else {
      textOutput(ns("text_FVExposureSelected"))
    }
  })

  output$json_FVExposureSelected <- renderText({
    toJSON(result$tbl_fileData, pretty = TRUE)
  })

  output$text_FVExposureSelected <- renderText({
    result$tbl_fileData
  })

  output$dt_FVExposureSelected <- renderDT(
    if (!is.null(result$tbl_fileData )) {
      datatable(
        result$tbl_fileData %>% capitalize_names_df() %>% as.data.frame(),
        class = "oasisui-table display",
        rownames = FALSE,
        selection = "none",
        filter = 'bottom',
        width = "100%",
        options = getTableOptions()
      )
    } else {
      nothingToShowTable("Nothing to show")
    }
  )

  # Export to .csv
  output$FVEdownloadexcel <- downloadHandler(
    filename = function(){result$currentFile},
    content = function(file) {
      session$userData$data_hub$write_file(data = result$tbl_fileData, dataset_identifier = result$currentFile, file_towrite = file)
    }
  )

  output$FVEdownloadparquet <- downloadHandler(
    filename = function(){result$currentFileP},
    content = function(file) {
      session$userData$data_hub$write_parquet_file(data = result$tbl_fileData, dataset_identifier = result$currentFileP,
                                              file_towrite = file)
    }
  )

  # Panel Map
  observeEvent(input$abuttonhidemapFVExposureSelected, {
    hide("oasisuiPanelmapFVExposureSelected")
  })

  observeEvent(input$abuttonmap, {
    show("oasisuiPanelmapFVExposureSelected")
  })

  observeEvent({input[["select_vbutton"]]}, {
    splitidx <- strsplit(input[["select_vbutton"]], "_")
    idx <- as.numeric(splitidx[[1]][length(splitidx[[1]])])
    showModal(FileContent)
    session$sendCustomMessage(type = 'resetInputValue', message =  session$ns("select_vbutton"))

    # Get dataframe
    result$currentFile <- result$tbl_filesListData_wButtons[idx, file_column] %>% as.character()
    result$fielCategory <- result$currentFile
    if (result$currentFile %in% c("location_file", "accounts_file", "reinsurance_info_file", "reinsurance_scope_file")) {
      result$tbl_fileData <- session$userData$data_hub$get_pf_dataset_content(id = param(), dataset_identifier = result$currentFile)
      if (!is.null(result$tbl_fileData)) {
        filecolumns <- session$userData$data_hub$get_pf_dataset_header(id = param(), dataset_identifier = result$currentFile)
        filerows <- session$userData$data_hub$get_pf_dataset_nrow(id = param(), dataset_identifier = result$currentFile)
        result$currentFileP <- paste0(result$currentFile, ".parquet")
        result$currentFile <- paste0(result$currentFile, ".csv")
        # Show buttons
        if ("latitude" %in% tolower(names(result$tbl_fileData)) && !is.null(result$tbl_fileData)) {
          names(result$tbl_fileData) <- tolower(names(result$tbl_fileData))
          output$plainmap <- renderLeaflet({
            createPlainMap(result$tbl_fileData, session = session, paramID = param(), step = NULL)
          })
          show("abuttonmap")
        } else {
          hide("abuttonmap")
        }
      }
    } else {
      result$tbl_fileData <- session$userData$data_hub$get_ana_dataset_content(id = param(), dataset_identifier = result$currentFile, type = folderpath)
      if (!is.null(result$tbl_fileData)) {
        filecolumns <- session$userData$data_hub$get_ana_dataset_header(id = param(), dataset_identifier = result$currentFile, type = folderpath)
        filerows <- session$userData$data_hub$get_ana_dataset_nrow(id = param(), dataset_identifier = result$currentFile, type = folderpath)
      }
      # Show buttons
      if ("latitude" %in% tolower(names(result$tbl_fileData))) {
        if (!is.null(result$tbl_fileData)) {
          output$plainmap <- renderLeaflet({
            createPlainMap(result$tbl_fileData, session = session, paramID = param(), step = NULL)
          })
        }
        show("abuttonmap")
      } else {
        hide("abuttonmap")
      }
    }

    # Extra info table
    output$FVExposureStatisticInfo <- renderUI({
      tagList(
        fluidRow(
          column(2,
                 h5("File Name")
          ),
          column(10,
                 p(result$fielCategory, style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          column(2,
                 h5("Number of Rows")
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
                 p(paste0(filecolumns, collapse = ", "), style = "margin-top: 10px;")
          )
        )
      )
    })
  }) # end observeEvent


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

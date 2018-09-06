# browseprogrammes Module -----------------------------
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater}; 
#' @param runIdList list of runs and their status
#' @param preselRunId reactive string expression for reselected run id from  landingpage
#' @param processRunId reactive string expression for reselected run id from  defineProgramme
#' @return list of reactives:
#' @rdname browseprogrammes
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr mutate select contains filter
#' @export
browseprogrammes <- function(input, output, session, dbSettings,
                             apiSettings, userId, 
                             runIdList = reactive(c(-1)),
                             preselRunId = reactive(-1),
                             processRunId = reactive(-1),
                             active = reactive(TRUE), logMessage = message,
                             reloadMillis = 10000) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ------------------------------------------
  
  navigation_state <- reactiveNavigation()
  
  # list of sub-modules 
  sub_modules <- list()
  
  result <- reactiveValues(
    #Reactive to know if one of the preselected runIds has changed
    RunIDchanged = -2,
    # preselRunId()
    preselRunId = -1,
    # processRunId()
    processRunId = -1,
    #selected run
    preselectedRunId = NULL,
    selectedRunId = NULL,
    # output files table
    filesListData = NULL
  )
  
  #number of plot output panels 
  n_panels <- 5
  
  
  # Run identification -----------------------------------------------------
  
  #Define reactive value to react if any of the preselected run Ids changes
  observe({
    preselRunId()
    processRunId()
    if (is.null(processRunId())) {
      result$processRunId <- -1
    } else {
      result$processRunId <- processRunId()
    }
    if (is.null(preselRunId())) {
      result$preselRunId <- -1
    } else {
      result$preselRunId <- preselRunId()
    }
    result$RunIDchanged <- result$preselRunId + result$processRunId 
  })
  
  #Update selected runID
  observe({
    result$RunIDchanged
    if (result$RunIDchanged == -2 ) {
      result$preselectedRunId = runIdList()$RunID[1]
    } else {
      if (result$preselRunId != -1) {
        result$preselectedRunId = isolate(result$preselRunId)
      }
      if (result$processRunId != -1) {
        result$preselectedRunId = isolate(result$processRunId)
      }
    }
  })
  
  #Update list of options
  observeEvent(result$preselectedRunId, {
    index <- match(c(result$preselectedRunId), runIdList()$RunID)
    if (!is.null(index) & !is.na(index)) {
      updateSelectInput(session, inputId = "selectRunID", choices = runIdList()$RunID, selected = runIdList()$RunID[index])
    }
  })
  
  # Go to Configure Output button ------------------------------------------
  observeEvent(input$abuttongotoconfig, {
    updateNavigation(navigation_state, "PS")
    result$preselPanel <- structure(panelsProgrammeWorkflow[4], count = input$abuttongotoconfig) 
    print(paste0("result$preselPanel is ", result$preselPanel))
  })
  
  # Summary Table ----------------------------------------------------------
  
  # collapse panel
  observeEvent(input$abuttonhidesummarytable, {
    num <- input$abuttonhidesummarytable
    if ((num %% 2) != 0 ) {
      updateActionButton(session = session, inputId = "abuttonhidesummarytable", label = NULL, icon = icon("expand"))
      hide("outputsummarytable")
    } else {
      updateActionButton(session = session, inputId = "abuttonhidesummarytable", label = NULL, icon = icon("minus"))
      show("outputsummarytable")
    }
  })


  observeEvent(input$selectRunID, {
    result$selectedRunId <- input$selectRunID
    if (!is.null(result$selectedRunId)) {
      sub_modules$panelSummaryTableModule <- callModule(
        panelSummaryTableModule,
        id = "panelSummaryTableModule",
        selectRunID = reactive(result$selectedRunId),
        dbSettings = dbSettings,
        apiSettings = apiSettings, 
        userId = userId, 
        logMessage = logMessage)
    }
  })
  

  # Extract Output files for given runID------------------------------------
   observeEvent( input$selectRunID, {if (input$selectRunID != "") {
    if (!is.null(runIdList())) {
      index <- match(c(input$selectRunID), runIdList()$RunID)
      status <- runIdList()[index, "Status"]
      if (!is.na(status)) {
        if (status == StatusCompleted) {
          result$filesListData <- getFileList(dbSettings, input$selectRunID)
          result$filesListData <- cbind(result$filesListData,do.call(rbind.data.frame,  lapply(result$filesListData$Description, .splitDescription)))        }
        }
      } 
  }
  })
  
  sub_modules$panelViewOutputFilesModule <- callModule(
    panelViewOutputFilesModule,
    id = "panelViewOutputFilesModule",
    filesListData =  reactive(result$filesListData),
    logMessage = logMessage)
  
  
 # panelOutputModule module -----------------------------------------------------
  
  #incremental panels
  panel_names <- paste0("flamingoIncrementalPanelpanelOutput-", c(seq_len(n_panels)))
  content_IDs <- paste0("flamingoIncrementalPanelpanelOutputcontent-", seq_len(n_panels))
  callIncrementalPanelModules(
    panel_names, "flamingoIncrementalPanelpanelOutput-0", content_IDs,
    panelOutputModuleUI, collapsible = TRUE, show = TRUE,
    ns = ns
  )
  
  
  # content modules
  plotsubmodules <- list()
  plotsubmodules <- lapply(content_IDs, callModule, module = panelOutputModule, 
                           filesListData =  reactive(result$filesListData))
  observeModuleNavigation(navigation_state, plotsubmodules, logger = NULL)

  # Helper functions --------------------------------------------------------
  
  #table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE), 
      processing = 0,
      scrollX = TRUE,
      pageLength = 10,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    return(options)
  }
  
  
  #function to split the description field of result$filesListData
  .splitDescription <- function(x){
    y <- unlist(strsplit(x,split =" ")) 
    z <- data.frame("Granularity" = y[2], "Losstype" = y[4], "Variable" = paste(y[5:length(y)], collapse = " "), stringsAsFactors = FALSE)
    return(z)}

  
  # Module Outout ------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      preselPanel = reactive(result$preselPanel)
    )
  ) 
}



# panelViewOutputFilesModule Module -----------------------
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param filesListData table of output files for a given runID
#' @return list of reactives:
#' @rdname panelViewOutputFilesModule
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr mutate select contains filter
#' @export
panelViewOutputFilesModule <- function(input, output, session, logMessage = message, filesListData ) {
  
  ns <- session$ns
  
  
  # Reactive values & parameters --------------------------------------------
  
  result <- reactiveValues(
    #current selected row
    currentrows = 0,
    #View output file content
    currentFile = NULL,
    fileData = NULL
  )
  
  observeEvent(filesListData(), {
    result$filesListData <- filesListData()
    if (nrow(result$filesListData) > 0) {
      result$filesListData <- cbind(result$filesListData, data.frame(View = .shinyInput(actionButton, "vrows_", nrow(result$filesListData), Label = "View", hidden = TRUE, onmousedown = 'event.preventDefault(); event.stopPropagation(); return false;')))
    }
  })
  

  output$outputfilestable <- renderDataTable(
    if (!is.null(result$filesListData)) {
      filesListDataFiltered <- result$filesListData %>% select(-contains("Location")) %>% select(-c("Variable", "Granularity", "Losstype")) 
      datatable(
        filesListDataFiltered,
        class = "flamingo-table display",
        rownames = TRUE,
        escape = FALSE,
        selection = "single",
        colnames = c('Row Number' = 1),
        options = .getFLTableOptions()
      )
      
    })
  
  output$FLTdownloadexcel <- downloadHandler(
    filename = "outputfilestable.csv",
    content = function(file) {
      write.csv(result$filesListData, file)
    }
  )
  
  # File content view ---------------------------------------------------
  
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
  
  #identify selected rows
  observe({
    currentrows <- input$outputfilestable_rows_selected
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
        .enableButton(i)})
      lapply(setdiff(input$outputfilestable_rows_current, result$currentrows), function(i) {
        .hideButtons(i)})
    } else {
      lapply(input$outputfilestable_rows_current, function(i){
        .hideButtons(i)})
    }
  })
  
  
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
  
  
  #Show content in Modal
  observe({
    if (!is.null(result$filesListData)) {
      lapply(
        X = 1:nrow(result$filesListData),
        FUN = function(i){
          observeEvent(input[[paste0("vrows_", i)]], {
            if (input[[paste0("vrows_", i)]] > 0) {
              result$currentFile <-  paste0(result$filesListData[i, 2])
              showModal(FileContent)
              # Extra info table
              output$tableFVExposureSelectedInfo <- renderUI({
                str1 <- paste("File Name: ", result$filesListData[i,2])
                str2 <- paste("Resource Key ", result$filesListData[i,10])
                HTML(paste(str1, str2, sep = '<br/>'))
              })
              # get data to show in modal table
              fileName <- file.path(result$filesListData[i, 5], result$filesListData[i, 2])
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
    manageButtons <- c("FO_btn_show_raw_content" = paste0("vrows_", i))
    # lapply(t(validButtons), function(btnIDs){enable(manageButtons[btnIDs])})
    lapply(t(validButtons), function(btnIDs){shinyjs::show(manageButtons[btnIDs])})
  }
  
  #hide buttons in a row
  .hideButtons <- function(i=NULL){
    shinyjs::hide(paste0("vrows_", i))
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


# panelOutputModule Module -----------------------
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param filesListData table of output files for a given runID
#' @return list of reactives:
#' @rdname panelOutputModule
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr mutate select contains filter
#' @export
panelOutputModule <- function(input, output, session, logMessage = message, filesListData ) {
  
  ns <- session$ns
  
  # Reactive values & parameters --------------------------------------------
  
  navigation_state <- reactiveNavigation()
  
  result <- reactiveValues(
    filesToPlot = NULL
  )
  
  sub_modules <- list()
  
  # Content of the panel ------------------ -------------------------------
  

  sub_modules$panelDefineDataToPlotModule <- callModule(
    panelDefineDataToPlotModule,
    id = "panelDefineDataToPlotModule",
    filesListData = filesListData,
    logMessage = logMessage)

  
  # Module Output -----------------------
invisible()
}


# panelDefineDataToPlotModule Module -----------------------
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param filesListData table of output files for a given runID
#' @return list of reactives:
#' @rdname panelDefineDataToPlotModule
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom dplyr rename left_join filter group_by summarise
#' @importFrom tidyr gather
#' @importFrom ggplot2 geom_line ggplot scale_color_manual labs theme aes element_text element_line element_blank 
#' @importFrom plotly ggplotly  renderPlotly
#' @export
panelDefineDataToPlotModule <- function(input, output, session, logMessage = message, filesListData) {
  
  ns <- session$ns
  
  # Reactive values & parameters --------------------------------------------
  
  result <- reactiveValues(
    #table of files to plot
    filesToPlot = NULL,
    #number of plots
    multipleplots = FALSE,
    #default plot title
    title = "",
    #DF to plot
    fileData = NULL,
    #vector identifying the type of plot
    plotstrc = NULL
  )
  

  # Enable / Disable options based on run ID -------------------------------

  observe({
    input$inputplottype
    if (!is.null(filesListData() )) {
      Granularities <- unique(filesListData()$Granularity) 
      Losstypes <- unique(filesListData()$Losstype) 
      Variables <- unique(filesListData()$Variable) 
      .reactiveUpdateGelectGroupInput(Losstypes, losstypes, "chkboxgrplosstypes", "Loss Types")
      .reactiveUpdateGelectGroupInput(Granularities, granularities, "chkboxgrpgranularities", "Granularities")
      .reactiveUpdateGelectGroupInput(Variables, variables, "chkboxgrpvariables", "Variables")
    }
  })
  
  # Enable / Disable options based on plot type ----------------------------
  
  observeEvent(input$inputplottype, {
    .reactiveUpdateGelectGroupInput(plottypeslist[[input$inputplottype]]$Variables, variables, "chkboxgrpvariables", "Variables")
  })

  # Extract dataframe to plot ----------------------------------------------
  
  #Logic to filter the files to plot
  #Missing logic in case either variables or granularities are not selected. For the moment not allowed
  observeEvent(input$abuttonloaddata, {
  result$fileData <- NULL
  # Plot parameters
  key <- plottypeslist[[input$inputplottype]]$keycols
  # construct DF of files to read
    #DF indicating structure of the plot
    result$plotstrc <- data.frame("Loss" = NULL, "Variable" = NULL, "Granularity" = NULL)
    # Not allowed to have no choices or multiple granularities in one plot
    if (length(input$chkboxgrplosstypes) == 0 | length(input$chkboxgrpgranularities) == 0 | length(input$chkboxgrpvariables) == 0 | length(input$chkboxgrpgranularities) > 1) {
      showNotification("Select the loss type(s), the variable(s) and the granularity to plot", type = "error")
    } else {
      if (length(input$chkboxgrplosstypes) > 1 ) {
        if (length(input$chkboxgrpvariables) > 1) {
          showNotification("With multiple loss types only one variable per plot is allowed", type = "error")
        } else {
          result$plotstrc <- data.frame("Loss" = c(2), "Variable" = c(1), "Granularity" = c(1))
          result$title <- paste0(key, " per ", input$chkboxgrpgranularities)
        }
      } else {
        result$title <- paste0(input$chkboxgrplosstypes, " per ", input$chkboxgrpgranularities)
        if (length(input$chkboxgrpvariables) > 1) {
            result$plotstrc <- data.frame("Loss" = c(1), "Variable" = c(length(input$chkboxgrpvariables)), "Granularity" = c(1))
        } else {
          result$plotstrc <- data.frame("Loss" = c(1), "Variable" = c(1), "Granularity" = c(1))
        }
      }
    }
    # get table of files to plot
    if (!is.null(filesListData()) & nrow(result$plotstrc) > 0 ) {
      result$filesToPlot <- filesListData()  %>% filter(Losstype %in% input$chkboxgrplosstypes, 
                                                        Variable %in% input$chkboxgrpvariables, 
                                                        Granularity %in% input$chkboxgrpgranularities) 
      if (nrow(result$filesToPlot) !=  prod(result$plotstrc)) {
        showNotification("The run did not produce the selected output. Please check the logs", type = "error")
        result$filesToPlot <- NULL
      }
    }

  # Read files to plot
    if (!is.null(result$filesToPlot)) {
      # Plot parameters
      suffix <- c("Losstype", "Variable", "Granularity" )
      key <- plottypeslist[[input$inputplottype]]$keycols
      x <- plottypeslist[[input$inputplottype]]$x
      colsToDrop <- plottypeslist[[input$inputplottype]]$extracols
      colsToPlot <- c("xaxis", "key", "value")
      #get data to show in modal table
      lapply(seq(nrow(result$filesToPlot)), function(i) {
        #read file
        # oasisBasePath <- "/home/mirai/Desktop/FV/R-projects/miscellaneous/oasis/data/FileManagement/oasis-run-58/"
        # fileName <- file.path(oasisBasePath, result$filesToPlot[i, 2])
        fileName <- file.path(result$filesToPlot[i, 5], result$filesToPlot[i, 2])
        fileData <- .readFile(fileName)
        logMessage(paste0("Reading file ", fileName))
        #replace names with standards
        if (any(which(result$plotstrc == 2))) {
          newname <- paste0(key, ".", result$filesToPlot[i, suffix[which(result$plotstrc == 2)]]) 
        } else {
          newname <- paste0(key, ".", result$filesToPlot[i, suffix[3]])
        }
        oldname <- plottypeslist[[input$inputplottype]]$keycols
        names(fileData)[names(fileData) == oldname] <- newname
        #Join data
        if (is.null(result$fileData)) {
          result$fileData <- fileData
        } else {
          bycol <- names(fileData)[ !grepl(key, names(fileData))]
          result$fileData <- left_join(result$fileData, fileData, by = bycol )
        }
      })
      nonkey <- names(result$fileData)[ !grepl(key, names(result$fileData))]
      #make ggplot friendly
      result$fileData <- result$fileData %>% gather( key = key, value = "value", -nonkey)
      names(result$fileData)[names(result$fileData) == x] <- "xaxis"
      if (input$chkboxaggregate) {
        result$fileData <- result$fileData %>%
          group_by(xaxis, key) %>%
          summarise(value = sum(value))
        result$title <- paste0("aggregated ", result$title)
        result$multipleplots <- FALSE
        names(result$fileData)[names(result$fileData) == "key"] <- "colour"
      } else {
        colsToGrid <- names(result$fileData)[!(names(result$fileData) %in% c(colsToPlot, colsToDrop))]
        names(result$fileData)[names(result$fileData) == colsToGrid] <- "gridCol" 
        if (any(result$plotstrc == 2)) {
          result$multipleplots <- TRUE
          names(result$fileData)[names(result$fileData) == "key"] <- "colour"
        } else {
          result$multipleplots <- FALSE
          names(result$fileData)[names(result$fileData) == "gridCol"] <- "colour"
        }
      }
      
      print(paste0("result$fileData is"))
      print(result$fileData)
    }
  })


  observeEvent(input$abuttondraw, {
    xlabel <- plottypeslist[[input$inputplottype]]$xlabel
    ylabel <- plottypeslist[[input$inputplottype]]$ylabel
    if (input$textinputtitle != "") {
      titleToUse <- toupper(input$textinputtitle) 
    } else {
      titleToUse <- toupper(result$title)
    }
    if (!is.null(result$fileData)) {
      p <- .plotDF(xlabel, ylabel, titleToUse, result$fileData, result$multipleplots)
      # https://github.com/rstudio/rstudio/issues/2919
      output$outputplot <- renderPlotly({ggplotly(p)})
    }
  })

  # Helper functions -------------------------
  
  .reactiveUpdateGelectGroupInput <- function(reactivelistvalues, listvalues, inputid, Label){
    if (!is.null(reactivelistvalues)) {
      updateCheckboxGroupInput(session = session, inputId = inputid, choices = listvalues, selected = reactivelistvalues, label = Label, inline = TRUE)
      # #extract newest selection by identifying which values are in the CheckboxGroupInput choices but not in the reactivelistvalues
      # # selector #browseprogrammes-panelOutputModule-chkboxgrpgranularities > div > label:nth-child(1) > input[type="checkbox"]
      # idxtoBeDisabled <- which(!(listvalues %in% reactivelistvalues))
      # idxtoBeEnabled <- which((listvalues %in% reactivelistvalues))
      # subElementDisabled <- paste0("#browseprogrammes-panelOutputModule-panelDefineDataToPlotModule-",inputid," .checkbox:nth-child( 1 ) label")
      # subElementEnabled <- paste0("#browseprogrammes-panelOutputModule-panelDefineDataToPlotModule-",inputid," .checkbox:nth-child(2) label")
      # subElementDisabled <- paste0("#browseprogrammes-panelOutputModule-panelDefineDataToPlotModule-",inputid," .checkbox:nth-child(", idxtoBeDisabled,") label")
      # subElementEnabled <- paste0("#browseprogrammes-panelOutputModule-panelDefineDataToPlotModule-",inputid," .checkbox:nth-child(", idxtoBeEnabled,") label")
      #disable single checkbox of subElementDisabled
      # shinyjs::disable(selector = subElementDisabled)
      # #enable single checkbox of subElementEnabled
      # shinyjs::enable(selector = subElementEnabled)
    } else {
      updateCheckboxGroupInput(session = session, inputId = inputid, choices = listvalues, selected = FALSE, label = Label , inline = TRUE )
      # subElementDisabled <- paste0("#browseprogrammes-panelOutputModule-panelDefineDataToPlotModule-",inputid," .checkbox:nth-child(", seq(1:length(listvalues)),") label")
      # shinyjs::disable(selector = subElementDisabled)
    }
  }
  
  #Helper function to plot DF
  #Expected DF with columns: 
  # xaxis : column for aes x
  # value : column for aes y
  # color : column for the aes col
  .plotDF <- function(xlabel, ylabel, titleToUse, data, multipleplots = FALSE){
      p <- ggplot(data, aes(x = xaxis, y = value, col = as.factor(colour))) +
        geom_line(size = 2) +
        # scale_color_manual(labels= c("GUL", "IL"), values = c("red", "blue")) +
        labs(title = titleToUse, x = xlabel, y = ylabel) +
        theme(
          plot.title = element_text(color = "grey45", size = 18, face = "bold.italic", hjust = 0.5),
          text = element_text(size = 18),
          panel.background = element_blank(),
          axis.line.x = element_line(color = "grey45", size = 0.5),
          axis.line.y = element_line(color = "grey45", size = 0.5),
          legend.title =  element_blank(),
          legend.position = "top"
        ) 
      if (multipleplots) {
        p <- p + facet_wrap(.~ gridCol)
      } 
      p
  }
  
  #Helper function to read one file from DB
  .readFile <- function(fileName){
    if (!is.na(fileName)) {
      tryCatch({
        fileData <- read.csv(fileName, header = TRUE, sep = ",",
                             quote = "\"", dec = ".", fill = TRUE, comment.char = "")
      }, error = function(e) {
        showNotification(type = "error",
                         paste("Could not read file:", e$message))
        fileData <- NULL
      }) 
    } else {
      showNotification(type = "error",
                       paste("File invalid"))
      fileData <- NULL
    }
    return(fileData)
  }
  
  # Module Output -----------------------
  invisible()
  
}

# panelSummaryTableModule Module -----------------------
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param selectRunID selected runID
#' @return null
#' @rdname panelSummaryTableModule
#' @importFrom DT renderDataTable datatable
#' @export
panelSummaryTableModule <- function(input, output, session, dbSettings,
                                    apiSettings, userId, logMessage = message, selectRunID ) {
  
  ns <- session$ns
  
  result <- reactiveValues(
    selectRunID = NULL,
    outputSummaryData = NULL
  )
  
  observe({
    result$selectRunID <- selectRunID()
  })
  
observe({
  output$outputsummarytable <- renderDataTable({
   outputSummaryData <- executeDbQuery(dbSettings,
                                        paste("exec getOutputSummary", result$selectRunID))
    if (!is.null(outputSummaryData)) {
      datatable(
        outputSummaryData,
        class = "flamingo-table display",
        rownames = TRUE,
        selection = "none",
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )      
    } else {
      datatable(
        data.frame(content = "nothing to show"),
        class = "flamingo-table display",
        rownames = FALSE,
        selection = "none",
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    }
    
  })
  
  
})
  # Helper functions --------------------------------------------------------
  
  #table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE), 
      processing = 0,
      scrollX = TRUE,
      pageLength = 10,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    return(options)
  }
  
  # Module Output -----------------------
  invisible()
}

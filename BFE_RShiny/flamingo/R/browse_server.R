# browseprogrammes Module -----------------------------
#' browseprogrammes Module
#' @description Server logic for viewing results of a single run
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater}
#' @param runIdList list of runs and their status
#' @param preselRunId reactive string expression for reselected run id from landingpage
#' @param processRunId reactive string expression for reselected run id from defineProgramme
#' @return list of reactives:
#' @rdname browseprogrammes
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable
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
    #Panel to select
    preselPanel = "1",
    # output files table
    filesListData = NULL
  )
  
  #number of plot output panels
  n_panels <- 5
  
  #clean value
  observeEvent(active(), {
    result$preselPanel <- "1"
  })
  
  
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
    result$preselPanel <- "4"
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
      if (!is.na(status) && status == StatusCompleted) {
        result$filesListData <- getFileList(dbSettings, input$selectRunID)
        result$filesListData <- cbind(result$filesListData,do.call(rbind.data.frame,  lapply(result$filesListData$Description, .splitDescription)))        
      } else {
        result$filesListData <- NULL
      }
    } else {
      result$filesListData <- NULL
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
  panel_names <- paste0("flamingoIncrementalPanelOutput-", c(seq_len(n_panels)))
  content_IDs <- paste0("flamingoIncrementalPanelOutputcontent-", seq_len(n_panels))
  plotPanels <- callIncrementalPanelModules(
    panel_names, "flamingoIncrementalPanelOutput-0", content_IDs,
    panelOutputModuleUI,
    headings = lapply(seq_len(n_panels), function(i) {flamingoPanelHeadingOutput(ns(paste0("paneltitle", i)))}),
    collapsible = TRUE, show = TRUE,
    ns = ns
  )
  plotsubmodules <- lapply(seq_along(content_IDs), function(i) {
    callModule(panelOutputModule, content_IDs[i],
               filesListData =  reactive(result$filesListData),
               active = reactive(plotPanels$state()[[i]]))
  })
  lapply(seq_along(plotsubmodules), function(i) {
    output[[paste0("paneltitle", i)]] <- renderflamingoPanelHeading(plotsubmodules[[i]]())
  })
  
  observeEvent(result$selectedRunId, {
    plotPanels$remove_all()
  })
  
  
  # content modules
  # observeModuleNavigation(navigation_state, plotsubmodules, logger = NULL)
  
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
    y <- unlist(strsplit(x,split = " "))
    z <- data.frame("Granularity" = y[2], "Losstype" = y[4], "Variable" = paste(y[5:length(y)], collapse = " "), stringsAsFactors = FALSE)
    return(z)}
  
  
  # Module Outout ------------------------------------------------------------
  
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      preselPanel = reactive({result$preselPanel})
    )
  )
  
  moduleOutput
}



# panelViewOutputFilesModule Module -----------------------
#' Module for the Panel to View Output Files
#' @description Server logic to view output files
#' @inheritParams flamingoModule
#' @param filesListData table of output files for a given runID
#' @return list of reactives:
#' @rdname panelViewOutputFilesModule
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom dplyr mutate select contains filter
#' @export
panelViewOutputFilesModule <- function(input, output, session, logMessage = message, filesListData) {
  
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
  
  
  output$outputfilestable <- renderDT(
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
  
  output$FLTdownloadexcel <- downloadHandler(
    filename = "outputfilestable.csv",
    content = function(file) {
      write.csv(result$filesListData, file)
    }
  )
  
  # File content view ---------------------------------------------------
  
  # Exposure table
  output$tableFVExposureSelected <- renderDT(
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
      DTOutput(ns("tableFVExposureSelected")),
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
#' Module for Output Panel
#' @description Server logic to show graphical output such as plots
#' @inheritParams flamingoModule
#' @param filesListData table of output files for a given runID
#' @return reactive value of the title
#' @rdname panelOutputModule
#' @importFrom shinyjs enable disable
#' @importFrom dplyr rename left_join filter group_by summarise intersect
#' @importFrom tidyr gather separate spread
#' @importFrom ggplot2 geom_line ggplot scale_color_manual labs theme aes element_text element_line element_blank  geom_point geom_area facet_wrap scale_x_continuous geom_bar geom_errorbar
#' @importFrom plotly ggplotly  renderPlotly
#' @export
panelOutputModule <- function(input, output, session, logMessage = message, filesListData, active) {
  
  ns <- session$ns
  
  # Reactive values & parameters --------------------------------------------
  
  result <- reactiveValues(
    #plot and panel title
    Title = "",
    Granularities = character(0),
    Losstypes = character(0),
    Variables = character(0)
  )
  
  # reactive values holding checkbox state
  chkbox <- list(
    chkboxgrplosstypes = reactiveVal(NULL),
    chkboxgrpvariables = reactiveVal(NULL),
    chkboxgrpgranularities = reactiveVal(NULL) 
  )
  
  lapply(names(isolate(chkbox)), function(id) {
    observe(chkbox[[id]](input[[id]]))
  })
  
  #reactive triggered by the existence of the input$plottype and the changes in the data. It hoplds the selected plottype
  inputplottype <- reactive(if (active()) {
    filesListData()
    input$inputplottype
  })

  
  #clean up panel objects when inactive
  observe(if (!active()) {
    result$Title <- ""
    result$Granularities <- character(0)
    result$Losstypes <- character(0)
    result$Variables <- character(0)
    # plotlyOutput persists to re-creating the UI
    output$outputplot <- renderPlotly(NULL)
    for (id in names(chkbox)) chkbox[[id]](NULL)
  })
  
  observeEvent(inputplottype(), {
    result$Title <- ""
    # plotlyOutput persists to re-creating the UI
    output$outputplot <- renderPlotly(NULL)
    if (length( plottypeslist[[inputplottype()]]$uncertaintycols) > 0) {
      show("chkboxuncertainty")
    } else {
      updateCheckboxInput(session = session, inputId = "chkboxuncertainty", value = FALSE)
      hide("chkboxuncertainty")
    }
  })
  
 
  # Enable / Disable options -------------------------------
  
  # > based on run ID ----
  #Gather the Granularities, Variables and Losstypes based on the runID output presets
   observe(if (active()) {
    if (!is.null(filesListData() )) {
      result$Granularities <- unique(filesListData()$Granularity)
      result$Losstypes <- unique(filesListData()$Losstype)
      result$Variables <- unique(filesListData()$Variable)
    } else {
      result$Granularities <- character(0)
      result$Losstypes <-  character(0)
      result$Variables <-  character(0)
    }
  })

  observeEvent({
    inputplottype()
    result$Losstypes
    result$Granularities
    result$Variables
  }, ignoreNULL = FALSE, {
    if (!is.null(inputplottype())) {
      .reactiveUpdateSelectGroupInput(result$Losstypes, losstypes, "chkboxgrplosstypes", inputplottype())
      .reactiveUpdateSelectGroupInput(result$Variables, variables, "chkboxgrpvariables", inputplottype())
      .reactiveUpdateSelectGroupInput(result$Granularities, granularities, "chkboxgrpgranularities", inputplottype())
      # Check length(result$Variables) != 0 necessary because this part is triggered two times if inputplottype changes, once when the reactives are cleared and once with the updated reactives
      if ( length(result$Variables) != 0 && length(intersect(result$Variables, plottypeslist[[inputplottype()]][["Variables"]])) == 0) {
        showNotification("No data available for this plot type", type = "error")
      }
    }
  })
  
  # > based on inputs ----

  #GUL does not have policy
  observeEvent(
    chkbox$chkboxgrplosstypes(),
    ignoreNULL = FALSE, {
        #if losstype = GUL then policy inactive
        if ( "GUL" %in% chkbox$chkboxgrplosstypes()) {
          Granularities <- result$Granularities[which(result$Granularities != "Policy")]
        } else {
          Granularities <- result$Granularities
        }
        .reactiveUpdateSelectGroupInput(Granularities, granularities, "chkboxgrpgranularities", inputplottype())
        .reactiveUpdateSelectGroupInput(result$Variables, variables, "chkboxgrpvariables", inputplottype())
    })

  # Extract dataframe to plot ----------------------------------------------
  
  #Logic to filter the files to plot
  #Missing logic in case either variables or granularities are not selected. For the moment not allowed
  observeEvent(input$abuttondraw, {
    
    # > print current selection
    logMessage(paste0("Plotting ", inputplottype(),
                      " for loss types: ", chkbox$chkboxgrplosstypes(),
                      ", variables: ", chkbox$chkboxgrpvariables(),
                      ", granularities: ",chkbox$chkboxgrpgranularities()
                      # ", aggregated to Portfolio Level: ", input$chkboxaggregate
    ))
    
    # > clear data
    # Content to plot
    fileData <- NULL
    # List of files to plot
    filesToPlot <- NULL
    # DF indicating structure of the plot
    plotstrc <- data.frame("Loss" = NULL, "Variable" = NULL, "Granularity" = NULL)
    # single plot or grid
    multipleplots = FALSE
    
    # > Plot parameters
    key <- plottypeslist[[inputplottype()]]$keycols[1]
    keycols <- plottypeslist[[inputplottype()]]$keycols
    x <- plottypeslist[[inputplottype()]]$x
    suffix <- c("Losstype", "Variable", "Granularity" )
    extracols <- plottypeslist[[inputplottype()]]$extracols
    xlabel <- plottypeslist[[inputplottype()]]$xlabel
    ylabel <- plottypeslist[[inputplottype()]]$ylabel
    uncertainty <- plottypeslist[[inputplottype()]]$uncertaintycols
    plottype <- plottypeslist[[inputplottype()]]$plottype
    
    # > sanity checks ----
    #If no data to plot show error
    if (length(chkbox$chkboxgrplosstypes()) == 0    | 
        length(chkbox$chkboxgrpvariables()) == 0    | 
        length(chkbox$chkboxgrpgranularities()) == 0) {
      showNotification("Select the loss type(s), the variable(s) and the granularity of the data to plot", type = "error")
    } else {
      #If data to plot
      #Only one granularity is allowed
      if (length(chkbox$chkboxgrpgranularities()) > 1) {
        showNotification("Select only one granularity to plot", type = "error")
      } else {
        # Max 2 variables are allowed
        if (length(chkbox$chkboxgrpvariables()) > 2) {
          showNotification("Select max two variables to plot", type = "error")
        } else {
          # If 2 loss types
          if (length(chkbox$chkboxgrplosstypes()) > 1) {
            #With 2 loss types only one variable is allowed
            if (length(chkbox$chkboxgrpvariables()) > 1) {
              showNotification("Select only one variable to plot", type = "error")
            } else {
              plotstrc <- data.frame("Loss" = c(2), "Variable" = c(1), "Granularity" = c(1))
              result$Title <- paste0(key, " per ", chkbox$chkboxgrpgranularities())
            } # One variable
          } else {
            # If one loss type
            # if 2 variables
            if (length(chkbox$chkboxgrpvariables()) > 1) {
              plotstrc <- data.frame("Loss" = c(1), "Variable" = c(2), "Granularity" = c(1))
              result$Title <- paste0(chkbox$chkboxgrplosstypes(), " per ", chkbox$chkboxgrpgranularities())
            } else {
              result$Title <- paste0(chkbox$chkboxgrpvariables(), " of ", chkbox$chkboxgrplosstypes(), "per ", chkbox$chkboxgrpgranularities())
              plotstrc <- data.frame("Loss" = c(1), "Variable" = c(1), "Granularity" = c(1)) 
            } # One variable
          }# One loss type
        } # Variables < 3
      } # Granularity is one
    } # End of sanity checksThere is data to plot
    
    
    # > filter out files ----
    if (!is.null(filesListData()) & nrow(plotstrc) > 0 ) {
      filesToPlot <- filesListData()  %>% filter(Losstype %in% chkbox$chkboxgrplosstypes(),
                                                 Variable %in% chkbox$chkboxgrpvariables(),
                                                 Granularity %in%  chkbox$chkboxgrpgranularities())
      if (nrow(filesToPlot) !=  prod(plotstrc)) {
        showNotification("The run did not produce the selected output. Please check the logs", type = "error")
        filesToPlot <- NULL
      }
    }
    
    if (!is.null(filesToPlot)) {
      # > read files to plot ---------
      for (i in seq(nrow(filesToPlot))) { # i<- 1
        fileName <- file.path(filesToPlot[i, 5], filesToPlot[i, 2])
        if (TRUE) {
          oasisBasePath <- "/home/mirai/Desktop/FV/R-projects/miscellaneous/oasis/data/FileManagement/oasis-run-58/"
          # oasisBasePath <- "~/GitHubProjects/miscellaneous/oasis/data/FileManagement/oasis-run-58/"
          fileName <- file.path(oasisBasePath, filesToPlot[i, 2])
        }
        currfileData <- .readFile(fileName)
        nonkey <- names(currfileData)[ !(names(currfileData) %in% keycols)]
        gridcol <- names(currfileData)[ !(names(currfileData) %in% keycols) & !(names(currfileData) %in% extracols) & !(names(currfileData) %in% x)]
        if (any(which(plotstrc == 2))) {
          extension <- filesToPlot[i, suffix[which(plotstrc == 2)]]
        } else {
          extension <- filesToPlot[i, suffix[3]]
        }
        for (k in keycols) {
          newnamekey <- paste0(k, ".", extension)
          names(currfileData)[names(currfileData) == k] <- newnamekey
        }
        #Join data
        if (is.null(fileData)) {
          fileData <- currfileData
        } else {
          fileData <- left_join(fileData, currfileData, by = nonkey )
        }
      }
    }
    
    if (!is.null(fileData)) {
      # > make ggplot friendly ------
      data <- fileData %>% gather(key = variables, value = value, -nonkey) %>% separate(variables, into = c("variables", "keyval"), sep = "\\.") %>% spread(variables, value)
      if (length(gridcol) > 0) {
        data <- data %>% rename("gridcol" = gridcol)
      }
      data <- data %>% rename("value" = key)
      data <- data %>% rename("xaxis" = x)
      if (length(uncertainty) > 0) {
        data <- data %>% rename("uncertainty" = uncertainty)
      }
      if (any(plotstrc == 2) & length(gridcol) > 0) {
        multipleplots <- TRUE
        data <- data %>% rename("colour" = keyval)
      } else {
        multipleplots <- FALSE
        if (length(gridcol) > 0) {
          data <- data %>% rename("colour" = gridcol)
        }  else {
          data <- data %>% rename("colour" = keyval)
        }
      }
      print("data")
      print(data)
      # > draw plot ----
      if (input$textinputtitle != "") {
        result$Title <- input$textinputtitle
      }
      if (!is.null(data)) {
        if (plottype == "line") {
          p <- .linePlotDF(xlabel, ylabel, toupper(result$Title), data,
                           multipleplots = multipleplots)
        } else if (plottype == "bar"){
          p <- .barPlotDF (xlabel, ylabel, toupper(result$Title), data, wuncertainty = input$chkboxuncertainty, multipleplots = multipleplots)
        }
        output$outputplot <- renderPlotly({ggplotly(p)})
      } else {
        showNotification("No data to plot", type = "error")
      }
    }
    
  })
  
  
  # Helper functions -------------------------
  
  .reactiveUpdateSelectGroupInput <- function(reactivelistvalues, listvalues, inputid, plotType) {
    # disable and untick variables that are not relevant
    if (inputid == "chkboxgrpvariables" && !is.null(plotType)) {
      relevantVariables <- plottypeslist[[plotType]][["Variables"]]
      selectable <- intersect(reactivelistvalues, relevantVariables)
    } else {
      selectable <- reactivelistvalues
    }
    selection <- intersect(selectable, chkbox[[inputid]]())
    updateCheckboxGroupInput(session = session, inputId = inputid, selected = FALSE)
    # N.B.: JavaScript array indices start at 0
    js$disableCheckboxes(checkboxGroupInputId = ns(inputid),
                         disableIdx = which(listvalues %in% setdiff(listvalues, selectable)) - 1)
    updateCheckboxGroupInput(session = session, inputId = inputid, selected = selection)
  }
  
  .enableDisableUponCondition <- function(ID, condition){
    if (condition ) {
      disable(id = ID)
    } else {
      enable(id = ID)
    }
  }
  
  
  #Helper function to read one file from DB
  .readFile <- function(fileName){
    if (!is.na(fileName)) {
      logMessage(paste0("Reading file ", fileName))
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
  
  #Helper functions to plot DF
  #Expected DF with columns:
  # xaxis : column for aes x
  # value : column for aes y
  # colour : column for the aes col
  # flag multipleplots generates grid over col gridcol
  
  .basicplot <- function(xlabel, ylabel, titleToUse, data){
    p <- ggplot(data, aes(x = xaxis, y = value, col = as.factor(colour))) +
      labs(title = titleToUse, x = xlabel, y = ylabel) +
      scale_x_continuous(breaks = data$xaxis, labels = data$xaxis) +
      theme(
        plot.title = element_text(color = "grey45", size = 18, face = "bold.italic", hjust = 0.5),
        text = element_text(size = 18),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey45", size = 0.5),
        axis.line.y = element_line(color = "grey45", size = 0.5),
        legend.title =  element_blank(),
        legend.position = "top"
      )
    p
  }
  
  .multiplot <- function(p, multipleplots = FALSE){
    if (multipleplots) {
      p <- p + facet_wrap(.~ gridcol)
    }
    p
  }
  
  #Line plot
  .linePlotDF <- function(xlabel, ylabel, titleToUse, data, multipleplots = FALSE){
    p <- .basicplot(xlabel, ylabel, titleToUse, data)
    p <- p +
      geom_line(size = 1) +
      geom_point(size = 2)
    p <- .multiplot(p, multipleplots)
    p
  }
  
  #Bar Plot
  .barPlotDF <- function(xlabel, ylabel, titleToUse, data, wuncertainty = FALSE, multipleplots = FALSE){
    p <- .basicplot(xlabel, ylabel, titleToUse, data)
    p <- p +
      geom_bar(position = position_dodge(), stat = "identity", aes(fill = as.factor(colour))) 
    if (wuncertainty){
      p <- p +
        geom_errorbar(aes(ymin = value - uncertainty, ymax = value + uncertainty),
                         size = .3,
                         width = .2,                    # Width of the error bars
                         position = position_dodge(.9)) 
    }
    p <- .multiplot(p,multipleplots)
    p
  }
  
  # Module Output -----------------------
  reactive(result$Title)
}

# panelSummaryTableModule Module -----------------------
#' Module for Summary Table Panel
#' @description Server logic to show the summary table output
#' @inheritParams flamingoModule
#' @param selectRunID selected runID
#' @return null
#' @rdname panelSummaryTableModule
#' @importFrom DT renderDT datatable
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
    output$outputsummarytable <- renderDT({
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

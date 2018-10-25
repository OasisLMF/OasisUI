# outputplots Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title outputplots_ui
#' Run outputplots UI
#' @rdname summaryUI
#' @description Output plots of a Run
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' @export
outputplotsUI <- function(id) {
  
  ns <- NS(id)
  
  flamingoIncrementalPanelUI(
    id = ns("flamingoIncrementalPanelOutput-0"),
    heading = "New Plot",
    collapsible = FALSE, show = FALSE, removable = FALSE)
  
}

# UI Functions -----------------------------------------------------------------

#' @title panelOutputModuleUI
#' @rdname panelOutputModuleUI
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel
#' @importFrom shinyjs hidden
#' @importFrom plotly plotlyOutput
#' @export
panelOutputModuleUI <-  function(id){
  ns <- NS(id)
  tagList(
    flamingoPanel(
      id = ns("flamingoPanelOutputModule"),
      collapsible = TRUE,
      heading = "Custom Plot",
      h4("Data to plot"),
      column(12,
             div( class = "InlineSelectInput",
                  selectInput(inputId = ns("inputplottype"), label = "Select a plot type", choices = names(plottypeslist), selected = names(plottypeslist)[1]))
      ),
      br(),
      column(4,
             checkboxGroupInput(inputId = ns("chkboxgrplosstypes"), label = "Perspective", choices = losstypes, inline = TRUE)),
      column(8,
             checkboxGroupInput(inputId = ns("chkboxgrpgranularities"), label = "Summary Level", choices = granularities, inline = TRUE)),
      br(),
      column(12,
             checkboxGroupInput(inputId = ns("chkboxgrpvariables"), label = "Report", choices = variables, inline = TRUE)),
      br(),
      h4("Customize Plot"),
      column(4,
             div(class = "InlineTextInput",
                 textInput(ns("textinputtitle"), "Title", ""))),
      column(4,
             hidden(checkboxInput(ns("chkboxuncertainty"), "Include Uncertainty", FALSE))),
      flamingoButton(inputId = ns("abuttondraw"), label = "Draw Plot",  style = "float:right")
    ),
    
    panel(
      # heading = h4("Plot"),
      plotlyOutput(ns("outputplot"))
    )
  )
}

# Server -----------------------------------------------------------------------

#' @title outputplots_server
#' Run outputplots Server
#' @rdname outputplots
#' @description outputplots of a Run
#' @inheritParams flamingoModule
#' @return list of tags
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable
#' @importFrom dplyr mutate select contains filter
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom ggplot2 geom_line geom_hline ggplot scale_color_manual labs theme aes element_text element_line element_blank geom_point geom_area facet_wrap scale_x_continuous geom_bar geom_errorbar
#' @export
#' @export
outputplots <- function(input, output, session, dbSettings,
                    apiSettings, userId,
                    selectRunID, 
                    n_panels,
                    filesListData = reactive(NULL),
                    active, logMessage = message) {
  
  ns <- session$ns
  
  # list of sub-modules
  sub_modules <- list()
  
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
               filesListData =  reactive(filesListData()),
               active = reactive(plotPanels$state()[[i]]))
  })
  lapply(seq_along(plotsubmodules), function(i) {
    output[[paste0("paneltitle", i)]] <- renderflamingoPanelHeading(plotsubmodules[[i]]())
  })
  
  observeEvent(selectRunID(), {
    plotPanels$remove_all()
  })
  
}

# panelOutputModule Module -----------------------
#' Module for Output Panel
#' @rdname panelOutputModule
#' @description Server logic to show graphical output such as plots
#' @inheritParams flamingoModule
#' @param filesListData table of output files for a given runID
#' @return reactive value of the title
#' @importFrom shinyjs enable disable
#' @importFrom dplyr rename left_join filter group_by summarise intersect
#' @importFrom tidyr gather separate spread
#' @importFrom ggplot2 geom_line geom_hline ggplot scale_color_manual labs theme aes element_text element_line element_blank  geom_point geom_area facet_wrap scale_x_continuous geom_bar geom_errorbar
#' @importFrom plotly ggplotly  renderPlotly
#' @export
panelOutputModule <- function(input, output, session, logMessage = message, filesListData = reactive(NULL), active) {
  
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
        flamingoNotification("No data available for this plot type", type = "error")
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
    key <- plottypeslist[[inputplottype()]]$keycols
    uncertainty <- plottypeslist[[inputplottype()]]$uncertaintycols
    reference <- plottypeslist[[inputplottype()]]$referencecols
    keycols <- c(key, uncertainty, reference)
    x <- plottypeslist[[inputplottype()]]$x
    xtickslabels <- plottypeslist[[inputplottype()]]$xtickslabels
    suffix <- c("Losstype", "Variable", "Granularity" )
    extracols <- plottypeslist[[inputplottype()]]$extracols
    xlabel <- plottypeslist[[inputplottype()]]$xlabel
    ylabel <- plottypeslist[[inputplottype()]]$ylabel
    plottype <- plottypeslist[[inputplottype()]]$plottype
    
    # > sanity checks ----
    #If no data to plot show error
    if (length(chkbox$chkboxgrplosstypes()) == 0    |
        length(chkbox$chkboxgrpvariables()) == 0    |
        length(chkbox$chkboxgrpgranularities()) == 0) {
      flamingoNotification("Select the loss type(s), the variable(s) and the granularity of the data to plot", type = "error")
    } else {
      #If data to plot
      #Only one granularity is allowed
      if (length(chkbox$chkboxgrpgranularities()) > 1) {
        flamingoNotification("Select only one granularity to plot", type = "error")
      } else {
        # Max 2 variables are allowed
        if (length(chkbox$chkboxgrpvariables()) > 2) {
          flamingoNotification("Select max two variables to plot", type = "error")
        } else {
          # If 2 loss types
          if (length(chkbox$chkboxgrplosstypes()) > 1) {
            #With 2 loss types only one variable is allowed
            if (length(chkbox$chkboxgrpvariables()) > 1) {
              flamingoNotification("Select only one variable to plot", type = "error")
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
        flamingoNotification("The run did not produce the selected output. Please check the logs", type = "error")
        filesToPlot <- NULL
      }
    }
    
    if (!is.null(filesToPlot)) {
      # > read files to plot ---------
      for (i in seq(nrow(filesToPlot))) { # i<- 1
        fileName <- file.path(filesToPlot[i, 5], filesToPlot[i, 2])
        # if (TRUE) {
        #   oasisBasePath <- "/home/mirai/Desktop/FV/R-projects/miscellaneous/oasis/data/FileManagement/oasis-run-58/"
        #   # oasisBasePath <- "~/GitHubProjects/miscellaneous/oasis/data/FileManagement/oasis-run-58/"
        #   fileName <- file.path(oasisBasePath, filesToPlot[i, 2])
        # }
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
      if (length(reference) > 0) {
        data <- data %>% rename("reference" = reference)
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
      # print("data")
      # print(data)
      # > draw plot ----
      if (input$textinputtitle != "") {
        result$Title <- input$textinputtitle
      }
      if (!is.null(data)) {
        if (plottype == "line") {
          p <- .linePlotDF(xlabel, ylabel, toupper(result$Title), data,
                           multipleplots = multipleplots)
        } else if (plottype == "bar"){
          p <- .barPlotDF (xlabel, ylabel, toupper(result$Title), data, wuncertainty = input$chkboxuncertainty, multipleplots = multipleplots, xtickslabels = xtickslabels)
        }
        output$outputplot <- renderPlotly({ggplotly(p)})
      } else {
        flamingoNotification("No data to plot", type = "error")
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
        flamingoNotification(type = "error",
                             paste("Could not read file:", e$message))
        fileData <- NULL
      })
    } else {
      flamingoNotification(type = "error",
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
  
  .addRefLine <- function(p, reference){
    if (!is.null(reference)) {
      p <- p + geom_hline(yintercept = reference)
    }
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
      geom_point(size = 2) +
      p <- .multiplot(p, multipleplots)
    p
  }
  
  #Bar Plot
  .barPlotDF <- function(xlabel, ylabel, titleToUse, data, wuncertainty = FALSE, multipleplots = FALSE, xtickslabels = NULL ){
    p <- .basicplot(xlabel, ylabel, titleToUse, data)
    p <- p +
      geom_bar(position = "dodge", stat = "identity", aes(fill = as.factor(colour))) +
      scale_x_continuous(breaks = data$xaxis, labels = xtickslabels[1:length(data$xaxis)])
    if (wuncertainty){
      p <- p +
        geom_errorbar(aes(ymin = value - uncertainty, ymax = value + uncertainty),
                      size = .3,
                      width = .2,                    # Width of the error bars
                      position = position_dodge(.9))
    }
    # p <- .addRefLine(p, unique(data$reference))
    p <- .multiplot(p,multipleplots)
    p
  }
  
  # Module Output -----------------------
  reactive(result$Title)
}

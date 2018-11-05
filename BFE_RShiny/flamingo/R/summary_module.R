# Summary Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title summarytab_ui
#' 
#' @rdname summarytab
#' 
#' @description UI/View of summary elements of a run.
#' 
#' @template params-module-ui
#' 
#' @return list of tags
#' 
#' @importFrom plotly plotlyOutput
#' 
#' @export
summarytabUI <- function(id) {
  
  ns <- NS(id)
  
  flamingoPanel(
    id = ns("flamingoPanelSummaryTable"),
    collapsible = FALSE,
    heading =  tagAppendChildren(
      h4("Summary Table")
    ),
    
    fluidRow(
      column(6,
             div(
               id = ns("summarypanel"),
               h5("Inputs"),
               flamingoTableUI(ns("summaryInputTable")),
               h5("Parameters"),
               flamingoTableUI(ns("summaryParamsTable")),
               h5("Output"),
               flamingoTableUI(ns("summaryOutputTable"))
             )
      ),
      column(6,
             plotlyOutput(ns("summaryOutputPlot"))
      )
    )
  )
  
}

# Server -----------------------------------------------------------------------

#' Run Summary Server
#' @title summarytab_server
#' 
#' @rdname summarytab
#' 
#' @description Server logic of summary elements of a run.
#' 
#' @template params-module
#' @template params-flamingo-module
#' 
#' @return list of tags
#' 
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom dplyr filter
#' @importFrom plotly ggplotly 
#' @importFrom plotly renderPlotly 
#' 
#' @export
summarytab <- function(input, output, session, dbSettings,
                       apiSettings, userId,
                       selectRunID = reactive(""), active, logMessage = message) {
  
  ns <- session$ns
  
  # Reactive Values & Params ---------------------------------------------------
  result <- reactiveValues(
    inputSummaryData = NULL,
    paramSummaryData = NULL,
    outputSummaryData = NULL
  )
  
  # list of sub-modules
  sub_modules <- list()
  
  # strings to filer
  outputStrg <- c("portfolio")
  paramStrg <- c("treshold|number|peril|set")
  
  # Extract Summary Data -------------------------------------------------------
  observeEvent({
    selectRunID()
    active()
   }, {
    if (!is.null(selectRunID()) && selectRunID() != "") {
      SummaryData <- executeDbQuery(dbSettings,
                                    paste("exec getOutputSummary", selectRunID()))
      if (!is.null(SummaryData)) {
        result$inputSummaryData <- SummaryData %>% 
          filter(!grepl(paste0(outputStrg, "|", paramStrg), SummaryType)) %>%
          as.data.frame()
        result$paramSummaryData <- SummaryData %>% 
          filter(grepl(paramStrg, SummaryType)) %>%
          as.data.frame()
        result$outputSummaryData <- SummaryData %>% 
          filter(grepl(outputStrg, SummaryType)) %>%
          as.data.frame()
      }
      show("summarypanel")
      show("summaryOutputPlot")
    } else {
      hide("summarypanel")
      hide("summaryOutputPlot")
    }
  })
  
  
  # Summary Input tables
  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryInputTable",
    data = reactive(result$inputSummaryData),
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE,
    colnames = FALSE,
    maxrowsperpage = 10,
    logMessage = logMessage)
  
  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryParamsTable",
    data = reactive(result$paramSummaryData),
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE,
    colnames = FALSE,
    maxrowsperpage = 10,
    logMessage = logMessage)
  
  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryOutputTable",
    data = reactive(result$outputSummaryData),
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    rownames = FALSE,
    colnames = FALSE,
    maxrowsperpage = 10,
    logMessage = logMessage)
  
  
  # Plot -----------------------------------------------------------------------
  output$summaryOutputPlot <- renderPlotly({
    if (!is.null(result$outputSummaryData)) {
      data <- result$outputSummaryData
      data <- cbind(result$outputSummaryData,do.call(rbind.data.frame,  lapply(result$outputSummaryData[,1], function(i){
        x <- as.character(i)
        y <- unlist(strsplit(x,split = " "))
        z <- data.frame("Loss Type" = y[2], "Sample Type" = y[4], stringsAsFactors = FALSE)
        return(z)
      })))
      names(data) <- c("description", "value", "colour", "xaxis")
      xlabel <- "Sample Type"
      ylabel <- "Loss"
      titleToUse <- "AAL"
      p <- barPlot(xlabel, ylabel, titleToUse, data)
      ggplotly(p)
    }
  })
  
}

# Plot functions ---------------------------------------------------------------

#' basicplot structure
#' @title basicplot
#' 
#' @rdname basicplot
#' 
#' @description basic plot
#' 
#' @importFrom ggplot2 ggplot 
#' @importFrom ggplot2 labs 
#' @importFrom ggplot2 theme 
#' @importFrom ggplot2 aes 
#' @importFrom ggplot2 element_text 
#' @importFrom ggplot2 element_line 
#' @importFrom ggplot2 element_blank

#' @export
#' 
# Expected DF with columns:
# xaxis : column for aes x
# value : column for aes y
# colour : column for the aes col
# flag multipleplots generates grid over col gridcol

basicplot <- function(xlabel, ylabel, titleToUse, data){
  p <- ggplot(data, aes(x = xaxis, y = value)) +
    labs(title = titleToUse, x = xlabel, y = ylabel) +
    theme(
      plot.title = element_text(color = "grey45", size = 14, face = "bold.italic", hjust = 0.5),
      text = element_text(size = 12),
      panel.background = element_blank(),
      axis.line.x = element_line(color = "grey45", size = 0.5),
      axis.line.y = element_line(color = "grey45", size = 0.5),
      legend.title =  element_blank(),
      legend.position = "right"
    )
  p
}

#' barPlot structure
#' @title barPlot
#' 
#' @rdname barPlot
#' 
#' @description bar plot
#' 
#' @importFrom ggplot2 aes 
#' @importFrom ggplot2 scale_x_continuous 
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_bar 
#' 
#' @export
# Expected DF with columns:
# xaxis : column for aes x
# value : column for aes y
# colour : column for the aes col
# flag multipleplots generates grid over col gridcol

barPlot <- function(xlabel, ylabel, titleToUse, data){
  p <- basicplot(xlabel, ylabel, titleToUse, data) +
    geom_bar(position = "dodge", stat = "identity", aes(fill = as.factor(data$colour)))
  #scale_x_continuous(breaks = data$xaxis, labels = xtickslabels[1:length(data$xaxis)])
  p
}
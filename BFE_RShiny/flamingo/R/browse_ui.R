#' Browse programmes UI
#' @rdname browseprogrammesUI
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets sliderTextInput
#' @export
browseprogrammesUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    
    h4("Browse Processes outputs", class = "flamingo-page-title") ,
    
    div(id = ns("panelDefineRunID"), panelDefineRunID(id)),
    div(id = ns("panelSummaryTable"), panelSummaryTable(id)),
    div(id = ns("panelOutput"), panelOutput(id)),
    div(id = ns("panelViewOutputFiles"), panelViewOutputFiles(id))
  )
}


# Functions for UI Panels ------------------------------------------------------------------------------  

#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyBS bsTooltip
#' @export
panelDefineRunID <-  function(id){
  ns <- NS(id)
  
  panel(
    status = "primary",
    fluidRow(
      column(6,
             selectInput(inputId =  ns("selectRunID"), label = "Run ID", choices = "", selected = NULL),
             bsTooltip(ns("selectRunID"), 
                       browse_programmes$selectRunID, 
                       placement = "right", 
                       options   = list(container = "body"))),
      column(6,
             align = "right",
             actionButton(inputId = ns("abuttongotoconfig"), label = "Go to Configure Output",  class = "btn btn-primary"))
    )
  )
}


#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @export
panelSummaryTable <-  function(id){
  ns <- NS(id)
  flamingoPanel(
    id = ns("flamingoPanelpanelSummaryTable"),
    collapsible = TRUE,
    heading = "Process Run Summary Table",
    div( id = ns("outputsummarytable"),
    panelSummaryTableModuleUI(ns( "panelSummaryTableModule"))
    )
  )
}

#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @export
panelSummaryTableModuleUI <-  function(id){
  ns <- NS(id)
  
  tagList(
    dataTableOutput(ns("outputsummarytable"))
  )
}


#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @export
panelOutput <-  function(id){
  ns <- NS(id)
  
  flamingoIncrementalPanelUI(
    id = ns("flamingoIncrementalPanelpanelOutput-0"),
    heading = "Process Run Output Plot",
    collapsible = FALSE, show = FALSE, removable = FALSE)
}


#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyBS bsTooltip
#' @export
panelOutputModuleUI <-  function(id){
  ns <- NS(id)
  
  tagList(
    panelDefineDataToPlotModuleUI(ns("panelDefineDataToPlotModule"))
  )
}




#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom plotly plotlyOutput
#' @export
panelDefineDataToPlotModuleUI <-  function(id){
  ns <- NS(id)
  tagList(
    # Define data to plot
  fluidRow(
    column(12,
           panel(
             heading = fluidRow(column(10, h4("Define Data to Plot"))),
             fluidRow(
               column(4,
                      div( id = ns("inputplottype"), selectInput(inputId = ns("inputplottype"), label = "Select a plot type", choices = names(plottypeslist), selected = names(plottypeslist)[1]))),
               column(8, 
                      checkboxInput(ns("chkboxaggregate"), "Aggregate by granularity", TRUE))),
             br(),
             fluidRow(
               column(4,
                      checkboxGroupInput(inputId = ns("chkboxgrplosstypes"), label = "Loss Types", choices = losstypes, inline = TRUE)),
               column(8,
                      checkboxGroupInput(inputId = ns("chkboxgrpgranularities"), label = "Granularities", choices = granularities, inline = TRUE))),
             br(),
             checkboxGroupInput(inputId = ns("chkboxgrpvariables"), label = "Variables", choices = variables, inline = TRUE),
             br(),
             fluidRow(
               column(6,
                      actionButton(inputId = ns("abuttonloaddata"), label = "Load Data",  class = "btn btn-primary")))
           ))),
  fluidRow(
    # Customize plot
    column(3,
           panel(
             heading = fluidRow(column(10, h4("Customize Plot"))),
             textInput(ns("textinputtitle"), "Title", ""),
             actionButton(inputId = ns("abuttondraw"), label = "Draw Plot",  class = "btn btn-primary"))),
    # Plot 
    column(9,
           panel(
             heading = fluidRow(column(10, h4("Plot"))),
             fluidRow(
               column(12,
                      plotlyOutput(ns("outputplot"))
                      # plotOutput(ns("outputplot"))
                      )),
             fluidRow(
               column(12,
                      stile = "float:right;",
                      downloadButton(ns("BRdownloadplot"),
                                     label = "Download plot")))
           )))
  )
}


#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @export
panelViewOutputFiles <-  function(id){
  ns <- NS(id)
  
  panel(
    heading = fluidRow(column(10, h4("Process Run Output Files Table")),
                       column(2, align = "right",  actionButton(inputId = ns("abuttonhidefiles"), label = NULL, icon = icon("minus")))),
    panelViewOutputFilesModuleUI(ns("panelViewOutputFilesModule"))
  )
}

#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @export
panelViewOutputFilesModuleUI <-  function(id){
  ns <- NS(id)
  
  tagList(
    dataTableOutput(ns("outputfilestable")),
    downloadButton(ns("FLTdownloadexcel"), label = "Export to csv")
  )
}
#' Browse programmes UI
#' @rdname browseprogrammesUI
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT dataTableOutput
#' @export
browseprogrammesUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    
    h4("Browse Processes outputs", class = "flamingo-page-title") ,
    panelDefineRunID(id),
    panelSummaryTable(id),
    panelOutput(id),
    panelViewOutputFiles(id)
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
    id = ns("flamingoPanelSummaryTable"),
    collapsible = TRUE,
    heading = "Process Run Summary Table",
    panelSummaryTableModuleUI(ns( "panelSummaryTableModule"))
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
    id = ns("flamingoIncrementalPanelOutput-0"),
    # heading = "Process Run Output Custom Plot",
    collapsible = FALSE, show = FALSE, removable = FALSE)
}


#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
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
      br(),
      div( id = ns("inputplottype"), class = "InlineSelectInput", 
           selectInput(inputId = ns("inputplottype"), label = "Select a plot type", choices = names(plottypeslist), selected = names(plottypeslist)[1])),
      br(),
      column(4,
             checkboxGroupInput(inputId = ns("chkboxgrplosstypes"), label = "Loss Types", choices = losstypes, inline = TRUE)),
      column(8,
             checkboxGroupInput(inputId = ns("chkboxgrpgranularities"), label = "Granularities", choices = granularities, inline = TRUE)),
      br(),
      checkboxGroupInput(inputId = ns("chkboxgrpvariables"), label = "Variables", choices = variables, inline = TRUE),
      br(),
      br(),
      h4("Customize Plot"),
      column(4,
             textInput(ns("textinputtitle"), "Title", "")), 
      column(4,
             checkboxInput(ns("chkboxaggregate"), "Aggregate by granularity", TRUE)),
      column(4,
             checkboxInput(ns("chkboxcumulate"), "Cumulate plot", FALSE)),
      actionButton(inputId = ns("abuttondraw"), label = "Draw Plot",  class = "btn btn-primary", style = "float:right")
    ),
    
    panel(
      # heading = h4("Plot"),
      plotlyOutput(ns("outputplot"))
    )
  )
}


#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @export
panelViewOutputFiles <-  function(id){
  ns <- NS(id)
  flamingoPanel(
    id = ns("flamingoPanelViewOutputFiles"),
    collapsible = TRUE,
    heading = "Process Run Output Files Table",
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
# visualization Single Run Browse Module UI ------------------------------------

#' visualizationSBRUI
#' 
#' @rdname visualizationSBR
#' 
#' @description UI/View for viewing results of a single run.
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#' 
#' @export
visualizationSBRUI <- function(id) {
  
  ns <- NS(id)
  tagList(

    panelDefineRunID(id),
    panelSummaryTable(id),
    panelOutput(id),
    panelViewOutputFiles(id)
  )
}


# Functions for UI Panels ------------------------------------------------------
#' panelDefineRunID
#' 
#' @rdname panelDefineRunID
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' 
#' @export
panelDefineRunID <-  function(id){
  ns <- NS(id)
  panel(
    status = "primary",
    fluidRow(
      column(6,
             selectInput(inputId =  ns("selectRunID"), label = "Run ID", choices = "", selected = NULL) %>%
               bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right")),
      column(6,
             align = "right",
             flamingoButton(inputId = ns("abuttongotoconfig"), label = "Go to Configure Output"))
    )
  )
}

#' panelSummaryTable
#' 
#' @rdname panelSummaryTable
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @export
panelSummaryTable <-  function(id){
  ns <- NS(id)
  flamingoPanel(
    id = ns("flamingoPanelSummaryTable"),
    collapsible = TRUE,
    heading = "Summary Table",
    panelSummaryTableModuleUI(ns( "panelSummaryTableModule"))
  )
}

#' panelSummaryTableModuleUI
#' @rdname panelSummaryTableModuleUI
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @importFrom DT DTOutput
#' 
#' @export
panelSummaryTableModuleUI <-  function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("outputsummarytable"))
  )
}

#' panelOutput
#' 
#' @rdname panelOutput
#' 
#' @details Function wrapping panel to define prgramme and model IDs
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @export
panelOutput <-  function(id){
  ns <- NS(id)
  flamingoIncrementalPanelUI(
    id = ns("flamingoIncrementalPanelOutput-0"),
    heading = "New Plot",
    collapsible = FALSE, show = FALSE, removable = FALSE)
}

#' panelOutputModuleUI
#' 
#' @rdname panelOutputModuleUI
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#' @importFrom shinyjs hidden
#' @importFrom plotly plotlyOutput
#' 
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

#' panelViewOutputFiles
#' 
#' @rdname panelViewOutputFiles
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @export
panelViewOutputFiles <-  function(id){
  ns <- NS(id)
  flamingoPanel(
    id = ns("flamingoPanelViewOutputFiles"),
    collapsible = TRUE,
    heading = "Files Table",
    ViewFilesModuleUI(id  = ns("ViewFilesModule"), includechkbox = FALSE)
  )
}


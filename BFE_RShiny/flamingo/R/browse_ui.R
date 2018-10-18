#' @title browse_ui
#' Browse programmes UI
#' @rdname browseprogrammesUI
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @export
browseprogrammesUI <- function(id) {

  ns <- NS(id)
  tagList(

    # h4("Browse Run Outputs", class = "flamingo-page-title") ,
    panelDefineRunID(id),
    panelSummaryTable(id),
    panelOutput(id),
    panelViewOutputFiles(id)
  )
}


# Functions for UI Panels ------------------------------------------------------------------------------
#' @title panelDefineRunID
#' @rdname panelDefineRunID
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
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

#' @title panelSummaryTable
#' @rdname panelSummaryTable
#' @inheritParams flamingoModuleUI
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

#' @title panelSummaryTableModuleUI
#' @rdname panelSummaryTableModuleUI
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
panelSummaryTableModuleUI <-  function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("outputsummarytable"))
  )
}

#' @title panelSummaryTableModuleUI
#' @rdname panelOutput
#' @details Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @export
panelOutput <-  function(id){
  ns <- NS(id)
  flamingoIncrementalPanelUI(
    id = ns("flamingoIncrementalPanelOutput-0"),
    heading = "New Plot",
    collapsible = FALSE, show = FALSE, removable = FALSE)
}

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

#' @title panelViewOutputFiles
#' @rdname panelViewOutputFiles
#' @inheritParams flamingoModuleUI
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


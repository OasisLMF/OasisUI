# Single Analysis Definition Module UI ------------------------------------------

#' singleAnaUI
#'
#' @rdname singleAna
#'
#' @description UI/Viewt o define an analysis
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
singleAnaUI <- function(id) {

  ns <- NS(id)

  tagList(
    singleAnaWorkflowStepsUI(ns("workflowsteps")),
    hidden(div(id = ns("panelDefineIDs"), panelDefineIDs(id))),
    step1_choosePortfolioUI(ns("step1_choosePortfolio")),
    step2_chooseAnalysisUI(ns("step2_chooseAnalysis")),
    step3_configureOutputUI(ns("step3_configureOutput"))
  )
}


#' panelDefineIDs
#'
#' @rdname panelDefineIDs
#'
#' @description Function wrapping panel to define prgramme and model IDs.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom shinyWidgets panel
#' @importFrom shinyjs hidden
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelDefineIDs <- function(id) {
  ns <- NS(id)

  panel(
    status = "primary",
    fluidRow(
      div(id = ns("divportfolioID"),
          column(12,
                 div(class = "InlineSelectInputSmall",
                     selectizeInput(inputId = ns("portfolioID"), label = "Portfolio ID",
                                    choices = c(),
                                    selected = character(0),
                                    options = list(
                                      allowEmptyOption = TRUE,
                                      placeholder = 'Select',
                                      onInitialize = I('function() { this.setValue(""); }'))
                     )  %>%
                       bs_embed_tooltip(title = defineSingleAna_tooltips$portfolioID,
                                        placement = "right")
                 )
          )
      )
    )
  )
}

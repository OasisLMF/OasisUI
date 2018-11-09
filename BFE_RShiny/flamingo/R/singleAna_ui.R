# Single Analysis Definition Module UI ------------------------------------------

#' singleAnaUI
#'
#' @rdname singleAna
#'
#' @description UI/Viewt o define an analysis
#'
#' @template params-module-ui
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
  step1_chooseProgrammeUI(ns("step1_chooseProgramme")),
  step2_chooseModelUI(ns("step2_chooseModel")),
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
      div(id = ns("divselectprogrammeID"),
          column(4,
                 div(class = "InlineSelectInputSmall",
                     selectizeInput(inputId = ns("selectprogrammeID"), label = "Programme ID",
                                    choices = c(),
                                    selected = character(0),
                                    options = list(
                                      allowEmptyOption = TRUE,
                                      placeholder = 'Select',
                                      onInitialize = I('function() { this.setValue(""); }'))
                     )  %>%
                       bs_embed_tooltip(title = programme_Definition_Single$selectprogrammeID,
                                        placement = "right")
                 ))),
      hidden(div(id = ns("divselectprogOasisID"),
                 column(5,
                        div(class = "InlineSelectInputSmall",
                            selectizeInput(inputId = ns("selectprogOasisID"), label = "Oasis Programme ID",
                                           choices = c(),
                                           selected = character(0),
                                           options = list(
                                             allowEmptyOption = TRUE,
                                             placeholder = 'Select',
                                             onInitialize = I('function() { this.setValue(""); }'))
                            ) %>%
                              bs_embed_tooltip(title = programme_Definition_Single$selectprogOasisID,
                                               placement = "right")
                        ))))
    )
  )
}

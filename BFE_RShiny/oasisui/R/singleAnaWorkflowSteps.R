### Panels Programme Workflow -----
# N.B.: checkbox values need to have type = "character", otherwise jQuery errors
# are thrown when programmatically setting choices
# analysisWorkflowSteps <- list("Choose Portfolio" = "1",
#                               "Choose Analysis"" = "2",
#                               "Configure Output & Run" = "4")

#' analysisWorkflowSteps
#'
#' @rdname analysisWorkflowSteps
#'
#' @return List.
#'
#' @export
analysisWorkflowSteps <- list("Choose Portfolio" = "1",
                               "Choose Analysis" = "2",
                               "Configure Output & Run" = "3")

#' singleAnaWorkflowStepsUI
#'
#' @rdname singleAnaWorkflowSteps
#'
#' @return List of tags.
#'
#' @importFrom shinyWidgets radioGroupButtons
#'
#' @export
singleAnaWorkflowStepsUI <- function(id) {
  ns <- NS(id)
  tagList(
    # h4("Process Definition Steps"),
    # br(),
    radioGroupButtons(
      inputId = ns("radiobuttons"), label = NULL,
      choices = analysisWorkflowSteps,
      justified = TRUE, individual = TRUE, status = "primary",
      checkIcon = list(yes = icon("triangle-right", lib = "glyphicon"), no = NULL)
    )
  )
}

#' singleAnaWorkflowSteps
#'
#' @rdname singleAnaWorkflowSteps
#'
#' @template params-module
#'
#' @return smth.
#'
#' @importFrom shinyWidgets updateRadioGroupButtons
#' @importFrom shinyjs js
#'
#' @export
singleAnaWorkflowSteps <- function(input, output, session) {
  observeEvent(input$radiobuttons, {
    # update steps colors
    js$updateStepColors(radioButtonsId = session$ns("radiobuttons"))
  })
  list(step = reactive({input$radiobuttons}),
       update = function(step) {
         updateRadioGroupButtons(session, inputId = "radiobuttons", selected = step)
       })
}

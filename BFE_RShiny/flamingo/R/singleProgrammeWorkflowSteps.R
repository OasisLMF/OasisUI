### Panels Programme Workflow -----
# N.B.: checkbox values need to have type = "character", otherwise jQuery errors
# are thrown when programmatically setting choices
programmeWorkflowSteps <- list("Create Programme" = "1",
                               "Select Programme & Associate Model" = "2",
                               "Configure Workflow Output" = "3",
                               "Browse & re-run" = "4")

#' @importFrom shinyWidgets radioGroupButtons
singleProgrammeWorkflowStepsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Process Definition Steps"),
    br(),
    radioGroupButtons(
      inputId = ns("radiobuttons"), label = NULL,
      choices = programmeWorkflowSteps,
      justified = TRUE, individual = TRUE, status = "primary",
      checkIcon = list(yes = icon("triangle-right", lib = "glyphicon"), no = NULL)
    )
  )
}

#' @importFrom shinyWidgets updateRadioGroupButtons
singleProgrammeWorkflowSteps <- function(input, output, session) {
  observeEvent(input$radiobuttons, {
    # update steps colors
    js$updateStepColors(radioButtonsId = session$ns("radiobuttons"))
  })
  list(step = reactive({input$radiobuttons}),
       update = function(step) {
         updateRadioGroupButtons(session, inputId = "radiobuttons", selected = step)
       })
}

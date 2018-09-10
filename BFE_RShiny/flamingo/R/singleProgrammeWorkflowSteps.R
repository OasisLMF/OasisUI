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
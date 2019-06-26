# Analysis inputs Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' anainputsUI
#' @rdname anainputs
#'
#' @description UI/View for inputs of an analysis.
#'
#' @param heading Panel title.
#' @param collapsible Option to collapse panel, Default FALSE.
#' @param show Show panel. Default FALSE.
#'
#' @return List of tags.
#'
#' @export
anainputsUI <- function(id,
                        heading = NULL,
                        collapsible = FALSE,
                        show = FALSE) {

  ns <- NS(id)

  tagList(
    flamingoPanel(
      heading = heading,
      collapsible = collapsible,
      show = show,
      selection = list(mode = 'none'),
      escape = FALSE,
      filter = "none",
      rownames = TRUE,
      id = ns("panel_anainputs"),
      div(id = ns("refresh_ana"), flamingoRefreshButton(ns("abuttonanainputrefresh"))),
      ViewFilesInTableUI(id  = ns("ViewIGFiles"), includechkbox = TRUE)
    )
  )
}

# Server -----------------------------------------------------------------------

#' anainputs
#'
#' @rdname anainputs
#'
#' @description  Server logic for inputs of an analysis.
#'
#' @template params-module
#' @template params-active
#' @param analysisID Selected analysis ID.
#' @param refresh_opt Option to hide/show refresh button.
#' @param counter Reactive value storing actionButton status.
#'
#' @importFrom tibble add_column
#' @importFrom shinyjs onclick
#'
#' @export
anainputs <- function(input,
                      output,
                      session,
                      analysisID,
                      refresh_opt = TRUE,
                      counter = reactive(NULL),
                      active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    dt_generated = NULL,
    counter = 0
  )

  observeEvent( input[["panel_anainputs-collapse-button"]], ignoreNULL = FALSE, {
    logMessage(paste0("changing result$counter to ", result$counter, " because input[['panel_anainputs-collapse-button']] chanhed to ",  input[["panel_anainputs-collapse-button"]]))
    result$counter <- input[["panel_anainputs-collapse-button"]]
  })

  observeEvent(counter(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    logMessage(paste0("changing result$counter to ", result$counter, " because counter() chanhed to ", counter()))
    if (is.null(input[["panel_anainputs-collapse-button"]])) {
      result$counter <- -1
    } else {
      result$counter <- counter()
    }
  })

  # Create table ---------------------------------------------------------------
  observeEvent({
    active()
    result$counter
    analysisID()
  }, ignoreInit = TRUE, {
    print(paste0("result$counter ", result$counter))
    if (length(active()) > 0 && active() && !is.null(analysisID()) && !is.na(result$counter) &&  result$counter != "" &&  result$counter != 0 && !is.null(result$counter)) {
      if (!refresh_opt) {
        hide("refresh_ana")
      }
      .reloadInputs()
    }
  })

  callModule(
    ViewFilesInTable,
    id = "ViewIGFiles",
    tbl_filesListData = reactive({result$dt_generated}),
    param = analysisID,
    file_column = "files",
    folderpath = "input",
    includechkbox = TRUE)

  # reload Generated Inputs table-----------------------------------------------
  onclick("abuttonanainputrefresh", {
    .reloadInputs()
  })

  # Reload input generated table -----------------------------------------------
  .reloadInputs <- function(){
    logMessage(".reloadInputs called")
    if (!is.null(analysisID())) {
      withModalSpinner(
        dt_generated <- session$userData$data_hub$return_analyses_input_file_wicons_df(analysisID(),Status),
        "Loading...",
        size = "s"
      )


    } else {
      dt_generated <-  NULL
    }
    result$dt_generated  <- dt_generated
  }


}

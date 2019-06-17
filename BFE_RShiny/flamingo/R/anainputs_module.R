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
    counter = NULL
  )

  observeEvent( input[["panel_anainputs-collapse-button"]], {
    logMessage(paste0("changing result$counter to ", result$counter, " because input[['panel_anainputs-collapse-button']] chanhed to ",  input[["panel_anainputs-collapse-button"]]))
    result$counter <- input[["panel_anainputs-collapse-button"]]
  })

  observeEvent(counter(), {
    logMessage(paste0("changing result$counter to ", result$counter, " because counter() chanhed to ", counter()))
    result$counter <- counter()
  })

  # Create table ---------------------------------------------------------------
  observeEvent({
    active()
    result$counter
  }, ignoreInit = TRUE, {
    if (length(active()) > 0 && active() && !is.null(result$counter) && !is.na(result$counter) &&  result$counter != "" &&  result$counter != 0) {
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
        dt_generated <- return_analyses_input_file_wicons_df(analysisID(), session$userData$data_hub,  session$userData$oasisapi),
        "Loading...",
        size = "s"
      )


    } else {
      dt_generated <-  NULL
    }
    result$dt_generated  <- dt_generated
  }


}

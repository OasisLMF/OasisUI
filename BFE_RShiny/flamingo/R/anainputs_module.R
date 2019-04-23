# Analysis inputs Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' anainputsUI
#' @rdname anainputs
#'
#' @description UI/View for inputs of an analysis.
#'
#' @return List of tags.
#'
#' @export
anainputsUI <- function(id) {

  ns <- NS(id)

  tagList(
    flamingoPanel(
      selection = list(mode = 'none'),
      escape = FALSE,
      scrollX = TRUE,
      filter = "none",
      rownames = TRUE,
      colnames = c('row number' = 1),
      id = ns("panel_anainputs"),
      flamingoRefreshButton(ns("abuttongeneratedrefresh")),
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
#' @param analysisID Selected analysis id.
#' @param counter Reactive value storing actionButton status.
#'
#' @export
anainputs <- function(input,
                            output,
                            session,
                            analysisID,
                            counter = NULL,
                            active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    dt_generated = NULL
  )

  # Create table ---------------------------------------------------------------
  observeEvent({
    active()
    counter()
  }, ignoreInit = TRUE, {
    if (length(active()) > 0 && active()) {
      extractFolder <- set_extractFolder(id = analysisID(), label = "_inputs/")
      if (!dir.exists(extractFolder) || is.na(file.size(extractFolder))) {
        withModalSpinner(
          api_get_analyses_input_file(analysisID()),
          "Loading...",
          size = "s"
        )
      }
      .reloadGeneratediInputs()
    }
  })

  callModule(
    ViewFilesInTable,
    id = "ViewIGFiles",
    tbl_filesListData = reactive({result$dt_generated}),
    param = analysisID,
    file_column = "files",
    folderpath = "_inputs/",
    includechkbox = TRUE)

  # reload Generated Inputs table-----------------------------------------------
  onclick("abuttongeneratedrefresh", {
    withModalSpinner(
      api_get_analyses_input_file(analysisID()),
      "Refreshing...",
      size = "s"
    )
    .reloadGeneratediInputs()
  })

  # Reload input generated table -----------------------------------------------
  .reloadGeneratediInputs <- function(){
    logMessage(".reloadGeneratediInputs called")
    if (!is.null(analysisID()) && analysisID() != "") {
      result$dt_generated <- return_analyses_input_file_wicons_df(analysisID())
    } else {
      result$dt_generated <-  NULL
    }
  }
}

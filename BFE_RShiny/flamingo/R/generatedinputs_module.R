# uploaded inputs Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' generatedinputsUI
#' @rdname generatedinputs
#'
#' @description UI/View for uploaded inputs of an analysis.
#'
#' @return List of tags.
#'
#' @export
generatedinputsUI <- function(id) {

  ns <- NS(id)

  tagList(
    flamingoPanel(
      selection = list(mode = 'none'),
      escape = FALSE,
      scrollX = TRUE,
      filter = "none",
      rownames = TRUE,
      colnames = c('row number' = 1),
      id = ns("panel_generatedinputs"),
      flamingoRefreshButton(ns("abuttongeneratedrefresh")),
      ViewFilesInTableUI(id  = ns("ViewIGFiles"), includechkbox = TRUE)
    )
  )
}

# Server -----------------------------------------------------------------------

#' generatedinputs
#'
#' @rdname generatedinputs
#'
#' @description  Server logic for uploaded inputs of an analysis.
#'
#' @param analysisID selected analysis ID.
#'
#' @export
generatedinputs <- function(input,
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

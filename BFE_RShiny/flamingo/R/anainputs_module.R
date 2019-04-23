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
#' @param analysisID Selected analysis ID.
#' @param portfolioID Selected portfolio ID.
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
                      portfolioID,
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
      dt_generated <- return_analyses_input_file_wicons_df(analysisID())
      if (!is.null(dt_generated)) {
        dt_generated <- .replace_uploaded_files(dt_generated, portfolioID())
      }
    } else {
      dt_generated <-  NULL
    }
    result$dt_generated  <- dt_generated
  }

  .replace_uploaded_files <- function(dt_generated, portfolioID){

    logMessage(".replace_uploaded_files called")

    filetypes <- c("location_file", "accounts_file", "reinsurance_info_file", "reinsurance_source_file")

    types <- sapply(dt_generated$files, function(fi){strsplit(fi, split = "[.]")[[1]][1]})
    dt_generated <- dt_generated %>%
      add_column(type = types , .after = "files")

    for (filetype in filetypes){
      stored_name <- return_portfolios_stored_name(portfolioID, filetype)
      if (!is.null(stored_name) && !is.na(stored_name) && stored_name %in% dt_generated$files) {
        dt_generated$type[which(dt_generated$files == stored_name)] <- filetype
      }
    }

    return(dt_generated)
  }

}

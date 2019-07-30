
#' Helper text
#'
#' Helper texts to show explanations next to buttons, one per file (?).
#'
#' @format Each object is a named list of character strings, the names
#'   corresponding to the button IDs.
#'
#' @name helper-text
NULL

#' @rdname helper-text
#'
#' @export
file_Viewer <- list(
  FLdownloadzip = "Download all selected files"
  #FVFLdownloadexcel = "Download files table"
)

#' @rdname helper-text
#'
#' @export
landing_page <- list(
  abuttonanalysis = "Define analysis to run",
  abuttonbrowse = "Visualize outputs",
  abuttondefineanasingle = "Define single analysis",
  abuttondefineanabatch = "Define batch analysis",
  abuttonbrowseSBR = "Visualize outputs from single analysis",
  abuttonbrowseBBR = "Visualize results across analyses or from batch runs",
  abuttonbrowseCBR = "Compare runs",
  abuttongotoana = "Visualize outputs of selected analysis",
  abuttondelana = "Delete selected analysis",
  abuttonConfirmDelAna = "Confirm deleting analysis",
  downloadexcel_ana = "Download analyses table",
  accountDD = "User information",
  abuttonlogout = "Logout the application. You will need to sign back in"
)

#' @rdname helper-text
#'
#' @export
defineSingleAna <- list(
  abuttoncreateana = "Create new analysis",
  abuttonshowanadetails = "Show details for selected analysis",
  abuttonuploadsourcefiles = "Upload source files to selected portfolio",
  portfolioID = "Select a portfolio ID",
  abuttonpfdetails = "Show details for selected portfolio",
  abuttonamendpf = "Amend selected portfolio name",
  abuttonpfsubmit = "Submit new portfolio",
  abuttondeletepf = "Delete selected portfolio",
  abuttonmodeldetails = "Show details for selected model",
  abuttonexecuteanarun = "Execute run with given output configuration",
  abuttonshowlog = "Show log for selected analysis",
  abuttondisplayoutput = "Visualize outputs of selected analysis",
  abuttoncancelana = "Cancel selected analysis",
  abuttonConfirmDel = "Confirm deleting analysis",
  abuttonstartcancIG = "Start/Cancel input generation for selected analysis",
  abuttonConfirmDelIG = "Confirm interruption input generation",
  abuttonrunconfig = "Define output configuration and run analysis"
)

### Panel Browse ----

#' @rdname helper-text
#'
#' @export
dashboard <- list(
  selectAnaID = "Select analysis ID",
  selectBatchAnaID = "Select Batch analysis ID",
  abuttonplusplot = "Add a new plot",
  abuttonselectAna = "Select analysis"
)

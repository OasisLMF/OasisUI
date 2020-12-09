
#' Helper text
#'
#' Helper texts to show explanations next to buttons. Grouped according to where the button is visible in the UI.
#'
#' @format Each object is a named list of character strings, the names
#'   corresponding to the button IDs.
#'
#' @name helper-text
NULL

#' @rdname helper-text
#'
#' @export
file_Viewer_tooltips <- list(
  FLdownloadzip = "Download all selected files"
)

#' @rdname helper-text
#'
#' @export
landing_page_tooltips <- list(
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
defineSingleAna_tooltips <- list(
  abuttoncreateana = "Create new analysis",
  abuttonshowanadetails = "Show details for selected analysis",
  abuttonuploadsourcefiles = "Upload source files to selected portfolio",
  portfolioID = "Select a portfolio ID",
  abuttonpfdetails = "Show details for selected portfolio",
  abuttonamendpf = "Amend selected portfolio name",
  abuttonpfsubmit = "Submit new portfolio",
  abuttondeletepf = "Delete selected portfolio",
  abuttonmodeldetails = "Show details for selected model",
  abuttonbuildfly = "Customize parameters and datafiles of model",
  abuttonexecuteanarun = "Execute run with given output configuration",
  abuttonshowlog = "Show log for selected analysis",
  abuttondisplayoutput = "Visualize outputs of selected analysis",
  abuttoncancelana = "Cancel selected analysis",
  abuttonConfirmDel = "Confirm deleting analysis",
  abuttonstartcancIG = "Start/Cancel input generation for selected analysis",
  abuttonConfirmDelIG = "Confirm interruption input generation",
  abuttonrunconfig = "Define output configuration and run analysis",
  abuttonchoosetag = "Select tag from a previously used configuration.",
  download_out_params_review_tbl = "Download output parameters review.",
  clearselection = "Clear all fields",
  addBtn = "Add new summary levels and reports",
  removeBtn = "Remove summary levels and reports",
  abuttonsubmit = "Submit new analysis",
  abuttonselsettings = "Apply model changes"
)

### Dashboard ------------------------------------------------------------------

#' @rdname helper-text
#'
#' @export
dashboard_tooltips <- list(
  selectAnaID = "Select analysis ID",
  abuttonselectAna = "Select analysis"
)

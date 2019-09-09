# Status -----------------------------------------------------------------------

#' Status Icons
#' @description List of icons representing status
#' @format Named \code{list} of Staus icons.
#' @export
Status <- list(
  Ready = '<i class="fa fa-check"></i>',
  Failed = '<i class="fa fa-times-circle"></i>',
  Completed = '<i class="fa fa-check-circle"></i>',
  Processing = '<i class="fa fa-spinner"></i>'
)

#' Status Details
#' @description List of text representing status as returned by API
#' @format Named \code{list} of Staus details.
#' @export
Status_details <- list(
  input_gen_started = "inputs generation started",
  input_gen_failed = "inputs generation error",
  run_err = "run error",
  ready = "ready",
  run_ok = "run completed"
)

# Default Tags -----------------------------------------------------------------
#' Default Tags
#' @description Set of default tags for output parameters
#' @export
default_tags <- c("Summary", "Drill-down", "Custom")

# Lists for tables cols names --------------------------------------------------


#' tbl_portfoliosDataNames
#' @description Creating List for col names of Portfolio Table
#' @format Named \code{list} of bl_portfoliosData column names.
#' @export
tbl_portfoliosDataNames <- list(
  id = "id",
  name = "name",
  created = "created",
  modified = "modified",
  status = "status"
)

#' tbl_modelsDataNames
#' @description Creating List for col names of Model Table
#' @format Named \code{list} of tbl_modelsData column names.
#' @export
tbl_modelsDataNames <- list(
  id = "id",
  name = "name",
  created = "created",
  modified = "modified",
  model_id = "model_id",
  supplier_id = "supplier_id",
  version_id = "version_id"
)

#' tbl_analysesDataNames
#' @description Creating List for col names of Analyses Table
#' @format Named \code{list} of tbl_analysesData column names.
#' @export
tbl_analysesDataNames <- list(
  id = "id",
  name = "name",
  created = "created",
  modified = "modified",
  status_detailed = "status_detailed",
  status = "status",
  portfolio = "portfolio",
  model = "model",
  input_file = "input_file",
  settings_file = "settings_file",
  input_errors_file = "input_errors_file",
  input_generation_traceback_file = "input_generation_traceback_file",
  output_file = "output_file",
  run_traceback_file = "run_traceback_file"
)


#' filesListDataNames
#' @description Creating List for  col names of filesListData
#' @format Named \code{list} of filesListData column names.
#' @export
filesListDataNames <- list(
  id = "FileID",
  name = "File Name",
  location_unix <- "Location Unix",
  resource_key <- "Resource Key"
)

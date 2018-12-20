# Icons ------------------------------------------------------------------------
#' @export
# List of icons representing status
Status <- list(
  Ready = '<i class="fa fa-check"></i>',
  Failed = '<i class="fa fa-times-circle"></i>',
  Completed = '<i class="fa fa-check-circle"></i>',
  Processing = '<i class="fa fa-spinner"></i>'
)

# Output Configuration options -------------------------------------------------

#' @export  
# List of variables per granularity
LosstypesChoices <- list(
" " = "Summary",
" " = "ELT",
" " = "FullUncAEP",
" " = "FullUncOEP",
" " = "AEPWheatsheaf",
" " = "OEPWheatsheaf",
# " " = "MeanAEPWheatsheaf",
# " " = "MeanOEPWheatsheaf",
# " " = "SampleMeanAEP",
# " " = "gSampleMeanOEP",
" " = "AAL",
" " = "PLT")

#' @export
# list of granularities, variables and granularities allowed
output_options <- list(
  granularities = c("LOB", "Location", "County","State", "Policy", "Portfolio"),
  losstypes = c("GUL", "IL", "RI"),
  variables = c("Full Sample", "PLT", "AAL", "LEC Wheatsheaf OEP", "LEC Wheatsheaf AEP", "LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP", "ELT") #label fisible in plot output. Should be the same as in the checkboxes of output configuration
)

#' @export
# data frame of variables, with respective analysis_settings json field and info about being under lec_output in analysis_settings.json
varsdf <- data.frame(vars = c('Summary', 'ELT', 'FullUncAEP', 'FullUncOEP', 'AEPWheatsheaf', 'OEPWheatsheaf', 'MeanAEPWheatsheaf', 'MeanOEPWheatsheaf', 'SampleMeanAEP', 'SampleMeanOEP', 'AAL', 'PLT'),
                     labels = c("Full Sample", "ELT","LEC Full Uncertainty AEP", "LEC Full Uncertainty OEP", "LEC Wheatsheaf AEP", "LEC Wheatsheaf OEP", "LEC Mean Wheatsheaf AEP", "LEC Mean Wheatsheaf OEP", "LEC Sample Mean AEP", "LEC Sample Mean OEP", "AAL","PLT"),
                     fields = c('summarycalc', 'eltcalc', 'full_uncertainty_aep', 'full_uncertainty_oep', 'wheatsheaf_aep',  'wheatsheaf_oep', 'wheatsheaf_mean_aep', 'wheatsheaf_mean_oep', 'sample_mean_aep', 'sample_mean_oep',  'aalcalc', 'pltcalc'),
                     defaultChoice = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
                     lec_output = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
                     stringsAsFactors = FALSE)

#' @export
# data frame of variables, with respective oed field and info from analysis_settings.json
granToOed <- data.frame(oed = c("lob", "location", "county","state", "policy", "prog"),
                        gran = output_options$granularities,
                        outputlosstype = c("lob", "loc", "county","state", "policy", "prog"),
                        stringsAsFactors = FALSE)

#' @export
# function to create a nemed list connecting the file names patterns with the coorresponding vaariables
# takes as input varsdf
reportToVar <- function(varsdf){
  L <- lapply(seq(length(varsdf$labels)), function(i){varsdf$labels[i]})
  namesL <- varsdf$fields
  namesL[varsdf$lec_output] <- paste0("leccalc_", namesL[varsdf$lec_output] )
  L <- setNames(L, namesL)
  return(L)
}


### Plots types ----------------------------------------------------------------

#' @export
# list containing settings for plots
plottypeslist <- list("loss per return period" = list("Variables" = c("LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP"),
                                                      "keycols" = c("loss"),
                                                      "uncertaintycols" = c(),
                                                      "referencecols" = c(),
                                                      "x" = c("return_period"),
                                                      "xtickslabels" = list(),
                                                      "extracols" = c("X", "summary_id"),
                                                      "plottype" = "line",
                                                      "xlabel" = c("Return Period"),
                                                      "ylabel" = c("Loss")
),
"AAL bar plot" = list("Variables" = c("AAL"),
                      "keycols" = c("mean"),
                      "uncertaintycols" = c("standard_deviation"),
                      "referencecols" = c("exposure_value"),
                      "x" = c("type"),
                      "xtickslabels" = c("Numerically Integrated", "Sample Statistics"),
                      "extracols" = c("X", "summary_id"),
                      "plottype" = "bar",
                      "xlabel" = c("Type"),
                      "ylabel" = c("Loss")
)#,
# "Wheatsheaf violin plot" = list("Variables" = c("LEC Wheatsheaf AEP", "LEC Wheatsheaf OEP"),
#                               "keycols" = c("loss"),
#                               "uncertaintycols" = c(),
#                               "referencecols" = c(),
#                               "x" = c("return_period"),
#                               "xtickslabels" = list(),
#                               "extracols" = c("X", "summary_id", "sidx"),
#                               "plottype" = "violin",
#                               "xlabel" = c("Return Period"),
#                               "ylabel" = c("Loss")
# )
)

# Lists for tables cols names --------------------------------------------------

#' @export
#Creating List for col names of Portfolio Table
tbl_portfoliosDataNames <- list(
  id = "id",
  name = "name",
  created = "created",
  modified = "modified",
  status = "status"
)

#' @export
# Creating List for col names of Model Table
tbl_modelsDataNames <- list(
  id = "id",
  name = "name",
  created = "created",
  modified = "modified",
  model_id = "model_id", 
  supplier_id = "supplier_id",
  version_id = "version_id"
)


#' @export
# Creating List for col names of Model Runs Table
tbl_analysesDataNames <- list(
  id = "id",
  name = "name",
  created = "created",
  modified = "modified",
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


#' @export
# Creating List for  col names of filesListData
filesListDataNames <- list(
  id = "FileID",
  name = "File Name",
  location_unix <- "Location Unix",
  resource_key <- "Resource Key"
)

# Output configuration options -------------------------------------------------

#' LosstypesChoices
#' @description List of variables per granularity.
#' @format Named \code{list} of Losstypes choices. Names being an empty space.
#' @export
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

#' output_options
#' @description List of granularities, losstypes and variables allowed.
#' @format Named \code{list} of output options.
#' @export
output_options <- list(
  granularities = c("LOB", "Location", "County","State", "Policy", "Portfolio"),
  losstypes = c("GUL", "IL", "RI"),
  variables = c("Full Sample", "PLT", "AAL", "LEC Wheatsheaf OEP", "LEC Wheatsheaf AEP", "LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP", "ELT"), #label fisible in plot output. Should be the same as in the checkboxes of output configuration
  order = c(6,2,3,4,1,5),
  variables_default = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
)

#' varsdf
#' @description Data frame of variables, with respective analysis_settings json field and info about whether a variable is under lec_output in analysis_settings.json.
#' @format \code{data.frame} of variables for output configuration.
#' @export
varsdf <- data.frame(
  vars = c('Summary', 'ELT', 'FullUncAEP', 'FullUncOEP', 'AEPWheatsheaf', 'OEPWheatsheaf', 'MeanAEPWheatsheaf', 'MeanOEPWheatsheaf', 'SampleMeanAEP', 'SampleMeanOEP', 'AAL', 'PLT'),
  labels = c("Full Sample", "ELT", "LEC Full Uncertainty AEP", "LEC Full Uncertainty OEP", "LEC Wheatsheaf AEP", "LEC Wheatsheaf OEP", "LEC Mean Wheatsheaf AEP", "LEC Mean Wheatsheaf OEP", "LEC Sample Mean AEP", "LEC Sample Mean OEP", "AAL","PLT"),
  fields = c('summarycalc', 'eltcalc', 'full_uncertainty_aep', 'full_uncertainty_oep', 'wheatsheaf_aep',  'wheatsheaf_oep', 'wheatsheaf_mean_aep', 'wheatsheaf_mean_oep', 'sample_mean_aep', 'sample_mean_oep',  'aalcalc', 'pltcalc'),
  defaultChoice = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  lec_output = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE)

#' granToOed
#' @description Data frame of variables, with respective oed field and info from analysis_settings.json.
#' @format \code{data.frame} of granularities for output configuration.
#' @export
granToOed <- data.frame(
  oed = c("lob", "location", "county","state", "policy", "prog"),
  gran = output_options$granularities,
  outputlosstype = c("lob", "loc", "county","state", "policy", "prog"),
  order = c(6,2,3,4,1,5),
  stringsAsFactors = FALSE)

#' reportToVar
#' @description Function to create a named list connecting the file names patterns with the corresponding variables.
#' Operates on `varsdf`.
#' @md
#' @export
reportToVar <- function() {
  L <- as.list(varsdf$labels)
  names(L) <- varsdf$fields
  names(L)[varsdf$lec_output] <- paste("leccalc", names(L)[varsdf$lec_output], sep = "_")
  L
}


# Plots types ----------------------------------------------------------------

#' plottypeslist
#' @description List containing settings for plots.
#' @format Named \code{list} of output plot options.
#' @export
plottypeslist <- list(
  "loss per return period" = list(
    "Variables" = c("LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP"),
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
  "AAL bar plot" = list(
    "Variables" = c("AAL"),
    "keycols" = c("mean"),
    "uncertaintycols" = c("standard_deviation"),
    "referencecols" = c("exposure_value"),
    "x" = c("type"),
    "xtickslabels" = c("Numerically Integrated", "Sample Statistics"),
    "extracols" = c("X", "summary_id"),
    "plottype" = "bar",
    "xlabel" = c("Type"),
    "ylabel" = c("Loss")
  )
)

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

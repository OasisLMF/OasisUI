# Output configuration options -------------------------------------------------

#' output_options
#' @description List of losstypes and variables allowed.
#' @format Named \code{list} of output options.
#' @export
output_options <- list(
  # granularities = c("LOB", "Location", "County","State", "Policy", "Portfolio"),
  losstypes = c("GUL", "IL", "RI"),
  # reports feasible in plot output:
  variables = c("Full Sample", "ELT",
                "LEC Full Uncertainty AEP", "LEC Full Uncertainty OEP",
                "LEC Wheatsheaf AEP", "LEC Wheatsheaf OEP",
                "LEC Mean Wheatsheaf AEP", "LEC Mean Wheatsheaf OEP",
                "LEC Sample Mean AEP", "LEC Sample Mean OEP",
                "AAL", "PLT",
                "ELT Sample", "ELT Quantile", "ELT Moment",
                "PLT Sample", "PLT Quantile", "PLT Moment",
                "ALT Period",
                "EPT Full Uncertainty AEP", "EPT Full Uncertainty OEP",
                "EPT Mean Sample AEP", "EPT Mean Sample OEP",
                "EPT per Sample Mean AEP", "EPT per Sample Mean OEP",
                "Psept AEP", "Psept OEP"),

  # order = c(6,2,3,4,1,5),
  variables_default = c(FALSE, FALSE,
                        TRUE, TRUE,
                        FALSE, FALSE,
                        FALSE, FALSE,
                        FALSE, FALSE,
                        TRUE, FALSE,
                        FALSE, FALSE, FALSE,
                        FALSE, FALSE, FALSE,
                        FALSE,
                        FALSE, FALSE,
                        FALSE, FALSE,
                        FALSE, FALSE,
                        FALSE, FALSE),

  # default empty string is interpreted as aggregation should happen across everything, i.e. without any specific summary level
  # REF: perhaps to be changed to "All Risks" and replace string with default_level elsewhere
  default_level = ""
)

#' varsdf
#' @description Data frame of variables, with respective analysis_settings json field and info about whether a variable is under lec_output in analysis_settings.json.
#' @format \code{data.frame} of variables for output configuration.
#' @export
varsdf <- data.frame(
  vars = c('Summary', 'ELT', 'FullUncAEP', 'FullUncOEP', 'AEPWheatsheaf', 'OEPWheatsheaf',
           'MeanAEPWheatsheaf', 'MeanOEPWheatsheaf', 'SampleMeanAEP', 'SampleMeanOEP', 'AAL', 'PLT',
           'ELTSample', 'ELTQuantile', 'ELTMoment', 'PLTSample', 'PLTQuantile', 'PLTMoment',
           'ALTPeriod', 'EPTFullUncertainty_aep', 'EPTFullUncertaintyOEP', 'EPTMeanSampleAEP',
           'EPTMeanSampleOEP', 'EPTperSampleMeanAEP', 'EPTperSampleMeanOEP', 'PseptAEP', 'PseptOEP'),
  labels = output_options$variables,
  fields = c('summarycalc', 'eltcalc', 'full_uncertainty_aep', 'full_uncertainty_oep',
             'wheatsheaf_aep',  'wheatsheaf_oep', 'wheatsheaf_mean_aep', 'wheatsheaf_mean_oep',
             'sample_mean_aep', 'sample_mean_oep',  'aalcalc', 'pltcalc', 'elt_sample', 'elt_quantile',
             'elt_moment', 'plt_sample', 'plt_quantile', 'plt_moment', 'alt_period', 'ept_full_uncertainty_aep',
             'ept_full_uncertainty_oep', 'ept_mean_sample_aep', 'ept_mean_sample_oep', 'ept_per_sample_mean_aep',
             'ept_per_sample_mean_oep', 'psept_aep', 'psept_oep'),
  # defaultChoice = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  lec_output = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                 TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, FALSE),
  ord_output = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                 TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

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
  "loss per return period line plot" = list(
    "Variables" = c("LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP"),
    "keycols" = c("loss"),
    "uncertaintycols" = c(),
    "referencecols" = c(),
    "x" = c("return_period"),
    "xtickslabels" = list(),
    "extracols" = c("X", "summary_id"),
    "plottype" = "line",
    "xlabel" = c("RP"),
    "ylabel" = c("Loss")
  ),
  "AAL bar plot" = list(
    "Variables" = c("AAL"),
    "x" = c("type"),
    "keycols" = c("mean"),
    "uncertaintycols" = c("standard_deviation"),
    # 277: prevent crash due to no more existing reference col "exposure_value"
    "referencecols" = c(),
    "xtickslabels" = c("Numerically Integrated", "Sample Statistics"),
    "extracols" = c("X", "summary_id"),
    "plottype" = "bar",
    "xlabel" = c("Type"),
    "ylabel" = c("Loss")
  ),
  "loss for return period map" = list(
    "Variables" = c("LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP"),
    "plottype" = "map"
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
#                               "xlabel" = c("RP"),
#                               "ylabel" = c("Loss")
# )

# Summary template for analysis settings -------------------------------
#' summary_template
#' @description List defining the structure of a "summary output" in the analysis settings.
#' @format Named \code{list} of output options.
#' @export
summary_template <- list(
  summarycalc = FALSE,
  eltcalc = FALSE,
  aalcalc = FALSE,
  pltcalc = FALSE,
  id = 1,
  oed_fields = list(),
  lec_output = FALSE,
  leccalc = list(
    return_period_file = FALSE,
    full_uncertainty_aep = FALSE,
    full_uncertainty_oep = FALSE,
    wheatsheaf_aep = FALSE,
    wheatsheaf_oep = FALSE,
    wheatsheaf_mean_aep = FALSE,
    wheatsheaf_mean_oep = FALSE,
    sample_mean_aep = FALSE,
    sample_mean_oep = FALSE
  ),
  # ord_output = FALSE,
  ord_output = list(
    elt_sample = FALSE,
    elt_quantile = FALSE,
    elt_moment = FALSE,
    plt_sample = FALSE,
    plt_quantile = FALSE,
    plt_moment = FALSE,
    alt_period = FALSE,
    ept_full_uncertainty_aep = FALSE,
    ept_full_uncertainty_oep = FALSE,
    ept_mean_sample_aep = FALSE,
    ept_mean_sample_oep = FALSE,
    ept_per_sample_mean_aep = FALSE,
    ept_per_sample_mean_oep = FALSE,
    psept_aep = FALSE,
    psept_oep = FALSE
  )
)

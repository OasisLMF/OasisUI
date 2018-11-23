# Input File -------------------------------------------------------------------
#' Get analysis input file
#'
#' Gets the analysis input_file contents
#'
#' @rdname api_get_analyses_input_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted analysis input file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_get_analyses_input_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Post analysis input file
#'
#' Sets the analysis input_file contents.
#'
#' @rdname api_post_analyses_input_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_input path to the input file.
#'
#' @return the posted analysis input file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_input_file <- function(id, filepath_input) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

# Cancel analysis --------------------------------------------------------------

#' Cancel analysis
#'
#' Cancel the selected analysis.
#'
#' @rdname api_post_analyses_cancel
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return the cancelled analysis.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_cancel <- function(id) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(id = id),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "cancel", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}


# Settings File ----------------------------------------------------------------
#' Get analysis settings file
#'
#' Gets the analysis settings_file contents
#'
#' @rdname api_get_analyses_settings_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted analysis settings file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_get_analyses_settings_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "settings_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Post analysis settings file
#'
#' Sets the analysis settings_file contents.
#'
#' @rdname api_post_analyses_settings_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_settings path to the settings file.
#'
#' @return the posted analysis settings file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_settings_file <- function(id, filepath_settings) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_settings)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "settings_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Return ianalyses_settings_file List
#'
#' @rdname return_analyses_settings_file_list
#'
#' @description Returns a list of analyses_settings
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return list of analyses_settings_file
#'
#' @importFrom httr content
#'
#' @export
return_analyses_settings_file_list <- function(id){
  get_analyses_settings_file <- api_get_analyses_settings_file(id)
  analyses_settings_fileList <- content(get_analyses_settings_file$result)
  return(analyses_settings_fileList)
}


#' Construct analysis settings list
#'
#' Construct analysis_settingsList
#'
#' @rdname construct_analysis_settingsList
#'
#' @description Constructs a list of analysis settings
#'
#' @param source_tag model associated to the analysis.
#' @param prog_id portfolio id?
#' @param number_of_samples user input number of samples
#' @param module_supplier_id identification of model supplied
#' @param model_version_id identification of model version
#' @param event_occurrence_file_id user input Event Occurrence Set
#' @param use_random_number_file use_random_number_file
#' @param event_set user input Event Set
#' @param peril_wind peril wind
#' @param demand_surge demand surge
#' @param peril_quake peril quake
#' @param peril_flood peril flood
#' @param peril_surge peril surge
#' @param leakage_factor user input Leakage factor
#' @param gul_threshold user input gul threshold
#' @param exposure_location exposure_location
#' @param chkinputsummaryoption logical to include summaries
#' @param outputsGUL output config GUL
#' @param outputsIL output config IL
#' @param outputsRI output config RI
#' @param analysis_tag analysis id?
#' @param gul_output logical for GUL
#' @param il_output logical for IL
#' @param ri_output logical for RI
#' @param return_period_file param = TRUE
#' @param uniqueItems uniqueItems
#' @param id id
#'
#' @return list of analysis settings.
#'
#' @export
construct_tbl_analyses_settings <- function(source_tag, prog_id, number_of_samples,
                                            module_supplier_id, model_version_id,
                                            event_occurrence_file_id,use_random_number_file = FALSE, event_set,
                                            peril_wind, demand_surge, peril_quake, peril_flood, peril_surge, leakage_factor,
                                            gul_threshold,exposure_location = 'L',
                                            outputsGUL, outputsIL, outputsRI, chkinputsummaryoption,
                                            gul_output, il_output, ri_output,
                                            return_period_file,
                                            analysis_tag, uniqueItems = FALSE, id = 1){

  # > Params -------------------------------------------------------------------
  parameterStub <- c('Summary',
                     'ELT',
                     'FullUncAEP',
                     'FullUncOEP',
                     'AEPWheatsheaf',
                     'OEPWheatsheaf',
                     'MeanAEPWheatsheaf',
                     'MeanOEPWheatsheaf',
                     'SampleMeanAEP',
                     'SampleMeanOEP',
                     'AAL',
                     'PLT')
  analysisFileNameStub <- c('summarycalc',
                            'eltcalc',
                            'full_uncertainty_aep',
                            'full_uncertainty_oep',
                            'wheatsheaf_aep',
                            'wheatsheaf_oep',
                            'wheatsheaf_mean_aep',
                            'wheatsheaf_mean_oep',
                            'sample_mean_aep',
                            'sample_mean_oep',
                            'aalcalc',
                            'pltcalc')

  # > Utility functions --------------------------------------------------------
  .addsummaryGUL <- function(summaryreports, outputsGUL) {
    if (summaryreports) {
      outputsGUL <- unique(c(outputsGUL, c('gulprogFullUncAEP', 'gulprogFullUncOEP', 'gulprogAAL')))
    }
    outputsStringGUL <- paste(collapse = ", ",outputsGUL)
  }

  .addsummaryIL <- function(summaryreports, outputsIL) {
    if (summaryreports) {
      outputsIL  <- unique(c(outputsIL,  c('ilprogFullUncAEP', 'ilprogFullUncOEP', 'ilprogAAL')))
    }
    outputsStringIL <- paste(collapse = ", ",outputsIL)
  }

  .addsummaryRI <- function(summaryreports, outputsRI) {
    if (summaryreports) {
      outputsRI  <- unique(c(outputsRI,  c('riprogFullUncAEP', 'riprogFullUncOEP', 'riprogAAL')))
    }
    outputsStringRI <- paste(collapse = ", ",outputsRI)
  }

  .gatheModelSettings <- function(event_occurrence_file_id,use_random_number_file, event_set, perilsvec){

    model_settings <- list()
    model_settings[["event_set"]] <- event_set
    for (p in names(perilsvec)) {
      if (!is.null(perilsvec[[p]]) && perilsvec[[p]])
        model_settings[[p]] <- perilsvec[[p]]
    }
    model_settings[["use_random_number_file"]] <- use_random_number_file
    model_settings[["event_occurrence_file_id"]] <- event_occurrence_file_id
    model_settings
  }

  .gaterSummaries <- function(parameterStub,  outputsString, summaryreports, uniqueItems, id){

    aalcalc <- grepl(parameterStub[11], outputsString)
    eltcalc <- grepl(parameterStub[2], outputsString)
    pltcalc <- grepl(parameterStub[12], outputsString)
    full_uncertainty_aep <- grepl(parameterStub[3], outputsString)
    full_uncertainty_oep <- grepl(parameterStub[4], outputsString)
    wheatsheaf_aep <- grepl(parameterStub[5], outputsString)
    wheatsheaf_oep <- grepl(parameterStub[6], outputsString)
    wheatsheaf_mean_aep <- wheatsheaf_aep #grepl(parameterStub[7], outputsString)
    wheatsheaf_mean_oep <- wheatsheaf_oep #grepl(parameterStub[8], outputsString)
    sample_mean_aep <- wheatsheaf_aep #grepl(parameterStub[9], outputsString)
    sample_mean_oep <-  wheatsheaf_oep #grepl(parameterStub[10], outputsString)
    leccalcoutputs <- data.frame("full_uncertainty_aep" = full_uncertainty_aep,
                                 "full_uncertainty_oep" = full_uncertainty_oep,
                                 "wheatsheaf_aep" = wheatsheaf_aep,
                                 "wheatsheaf_oep" = wheatsheaf_oep,
                                 "wheatsheaf_mean_aep" = wheatsheaf_mean_aep,
                                 "wheatsheaf_mean_oep" = wheatsheaf_mean_oep,
                                 "sample_mean_aep" = sample_mean_aep,
                                 "sample_mean_oep" = sample_mean_oep)
    leccal <- data.frame("outputs" = I(leccalcoutputs),
                         "return_period_file" = return_period_file)
    lec_output <- any(leccalcoutputs == TRUE)
    df <- data.frame("uniqueItems" = c(uniqueItems),
                     "summarycalc" = c(summaryreports),
                     "aalcalc" = c(aalcalc),
                     "eltcalc" = c(eltcalc),
                     "pltcalc" = c(pltcalc),
                     "id" = c(id),
                     "lec_output" = lec_output,
                     "leccalc" = I(leccal))
  }

  # > Restructure inputs -------------------------------------------------------
  outputsStringGUL <- .addsummaryGUL(summaryreports = chkinputsummaryoption, outputsGUL)
  outputsStringIL <- .addsummaryIL(summaryreports = chkinputsummaryoption, outputsIL)
  outputsStringRI <- .addsummaryRI(summaryreports = chkinputsummaryoption, outputsRI)


  # > Make analysis settings list ----------------------------------------------
  analysis_settings <- list()
  analysis_settings[["analysis_settings"]] <- list()
  analysis_settings[["analysis_settings"]][["analysis_tag"]] <- analysis_tag
  analysis_settings[["analysis_settings"]][["exposure_location"]] <- exposure_location

  analysis_settings[["analysis_settings"]][["gul_output"]] <- gul_output
  analysis_settings[["analysis_settings"]][["gul_summaries"]] <- .gaterSummaries(parameterStub,
                                                                                 outputsStringGUL,
                                                                                 summaryreports = chkinputsummaryoption,
                                                                                 uniqueItems,
                                                                                 id)
  analysis_settings[["analysis_settings"]][["gul_threshold"]] <- gul_threshold

  analysis_settings[["analysis_settings"]][["il_output"]] <- il_output
  analysis_settings[["analysis_settings"]][["il_summaries"]] <- .gaterSummaries(parameterStub,
                                                                                outputsStringIL,
                                                                                summaryreports = chkinputsummaryoption,
                                                                                uniqueItems,
                                                                                id)
  analysis_settings[["analysis_settings"]][["ri_output"]] <- ri_output
  analysis_settings[["analysis_settings"]][["ri_summaries"]] <- .gaterSummaries(parameterStub,
                                                                                outputsStringRI,
                                                                                summaryreports = chkinputsummaryoption,
                                                                                uniqueItems,
                                                                                id)
  perilsvec <- list("peril_wind" = peril_wind,
                    "demand_surge" = demand_surge,
                    "peril_quake" =  peril_quake,
                    "peril_flood" = peril_flood,
                    "peril_surge" = peril_surge,
                    "leakage_factor" = leakage_factor)
  analysis_settings[["analysis_settings"]][["model_settings"]] <- .gatheModelSettings(event_occurrence_file_id,use_random_number_file, event_set, perilsvec)
  analysis_settings[["analysis_settings"]][["model_version_id"]] <- model_version_id
  analysis_settings[["analysis_settings"]][["module_supplier_id"]] <- module_supplier_id

  analysis_settings[["analysis_settings"]][["number_of_samples"]] <- number_of_samples
  analysis_settings[["analysis_settings"]][["prog_id"]] <- prog_id
  analysis_settings[["analysis_settings"]][["source_tag"]] <- source_tag

  return(analysis_settings)

}

# Input errors file ------------------------------------------------------------

#' Get analysis input_errors file
#'
#' Gets the analysis input_errors_file contents
#'
#' @rdname api_get_analyses_input_errors_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted analysis input_errors file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_get_analyses_input_errors_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_errors_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Post analysis input_errors file
#'
#' Sets the analysis input_errors_file contents.
#'
#' @rdname api_post_analyses_input_errors_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_input_errors path to the input_errors file.
#'
#' @return the posted analysis input_errors file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_input_errors_file <- function(id, filepath_input_errors) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input_errors)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_errors_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

# analysis input generation ----------------------------------------------------

#' Post analysis input generation
#'
#' Sets the analysis generate_inputs contents.
#'
#' @rdname api_post_analyses_generate_inputs
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return the posted analysis input generation.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_generate_inputs <- function(id) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(id = id),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "generate_inputs", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )

}

#' Cancel analysis input generation
#'
#' Cancel the inputs generated in the analysis.
#'
#' @rdname api_post_analyses_cancel_generate_inputs
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return the cancelled analysis input generation.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_cancel_generate_inputs <- function(id) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(id = id),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "cancel_generate_inputs", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}


# input generation traceback file ----------------------------------------------

#' Get analysis input_generation_traceback file
#'
#' Gets the analysis input_generation_traceback_file contents
#'
#' @rdname api_get_analyses_input_generation_traceback_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted analysis input_generation_traceback file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_get_analyses_input_generation_traceback_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_generation_traceback_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Post analysis input_generation_traceback file
#'
#' Sets the analysis input_generation_traceback_file contents.
#'
#' @rdname api_post_analyses_input_generation_traceback_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_input_generation_traceback path to the input_generation_traceback file.
#'
#' @return the posted analysis input_generation_traceback file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_input_generation_traceback_file <- function(id, filepath_input_generation_traceback) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input_generation_traceback)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_generation_traceback_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Return input_generation_traceback_file Dataframe
#'
#' @rdname return_input_generation_traceback_file_df
#'
#' @description Returns a dataframe of input_generation_traceback_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return dataframe of input_generation_traceback_file
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr sym
#' @importFrom httr content
#'
#' @export
return_input_generation_traceback_file_df <- function(id){
  get_input_generation_traceback_file <- api_get_analyses_input_generation_traceback_file(id)
  input_generation_traceback_fileList <- content(get_input_generation_traceback_file$result)
  if (is.null(names(input_generation_traceback_fileList))) {
    input_generation_traceback_file_df <- strsplit(input_generation_traceback_fileList, split = "\n") %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(input_generation_traceback_file_df) <- input_generation_traceback_file_df[1, ]
    # input_generation_traceback_file_df <- input_generation_traceback_file_df %>% filter(!! sym(colnames(input_generation_traceback_file_df)) != colnames(input_generation_traceback_file_df) )
  } else {
    input_generation_traceback_file_df <- bind_rows(input_generation_traceback_fileList) %>%
      as.data.frame()
  }
  return(input_generation_traceback_file_df)
}

# output file ------------------------------------------------------------------

#' Get analysis output file
#'
#' Gets the analysis output_file contents
#'
#' @rdname api_get_analyses_output_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted analysis output file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_get_analyses_output_file <- function(id) {

  dest <- file.path(".", paste0(id, "_outputs.tar"))
  extractFolder <- file.path(".", paste0(id, "_output"))
  dir.create(extractFolder, showWarnings = FALSE)
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "output_file", "", sep = "/"),
    write_disk(dest, overwrite = TRUE)
  )
  
 untar(tarfile = dest, exdir = extractFolder)

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Return analyses output files  Dataframe
#'
#' @rdname return_analyses_output_file_df
#'
#' @description Returns a dataframe of output files 
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return dataframe of output files.
#'
#' @importFrom stats setNames
#'
#' @export

return_analyses_output_file_df <- function(id) {
  extractFolder <- file.path(".", paste0(id, "_output/output"))
  api_get_analyses_output_file(id)
  analyses_output_file_df <- list.files(extractFolder) %>% as.data.frame() %>% setNames("files")
  return(analyses_output_file_df)
}

#' Post analysis output file
#'
#' Sets the analysis output_file contents.
#'
#' @rdname api_post_analyses_output_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_output path to the output file.
#'
#' @return the posted analysis output file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_output_file <- function(id, filepath_output) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_output)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "output_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

# run traceback file -----------------------------------------------------------
#' Get analysis run_traceback file
#'
#' Gets the analysis run_traceback_file contents
#'
#' @rdname api_get_analyses_run_traceback_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted analysis run_traceback file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_get_analyses_run_traceback_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "run_traceback_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Post analysis run_traceback file
#'
#' Sets the analysis run_traceback_file contents.
#'
#' @rdname api_post_analyses_run_traceback_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_run_traceback path to the run_traceback file.
#'
#' @return the posted analysis run_traceback file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_run_traceback_file <- function(id, filepath_run_traceback) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_run_traceback)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "run_traceback_file", "", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}


#' Return analyses_run_traceback_file Dataframe
#'
#' @rdname return_analyses_run_traceback_file_df
#'
#' @description Returns a dataframe of analyses_run_traceback_file
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return dataframe of analyses_run_traceback_file
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_analyses_run_traceback_file_df <- function(id){
  get_analyses_run_traceback_file <- api_get_analyses_analyses_run_traceback_file(id)
  analyses_run_traceback_fileList <- content(get_analyses_run_traceback_file$result)
  if (is.null(names(analyses_run_traceback_fileList))) {
    analyses_run_traceback_file_df <- strsplit(analyses_run_traceback_fileList, split = "\n") %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(analyses_run_traceback_file_df) <- analyses_run_traceback_file_df[1, ]
    # analyses_run_traceback_file_df <- analyses_run_traceback_file_df %>% filter(!! sym(colnames(analyses_run_traceback_file_df)) != colnames(analyses_run_traceback_file_df) )
  } else {
    analyses_run_traceback_file_df <- bind_rows(analyses_run_traceback_fileList) %>%
      as.data.frame()
  }
  return(analyses_run_traceback_file_df)
}

# Run --------------------------------------------------------------------------
#' Run analyses id
#'
#' Returns the analysis status
#'
#' @rdname api_post_analyses_run
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return analysis status..
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_post_analyses_run <- function(id) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "run","", sep = "/")
  )

  logWarning = warning

  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Return analyses run Dataframe
#'
#' @rdname return_analyses_run_df
#'
#' @description Returns a dataframe of analyses after run started
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return dataframe of previously posted analysis states.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_analyses_run_df <- function(id){
  analyses_run <- api_post_analyses_run(id)
  analyses_runList <- content(analyses_run$result)
  analyses_run_df <- bind_rows(analyses_runList) %>%
    as.data.frame()
  return(analyses_run_df)
}

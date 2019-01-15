# Input File -------------------------------------------------------------------
#' Get analysis input file
#'
#' Gets the analysis input_file contents.
#'
#' @rdname api_get_analyses_input_file
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Previously posted analysis input file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr write_disk
#' @importFrom utils untar
#'
#' @export
api_get_analyses_input_file <- function(id) {

  currfolder <- getOption("flamingo.settings.api.share_filepath")
  dest <- file.path(currfolder, paste0(id, "_inputs.tar"))
  extractFolder <- file.path(currfolder, paste0(id, "_inputs"))
  dir.create(extractFolder, showWarnings = FALSE)

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_file", "", sep = "/"),
    write_disk(dest, overwrite = TRUE)
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
  # FIXME: where / why is response needed here?!

  untar(tarfile = dest, exdir = extractFolder)

  # wait for untar to finish
  oldfileList <- NULL
  while (all.equal(oldfileList, list.files(extractFolder)) != TRUE) {
    oldfileList <- list.files(extractFolder)
    Sys.sleep(2)
  }
}

#' Return analyses input files dataframe with icons
#'
#' @rdname return_analyses_input_file_wicons_df
#'
#' @description Returns a dataframe of input files with icons.
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Dataframe of input files with icons.
#'
#' @export
return_analyses_input_file_wicons_df <- function(id) {

  currfolder <- getOption("flamingo.settings.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_inputs/"))
  status_code_notfound <- 404

  if (!file.exists(extractFolder)) {
    api_get_analyses_input_file(id)
  }
  analyses_input_file_df <- list.files(extractFolder) %>% as.data.frame() %>% setNames("files")

  fnames <- analyses_input_file_df$files
  fnum <- length(fnames)
  status <- data.frame(status = rep(status_code_notfound, fnum))
  for (i in seq(fnum) ) {
    fname <- as.character(fnames[i])
    filePath <- file.path(extractFolder, fname)
    info <- file.info(filePath)
    if (is.na(info$size)) {
      status[i, "status"] <- Status$Processing
    } else if (info$size == 0) {
      status[i, "status"] <- StatusF$ailed
    } else {
      status[i, "status"] <- Status$Completed
    }
  }
  analyses_input_file_df <- cbind(analyses_input_file_df, status) %>%
    as.data.frame()
  analyses_input_file_df
}

# Cancel analysis --------------------------------------------------------------

#' Cancel analysis
#'
#' Cancel the selected analysis.
#'
#' @rdname api_post_analyses_cancel
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return The cancelled analysis.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#'
#' @export
api_post_analyses_cancel <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(id = id),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "cancel", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Settings File ----------------------------------------------------------------
#' Get analysis settings file
#'
#' Gets the analysis settings_file contents.
#'
#' @rdname api_get_analyses_settings_file
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Previously posted analysis settings file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_analyses_settings_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "settings_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Post analysis settings file
#'
#' Sets the analysis settings_file contents.
#'
#' @rdname api_post_analyses_settings_file
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_settings Path to the settings file.
#'
#' @return The posted analysis settings file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_settings_file <- function(id, filepath_settings) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_settings)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "settings_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

#' Return analyses_settings_file list
#'
#' @rdname return_analyses_settings_file_list
#'
#' @description Returns a list of analyses_settings.
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return List of analyses_settings_file.
#'
#' @importFrom httr content
#'
#' @export
return_analyses_settings_file_list <- function(id) {
  content(api_get_analyses_settings_file(id)$result)
}

#' Construct analysis settings
#'
#' @rdname construct_analysis_settings
#'
#' @description Constructs a list of analysis settings.
#'
#' @param inputsettings List of input settings.
#' @param outputsLossTypes List of output configuration.
#'
#' @return List of analysis settings.
#'
#' @export
construct_analysis_settings <- function(inputsettings, outputsLossTypes) {

  analysisSettingsMapping <- list(
    "analysis_tag" = list(
      "path" = "analysis_settings",
      "value" = inputsettings$analysis_tag
    ),
    "exposure_location" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$exposure_location
    ),
    "gul_threshold" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$gul_threshold
    ),
    "model_version_id"  = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$model_version_id
    ),
    "module_supplier_id" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$module_supplier_id
    ),
    "number_of_samples" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$number_of_samples
    ),
    "prog_id" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$prog_id
    ),
    "source_tag" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$source_tag
    )
  )

  modelSettingsMapping <- list(
    "event_set" = list(
      "path" = "model_settings",
      "value" =  inputsettings$event_set
    ),
    "leakage_factor" = list(
      "path" = "model_settings",
      "value" =  inputsettings$leakage_factor
    ),
    "peril_wind" = list(
      "path" = "model_settings",
      "value" =  inputsettings$peril_wind
    ),
    "demand_surge" = list(
      "path" = "model_settings",
      "value" =  inputsettings$demand_surge
    ),
    "peril_quake" = list(
      "path" = "model_settings",
      "value" =  inputsettings$peril_quake
    ),
    "peril_flood" = list(
      "path" = "model_settings",
      "value" =  inputsettings$peril_flood
    ),
    "peril_surge" = list(
      "path" = "model_settings",
      "value" =  inputsettings$peril_surge
    ),
    "event_occurrence_file_id" = list(
      "path" = "model_settings",
      "value" =  inputsettings$event_occurrence_file_id
    )
  )

  outoutSettingsMappings <- list(
    "gul_output" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$gul_output
    ),
    "il_output" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$il_output
    ),
    "ri_output" = list(
      "path" = "analysis_settings",
      "value" =  inputsettings$ri_output
    )
  )

  analysis_settings <- list(
    "analysis_settings" = list()
  )

  analysis_settings$model_settings <- list()
  for (i in names(analysisSettingsMapping)) {
    if (!is.null(analysisSettingsMapping[[i]][["value"]])) {
      analysis_settings$analysis_settings[i] <- analysisSettingsMapping[[i]][["value"]]
    }
  }

  for (j in names(modelSettingsMapping)) {
    if (!is.null(modelSettingsMapping[[j]][["value"]])) {
      analysis_settings$model_settings[j] <- modelSettingsMapping[[j]][["value"]]
    }
  }

  for (l in names(outoutSettingsMappings)) {
    losstype <- gsub( "_output", "", l)
    losssummary <- paste0(losstype, "_summaries")
    losstype_output <- outoutSettingsMappings[[l]][["value"]]
    analysis_settings$analysis_settings[l] <- losstype_output
    if (!is.null(losstype_output) && losstype_output) {
      counter_id <- 0
      granularities <- names(outputsLossTypes[[l]])
      outputsLossType <- outputsLossTypes[[l]]
      list_summary <- list()
      for (g in granularities) {
        outputsLossTypeGran <- outputsLossType[[g]]
        if (!is.null(outputsLossTypeGran)){
          oed_g <- g # provide here oed field of granularity
          counter_id <- counter_id + 1
          losstypeSettingsMapping = list()
          # construct list with all information
          for (v in outputsLossTypeGran) {
            losstypeSettingsMapping[varsdf$fields[which(varsdf$vars == v)]] = TRUE
          }
          list_summary_g <- list()
          summary_g <- varsdf$fields[!varsdf$lec_output]
          for (n in summary_g) {
            if (!is.null(losstypeSettingsMapping[[n]]) && !is.null(losstypeSettingsMapping[[n]])) {
              list_summary_g[n] <- losstypeSettingsMapping[[n]]
            }
          }
          list_summary_g$id <- counter_id
          list_summary_g$oed_fields <- oed_g
          list_summary_g$lec_output <- any(losstypeSettingsMapping)

          list_leccalcoutputs_g <- list()
          leccalcoutputs_g <- varsdf$fields[varsdf$lec_output]
          for (m in leccalcoutputs_g) {
            if (!is.null(losstypeSettingsMapping[[m]]) && !is.null(losstypeSettingsMapping[[m]])) {
              list_leccalcoutputs_g[m] <- losstypeSettingsMapping[[m]]
            }
          }

          list_summary_g$leccalc$return_period_file <- inputsettings$return_period_file
          if (length(list_leccalcoutputs_g) > 0) {
            list_summary_g$leccalc$outputs <- list_leccalcoutputs_g
          }

          list_summary[[counter_id]] <-  list_summary_g
        }
      }
      analysis_settings$analysis_settings[[losssummary]] <- list_summary
    }
  }

  return(analysis_settings)
}

# Input errors file ------------------------------------------------------------

#' Get analysis input_errors file
#'
#' Gets the analysis input_errors_file contents.
#'
#' @rdname api_get_analyses_input_errors_file
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Previously produced analysis input_errors file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_analyses_input_errors_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_errors_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Post analysis input_errors file
#'
#' Sets the analysis input_errors_file contents.
#'
#' @rdname api_post_analyses_input_errors_file
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_input_errors Path to the input_errors file.
#'
#' @return The posted analysis input_errors file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_input_errors_file <- function(id, filepath_input_errors) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input_errors)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_errors_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Analysis input generation ----------------------------------------------------

#' Post analysis input generation
#'
#' Sets the analysis generate_inputs contents.
#'
#' @rdname api_post_analyses_generate_inputs
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return The posted analysis input generation.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#'
#' @export
api_post_analyses_generate_inputs <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(id = id),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "generate_inputs", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

#' Cancel analysis input generation
#'
#' Cancel the input generation process for the analysis.
#'
#' @rdname api_post_analyses_cancel_generate_inputs
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return The cancelled analysis input generation.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#'
#' @export
api_post_analyses_cancel_generate_inputs <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(id = id),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "cancel_generate_inputs", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Input generation traceback file ----------------------------------------------

#' Get analysis input_generation_traceback file
#'
#' Gets the analysis input_generation_traceback_file contents.
#'
#' @rdname api_get_analyses_input_generation_traceback_file
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Previously produced analysis input_generation_traceback file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_analyses_input_generation_traceback_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_generation_traceback_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Post analysis input_generation_traceback file
#'
#' Sets the analysis input_generation_traceback_file contents.
#'
#' @rdname api_post_analyses_input_generation_traceback_file
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_input_generation_traceback Path to the input_generation_traceback file.
#'
#' @return The posted analysis input_generation_traceback file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_input_generation_traceback_file <- function(id, filepath_input_generation_traceback) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input_generation_traceback)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_generation_traceback_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Output file ------------------------------------------------------------------

#' Get analysis output file
#'
#' Gets the analysis output_file contents
#'
#' @rdname api_get_analyses_output_file
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Previously posted analysis output file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr write_disk
#' @importFrom utils untar
#'
#' @export
api_get_analyses_output_file <- function(id) {

  currfolder <- getOption("flamingo.settings.api.share_filepath")
  dest <- file.path(currfolder, paste0(id, "_outputs.tar"))
  extractFolder <- file.path(currfolder, paste0(id, "_output"))
  dir.create(extractFolder, showWarnings = FALSE)

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "output_file", "", sep = "/"),
    write_disk(dest, overwrite = TRUE)
  ))

  response <- api_fetch_response("GET", request_list)

  untar(tarfile = dest, exdir = extractFolder)

  # wait for untar to finish
  oldfileList <- NULL
  while (all.equal(oldfileList, list.files(extractFolder)) != TRUE) {
    oldfileList <- list.files(extractFolder)
    Sys.sleep(2)
  }

  # FIXME: unclear how response relates to the above and what should be returned at all?!
  api_handle_response(response)
}

#' Return analyses output files  Dataframe
#'
#' @rdname return_analyses_output_file_df
#'
#' @description Returns a dataframe of output files.
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Dataframe of output files.
#'
#' @importFrom stats setNames
#'
#' @export
return_analyses_output_file_df <- function(id) {
  currfolder <- getOption("flamingo.settings.api.share_filepath")
  # FIXME: the extractFolder path definition should not be in multiple places
  extractFolder <- file.path(currfolder, paste0(id, "_output/output"))
  if (!file.exists(extractFolder)) {
    # FIXME: this call looks wrong since the function returns something
    api_get_analyses_output_file(id)
  }
  list.files(extractFolder) %>% as.data.frame() %>% setNames("files")
}

#' Return specific analyses output file as dataframe
#'
#' @rdname return_analyses_spec_output_file_df
#'
#' @description Returns a dataframe of a specific output file.
#'
#' @param id A unique integer value identifying this analysis.
#' @param fileName Name of file to read.
#'
#' @return Dataframe of specific output file.
#'
#' @importFrom stats setNames
#' @importFrom data.table fread
#'
#' @export
return_analyses_spec_output_file_df <- function(id, fileName) {
  currfolder <- getOption("flamingo.settings.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_output/output/"))
  filePath <- file.path(currfolder, paste0(id, "_output/output/", fileName))
  info <- file.info(filePath)
  analyses_spec_output_file_df <- NULL
  if (!is.na(info$size) && info$size != 0 ) {
    analyses_spec_output_file_df <- fread(filePath)
  }
  analyses_spec_output_file_df
}

#' Post analysis output file
#'
#' Sets the analysis output_file contents.
#'
#' @rdname api_post_analyses_output_file
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_output Path to the output file.
#'
#' @return The posted analysis output file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_output_file <- function(id, filepath_output) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_output)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "output_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Run traceback file -----------------------------------------------------------
#' Get analysis run_traceback file
#'
#' Gets the analysis run_traceback_file contents.
#'
#' @rdname api_get_analyses_run_traceback_file
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Previously produced analysis run_traceback file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_analyses_run_traceback_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "run_traceback_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Post analysis run_traceback file
#'
#' Sets the analysis run_traceback_file contents.
#'
#' @rdname api_post_analyses_run_traceback_file
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_run_traceback Path to the run_traceback file.
#'
#' @return The posted analysis run_traceback file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_analyses_run_traceback_file <- function(id, filepath_run_traceback) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_run_traceback)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "run_traceback_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Run --------------------------------------------------------------------------
#' Run analyses id
#'
#' Returns the analysis status.
#'
#' @rdname api_post_analyses_run
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Analysis status.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#'
#' @export
api_post_analyses_run <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "run","", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

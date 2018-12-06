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
#' @importFrom httr write_disk
#'
#' @export
api_get_analyses_input_file <- function(id) {
  
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  dest <- file.path(currfolder, paste0(id, "_inputs.tar"))
  extractFolder <- file.path(currfolder, paste0(id, "_inputs"))
  dir.create(extractFolder, showWarnings = FALSE)
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_file", "", sep = "/"),
    write_disk(dest, overwrite = TRUE)
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
  
  untar(tarfile = dest, exdir = extractFolder)
  
  #wait for untar to finish
  oldfileList <- NULL
  while (all.equal(oldfileList, list.files(extractFolder)) != TRUE) {
    oldfileList <- list.files(extractFolder)
    Sys.sleep(2)
  }
}


#' Return analyses input files  Dataframe
#'
#' @rdname return_analyses_input_file_df
#'
#' @description Returns a dataframe of input files 
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return dataframe of input files.
#'
#' @importFrom stats setNames
#'
#' @export

return_analyses_input_file_df <- function(id) {
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_inputs"))
  if (!file.exists(extractFolder)) {
    api_get_analyses_input_file(id)
  }
  analyses_input_file_df <- list.files(extractFolder) %>% as.data.frame() %>% setNames("files")
  return(analyses_input_file_df)
}

#' Return analyses input files  Dataframe with Icons
#'
#' @rdname return_analyses_input_file_wicons_df
#'
#' @description Returns a dataframe of input files with Icons
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return dataframe of input files with Icons.
#'
#' @export

return_analyses_input_file_wicons_df <- function(id) {
  
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_inputs/"))
  
  analyses_input_file_df <- return_analyses_input_file_df(id)
  fnames <- analyses_input_file_df$files
  fnum <- length(fnames)
  status <- data.frame(status = rep(status_code_notfound, fnum))
  for (i in seq(fnum) ) {
    fname <- as.character(fnames[i])
    filePath <- file.path(extractFolder, fname)
    info <- file.info(filePath)
    if (is.na(info$size)) {
      status[i, "status"] <- StatusProcessing
    } else if (info$size == 0) {
      status[i, "status"] <- StatusFailed
    } else {
      status[i, "status"] <- StatusCompleted
    }
  }
  analyses_input_file_df <- cbind(analyses_input_file_df, status) %>%
    as.data.frame()
  return(analyses_input_file_df)
}

#' Return specific analyses input file as Dataframe
#'
#' @rdname return_analyses_spec_input_file_df
#'
#' @description Returns a dataframe of specific input file
#'
#' @param id a unique integer value identifying this analysis.
#' @param fileName name of file to read
#'
#' @return dataframe of specific input file.
#'
#' @importFrom stats setNames
#' @importFrom data.table fread
#'
#' @export

return_analyses_spec_input_file_df <- function(id, fileName) {
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_inputs/"))
  filePath <- file.path(extractFolde, fileName)
  info <- file.info(filePath)
  analyses_spec_input_file_df <- NULL
  if (!is.na(info$size) && info$size != 0 ) {
    analyses_spec_input_file_df <- fread(filePath)
  }
  return(analyses_spec_input_file_df)
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
#' @param inputsettings list of input settings
#' @param outputsLossType list of output configuration
#'
#' @return list of analysis settings.
#'
#' @export
construct_analysis_settings <- function(inputsettings, outputsLossTypes){
  
  .ifnullFALSE <- function(variable) {
    ifelse(is.null(variable), FALSE, variable)
  }
  
  .list_summary <- function(counter_id, oed_g, inputsettings, losstypeSettingsMapping){
    list_summary <- data.frame("uniqueItems" = inputsettings$uniqueItems,
                               "summarycalc" = inputsettings$summarycalc,
                               "aalcalc" = .ifnullFALSE(losstypeSettingsMapping$aalcalc),
                               "eltcalc" = .ifnullFALSE(losstypeSettingsMapping$eltcalc),
                               "pltcalc" = .ifnullFALSE(losstypeSettingsMapping$pltcalc),
                               "id" = counter_id,
                               "oed_fields" = oed_g,
                               "lec_output" = any(losstypeSettingsMapping),
                               stringsAsFactors = FALSE
    )
  }
  
  .leccalc_outputs <- function(losstypeSettingsMapping) {
    leccalcoutputs <- data.frame("full_uncertainty_aep" = .ifnullFALSE(losstypeSettingsMapping$full_uncertainty_aep),
                                 "full_uncertainty_oep" = .ifnullFALSE(losstypeSettingsMapping$full_uncertainty_oep),
                                 "wheatsheaf_aep" = .ifnullFALSE(losstypeSettingsMapping$wheatsheaf_aep),
                                 "wheatsheaf_oep" = .ifnullFALSE(losstypeSettingsMapping$wheatsheaf_oep),
                                 "wheatsheaf_mean_aep" = .ifnullFALSE(losstypeSettingsMapping$wheatsheaf_mean_aep),
                                 "wheatsheaf_mean_oep" = .ifnullFALSE(losstypeSettingsMapping$wheatsheaf_mean_oep),
                                 "sample_mean_aep" = .ifnullFALSE(losstypeSettingsMapping$sample_mean_aep),
                                 "sample_mean_oep" = .ifnullFALSE(losstypeSettingsMapping$sample_mean_oep),
                                 stringsAsFactors = FALSE
    )
  }
  
  analysisSettingsMapping <- list(
    "analysis_tag" = inputsettings$analysis_tag,
    "exposure_location" = inputsettings$exposure_location,
    "gul_threshold" = inputsettings$gul_threshold,
    "model_version_id" = inputsettings$model_version_id,
    "module_supplier_id" = inputsettings$module_supplier_id,
    "number_of_samples" = inputsettings$number_of_samples,
    "prog_id" = inputsettings$prog_id,
    "source_tag" = inputsettings$source_tag
    )
  
  outoutSettingsMappings <- list(
    "gul_output" = inputsettings$gul_output,
    "il_output" = inputsettings$il_output,
    "ri_output" = inputsettings$ri_output
  )
  
  modelSettingsMapping <- list(
    "event_set" = inputsettings$event_set,
    "peril_wind" = inputsettings$peril_wind,
    "demand_surge" = inputsettings$demand_surge,
    "peril_quake" = inputsettings$peril_quake,
    "peril_flood" = inputsettings$peril_flood,
    "peril_surge" = inputsettings$peril_surge,
    "leakage_factor" = inputsettings$leakage_factor,
    "use_random_number_file" = inputsettings$use_random_number_file,
    "event_occurrence_file_id" = inputsettings$event_occurrence_file_id
  )
  
  analysis_settings <- list(
    "analysis_settings" = list(
    )
  )
  
  analysis_settings$model_settings <- list()
  for (i in names(analysisSettingsMapping)) {
    if (!is.null(analysisSettingsMapping[[i]])) {
      analysis_settings$analysis_settings[i] <- analysisSettingsMapping[[i]]
    }
  }
  
  for (j in names(modelSettingsMapping)) {
    if (!is.null(modelSettingsMapping[[j]])){
      analysis_settings$model_settings[j] <- modelSettingsMapping[[j]]
    }
  }
  
  for (l in names(outoutSettingsMappings)) {
    if (!is.null(outoutSettingsMappings[[l]]) && outoutSettingsMappings[[l]]) {
      analysis_settings$analysis_settings[l] <- outoutSettingsMappings[[l]] 
      losstype <- gsub( "_output", "", l)
      losssummary <- paste0(losstype, "_summaries")
      gran <- names(outputsLossTypes[[l]])
      counter_id <- 0
      outputsLossType <- outputsLossTypes[[l]]
      #define empty summary df
      leccalc_outputs <-  data.frame("full_uncertainty_aep" = NULL,
                                     "full_uncertainty_oep" = NULL,
                                     "wheatsheaf_aep" = NULL,
                                     "wheatsheaf_oep" = NULL,
                                     "wheatsheaf_mean_aep" = NULL,
                                     "wheatsheaf_mean_oep" = NULL,
                                     "sample_mean_aep" = NULL,
                                     "sample_mean_oep" = NULL,
                                     stringsAsFactors = FALSE)
      list_summary <- data.frame("uniqueItems" = NULL,
                                 "summarycalc" = NULL,
                                 "aalcalc" = NULL,
                                 "eltcalc" = NULL,
                                 "pltcalc" = NULL,
                                 "id" = NULL,
                                 "oed_fields" = NULL,
                                 "lec_output" = NULL,
                                 stringsAsFactors = FALSE
      )
      #loop over granularities
      for (g in gran) {
        outputsLossTypeGran <- outputsLossType[[g]]
        oed_g <- granToOed$oed[granToOed$outputlosstype == g] # provide here oed field of granularity
        if (!is.null(outputsLossTypeGran)) {
          counter_id <- counter_id + 1
          losstypeSettingsMapping = list()
          for (v in outputsLossTypeGran) {
            losstypeSettingsMapping[varsdf$fields[which(varsdf$vars == v)]] = TRUE
          }
          leccalc_outputs <- rbind(leccalc_outputs, .leccalc_outputs(losstypeSettingsMapping))
          list_summary <- rbind(list_summary, .list_summary(counter_id, oed_g, inputsettings, losstypeSettingsMapping))
        }
      }
      #assemble summary element
      leccal <- data.frame("return_period_file" = rep(inputsettings$return_period_file, counter_id),
                           stringsAsFactors = FALSE)
      leccal$outputs <- leccalc_outputs
      list_summary$leccalc <- leccal
      analysis_settings$analysis_settings[[losssummary]] <- list_summary
    }
  }
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
#' @importFrom httr write_disk
#'
#' @export
api_get_analyses_output_file <- function(id) {
  
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  dest <- file.path(currfolder, paste0(id, "_outputs.tar"))
  extractFolder <- file.path(currfolder, paste0(id, "_output"))
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
  
  #wait for untar to finish
  oldfileList <- NULL
  while (all.equal(oldfileList, list.files(extractFolder)) != TRUE) {
    oldfileList <- list.files(extractFolder)
    Sys.sleep(2)
  }
  
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
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_output/output"))
  if (!file.exists(extractFolder)) {
    api_get_analyses_output_file(id)
  }
  analyses_output_file_df <- list.files(extractFolder) %>% as.data.frame() %>% setNames("files")
  return(analyses_output_file_df)
}

#' Return specific analyses output file as Dataframe
#'
#' @rdname return_analyses_spec_output_file_df
#'
#' @description Returns a dataframe of specific output file
#'
#' @param id a unique integer value identifying this analysis.
#' @param fileName name of file to read
#'
#' @return dataframe of specific output file.
#'
#' @importFrom stats setNames
#' @importFrom data.table fread
#'
#' @export

return_analyses_spec_output_file_df <- function(id, fileName) {
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_output/output/"))
  filePath <- file.path(currfolder, paste0(id, "_output/output/", fileName))
  info <- file.info(filePath)
  analyses_spec_output_file_df <- NULL
  if (!is.na(info$size) && info$size != 0 ) {
    analyses_spec_output_file_df <- fread(filePath)
  }
  return(analyses_spec_output_file_df)
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
  if (length(names(analyses_runList)) > 1) {
    analyses_run_df <- bind_rows(analyses_runList) %>%
      as.data.frame()
  } else {
    analyses_run_df <- NULL
  }

  return(analyses_run_df)
}

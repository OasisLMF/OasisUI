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
#' @importFrom utils untar
#'
#' @export
api_get_analyses_input_file <- function(id) {

  currfolder <- getOption("flamingo.settings.api.share_filepath")
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

  api_handle_response(response)
  # FIXME: where / why is response needed here?!

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
  currfolder <- getOption("flamingo.settings.api.share_filepath")
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

  currfolder <- getOption("flamingo.settings.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, "_inputs/"))
  status_code_notfound <- 404
  
  analyses_input_file_df <- return_analyses_input_file_df(id)
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
  currfolder <- getOption("flamingo.settings.api.share_filepath")
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

  api_handle_response(response)
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

  api_handle_response(response)
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

  api_handle_response(response)
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


#' Construct analysis settings
#'
#' Construct analysis settings
#'
#' @rdname construct_analysis_settings
#'
#' @description Constructs a list of analysis settings
#'
#' @param inputsettings list of input settings
#' @param outputsLossTypes list of output configuration
#'
#' @return list of analysis settings.
#'
#' @export
construct_analysis_settings <- function(inputsettings, outputsLossTypes){

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


  for (l in names(outoutSettingsMappings)) { # l <- names(outoutSettingsMappings)[1]
    losstype <- gsub( "_output", "", l)
    losssummary <- paste0(losstype, "_summaries")
    losstype_output <- outoutSettingsMappings[[l]][["value"]]
    analysis_settings$analysis_settings[l] <- losstype_output
    if (!is.null(losstype_output) && losstype_output) {
      counter_id <- 0
      granularities <- names(outputsLossTypes[[l]])
      outputsLossType <- outputsLossTypes[[l]]
      list_summary <- list()
      for (g in granularities) { #g <- granularities[1]
        outputsLossTypeGran <- outputsLossType[[g]]
        if (!is.null(outputsLossTypeGran)){
          oed_g <- g # provide here oed field of granularity
          counter_id <- counter_id + 1
          losstypeSettingsMapping = list()
          #construct list with all infor
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

  api_handle_response(response)
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

  api_handle_response(response)
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

  api_handle_response(response)
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

  api_handle_response(response)
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

  api_handle_response(response)
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

  api_handle_response(response)
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
#' @importFrom utils untar
#'
#' @export
api_get_analyses_output_file <- function(id) {

  currfolder <- getOption("flamingo.settings.api.share_filepath")
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
  currfolder <- getOption("flamingo.settings.api.share_filepath")
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
  currfolder <- getOption("flamingo.settings.api.share_filepath")
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

  api_handle_response(response)
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

  api_handle_response(response)
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

  api_handle_response(response)
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
  get_analyses_run_traceback_file <- api_get_analyses_run_traceback_file(id)
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

  api_handle_response(response)
}

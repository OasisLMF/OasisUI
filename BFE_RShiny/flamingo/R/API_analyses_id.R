# Input File -------------------------------------------------------------------
#' Return analyses input files dataframe with icons
#'
#' @rdname return_analyses_input_file_wicons_df
#'
#' @description Returns a dataframe of input files with icons.
#'
#' @param id A unique integer value identifying this analysis.
#' @param datahub as stored in session$userData$datahub
#' @param oasisapi as stored in session$userData$oasisapi.
#'
#' @return Dataframe of input files with icons.
#'
#' @importFrom dplyr mutate
#'
#' @export
return_analyses_input_file_wicons_df <- function(id, data_hub, oasisapi) {

  status_code_notfound <- 404

  analyses_input_file_df <- data_hub$get_ana_inputs_data_list(id, oasisapi) %>%
    mutate(status = status_code_notfound) %>%
    as.data.frame()

  if (nrow(analyses_input_file_df) > 0) {
    analyses_input_file_df$status <- sapply(analyses_input_file_df$files, function(fname){
      size <- data_hub$get_ana_dataset_size(id, type = "input", dataset_identifier = fname, oasisapi)
      if (is.na(size)) {
        status <- Status$Processing
      } else if (size == 0) {
        status <- Status$Failed
      } else {
        status <- Status$Completed
      }
      status
    })
  } else{
    analyses_input_file_df <- NULL
  }


  analyses_input_file_df
}

# Settings File ----------------------------------------------------------------

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

  fixed_settings <- c("event_set", "event_occurrence_id")

  analysis_settings_names <- c("analysis_tag", "gul_threshold", "model_version_id",  "module_supplier_id", "number_of_samples", "prog_id", "source_tag")

  analysisSettingsMapping <- lapply(analysis_settings_names, function(p){
    list(
      "path" = "analysis_settings",
      "value" =  inputsettings[[p]]
    )
  }) %>%
    setNames(analysis_settings_names)

  output_settings_names <- c("gul_output", "il_output", "ri_output")
  outoutSettingsMappings <- lapply(output_settings_names, function(p){
    list(
      "path" = "analysis_settings",
      "value" =  inputsettings[[p]]
    )
  }) %>%
    setNames(output_settings_names)

  model_params <- names(inputsettings)[names(inputsettings) %notin% c(analysis_settings_names, output_settings_names)]
  modelSettingsMapping <- lapply(model_params, function(p){
    list(
      "path" = "model_settings",
      "value" =  inputsettings[[p]]
    )
  }) %>%
    setNames(model_params)


  analysis_settings <- list(
    "analysis_settings" = list(
      "model_settings" = list()
    )
  )

  for (i in names(analysisSettingsMapping)) {
    if (!is.null(analysisSettingsMapping[[i]][["value"]])) {
      analysis_settings$analysis_settings[i] <- analysisSettingsMapping[[i]][["value"]]
    }
  }

  for (j in names(modelSettingsMapping)) {
    if (!is.null(modelSettingsMapping[[j]][["value"]])) {
      analysis_settings$analysis_settings$model_settings[j] <- modelSettingsMapping[[j]][["value"]]
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
        if (!is.null(outputsLossTypeGran)) {
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

# Output file ------------------------------------------------------------------

#' Define Extract Folder Path
#'
#' @rdname set_extractFolder
#'
#' @description constructs the path to the folder where to extract files
#'
#' @return extractFolder
#'
#' @param id A unique integer value identifying this analysis.
#' @param label either input or output
#'
#' @export
set_extractFolder <- function(id, label) {
  currfolder <- getOption("flamingo.settings.api.share_filepath")
  extractFolder <- file.path(currfolder, paste0(id, label))
}

#' Define File to extract Path
#'
#' @rdname set_extractFilePath
#'
#' @description constructs the path to the file to extract
#'
#' @return filePath
#'
#' @param extractFolder path to the folder where the file is placed
#' @param fileName name of the file
#'
#' @export
set_extractFilePath <- function(extractFolder, fileName) {
  filePath <- file.path(extractFolder, fileName)
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

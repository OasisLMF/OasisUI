### Data Hub for OasisUI ----
# Description: Set of R6 classes for managing files and files lists in OasisUI

# library(R6)
# library(oasisui)

### R6 Class for OasisUI Data Hub ----

#' DataHub
#'
#' @rdname DataHub
#'
#' @description R6 Class for OasisUI Data Hub.
#'
#' @docType class
#'
#' @return Object of \code{\link{R6Class}} with methods for Oasis files lists and Datatasets explorations.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Arguments:
#' \describe{
#' \item{id}{Portfolio Id | Model Id | Analysis Id}
#' \item{type}{Identifier for Analysis Id: inputs | outputs}
#' \item{dataset_identifier}{Identifier:  file name for Analysis Id |  api call for Portfolio Id and Model Id}
#' }
#'
#' @section Methods:
#' \describe{
#'
#' GENERAL
#'
#' \item{\code{get_user_destdir()}}{return path of user specific foder}
#' \item{\code{oed_peril_codes()}}{return mapping of OED Peril names and codes.}
#' \item{\code{get_oed_summary_levels()}}{return list of oed summary levels by file.}
#' \item{\code{get_oed_columns_list()}}{return list of oed columns by files.}
#'
#' LISTS
#'
#' > Portfolio
#' \item{\code{get_pf_data_list(id)}}{return list of portfolio source files}
#' \item{\code{invalidate_pf_data_list(id)}}{invalidate list of portfolio source files}
#' > Models
#' \item{\code{get_model_data_list(id)}}{return lists of model resources}
#' \item{\code{get_model_hazard_data_list(id)}}{return lists of model hazard resources}
#' > Analysis
#' \item{\code{get_ana_inputs_data_list(id)}}{return list of analysis inputs}
#' \item{\code{get_ana_outputs_data_list(id)}}{return list of analysis outputs}
#' \item{\code{invalidate_ana_inputs_data_list(id)}}{invalidate list of analysis inputs}
#' \item{\code{invalidate_ana_outputs_data_list(id)}}{invalidate list of analysis outputs}
#'
#' DATASETS
#'
#' > Portfolio
#' \item{\code{get_pf_dataset_content(id, dataset_identifier)}}{extract a source file (location/account...) content given a portfolio id}
#' \item{\code{invalidate_pf_dataset_content(id, dataset_identifier)}}{invalidate a source file (location/account...) content given a portfolio id}
#' \item{\code{get_pf_location_content(id)}}{extract location source file content given a portfolio id}
#' \item{\code{invalidate_pf_location_content(id)}}{invalidate a source file (location/account...) content given a portfolio id}
#' \item{\code{get_pf_dataset_header(id, dataset_identifier)}}{extract a source file (location/account...) header given a portfolio id}
#' \item{\code{invalidate_pf_dataset_header(id, dataset_identifier)}}{invalidate a source file (location/account...) header given a portfolio id}
#' \item{\code{get_pf_dataset_nrow(id, dataset_identifier)}}{extract a source file (location/account...) nrow given a portfolio id}
#' \item{\code{invalidate_pf_dataset_nrow(id, dataset_identifier)}}{invalidate a source file (location/account...) header given a portfolio id}
#' > Model
#' \item{\code{get_model_resource_dataset_content(id)}}{extract model resource file given model id}
#' \item{\code{invalidate_model_resource_dataset_content(id)}}{invalidate model resource file given model id}
#' \item{\code{get_model_hazard_dataset_content(id)}}{extract model hazard resource file content given file id}
#' \item{\code{invalidate_model_hazard_dataset_content(id)}}{invalidate model hazard resource file content given file id}
#' > Analysis
#' \item{\code{get_ana_dataset_content(id, dataset_identifier, type)}}{extract a input/output file content given an analysis id}
#' \item{\code{invalidate_ana_dataset_content(id, dataset_identifier, type)}}{invalidate a input/output file content given an analysis id}
#' \item{\code{get_ana_inputs_dataset_content(id, dataset_identifier)}}{extract a inputs file content given an analysis id}
#' \item{\code{invalidate_ana_inputs_dataset_content(id,  dataset_identifier)}}{invalidate a inputs file content given an analysis id}
#' \item{\code{get_ana_outputs_dataset_content(id, dataset_identifier)}}{extract a outputs file content given an analysis id}
#' \item{\code{invalidate_ana_outputs_dataset_content(id,  dataset_identifier)}}{invalidate a outputs file content given an analysis id}
#' \item{\code{get_ana_dataset_header(id, type, dataset_identifier)}}{extract a inputs/outputs file nrow given an analysis id}
#' \item{\code{invalidate_ana_dataset_header(id, type, dataset_identifier)}}{invalidate a inputs/outputs file header given an analysis id}
#' \item{\code{get_ana_dataset_nrow(id, type, dataset_identifier)}}{extract a inputs/outputs file nrow given an analysis id}
#' \item{\code{invalidate_ana_dataset_nrow(id, type, dataset_identifier)}}{invalidate a inputs/outputs file nrow given an analysis id}
#' \item{\code{get_ana_dataset_size(id, type, dataset_identifier)}}{extract a inputs/outputs file size given an analysis id}
#' \item{\code{invalidate_ana_dataset_size(id, type, dataset_identifier)}}{invalidate a inputs/outputs file size given an analysis id}
#' \item{\code{get_ana_settings_content(id)}}{extract analysis settings content}
#' \item{\code{invalidate_ana_settings_content(id)}}{invalidate analysis settings content}
#' \item{\code{get_ana_validation_summary_content(id)}}{extract analysis validation summary content}
#' \item{\code{invalidate_ana_validation_summary_content(id)}}{invalidate analysis validation summary content}
#' > Write file
#' \item{\code{write_file()}}{Write data into a file file_towrite}
#' > Helper methods ----
#' \item{\code{get_analyses_tar(id, label, destdir = tempdir())}}{Extract input/output tar.}
#' \item{\code{get_analyses_inputs_tar(id, destdir = tempdir())}}{Extract input tar.}
#' \item{\code{get_analyses_outputs_tar(id, destdir = tempdir())}}{Extract output tar.}
#' > Return manipulated tables methods ----
#' # Portfolios
#' \item{\code{return_tbl_portfolioDetails(id)}}{Return dataframe of portfolio details.}
#' \item{\code{return_tbl_portfoliosData (name = "", Status, tbl_portfoliosDataNames)}}{Return data frame of portfolio data.}
#' # Models
#' \item{\code{return_tbl_modelsData(supplier_id = "", tbl_modelsDataNames)}}{Return data frame of models.}
#' \item{\code{return_tbl_modelData(id)}}{Return data frame of model data.}
#' # Analyses
#' \item{\code{return_analyses_input_file_wicons_df(id, Status)}}{Return analysis table with icons.}
#' \item{\code{return_tbl_analysesData_nice(tbl_analysesData, admin_mode, Status, tbl_modelsDataNames, tbl_portfoliosDataNames, tbl_analysesDataNames)}}{Return analysis table prettified.}
#' \item{\code{return_tbl_analysesData(name = "", Status, tbl_analysesDataNames)}}{Return dataframe of analyses.}
#' }
#'
#' @section Usage:
#' \preformatted{data_hub <- DataHub$new()
#' data_hub$get_pf_data_list(id)
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom utils untar
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr sym
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom httr content
#' @importFrom tidyr unite
#' @importFrom tidyr separate
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#'
#' @export

DataHub <- R6Class(
  "DataHub",
  # Private ----
  private = list(
    user = "", # string identifying the user
    user_destdir = "", # user specific destdir
    oasisapi = NULL # oasisapi as stored in session$userData$oasisapi
  ),
  public = list(
    # Initialize ----
    initialize = function(user, destdir = tempdir(), oasisapi){
      private$user <- user
      private$user_destdir <- file.path(destdir)
      # if (dir.exists(private$user_destdir)) stop()
      dir.create(private$user_destdir, showWarnings = FALSE, recursive = TRUE)
      private$oasisapi <- oasisapi
    },
    # Terminate ----
    terminate = function(){
      unlink(private$user_destdir, TRUE)
    },
    # GENERAL ----
    get_user_destdir = function(){
      private$user_destdir
    },
    get_oed_peril_codes_mapping = function(){
      oed_perils_codes_mapping <- content(private$oasisapi$api_basic_query(query_path = "oed_peril_codes", query_method = "GET")$result)
      oed_perils_codes_mapping$peril_codes
    },
    get_oed_summary_levels = function(){
      oed_summary_levels_lst <- content(private$oasisapi$api_basic_query(query_path = "oed_summary_levels", query_method = "GET")$result)
    },
    get_oed_columns_list = function(){
      oed_columns_lst <- content(private$oasisapi$api_basic_query(query_path = "oed_columns_list", query_method = "GET")$result)
    },
    # LISTS ----
    # > Portfolio ----
    #return list of portfolio source files
    get_pf_data_list = function(id, ...){
      data_list <- self$return_tbl_portfolioDetails(id) #assuming id !is.null(id) && id != "" && id != -1 && !is.na(id)
      data_list
    },
    #invalidate list of portfolio source files
    invalidate_pf_data_list = function(id, ...){
      invisible()
    },
    # > Models -----
    get_model_data_list = function(id, ...){
      full_lst <- content(private$oasisapi$api_get_query(query_path = paste("models", id,"data_files", sep = "/"))$result)
      full_lst
    },
    get_model_hazard_data_list = function(id, ...) {
      full_lst <- self$get_model_data_list(id)
      #remove nulls
      non_null_full_lst <- lapply(full_lst, Filter, f = Negate(is.null))
      non_null_full_lst <-  Filter(Negate(is.null), non_null_full_lst)
      full_data_df <- lapply(seq(length(non_null_full_lst)), function(i){
        non_null_full_lst[[i]] %>%
          as.data.frame(stringsAsFactors = FALSE)
      })
      hazard_data_df <- NULL
      if (!is.null(full_data_df)) {
        full_data_df <- full_data_df %>%
          bind_rows()
        # hazard files are recognized by the extension
        hazard_data_df <- full_data_df %>%
          filter( grepl("geojson", filename))
      }
      hazard_data_df
    },
    # > Analysis ----
    #return list of analysis resources
    get_ana_inputs_data_list = function(id, ...){
      tarfile <- self$get_analyses_inputs_tar(id,  destdir = private$user_destdir)
      data_list <- untar_list(tarfile)
      if (length(data_list) > 0) {
        data_list <- data_list %>%
          as.data.frame() %>%
          setNames("files")
      } else {
        data_list <- NULL
      }
      data_list
    },
    get_ana_outputs_data_list = function(id, ...){
      tarfile <- self$get_analyses_outputs_tar(id, destdir =  private$user_destdir)
      data_list <- NULL
      if (file.exists(tarfile)) {
        data_list <- untar_list(tarfile, to_strip = "output")
        if (length(data_list) > 0) {
          data_list <- data_list %>%
            as.data.frame() %>%
            setNames("files")
          analysis_settings <- self$get_ana_settings_content(id)
          data_list <- cbind(data_list,
                             do.call(rbind.data.frame,
                                     lapply(data_list$files,
                                            .addDescription, analysis_settings)))
        }
      }
      data_list
    },
    #invalidate list of analysis resources
    invalidate_ana_inputs_data_list = function(id, ...){
      invisible()
    },
    invalidate_ana_outputs_data_list = function(id, ...){
      invisible()
    },

    # DATASETS ----
    # > PORTFLIO METHODS ----
    #extract a source file (location/account...) content given a portfolio id
    #dataset_identifier is location/account/reinsurance_info/reinsurance_source
    #If file does not exists returns df details: Not Found
    get_pf_dataset_content = function(id, dataset_identifier, ...){
      dataset_content <- content(private$oasisapi$api_get_query(paste("portfolios", id, dataset_identifier, sep = "/"))$result)
      if (is.null(names(dataset_content))) {
        dataset_content <- strsplit(dataset_content, split = "\n") %>%
          as.data.frame(stringsAsFactors = FALSE)
        colnames(dataset_content) <- dataset_content[1, ]
      } else {
        dataset_content <- bind_rows(dataset_content) %>%
          as.data.frame()
      }
      dataset_content
    },
    #invalidate a source file (location/account...) content given a portfolio id
    #dataset_identifier is location/account/reinsurance_info/reinsurance_source
    invalidate_pf_dataset_content = function(id, dataset_identifier, ...){
      self$invalidate_pf_dataset_header(id, dataset_identifier, ...)
      self$invalidate_pf_dataset_nrow(id, dataset_identifier, ...)
      invisible()
    },
    #extract location source file content given a portfolio id
    get_pf_location_content = function(id, ...){
      dataset_content <-  self$get_pf_dataset_content(id, dataset_identifier = "location_file", ...)
      dataset_content
    },
    #invalidate a source file (location/account...) content given a portfolio id
    #dataset_identifier is location/account/reinsurance_info/reinsurance_source
    invalidate_pf_location_content = function(id, dataset_identifier, ...){
      invisible()
    },
    #extract a source file (location/account...) header given a portfolio id
    get_pf_dataset_header = function(id, dataset_identifier, ...){
      dataset_content <- self$get_pf_dataset_content(id, dataset_identifier, ...)
      dataset_header <- names(dataset_content)
      dataset_header
    },
    #invalidate a source file (location/account...) header given a portfolio id
    invalidate_pf_dataset_header = function(id, dataset_identifier, ...){
      invisible()
    },
    #extract a source file (location/account...) nrow given a portfolio id
    get_pf_dataset_nrow = function(id, dataset_identifier, ...){
      dataset_content <- self$get_pf_dataset_content(id, dataset_identifier, ...)
      dataset_nrow <- nrow(dataset_content)
      dataset_nrow
    },
    #invalidate a source file (location/account...) header given a portfolio id
    invalidate_pf_dataset_nrow = function(id, dataset_identifier, ...){
      invisible()
    },

    # > MODEL METHODS ----
    #extract model resource file given model id
    get_model_resource_dataset_content = function(id, ...){
      #retrieve model resource file from API
      get_response <- private$oasisapi$api_get_query(query_path = paste( "models", id, "resource_file", sep = "/"))
      modelsIdResourceFileList <- content(get_response$result)
      if (!is.null(modelsIdResourceFileList)) {
        modelsList_names <- names(modelsIdResourceFileList)
        return_list <- lapply(modelsList_names, function(i){
          #extract sublist
          curr_list <- unlist(unlist(modelsIdResourceFileList[i], recursive = FALSE, use.names = FALSE), recursive = FALSE)
          # get names of sub lists
          lst_names <- lapply(curr_list, names)
          # find sub-lists names that are common to all sublists
          colsResources <- Reduce(intersect, lst_names)
          # find must-haves columns which are missing
          colsmust <- setdiff(c("values", "name"), colsResources)
          #initialise dataframe to have the common names plus must-haves as columns
          if (length(colsmust) > 0) {
            df <- data.frame(matrix(ncol = length(colsResources) + length(colsmust), nrow = 0), stringsAsFactors = FALSE)
            names(df) <- c(colsResources, colsmust)
          } else {
            df <- data.frame(matrix(ncol = length(colsResources), nrow = 0), stringsAsFactors = FALSE)
            names(df) <- colsResources
          }
          #construct dataframe
          for (j in names(curr_list)) {
            curr_df <- unlist(curr_list[[j]]) %>%
              bind_rows() %>%
              as.data.frame(stringsAsFactors = FALSE)
            #identify columns to be merged together
            cols2merge <- setdiff(names(curr_df), colsResources)
            #identify missing columns for binding curr_df with df
            colmissing <- setdiff(unique(c(colsmust,colsResources)), names(curr_df))
            if (length(cols2merge) > 0) {
              curr_df <- curr_df %>%
                unite(col = "values", c(cols2merge), sep = ", " )
            } else if (length(colmissing) > 0 ) {
              df2add <- c("-")
              names(df2add) <- colmissing
              curr_df <- c(curr_df, df2add) %>% as.data.frame()
            }
            # make sure the column names is filled in
            if ("name" %notin% names(curr_df)) {
              curr_df <- cbind(curr_df, data.frame(name = j))
            }
            df <- rbind(df, curr_df)
          }
          df
        })
        names(return_list) <- modelsList_names
      } else {
        return_list <- NULL
      }
      return_list
    },
    #invalidate model resource file given model id
    invalidate_model_resource_dataset_content = function(id, ...){
      invisible()
    },
    #extract model hazard resource file given model id
    get_model_hazard_dataset_content = function(id,filename, ...){
      # #currently no api function
      # path <- system.file("app", "www", "hazard_files", dataset_identifier, package = "oasisui")
      # mapfile <- geojsonio::geojson_read(path, what = "sp") #SLOW!
      # mapfile
      mapfile_content <- NULL
      if (!is.null(id)) {
        mapfile <- private$oasisapi$api_return_query_res(query_path = paste("data_files", id, "content", sep = "/"), query_method = "GET")
        path_mapfile <- file.path(private$user_destdir,filename)
        readr::write_file(mapfile, path_mapfile)
        mapfile_content <- geojsonio::geojson_read(path_mapfile, what = "sp")
      }
      mapfile_content
    },
    #invalidate model hazard resource file given model id
    invalidate_model_hazard_dataset_content = function(id,  ...){
      invisible()
    },

    # > ANALYSIS METHODS ----
    #extract a input/output file given an analysis id
    get_ana_dataset_content = function(id, dataset_identifier, type = "", ...){
      if (type == "input") {
        dataset_content <- self$get_ana_inputs_dataset_content(id, dataset_identifier)
      } else if (type == "output") {
        dataset_content <- self$get_ana_outputs_dataset_content(id, dataset_identifier)
      } else {
        query_response <- private$oasisapi$api_get_query(query_path = paste("analyses", id, dataset_identifier,  sep = "/"))
        dataset_content <- content(query_response$result)
      }
      dataset_content
    },
    #invalidate a input/output file given an analysis id
    invalidate_ana_dataset_content = function(id, dataset_identifier, type, ...){
      self$invalidate_ana_dataset_header(id, dataset_identifier, type, ...)
      self$invalidate_ana_dataset_row(id, dataset_identifier, type, ...)
      invisible()
    },
    #extract a inputs file content given an analysis id
    get_ana_inputs_dataset_content = function(id, dataset_identifier, ...){
      tarfile <- self$get_analyses_inputs_tar(id, destdir =  private$user_destdir)
      dataset_content = NULL
      if (file.exists(tarfile)) {
        dataset_content <- read_file_from_tar(tarfile, dataset_identifier, destdir =  private$user_destdir)
      }
      dataset_content
    },
    #invalidate a inputs file content given an analysis id
    invalidate_ana_inputs_dataset_content = function(id, dataset_identifier, ...){
      self$invalidate_ana_dataset_header(id, dataset_identifier, type = "input", ...)
      self$invalidate_ana_dataset_row(id, dataset_identifier, type = "input", ...)
      invisible()
    },
    #extract a outputs file content given an analysis id
    get_ana_outputs_dataset_content = function(id, dataset_identifier, ...){
      tarfile <- self$get_analyses_outputs_tar(id, destdir =  private$user_destdir)
      #necessary step because outputs comes with subfolder
      dataset_identifier <- file.path("output", dataset_identifier)
      dataset_content = NULL
      if (file.exists(tarfile)) {
        dataset_content <- read_file_from_tar(tarfile, dataset_identifier, destdir =  private$user_destdir)
      }
      dataset_content
    },
    #invalidate a outputs file content given an analysis id
    invalidate_ana_outputs_dataset_content = function(id, dataset_identifier, ...){
      self$invalidate_ana_dataset_header(id, dataset_identifier, type = "output", ...)
      self$invalidate_ana_dataset_row(id, dataset_identifier, type = "output", ...)
      invisible()
    },
    #extract a inputs/outputs file header given an analysis id
    get_ana_dataset_header = function(id, type, dataset_identifier, ...){
      tarfile <- self$get_analyses_tar(id, label = type, destdir =  private$user_destdir)
      #necessary step because outputs comes with subfolder
      if (any(grepl(paste0(type, "/"),  untar(tarfile, list = TRUE)))) {
        dataset_identifier <- file.path(type, dataset_identifier)
      }
      dataset_header <- read_file_from_tar(tarfile, dataset_identifier, nrows = 1, destdir = private$user_destdir) %>%
        names()
      dataset_header
    },
    #invalidate a inputs/outputs file header given an analysis id
    invalidate_ana_dataset_header = function(id, type, dataset_identifier, ...){
      invisible()
    },
    #extract a inputs/outputs file nrow given an analysis id
    get_ana_dataset_nrow = function(id, type, dataset_identifier, ...){
      destdir =   private$user_destdir
      tarfile <- self$get_analyses_tar(id, label = type, destdir = destdir)
      if (any(grepl(paste0(type, "/"),  untar(tarfile, list = TRUE)))) {
        dataset_identifier <- file.path(type, dataset_identifier)
      }
      untar(tarfile, files = dataset_identifier, exdir = destdir)
      dataset_nrow <- read.table(pipe(paste0("wc -l ", file.path(destdir, dataset_identifier))))[[1]] - 1 # Remove header
      file.remove(file.path(destdir, dataset_identifier))
      dataset_nrow
    },
    #invalidate a inputs/outputs file nrow given an analysis id
    invalidate_ana_dataset_nrow = function(id, type, dataset_identifier, ...){
      invisible()
    },
    #extract a inputs/outputs file size given an analysis id
    get_ana_dataset_size = function(id, type, dataset_identifier,...){
      destdir =   private$user_destdir
      tarfile <- self$get_analyses_tar(id, label = type, destdir = destdir)
      if (any(grepl(paste0(type, "/"),  untar(tarfile, list = TRUE)))) {
        dataset_identifier <- file.path(type, dataset_identifier)
      }
      untar(tarfile, files = dataset_identifier, exdir = destdir)
      dataset_size <- file.size(file.path(destdir, dataset_identifier))
      file.remove(file.path(destdir, dataset_identifier))
      dataset_size
    },
    #invalidate a inputs/outputs file size given an analysis id
    invalidate_ana_dataset_size = function(id, type, dataset_identifier, ...){
      invisible()
    },
    #extract analysis settings content
    get_ana_settings_content = function(id,  ...){
      dataset_content <- private$oasisapi$api_get_query(query_path = paste("analyses", id, "settings_file",  sep = "/"))
      content(dataset_content$result)
    },
    #invalidate analysis settings content
    invalidate_ana_settings_content = function(id, ...){
      invisible()
    },
    #extract analysis validation summary content
    get_ana_validation_summary_content = function(id,  ...){
      query_response <- private$oasisapi$api_get_query(query_path = paste("analyses", id, "lookup_validation_file",  sep = "/"))
      json_lst <- content(query_response$result)
      dataset_content <- NULL
      if (!is.null(json_lst)) {
        dataset_content <- unlist(json_lst) %>%
          as.data.frame(stringsAsFactors = FALSE)  %>%
          setNames("vals") %>%
          mutate(rowname = rownames(.)) %>%
          separate(col = rowname, into = c("peril", "key", "type", "type2"), sep = "\\.") %>%
          mutate(type2 = case_when(
            is.na(type2) ~ "",
            TRUE ~ paste0(": ", type2)
          )) %>%
          unite("type", c("type", "type2"), sep = "") %>%
          mutate(type = gsub(pattern = "_", replacement = " ", type)) %>%
          spread(key, 1, convert = TRUE)
      }
      dataset_content
    },
    #invalidate analysis validation summary content
    invalidate_ana_validation_summary_content = function(id, dataset_identifier, type, ...){
      invisible()
      self$invalidate_ana_dataset_header(id, dataset_identifier, type, ...)
      self$invalidate_ana_dataset_row(id, dataset_identifier, type, ...)
    },
    # > Write file ----
    write_file = function(data, dataset_identifier, file_towrite = NULL, ...){
      fs <- writefile(data, dataset_identifier, destdir = private$user_destdir, file_towrite)
      fs
    },
    # > Helper methods ----
    get_analyses_tar = function(id, label, destdir = tempdir()) {
      dest <- tempfile(tmpdir = destdir, fileext = ".tar")
      response <- private$oasisapi$api_get_analyses_tar(id, paste0(label, "_file"), dest)
      dest
    },
    get_analyses_inputs_tar = function(id, destdir = tempdir()) {
      self$get_analyses_tar(id, label = "input", destdir)
    },
    get_analyses_outputs_tar = function(id, destdir = tempdir()) {
      self$get_analyses_tar(id, label = "output", destdir)
    },
    # > Return manipulated tables methods ----
    # Portfolios
    return_tbl_portfolioDetails = function(id) {
      tbl_portfolioDetails <- private$oasisapi$return_df(paste("portfolios", id,  sep = "/"))
      if (!is.null(tbl_portfolioDetails) && is.null(tbl_portfolioDetails$detail)) {
        tbl_portfolioDetails <- tbl_portfolioDetails %>%
          select(contains("file")) %>%
          as.data.frame()
        # reshape df
        tbl_portfolioDetails <- gather(tbl_portfolioDetails,  key = "files", value = "name") %>%
          as.data.frame()
      }
      tbl_portfolioDetails
    },
    return_tbl_portfoliosData = function(name = "", Status, tbl_portfoliosDataNames) {
      tbl_portfoliosData <-  private$oasisapi$return_df("portfolios",  api_param = list(name = name))
      if (!is.null(tbl_portfoliosData) && nrow(tbl_portfoliosData) > 0 && is.null(tbl_portfoliosData$detail)) {
        tbl_portfoliosData <- cbind(tbl_portfoliosData, data.frame(status = ifelse(tbl_portfoliosData$location_file == "Not Available", Status$Processing, Status$Completed)))
        tbl_portfoliosData <- convert_created_modified(tbl_portfoliosData)
        tbl_portfoliosDetailsStatus <- tbl_portfoliosData  %>%
          select(-contains("file") ) %>%
          arrange(desc(!! sym(tbl_portfoliosDataNames$id))) %>%
          as.data.frame()
      } else {
        tbl_portfoliosDetailsStatus <- NULL
      }
      tbl_portfoliosDetailsStatus
    },
    # Models
    return_tbl_modelsData = function(supplier_id = "", tbl_modelsDataNames) {
      tbl_modelsData <-  private$oasisapi$return_df(query_path = "models", api_param = list(`supplier_id` = supplier_id))
      if (!is.null(tbl_modelsData) && nrow(tbl_modelsData) > 0 && is.null(tbl_modelsData$detail)) {
        tbl_modelsData <- convert_created_modified(tbl_modelsData)
        tbl_modelsData <- tbl_modelsData %>%
          arrange(desc(!! sym(tbl_modelsDataNames$id)))
      } else {
        tbl_modelsData <- NULL
      }
      tbl_modelsData
    },
    return_tbl_modelData = function(id) {
      tbl_modelData <-  private$oasisapi$return_df(paste("models", id,  sep = "/"))
      if (!is.null(tbl_modelData) && nrow(tbl_modelData) > 0  && is.null(tbl_modelData$detail)) {
        tbl_modelData <- convert_created_modified(tbl_modelData)
      } else {
        tbl_modelData <- NULL
      }
      tbl_modelData
    },
    # Analyses
    return_analyses_input_file_wicons_df = function(id, Status) {
      status_code_notfound <- 404
      analyses_input_file_df <- self$get_ana_inputs_data_list(id) %>%
        mutate(status = status_code_notfound) %>%
        as.data.frame()
      if (nrow(analyses_input_file_df) > 0) {
        analyses_input_file_df$status <- sapply(analyses_input_file_df$files, function(fname){
          size <- self$get_ana_dataset_size(id, type = "input", dataset_identifier = fname)
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
    },
    return_tbl_analysesData_nice = function(tbl_analysesData, admin_mode, Status, tbl_modelsDataNames, tbl_portfoliosDataNames, tbl_analysesDataNames) {
      # fetch model data to merge in table
      tbl_modelsData <- self$return_tbl_modelsData(tbl_modelsDataNames = tbl_modelsDataNames) %>%
        mutate(supplier = !! sym(tbl_modelsDataNames$supplier_id)) %>%
        select(!! sym(tbl_modelsDataNames$id), !! sym(tbl_modelsDataNames$model_id), supplier, !! sym(tbl_modelsDataNames$version_id))
      # fetch portfolio data to merge in table
      tbl_portfoliosData <-self$return_tbl_portfoliosData(Status = Status, tbl_portfoliosDataNames = tbl_portfoliosDataNames) %>%
        select(!! sym(tbl_portfoliosDataNames$id), !! sym(tbl_portfoliosDataNames$name)) %>%
        rename("portfolio_name" = tbl_portfoliosDataNames$name)
      tbl_analysesData <- tbl_analysesData %>%
        left_join(tbl_modelsData, by = c("model" = "id")) %>%
        unite("model_version", c(tbl_modelsDataNames$model_id, tbl_modelsDataNames$version_id), sep = ", version ") %>%
        left_join(tbl_portfoliosData, by = c("portfolio" = "id"))
      if (admin_mode == "admin") {
        tbl_analysesData <- tbl_analysesData %>%
          select(!! sym(tbl_analysesDataNames$id),
                 !! sym(tbl_analysesDataNames$name),
                 !! sym(tbl_analysesDataNames$portfolio),
                 portfolio_name,
                 !! sym(tbl_analysesDataNames$model),
                 model_version,
                 supplier,
                 !! sym(tbl_analysesDataNames$created),
                 !! sym(tbl_analysesDataNames$modified),
                 !! sym(tbl_analysesDataNames$status_detailed),
                 !! sym(tbl_analysesDataNames$status)) %>%
          rename("portfolio_id" = tbl_analysesDataNames$portfolio) %>%
          rename("model_id" = tbl_analysesDataNames$model)
      } else {
        tbl_analysesData <- tbl_analysesData %>%
          select(!! sym(tbl_analysesDataNames$id),
                 !! sym(tbl_analysesDataNames$name),
                 portfolio_name,
                 model_version,
                 supplier,
                 !! sym(tbl_analysesDataNames$created),
                 !! sym(tbl_analysesDataNames$modified),
                 !! sym(tbl_analysesDataNames$status_detailed),
                 !! sym(tbl_analysesDataNames$status))
      }
      tbl_analysesData <- tbl_analysesData %>%
        capitalize_names_df()
      tbl_analysesData
    },
    return_tbl_analysesData = function(name = "", Status, tbl_analysesDataNames) {
      tbl_analysesData <- private$oasisapi$return_df("analyses", list(name = name))
      if (!is.null(tbl_analysesData) && nrow(tbl_analysesData) > 0 && is.null(tbl_analysesData$detail)) {
        tbl_analysesData <- tbl_analysesData %>%
          select(-contains("file")) %>%
          as.data.frame()
        tbl_analysesData <- convert_created_modified(tbl_analysesData)
        tbl_analysesData <- tbl_analysesData %>%
          mutate(status_detailed = tolower(gsub(pattern = "_", " ", tbl_analysesData[, tbl_analysesDataNames$status]))) %>%
          arrange(desc(!! sym(tbl_analysesDataNames$id))) %>%
          .addIcons(Status = Status) %>%
          select(c(!! sym(tbl_analysesDataNames$id), !! sym(tbl_analysesDataNames$name),
                   !! sym(tbl_analysesDataNames$portfolio), !! sym(tbl_analysesDataNames$model),
                   !! sym(tbl_analysesDataNames$modified), !! sym(tbl_analysesDataNames$created),
                   !! sym(tbl_analysesDataNames$status_detailed), !! sym(tbl_analysesDataNames$status)))

      } else {
        tbl_analysesData <- NULL
      }
      tbl_analysesData
    }
  )
)

# Helper functions -----

#Add descritption fields to outputs files
.addDescription <- function(x, analysis_settings){
  x <- as.character(x)
  x <- strsplit(x, split = "[.]")[[1]][1]
  y <- unlist(strsplit(x, split = "_"))
  report <-  paste(y[3:(length(y))], collapse = "_")
  g_idx <- as.integer(gsub("S", "", y[2]))
  g_oed <- analysis_settings[["analysis_settings"]][[paste0(y[1], "_summaries")]][[g_idx]][["oed_fields"]]
  if (is.null(g_oed)) {
    g_oed <- granToOed$oed[granToOed$order][g_idx]
  }
  g <- granToOed[granToOed$oed == g_oed, "gran"]
  z <- data.frame("perspective" = y[1], "summary_level" = toString(g), "report" = reportToVar(varsdf)[[ report ]], stringsAsFactors = FALSE)
}

.addIcons <- function(df, Status) {
  StatusGood <- "RUN_COMPLETED"
  StatusBad <- c("INPUTS_GENERATION_ERROR", "RUN_ERROR", NA_character_)
  StatusAvailable <- "READY"
  # replace status in df
  if (!is.null(df)) {
    logMessage(paste0("replacing icons"))
    df <- df %>%
      mutate(status = case_when(status %in% StatusGood ~ Status$Completed,
                                status %in% StatusBad ~ Status$Failed,
                                status %in% StatusAvailable ~ Status$Ready,
                                status %notin% c(StatusBad, StatusGood, StatusAvailable) ~ Status$Processing)) %>%
      as.data.frame()
  }
  df
}

### Data Hub for OasisUI ----
# Description: Set of R6 classes for managing files and files lists in OasisUI

# library(R6)
# library(flamingo)

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
#' @section Privates:
#' \describe{
#' \item{user}{string identifying the user.}
#' \item{destdir = tempdir()}{user specific destdir.}
#' }
#'
#' @section Methods:
#' \describe{
#'
#' Initialize
#' \item{\code{create_destdir(user, destir)}}{create and return path of user specific foder}
#' \item{\code{get_user_destdir()}}{return path of user specific foder}
#'
#' LISTS
#'
#' > Portfolio
#' \item{\code{get_pf_data_list(id)}}{return list of portfolio source files}
#' \item{\code{invalidate_pf_data_list(id)}}{invalidate list of portfolio source files}
#' > Model
#' \item{\code{get_model_data_list(id)}}{return list of model resources}
#' \item{\code{invalidate_model_data_list(id)}}{invalidate list of model resources}
#' > Analysis
#' \item{\code{get_ana_inputs_data_list(id, oasisapi)}}{return list of analysis inputs}
#' \item{\code{get_ana_outputs_data_list(id, oasisapi)}}{return list of analysis outputs}
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
#' \item{\code{get_model_hazard_dataset_content(id)}}{extract model hazard resource file given model id}
#' \item{\code{invalidate_model_hazard_dataset_content(id)}}{invalidate model hazard resource file given model id}
#' > Analysis
#' \item{\code{get_ana_dataset_content(id, dataset_identifier, type)}}{extract a input/output file content given an analysis id}
#' \item{\code{invalidate_ana_dataset_content(id, dataset_identifier, type)}}{invalidate a input/output file content given an analysis id}
#' \item{\code{get_ana_inputs_dataset_content(id, dataset_identifier), oasisapi}}{extract a inputs file content given an analysis id}
#' \item{\code{invalidate_ana_inputs_dataset_content(id,  dataset_identifier)}}{invalidate a inputs file content given an analysis id}
#' \item{\code{get_ana_outputs_dataset_content(id, dataset_identifier, oasisapi)}}{extract a outputs file content given an analysis id}
#' \item{\code{invalidate_ana_outputs_dataset_content(id,  dataset_identifier)}}{invalidate a outputs file content given an analysis id}
#' \item{\code{get_ana_dataset_header(id, type, dataset_identifier, oasisapi)}}{extract a inputs/outputs file nrow given an analysis id}
#' \item{\code{invalidate_ana_dataset_header(id, type, dataset_identifier)}}{invalidate a inputs/outputs file header given an analysis id}
#' \item{\code{get_ana_dataset_nrow(id, type, dataset_identifier, oasisapi)}}{extract a inputs/outputs file nrow given an analysis id}
#' \item{\code{invalidate_ana_dataset_nrow(id, type, dataset_identifier)}}{invalidate a inputs/outputs file nrow given an analysis id}
#' \item{\code{get_ana_dataset_size(id, type, dataset_identifier, oasisapi)}}{extract a inputs/outputs file size given an analysis id}
#' \item{\code{invalidate_ana_dataset_size(id, type, dataset_identifier)}}{invalidate a inputs/outputs file size given an analysis id}
#' \item{\code{get_ana_settings_content(id, oasisapi)}}{extract analysis settings content}
#' \item{\code{invalidate_ana_settings_content(id)}}{invalidate analysis settings content}
#' \item{\code{get_ana_validation_summary_content(id, oasisapi)}}{extract analysis validation summary content}
#' \item{\code{invalidate_ana_validation_summary_content(id)}}{invalidate analysis validation summary content}
#' # > Write file
#' \item{\code{write_file}}{Write data into a file file_towrite)}
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
#'
#' @export

DataHub <- R6Class(
  "DataHub",
  public = list (
    # Initialize ----
    initialize = function(user, destdir = tempdir()){
      self$create_destdir(user, destdir)
    },
    #create dest dir for user
    create_destdir = function(user, destdir){
      private$user <- user
      private$user_destdir <- file.path(destdir, private$user)
      dir.create(private$user_destdir, showWarnings = FALSE)
    },
    get_user_destdir = function(){
      private$user_destdir
    },
    # LISTS ----
    # > Portfolio ----
    #return list of portfolio source files
    get_pf_data_list = function(id, ...){
      data_list <- return_tbl_portfolioDetails(id) #assuming id !is.null(id) && id != "" && id != -1 && !is.na(id)
      data_list
    },
    #invalidate list of portfolio source files
    invalidate_pf_data_list = function(id, ...){
      invisible()
    },
    # > Model ----
    #return list of model resources
    get_model_data_list = function(id, ...){
      data_list <- NULL #assuming id !is.null(id) && id != "" && id != -1 && !is.na(id)
      data_list
    },
    #invalidate list of model resources
    invalidate_model_data_list = function(id, ...){
      invisible()
    },
    # > Analysis ----
    #return list of analysis resources
    get_ana_inputs_data_list = function(id, oasisapi, ...){
      tarfile <- get_analyses_inputs_tar(id,  destdir = private$user_destdir, oasisapi)
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
      tarfile <- get_analyses_outputs_tar(id, destdir =  private$user_destdir, oasisapi)
      data_list <- NULL
      if (file.exists(tarfile)) {
        data_list <- untar_list(tarfile, to_strip = "output")
        if (length(data_list) > 0) {
          data_list <- data_list %>%
            as.data.frame() %>%
            setNames("files")
          analysis_settings <- self$get_ana_settings_content(id, oasisapi)
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
      currNamespace <- ls("package:flamingo")
      func_wpattern <- currNamespace[grepl(dataset_identifier, currNamespace)]
      returnfunc <- func_wpattern[grepl("api_get",func_wpattern)]
      if (length(returnfunc) != 0) {
        func <- get(returnfunc)
      }
      dataset_content <- return_file_df(func, id)
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
    get_pf_location_content = function(id,  ...){
      dataset_content <-  self$get_pf_dataset_content(id, dataset_identifier = "location", ...)
      dataset_content
    },
    #invalidate a source file (location/account...) content given a portfolio id
    #dataset_identifier is location/account/reinsurance_info/reinsurance_source
    invalidate_pf_location_content = function(id, dataset_identifier, ...){
      invisible()
    },
    #extract a source file (location/account...) header given a portfolio id
    get_pf_dataset_header = function(id, dataset_identifier, ...){
      dataset_content <- self$get_pf_dataset_content(id, dataset_identifier,...)
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
      dataset_content <- return_models_id_resource_file_df(id)
      dataset_content
    },
    #invalidate model resource file given model id
    invalidate_model_resource_dataset_content = function(id, ...){
      invisible()
    },
    #extract model hazard resource file given model id
    get_model_hazard_dataset_content = function(id,  ...){
      #currently no api function
      path <- system.file("app", "www", "hazard_files", "hazard_500_PGA.geojson", package = "flamingo")
      mapfile <- geojsonio::geojson_read(path, what = "sp") #SLOW!
      mapfile
    },
    #invalidate model hazard resource file given model id
    invalidate_model_hazard_dataset_content = function(id,  ...){
      invisible()
    },

    # > ANALYSIS METHODS ----
    #extract a input/output file given an analysis id
    get_ana_dataset_content = function(id, dataset_identifier, type, oasisapi, ...){
      if (type == "input") {
        dataset_content <- self$get_ana_inputs_dataset_content(id, dataset_identifier, oasisapi)
      } else if (type == "output") {
        dataset_content <- self$get_ana_outputs_dataset_content(id, dataset_identifier, oasisapi)
      } else {
        dataset_content <- NULL
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
    get_ana_inputs_dataset_content = function(id, dataset_identifier, oasisapi, ...){
      tarfile <- get_analyses_inputs_tar(id, destdir =  private$user_destdir, oasisapi)
      dataset_content <- read_file_from_tar(tarfile, dataset_identifier, destdir =  private$user_destdir)
      dataset_content
    },
    #invalidate a inputs file content given an analysis id
    invalidate_ana_inputs_dataset_content = function(id, dataset_identifier, ...){
      self$invalidate_ana_dataset_header(id, dataset_identifier, type = "input", ...)
      self$invalidate_ana_dataset_row(id, dataset_identifier, type = "input", ...)
      invisible()
    },
    #extract a outputs file content given an analysis id
    get_ana_outputs_dataset_content = function(id, dataset_identifier, oasisapi, ...){
      tarfile <- get_analyses_outputs_tar(id, destdir =  private$user_destdir,oasisapi)
      #necessary step because outputs comes with subfolder
      dataset_identifier <- file.path("output", dataset_identifier)
      dataset_content <- read_file_from_tar(tarfile, dataset_identifier, destdir =  private$user_destdir)
      dataset_content
    },
    #invalidate a outputs file content given an analysis id
    invalidate_ana_outputs_dataset_content = function(id, dataset_identifier, ...){
      self$invalidate_ana_dataset_header(id, dataset_identifier, type = "output", ...)
      self$invalidate_ana_dataset_row(id, dataset_identifier, type = "output", ...)
      invisible()
    },
    #extract a inputs/outputs file header given an analysis id
    get_ana_dataset_header = function(id, type, dataset_identifier, oasisapi, ...){
      tarfile <- get_analyses_tar(id, label = type, destdir =  private$user_destdir, oasisapi)
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
    get_ana_dataset_nrow = function(id, type, dataset_identifier, oasisapi, ...){
      destdir =   private$user_destdir
      tarfile <- get_analyses_tar(id, label = type, destdir = destdir, oasisapi)
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
    get_ana_dataset_size = function(id, type, dataset_identifier, oasisapi, ...){
      destdir =   private$user_destdir
      tarfile <- get_analyses_tar(id, label = type, destdir = destdir, oasisapi)
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
    get_ana_settings_content = function(id, oasisapi, ...){
      dataset_content <- oasisapi$api_get_query(query_path = paste("analyses", id, "settings_file",  sep = "/"))
      dataset_content
    },
    #invalidate analysis settings content
    invalidate_ana_settings_content = function(id, ...){
      invisible()
    },
    #extract analysis validation summary content
    get_ana_validation_summary_content = function(id, oasisapi, ...){
      json_lst <- self$get_ana_inputs_dataset_content(id, dataset_identifier = "exposure_summary_report.json", oasisapi, ...)
      dataset_content <- NULL
      if (!is.null(json_lst)){
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
    invalidate_ana_validation_summary_content = function(id, ...){
      invisible()
      self$invalidate_ana_dataset_header(id, dataset_identifier, type, ...)
      self$invalidate_ana_dataset_row(id, dataset_identifier, type, ...)
    },
    # > Write file ----
    write_file = function(data, dataset_identifier, file_towrite = NULL, ...){
      fs <- write_file(data, dataset_identifier, destdir = private$user_destdir, file_towrite)
      fs
    }
  ),
  # Private ----
  private = list(
    user = "",
    user_destdir = ""
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
  g <- granToOed[granToOed$oed == g_oed, "gran"]
  z <- data.frame("perspective" = y[1], "summary_level" = toString(g), "report" = reportToVar(varsdf)[[ report ]], stringsAsFactors = FALSE)
}

# Helper functions for DataHub


#' Get analysis tar
#'
#' @rdname api_get_analyses_tar
#'
#' @description Downloads the analysis tar.
#'
#' @param id A unique integer value identifying this analysis.
#' @param dest path where to download tar file.
#' @param label input/output
#' @param oasisapi as stored in session$userData$oasisapi
#'
#' @return API respone.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr write_disk
#'
#' @export
api_get_analyses_tar <- function(id, label, dest = tempfile(fileext = ".tar"), oasisapi) {

  # request_list <- oasisapi$api_write_tar_query(id, query_path = "analyses", label, dest)

  request_list <- expression(list(
    oasisapi$get_url(), # get_url(),
    config = add_headers(
      Accept = oasisapi$get_http_type(), #get_http_type(),
      Authorization = sprintf("Bearer %s",oasisapi$get_access_token())
    ),
    path = paste(oasisapi$get_version(), "analyses", id, label, "", sep = "/"), #paste(get_version(), "analyses", id, label, "", sep = "/"),
    write_disk(dest, overwrite = TRUE)
  ))

   # response <- api_fetch_response("GET", request_list) # oasisapi$api_fetch_response("GET", request_list)
  response <- oasisapi$api_fetch_response("GET", request_list)

  #response needed to place icon
  # api_handle_response(response)  #
  oasisapi$api_handle_response(response, warning)

}


#' Get analysis tar path
#'
#' @rdname get_analyses_tar
#'
#' @description Downloads the analysis tar and returns download path.
#'
#' @param id A unique integer value identifying this analysis.
#' @param destdir path where to download tar file.
#' @param label input/output
#'
#' @return Path to file downloaded.
#'
#' @export
get_analyses_tar <- function(id, label, destdir = tempdir(), oasisapi) {
  dest <- tempfile(tmpdir = destdir, fileext = ".tar")
  response <- api_get_analyses_tar(id, paste0(label, "_file"), dest, oasisapi)

  dest
}

#' Get analysis inputs tar path
#'
#' @rdname get_analyses_inputs_tar
#'
#' @description Downloads the analysis inputs tar and returns download path.
#'
#' @param id A unique integer value identifying this analysis.
#' @param destdir path where to download tar file.
#' @param oasisapi as stored in session$userData$oasisapi.
#'
#' @return Path to file downloaded.
#'
#' @export

get_analyses_inputs_tar <- function(id, destdir = tempdir(), oasisapi) {
  get_analyses_tar(id, label = "input", destdir, oasisapi)
}

#' Get analysis outputs tar path
#'
#' @rdname get_analyses_outputs_tar
#'
#' @description Downloads the analysis outputs tar and returns download path.
#'
#' @param id A unique integer value identifying this analysis.
#' @param destdir path where to download tar file.
#' @param oasisapi as stored in session$userData$oasisapi.
#'
#' @return Path to file downloaded.
#'
#' @export
get_analyses_outputs_tar <- function(id, destdir = tempdir(),oasisapi) {
  get_analyses_tar(id, label = "output", destdir, oasisapi)
}

#' untar list
#'
#' @rdname untar_list
#'
#' @description Returns list of files in a tar bundle.
#'
#' @param tarfile path to tar bundle.
#' @param to_strip character vector of initial path to strip relative to the tar bundle content.
#'
#' @return list of files in tar bundle.
#'
#' @export

untar_list <- function(tarfile, to_strip = NULL){
  tar_list <- untar(tarfile, list = TRUE)
  tar_list <- simplify_path(tar_list, to_strip)
  tar_list
}

#' untar file
#'
#' @rdname read_file_from_tar
#'
#' @description Returns content of a file in a tar bundle.
#'
#' @param tarfile path to tar bundle.
#' @param dataset_identifier name and relative path of file to untar.
#' @param destdir path where to extract file.
#' @param nrows number of rows to read. Default Inf indicates full content.
#'
#' @return nrows rows of the content of dataset_identifier
#'
#' @importFrom data.table fread
#' @importFrom jsonlite read_json
#'
#' @export

read_file_from_tar <- function(tarfile, dataset_identifier, destdir = tempdir(), nrows = Inf){
  untar(tarfile, files = dataset_identifier, exdir = destdir)
  data = NULL
  if (file.exists(file.path(destdir, dataset_identifier))) {
    extension <-  strsplit(dataset_identifier, split = "\\.") %>% unlist() %>% tail(n = 1)
    if (extension == "csv") {
      data <- fread(file.path(destdir, dataset_identifier), nrows = nrows)
    } else if (extension == "json") {
      data <- read_json(file.path(destdir, dataset_identifier))
    } else{
      data <- scan(file.path(destdir, dataset_identifier), what="", sep = "\n")
    }
    file.remove(file.path(destdir, dataset_identifier)) #remove extracted file after reading
  }
  data
}

#' write file
#'
#' @rdname write_file
#'
#' @description Writes ojbect in the correct format.
#'
#' @param data object to write.
#' @param dataset_identifier name and relative path of file to write
#' @param destdir path where to write file.
#'
#' @importFrom data.table fread
#' @importFrom jsonlite read_json
#'
#' @export

write_file <- function(data, dataset_identifier = NULL, destdir = tempdir(), file_towrite = NULL){
  if (is.null(file_towrite)) {
    file_towrite <- file.path(destdir, dataset_identifier)
  }
  extension <-  strsplit(dataset_identifier, split = "\\.") %>% unlist() %>% tail(n = 1)
  if (extension == "csv") {
    fwrite(data, file_towrite, row.names = TRUE, quote = TRUE)
  } else if (extension == "json") {
    write(toJSON(data, pretty = TRUE), file_towrite)
  } else{
    write(data, file_towrite)
  }
  file_towrite
}


#' simplify_path
#'
#' @rdname simplify_path
#'
#' @description Returns simplified paths.
#'
#' @details directory paths ending with "/" are removed from set.
#'
#' @param x paths.
#' @param to_strip character vector of initial path to strip.
#'
#' @return simplified vector of paths.
#'
#' @export
simplify_path <- function(x, to_strip = NULL) {
  x <- x[!grepl("/$", x)] # exclude directories
  if (length(to_strip) > 0) {
    to_strip <- sub("/$", "", to_strip)
    to_strip <- sub("\\$", "/", glob2rx(to_strip))
    to_strip <- paste0(to_strip , collapse = "|")
    x <- sub(to_strip, "", x)
  }
  x
}

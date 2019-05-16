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
#'
#' @return API respone.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr write_disk
#'
#' @export
api_get_analyses_tar <- function(id, label, dest = tempfile(fileext = ".tar")) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, label, "", sep = "/"),
    write_disk(dest, overwrite = TRUE)
  ))

  response <- api_fetch_response("GET", request_list)

  #response needed to place icon
  api_handle_response(response)
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
get_analyses_tar <- function(id, label, destdir = tempdir()) {

  dest <- tempfile(tmpdir = destdir, fileext = ".tar")
  response <- api_get_analyses_tar(id, paste0(label, "_file"), dest)

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
#'
#' @return Path to file downloaded.
#'
#' @export

get_analyses_inputs_tar <- function(id, destdir = tempdir()) {
  get_analyses_tar(id, label = "input", dest)
}

#' Get analysis outputs tar path
#'
#' @rdname get_analyses_outputs_tar
#'
#' @description Downloads the analysis outputs tar and returns download path.
#'
#' @param id A unique integer value identifying this analysis.
#' @param destdir path where to download tar file.
#'
#' @return Path to file downloaded.
#'
#' @export
get_analyses_outputs_tar <- function(id, destdir = tempdir()) {
  get_analyses_tar(id, label = "output", dest)
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
#' @rdname untar_file
#'
#' @description Returns list of files in a tar bundle.
#'
#' @param tarfile path to tar bundle.
#' @param file_name name of file to untar.
#' @param destdir path where to extract file.
#' @param nrows number of rows to read. Default Inf indicates full content.
#' @param to_strip character vector of initial path to strip relative to the tar bundle content.
#'
#' @return content of file_name
#'
#' @importFrom data.table fread
#'
#' @export

untar_file <- function(tarfile, file_name, destdir = tempdir(), nrows = Inf, to_strip = ""){
  untar(tarfile, files = file.path(to_strip, file_name), exdir = destdir)
  data <- fread(file.path(destdir, to_strip, file_name))
  file.remove(file.path(destdir, to_strip, file_name)) #remove extracted file after reading
  data
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

# API functions for DataHub

#' Get analysis input tar
#'
#' @rdname api_get_analyses_inputs_tar
#'
#' @description Downloads the analysis inputs tar.
#'
#' @param id A unique integer value identifying this analysis.
#' @param dest path where to download tar file.
#'
#' @return API respone.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr write_disk
#'
#' @export
api_get_analyses_inputs_tar <- function(id, dest = tempfile(fileext = ".tar")) {

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

  #response needed in step2 to place icon
  api_handle_response(response)
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

  dest <- tempfile(tmpdir = destdir, fileext = ".tar")

  response <- api_get_analyses_inputs_tar(id, dest)

  dest
}


#' Get analysis outputs tar
#'
#' @rdname api_get_analyses_outputs_tar
#'
#' @description Downloads the analysis outputs tar.
#'
#' @param id A unique integer value identifying this analysis.
#' @param dest path to download tar file
#'
#' @return Previously posted analysis outputs tar file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr write_disk
#'
#' @export
api_get_analyses_outputs_tar <- function(id, dest = tempfile(fileext = ".tar")) {

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

  #response needed in step2 to place icon
  api_handle_response(response)

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
get_analyses_inputs_tar <- function(id, destdir = tempdir()) {

  dest <- tempfile(tmpdir = destdir, fileext = ".tar")

  response <- api_get_analyses_inputs_tar(id, dest)

  dest
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
  tar_list <- simplify_path(tarfile, to_strip)
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

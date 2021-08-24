# Helper functions for DataHub


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
#' @importFrom utils tail
#'
#' @export
read_file_from_tar <- function(tarfile, dataset_identifier, destdir = tempdir(), nrows = Inf){
  untar(tarfile, files = dataset_identifier, exdir = destdir)
  data = NULL
  if (file.exists(file.path(destdir, dataset_identifier))) {
    extension <-  strsplit(dataset_identifier, split = "\\.") %>% unlist() %>% tail(n = 1)
    if (extension == "csv") {
      if (length(dataset_identifier) > 1) {
        stop("More than 1 dataset_identifier was passed to read_file_from_tar function")
      } else {
        data <- fread(file.path(destdir, dataset_identifier), nrows = nrows)
      }
    } else if (extension == "json") {
      data <- read_json(file.path(destdir, dataset_identifier))
    } else{
      data <- scan(file.path(destdir, dataset_identifier), what = "", sep = "\n")
    }
    file.remove(file.path(destdir, dataset_identifier)) #remove extracted file after reading
  }
  data
}

#' write file
#'
#' @rdname write_file
#'
#' @description Writes object in the correct format.
#'
#' @param data object to write.
#' @param dataset_identifier name and relative path of file to write
#' @param destdir path where to write file.
#' @param file_towrite name of file where to write data.
#'
#' @importFrom data.table fread
#' @importFrom jsonlite read_json
#' @importFrom utils tail
#'
#' @export

writefile <- function(data, dataset_identifier = NULL, destdir = tempdir(), file_towrite = NULL){
  if (is.null(file_towrite)) {
    file_towrite <- file.path(destdir, dataset_identifier)
  }
  extension <-  strsplit(dataset_identifier, split = "\\.") %>% unlist() %>% tail(n = 1)
  if (extension == "json") {
    write(toJSON(data, pretty = TRUE), file_towrite)
  } else {
    fwrite(data, file_towrite, row.names = FALSE, quote = TRUE)
  }
  file_towrite
}

#' write parquet
#'
#' @rdname write_parquet
#'
#' @description Writes object in parquet format.
#'
#' @param data object to write.
#' @param dataset_identifier name and relative path of file to write
#' @param destdir path where to write file.
#' @param file_towrite name of file where to write data.
#'
#' @importFrom arrow write_parquet
#'
#' @export
writeParquet <- function(data, dataset_identifier = NULL, destdir = tempdir(), file_towrite = NULL) {
  if (is.null(file_towrite)) {
    file_towrite <- file.path(destdir, dataset_identifier)
  }
  write_parquet(data, file_towrite)
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
#' @importFrom utils glob2rx
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

#' Clean up downloaded data
#' 
#' @rdname clean_downloadedData
#' 
#' @description removes downloaded data
#' 
#' @export
clean_downloadedData <- function(){
  logMessage("clean_downloadedData called.")
  currfolder <- getOption("flamingo.settins.api.share_filepath")
  if (file.exists(currfolder)) {
    unlink(file.path(currfolder, "*"), TRUE) 
  } else {
      dir.create(currfolder, showWarnings = FALSE)
  }
  invisible()
}
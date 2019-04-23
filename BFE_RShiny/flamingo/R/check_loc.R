#' check_loc
#'
#' @description Compares uploaded and modeled locations.
#'
#' @param analysisID Selected analysis id.
#' @param portfolioID selected portfolio ID.
#'
#' @importFrom data.table fread
#' @importFrom dplyr select
#' @importFrom dplyr intersect
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'
#' @export
check_loc <- function(analysisID, portfolioID){

  logMessage(".check_loc called")

  uploaded_locs_check <- NULL

  # Get uploaded locations
  uploaded_locs <- return_file_df(api_get_portfolios_location_file, portfolioID)

  # Analysis location input
  extractFolder <- set_extractFolder(analysisID, label = "_inputs/")
  fileslist <- list.files(extractFolder)
  modeled_loc_filename <- httr::content(api_get_portfolios_id(portfolioID)$result)$location_file$stored

  if (!is.na(modeled_loc_filename)) {

    currfilepath <- paste0(extractFolder, modeled_loc_filename)
    modeled_locs <- fread(currfilepath, integer64 = "numeric")

    # Hack: drop LocName as it seems to cause issues in the inner_join
    modeled_locs <- modeled_locs %>%
      select(LocNumber)

    # compare uploaded locations with modeled locations
    idx_in <- intersect(uploaded_locs$LocNumber, modeled_locs$LocNumber)

    uploaded_locs_check <- uploaded_locs %>%
      mutate(modeled = case_when(LocNumber %in% idx_in ~ "TRUE",
                                 TRUE ~ "FALSE"))
  }

  return(uploaded_locs_check)
}

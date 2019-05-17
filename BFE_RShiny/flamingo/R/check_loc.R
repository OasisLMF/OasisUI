#' check_loc
#'
#' @description Compares uploaded and modeled locations.
#'
#' @param analysisID Selected analysis id.
#'
#' @importFrom data.table fread
#' @importFrom dplyr full_join
#' @importFrom dplyr one_of
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
check_loc <- function(analysisID){

  logMessage(".check_loc called")

  #initialize df
  uploaded_locs_check <- NULL
  uploaded_locs <- NULL
  modelled_locs <- NULL

  # Analysis location inputs
  extractFolder <- set_extractFolder(analysisID, label = "_inputs/")
  uploaded_locs_filepath <- paste0(extractFolder, "location.csv")
  modelled_locs_filepath <- paste0(extractFolder, "gul_summary_map.csv")

  if (file.exists(uploaded_locs_filepath)) {
    uploaded_locs <- fread(uploaded_locs_filepath, integer64 = "numeric") %>%
      mutate(loc_idx = seq(nrow(.)) - 1)
  }

  if (file.exists(modelled_locs_filepath)) {
    modelled_locs <- fread(modelled_locs_filepath, integer64 = "numeric")
  }

  uploaded_locs_check <- full_join(uploaded_locs, modelled_locs, by = "loc_idx") %>%
    select(-one_of( names(modelled_locs %>% select(-"peril_id")))) %>%
      distinct()

  return(uploaded_locs_check)
}

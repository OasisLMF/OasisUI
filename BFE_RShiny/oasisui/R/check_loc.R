#' check_loc
#'
#' @description Compares uploaded and modeled locations.
#'
#' @param analysisID Selected analysis id.
#' @param portfolioID selected portfolio ID.
#' @param data_hub data hub stored in session$userData$data_hub
#'
#' @return Checked uploaded locations (added peril_id).
#'
#' @importFrom dplyr full_join
#' @importFrom dplyr one_of
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#'
#' @export
check_loc <- function(analysisID, portfolioID, data_hub) {
  logMessage(".check_loc called")
  uploaded_locs <- data_hub$get_pf_location_content(id = portfolioID) %>%
    mutate(loc_idx = seq(nrow(.)) - 1)
  modelled_locs <- data_hub$get_ana_dataset_content(id = analysisID, dataset_identifier = "lookup_success_file")

  uploaded_locs_check <- full_join(uploaded_locs, modelled_locs, by = "loc_idx") %>%
    select(-one_of(names(modelled_locs %>% select(-"peril_id")))) %>%
    distinct()

  return(uploaded_locs_check)
}

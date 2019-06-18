# R functions calling Portfolio API functions and manipulating the output ------

#' Return portfolios data for DT
#'
#' @rdname return_tbl_portfoliosData
#'
#' @description Returns a dataframe of portfolios ready for being rendered as a data table.
#'
#' @param name Name of the portfolio.
#' @param oasisapi as stored in session$userData$oasisapi
#'
#' @return Dataframe of previously posted portfolios. Default empty string returns all portfolios.
#'
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#'
#' @export
return_tbl_portfoliosData <- function(name = "", oasisapi) {

  tbl_portfoliosData <-  oasisapi$return_df("portfolios",  api_param = list(name = name))

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
}

#' Return portfolio details for DT
#'
#' @rdname return_tbl_portfolioDetails
#'
#' @description Returns a dataframe of portfolio's details ready for being rendered as a data table.
#'
#' @param id Id of the portfolio.
#' @param oasisapi as stored in session$userData$oasisapi
#'
#' @return Dataframe of details of previously posted portfolio.
#'
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom tidyr gather
#'
#' @export
return_tbl_portfolioDetails <- function(id, oasisapi) {

  tbl_portfolioDetails <- oasisapi$return_df(paste("portfolios", id,  sep = "/"))

  if (!is.null(tbl_portfolioDetails) && is.null(tbl_portfolioDetails$detail)) {
    tbl_portfolioDetails <- tbl_portfolioDetails %>%
      select(contains("file")) %>%
      as.data.frame()
    # reshape df
    tbl_portfolioDetails <- gather(tbl_portfolioDetails,  key = "files", value = "name") %>%
      as.data.frame()
  }

  tbl_portfolioDetails
}

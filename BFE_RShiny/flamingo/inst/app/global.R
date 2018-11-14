###############################################################################
# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

# note with shiny::runApp this statement isn't needed anymore
#library(shiny, warn.conflicts = FALSE, quietly = TRUE)

#library(httr)
#suppressPackageStartupMessages(library(shinyjs, warn.conflicts = FALSE))
#library(bsplus)
#library(DT, warn.conflicts = FALSE)
#library(DBI)
#library(shinyWidgets)
#library(dplyr, warn.conflicts = FALSE)
#library(logging)

library(flamingo, warn.conflicts = FALSE)

source(file.path(".", "helper_text.R"), local = TRUE)

### logger ---------------------------------------------------------------------
#addHandler(writeToFile, logger = "flamingo",
#    file = file.path("var", "log", "shinyproxy", "flamingo.log"))

#addHandler(writeToConsole, logger = "flamingo")

loginfo <- function(..., logger) {message(...)}
logerror <- function(..., logger) {warning(...)}
loginfo("testing logger", logger = "flamingo.module")

logMessage <- function(msg) loginfo(msg, logger = "flamingo.module")

### flamingo database ----------------------------------------------------------
dbSettings <- flamingoDB(
  server = Sys.getenv("FLAMINGO_DB_IP"),
  port = Sys.getenv("FLAMINGO_DB_PORT"),
  database = Sys.getenv("FLAMINGO_DB_NAME"),
  uid = Sys.getenv("FLAMINGO_DB_USERNAME"),
  pwd = Sys.getenv("FLAMINGO_DB_PASSWORD")
)
# timeout for DB connection (secs)
dbSettings$timeout <- 10

tryCatch({
  conn <- do.call(DBI::dbConnect, dbSettings)
  DBI::dbDisconnect(conn)
  loginfo("sucessfully connected to database", logger = "flamingo.module")
}, error = function(e) {
  logerror(paste("Could not connect to database:", e$message), logger = "flamingo.module")
})

# global parameter, number of milliseconds to wait before refreshing tables ----
# (300000 == 5 mins)
reloadMillis <- 300000

# flamingo django API ----------------------------------------------------------
options(flamingo.settings.api = api_init("localhost", "8000"))
options(flamingo.settings.api.version = "v1")

loginfo(paste("flamingo API server:", get_url()), logger = "flamingo.module")
tryCatch({
  invisible(api_get_helthcheck())
}, error = function(e) {
  logerror(e$message, logger = "flamingo.module")
})

# Staus Code for files
status_code_exist <- 200
status_code_notfound <- 404

### Icons ----------------------------------------------------------------------
icon_failed <- "times-circle"
icon_completed <- "check-circle"
icon_inporgress <- "spinner"
icon_ready <- "check"
StatusReady <- '<i class="fa fa-check"></i>'
StatusFailed <- '<i class="fa fa-times-circle"></i>'
StatusCompleted <- '<i class="fa fa-check-circle"></i>'
StatusProcessing <- '<i class="fa fa-spinner"></i>'

### Default Selection Items ----------------------------------------------------
defaultSelectChoicesGUL <- c(
  "gulprogSummary", "gulprogELT", "gulprogAAL",
  "gulprogPLT", "gulprogFullUncAEP", "gulprogFullUncOEP"
)
defaultSelectChoicesIL <- c(
  "ilprogSummary", "ilprogELT", "ilprogAAL", "ilprogPLT",
  "ilprogFullUncAEP", "ilprogFullUncOEP", "ilpolicyELT", "ilpolicyAAL",
  "ilpolicyPLT", "ilpolicyFullUncAEP", "ilpolicyFullUncOEP"
)
defaultSelectChoicesRI <- c(
  "riprogSummary", "riprogELT", "riprogAAL", "riprogPLT",
  "riprogFullUncAEP", "riprogFullUncOEP", "ripolicyELT", "ripolicyAAL",
  "ripolicyPLT", "ripolicyFullUncAEP", "ripolicyFullUncOEP"
)


### Plots types ----------------------------------------------------------------
plottypeslist <- list("loss per return period" = list("Variables" = c("LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP"),
                                                      "keycols" = c("loss"),
                                                      "uncertaintycols" = c(),
                                                      "referencecols" = c(),
                                                      "x" = c("return_period"),
                                                      "xtickslabels" = list(),
                                                      "extracols" = c("X", "summary_id"),
                                                      "plottype" = "line",
                                                      "xlabel" = c("Return Period"),
                                                      "ylabel" = c("Loss")
                            ),
                            "AAL bar plot" = list("Variables" = c("AAL"),
                                                  "keycols" = c("mean"),
                                                  "uncertaintycols" = c("standard_deviation"),
                                                  "referencecols" = c("exposure_value"),
                                                  "x" = c("type"),
                                                  "xtickslabels" = c("Numerically Integrated", "Sample Statistics"),
                                                  "extracols" = c("X", "summary_id"),
                                                  "plottype" = "bar",
                                                  "xlabel" = c("Type"),
                                                  "ylabel" = c("Loss")
                            )#,
                            # "Wheatsif violin plot" = list("Variables" = c("LEC Wheatsheaf AEP", "LEC Wheatsheaf OEP"),
                            #                               "keycols" = c("loss"),
                            #                               "uncertaintycols" = c(),
                            #                               "referencecols" = c(),
                            #                               "x" = c("return_period"),
                            #                               "xtickslabels" = list(),
                            #                               "extracols" = c("X", "summary_id", "sidx"),
                            #                               "plottype" = "violin",
                            #                               "xlabel" = c("Return Period"),
                            #                               "ylabel" = c("Loss")
                            # )
)

#Output options ----------------------------------------------------------------
granularities <- c("LOB", "Location", "County","State", "Policy", "Portfolio")
losstypes <- c("GUL", "IL", "RI")
variables <- c("PLT", "AAL", "LEC Wheatsheaf OEP", "LEC Wheatsheaf AEP", "LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP", "ELT")


# > Variables for cols positions -----------------------------------------------
### Creating Variables for col names of Portfolio Table
#result$tbl_portfoliosData
tbl_portfoliosData.PortfolioID <- "id"
tbl_portfoliosData.PortfolioName <- "name"
tbl_portfoliosData.PortfolioCreated <- "created"
tbl_portfoliosData.PortfolioModified <- "modified"
tbl_portfoliosData.Status <- "Status"

### Creating Variables for portfolio Details table
#result$tbl_portfolioDetails
tbl_portfolioDetails.PortfolioID <- "id"
tbl_portfolioDetails.PortfolioName <- "name"
tbl_portfolioDetails.PortfolioCreated <- "created"
tbl_portfolioDetails.PortfolioModified <- "modified"
tbl_portfolioDetails.PortfolioLoc <- "location_file"
tbl_portfolioDetails.PortfolioAcc <- "accounts_file"
tbl_portfolioDetails.PortfolioRIinfo <- "reinsurance_info_file"
tbl_portfolioDetails.PortfolioRIsource <- "reinsurance_source_file"

### Creating Variables for col names of Model Table
# result$tbl_modelsData
tbl_modelsData.ModelId <- "id"
tbl_modelsData.ModelSupplierId <- "supplier_id"
tbl_modelsData.ModelVersionId <- "version_id"
tbl_modelsData.ModelCreated <- "created"
tbl_modelsData.ModelModified <- "modified"

### Creating Variables for col names of Process Runs Table
# result$tbl_analysisData
tbl_analysesData.AnaID <- "id"
tbl_analysesData.AnaModified <- "modified"
tbl_analysesData.AnaCreated <- "created"
tbl_analysesData.AnaName <- "name"
tbl_analysesData.PortfolioID <- "portfolio"
tbl_analysesData.ModelID <- "model"
tbl_analysesData.AnaStatus <- "status"
tbl_analysesData.AnaInputFile <- "input_file"
tbl_analysesData.AnaSettingFile <- "settings_file"
tbl_analysesData.AnaInputErrFile <- "input_errors_file"
tbl_analysesData.AnaInputGenTraceBackFile <- "input_generation_traceback_file"
tbl_analysesData.AnaOutputFile <- "output_file"
tbl_analysesData.AnaRunTracebackFile <- "run_traceback_file"


### Creating Variables for  col names of filesListData
# result$filesListData
# "FileID", "File Name", "Description", "Location","Location Unix", "File Type", "Owner", "Resource Table", "Resource Key"
filesListData.fileID <- "FileID"
filesListData.fileName <- "File Name"
filesListData.path <- "Location Unix"
filesListData.key <- "Resource Key"

### Creating Variables for  col names of  $inbox
# result$inbox
# "ProgOasisID"  "RunID"        "Run Name"     "Model"        "Status"       "Completed At"
inbox.ProgOasisID <- "ProgOasisID"
inbox.RunID <- "RunID"
inbox.RunName <- "Run Name"
inbox.Model <- "Model"
inbox.Status <- "Status"
inbox.ProgOasisID <- "ProgOasisID"
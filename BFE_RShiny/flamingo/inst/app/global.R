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

### logger ----
#addHandler(writeToFile, logger = "flamingo",
#    file = file.path("var", "log", "shinyproxy", "flamingo.log"))

#addHandler(writeToConsole, logger = "flamingo")

loginfo <- function(..., logger) {message(...)}
logerror <- function(..., logger) {warning(...)}
loginfo("testing logger", logger = "flamingo.module")

logMessage <- function(msg) loginfo(msg, logger = "flamingo.module")

### flamingo database ----
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

# flamingo django API ---
options(flamingo.settings.api = api_init("localhost", "8000"))

loginfo(paste("flamingo API server:", get_url()), logger = "flamingo.module")
tryCatch({
  invisible(api_get_helthcheck())
}, error = function(e) {
  logerror(e$message, logger = "flamingo.module")
})

### Icons ----
StatusFailed <- '<i class="fa fa-times-circle"></i>'
StatusCompleted <- '<i class="fa fa-check-circle"></i>'
StatusProcessing <- '<i class="fa fa-spinner"></i>'

### Default Selection Items ----
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


### Plots types ------
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

#Output options
granularities <- c("LOB", "Location", "County","State", "Policy", "Portfolio")
losstypes <- c("GUL", "IL", "RI")
variables <- c("PLT", "AAL", "LEC Wheatsheaf OEP", "LEC Wheatsheaf AEP", "LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP", "ELT")


# > Variables for cols positions -----------------------------------------------
### Creating Variables for col names of Programme Table
#result$tbl_portfoliosData
#"Programme ID", "Programme Name", "Account ID", "Account Name", "Transform ID", "Transform", "Status"
tbl_portfoliosData.ProgrammeID <- "Programme ID" #reactive(names(result$tbl_portfoliosData)[1])
tbl_portfoliosData.ProgrammeName <- "Programme Name" #reactive(names(result$tbl_portfoliosData)[2])
tbl_portfoliosData.AccountID <- "Account ID" #reactive(names(result$tbl_portfoliosData)[3])
tbl_portfoliosData.AccountName <- "Account Name" #reactive(names(result$tbl_portfoliosData)[4])
tbl_portfoliosData.TranformID <- "Transform ID" #reactive(names(result$tbl_portfoliosData)[5])
tbl_portfoliosData.Tranform <- "Transform" #reactive(names(result$tbl_portfoliosData)[6])
tbl_portfoliosData.Status <-  "Status" #reactive(names(result$tbl_portfoliosData)[7])

### Creating Variables for col names of Programme Model Table
# result$tbl_modelsData
#"ProgOasisId", "ProgName", "ModelName", "TransformName", "SourceFileId", "FileID", "Status"
tbl_modelsData.ProgOasisId <- "ProgOasisId" #reactive(names(result$tbl_modelsData)[1])
tbl_modelsData.ProgName <- "ProgName" #reactive(names(result$tbl_modelsData)[2])
tbl_modelsData.ModelName <- "ModelName" #reactive(names(result$tbl_modelsData)[3])
tbl_modelsData.TransformName <- "TransformName" #reactive(names(result$tbl_modelsData)[4])
tbl_modelsData.SourceFileId <- "SourceFileId" #reactive(names(result$tbl_modelsData)[5])
tbl_modelsData.FileID <- "FileID" #reactive(names(result$tbl_modelsData)[6])
tbl_modelsData.Status <- "Status" #reactive(names(result$tbl_modelsData)[7])

### Creating Variables for col names of Process Runs Table
# result$tbl_analysisData
#"ProcessRunID", "ProcessRunName", "ProgOasisID", "ProcessRunStatus"
tbl_analysisData.ProcessRunID <- "ProcessRunID" #reactive(names(result$tbl_analysisData)[1])
tbl_analysisData.ProcessRunName <- "ProcessRunName" #reactive(names(result$tbl_analysisData)[2])
tbl_analysisData.ProgOasisID <- "ProgOasisID" #reactive(names(result$tbl_analysisData)[3])
tbl_analysisData.ProcessRunStatus <- "Status" #reactive(names(result$tbl_analysisData)[4])
tbl_analysisData.ProcessRunStatus.old <- "ProcessRunStatus"


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
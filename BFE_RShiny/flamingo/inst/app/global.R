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
#library(shinyBS)
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

### flamingo API server ----
apiSettings <- flamingoServer(
  host = Sys.getenv("DOCKER_HOST_IP"),
  # host = Sys.getenv("FLAMINGO_API_IP"), #TODO this would be a better variable
  port = Sys.getenv("FLAMINGO_API_PORT")
)

loginfo(paste("flamingo server:", apiSettings$url), logger = "flamingo.module")
tryCatch({
  testFlamingoServer(apiSettings)
}, error = function(e) {
  logerror(e$message, logger = "flamingo.module")
})

### Icons ----
StatusFailed <- '<i class="fa fa-times-circle"></i>'
StatusCompleted <- '<i class="fa fa-check-circle"></i>'
StatusProcessing <- '<i class="fa fa-spinner"></i>'

### Defult Selection Items ----
defaultSelectChoicesGUL <- c(
  "gulprogSummary", "gulprogELT", "gulprogAAL",
  "gulprogPLT", "gulprogFullUncAEP", "gulprogFullUncOEP"
)
defaultSelectChoicesIL <- c(
  "ilprogSummary", "ilprogELT", "ilprogAAL", "ilprogPLT",
  "ilprogFullUncAEP", "ilprogFullUncOEP", "ilpolicyELT", "ilpolicyAAL",
  "ilpolicyPLT", "ilpolicyFullUncAEP", "ilpolicyFullUncOEP"
)


### Plots types ------
plottypeslist <- list("loss per return period" = list("Variables" = c("LEC Full Uncertainty OEP",
                                                                      "LEC Full Uncertainty AEP"),
                                                      "keycols" = c("loss"),
                                                      "uncertaintycols" = c(),
                                                      "x" = c("return_period"),
                                                      "extracols" = c("X", "summary_id"),
                                                      "plottype" = "line",
                                                      "xlabel" = c("Return Period"),
                                                      "ylabel" = c("Loss")),
                      "AAL" = list("Variables" = c("AAL"),
                                   "keycols" = c("mean",  "standard_deviation", "exposure_value"),
                                   "uncertaintycols" = c("standard_deviation"),
                                   "x" = c("type"),
                                   "extracols" = c("X", "summary_id"),
                                   "plottype" = "bar",
                                   "xlabel" = c("Type"),
                                   "ylabel" = c("Loss")
                                   )
)

#Output options
granularities <- c("LOB", "Location", "County","State", "Policy", "Portfolio")
losstypes <- c("GUL", "IL")
variables <- c("PLT", "AAL", "LEC Wheatsheaf OEP", "LEC Wheatsheaf AEP", "LEC Full Uncertainty OEP", "LEC Full Uncertainty AEP", "ELT")

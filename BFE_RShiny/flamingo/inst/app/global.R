# Flamingo Shiny
# 
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

library(shiny)
library(datasets)
library(httr)
library(rjson)
library(shinyjs, warn.conflicts = FALSE)
library(xml2)
library(shinyBS)
library(DT, warn.conflicts = FALSE)
library(DBI)
library(logging)

source(file.path(".", "helper_text.R"), local = TRUE)

library(shinyWidgets)
library(dplyr)

library(flamingo)

### logger

#addHandler(writeToFile, logger="flamingo",
#    file = file.path("var", "log", "shinyproxy", "flamingo.log"))

addHandler(writeToConsole, logger = "flamingo")

loginfo("testing logger", logger="flamingo.module")

logMessage <- function(msg) loginfo(msg, logger = "flamingo.module") 


### flamingo database 

dbSettings <- flamingoDB(
    server = Sys.getenv("FLAMINGO_DB_IP"),
    port = Sys.getenv("FLAMINGO_DB_PORT"),
    database = Sys.getenv("FLAMINGO_DB_NAME"),
    uid = Sys.getenv("FLAMINGO_DB_USERNAME"),
    pwd = Sys.getenv("FLAMINGO_DB_PASSWORD"))

tryCatch({
      conn <- do.call(dbConnect, dbSettings)
      dbDisconnect(conn)
      loginfo("sucessfully connected to database", logger = "flamingo.module")
    }, error = function(e) {
      logerror(paste("Could not connect to database:", e$message), logger="flamingo.module")
    })

reloadMillis <- 10000 # amount of time to wait before refreshing tables


### flamingo API server

apiSettings <- flamingoServer(
    host = Sys.getenv("DOCKER_HOST_IP"),
    # host = Sys.getenv("FLAMINGO_API_IP"), #TODO this would be a better variable
    port = Sys.getenv("FLAMINGO_API_PORT"))

loginfo(paste("flamingo server:", apiSettings$url), logger = "flamingo.module")
tryCatch({
      testFlamingoServer(apiSettings)
    }, error = function(e) {
      logerror(e$message, logger = "flamingo.module")
    })


## Icons

StatusFailed <- '<i class="fa fa-times-circle"></i>'
StatusCompleted <- '<i class="fa fa-check-circle"></i>'
StatusProcessing <- '<i class="fa fa-spinner"></i>'
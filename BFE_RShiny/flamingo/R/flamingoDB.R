
#' Convenience function to execute database queries
#' @description Opens a new connection to the database, executes the given query
#'   statement and then closes the connection again.
#' @param dbSettings setting object as returned by e.g. [flamingoDB()]
#' @param statement query statement to execute, e.g. as returned by
#'   [buildDbQuery()]
#' @param simplify if `TRUE`, a vector will be returned instead of a
#'   `data.frame` only a single column is returned.
#' @param verbose print query statement
#' @param logMessage function to call with log message
#' @param logError function to call with error log message
#' @param ... further arguments to [dbGetQuery()]
#' @return result of the query as a `data.frame` or a vector if `simplify` is
#'   set to `TRUE`. `NULL` if no result can be returned.
#' @importFrom DBI dbGetQuery dbConnect dbDisconnect
#' @export
#' @md
executeDbQuery <- function(dbSettings, statement, simplify = FALSE,
    verbose = FALSE, logMessage = message, logError = logMessage, ...) {

  if (!any(class(dbSettings) == "flamingoDB")) {
    stop("invalid database settings")
  }

  conn <- do.call(dbConnect, dbSettings)
  on.exit(dbDisconnect(conn))

  dbGetQuery(conn, "SET ANSI_NULLS ON")
  dbGetQuery(conn, "SET ANSI_WARNINGS ON")
  dbGetQuery(conn, paste("use", dbSettings$database))

  if (verbose) logMessage(statement)

  res <- NULL

  tryCatch({
        res <- dbGetQuery(conn, statement, ...)
      }, error = function(e) {
        if (verbose) logMessage(e$message)
      })

  if (simplify) {
    if (ncol(res) > 1) {
      stop("cannot simplify: result contains more than one column")
    }
    res <- res[,1]
  }

  return(res)

}


#' Convenience function to construct query statements
#' @description Constructs query statements that can be executed with
#'   [DBI::dbExecute()].
#' @param dbFunc database function to call
#' @param ... arguments to `dbFunc`. `character()` args are quoted with single
#'   quotes (').
#' @param dboPrefix prefix `dbFunc` with `"dbo."`
#' @param squareBrackets surround the argument list with square brackets
#' @export
#' @md
buildDbQuery <- function( 
    dbFunc, ...,
    dboPrefix = TRUE,
    squareBrackets = FALSE) {

  args <- list(...)

  args <- sapply(args, function(x) {
        if (is.character(x)) {
          return(paste0("'", x, "'"))
        } else {
          return(as.character(x))
        }
      })

  if (dboPrefix) {
    dbFunc <- paste0("dbo.", dbFunc)
  }

  stmt <- paste0("exec ", dbFunc, " ",
      if (squareBrackets) "[",
      paste(collapse = ", ", args),
      if (squareBrackets) "]")

  return(trimws(stmt))

}

#' Create a database settings object for the Flamingo Database
#' @description creates a database settings object which can then be used
#' to create new connections to the Flamingo Database
#' @param server host name
#' @param port host port
#' @param database database name
#' @param uid username
#' @param pwd user pass
#' @param driver connection driver
#' @return a database setttings object (list)
#' @importFrom odbc odbc
#' @export
flamingoDB <- function(
    server,
    port,
    database,
    uid,
    pwd,
    driver = "{FreeTDS}") {

  struct <- structure(
      # this flamingoDB class thing doesn't really add anything (isn't used anywhere)
      class = c("flamingoDB", "list"),
      list(
          drv = odbc(),
          server = server,
          port = port,
          database = database,
          uid = uid,
          pwd = pwd,
          driver = driver))

  return(struct)

}

### Authentication / Authorization

#' Login against the database
#' @description Construct and execute a login query for the given credentials
#' @param dbSettings object as returned by [flamingoDB()]
#' @param uid user name
#' @param pwd user password
#' @return user id if succesful, -1 otherwise
#' @export
#' @md
flamingoDBLogin <- function(dbSettings, uid, pwd) {

  stmt <- buildDbQuery("BFELogin", uid, pwd)
  res <- executeDbQuery(dbSettings, stmt)

  return(as.numeric(res))
}

#' Check interface permissions for a user against the database
#' @description Consult the interface permissions for the user with given
#' user identifier by creating and executing a database query.
#' @param dbSettings object as returned by [flamingoDB()]
#' @param userId user id as returned by [flamingoDBLogin()]
#' @param resourceId resource identifier, e.g. `c("1000")`
#' @return character vector of permission Modes, e.g. #' `c("CRUD" "R")`
#' @export
#' @md
flamingoDBCheckPermissions <- function(dbSettings, userId, resourceId) {

  stmt <- buildDbQuery("getResourceModeUser", userId, as.numeric(resourceId))
  res <- executeDbQuery(dbSettings, stmt)

  return(res[,1])
}



### Get

#' Get the list of companies from the database
#' @inheritParams executeDbQuery
#' @return companies; `data.frame` of 5 variables:
#' \itemize{
#' 		\item `Company ID`
#' 		\item `Company Name`
#'    \item `Company Domicile`
#' 		\item `Company Legal Name`
#' 		\item `Company Registration Number`
#' }
#' @export
#' @md
getCompanyList <- function(dbSettings) {

  stmt <- paste("exec dbo.getCompanies")
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Get the process run overview from the database
#' @param ... other arguments to [executeDbQuery()]
#' @param userId user id as returned by [flamingoDBLogin()]
#' @inheritParams executeDbQuery
#' @return inbox; `data.frame` of 6 variables:
#' \itemize{
#' 		\item `ProgOasisID`
#' 		\item `RunID`
#'    \item `Run Name`
#' 		\item `Model`
#' 		\item `Status`
#'    \item `Completed`
#' }
#' @export
#' @md
getInboxData <- function(dbSettings, userId, ...) {

  stmt <- paste0("exec dbo.getUserProcessDetails ", userId)
  res <- executeDbQuery(dbSettings, stmt, ...)

  return(res)
}

#' Get Process Data
#' @inheritParams executeDbQuery
#' @param pruser process user
#' @param prmodel process model
#' @param prprogramme process programme
#' @param prworkflow process workflow
#' @return process data; `data.frame` of 3 variables:
#' \itemize{
#'   \item `ProgOasisId`
#'   \item `ProgName`
#'   \item `ModelName`
#' }
#' @export
#' @md
getProcessData <- function(dbSettings, pruser, prmodel, prprogramme,
    prworkflow) {

  stmt <- buildDbQuery("getProcessData", pruser, prmodel, prprogramme,
      prworkflow)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Get Oasis Systems
#' @inheritParams executeDbQuery
#' @return oasis systems; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `OasisSystemID`
#' 		\item `OasisSystemName`
#' }
#' @export
#' @md
getOasisSystemId <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getOasisSystemID"))

  return(res)
}

#' Get Process Run
#' @inheritParams executeDbQuery
#' @param procid process id
#' @param prstatus process status
#' @export
#' @md
getProcessRun <- function(dbSettings, procid, prstatus) {

  stmt <- buildDbQuery("ListProcessRun", procid, prstatus)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Get log details for a run
#' @inheritParams executeDbQuery
#' @param wfid workflow id
#' @return log details; `data.frame` of 5 variables:
#' \itemize{
#'     \item `Element ID`
#'     \item `Element Name`
#'     \item `Status`
#'     \item `Description`
#'     \item `CompletedAt`
#' }
#' @export
#' @md
getProcessRunDetails <- function(dbSettings, wfid) {

  if ( wfid != 0 ) {

    res <- executeDbQuery(dbSettings, buildDbQuery("getProcessRunDetails", wfid))

  } else {

    warning("workflow id is 0; returning NULL")
    res <- NULL

  }

  return(res)
}

#' Get Output Files
#' @description Get the list of output files for a given process run id
#' @inheritParams executeDbQuery
#' @param prrunid process run id
#' @return output files; `data.frame` of 10 variables:
#' \itemize{
#'     \item `FileID`
#'     \item `File Name`
#'     \item `Description`
#'     \item `Location`
#'     \item `Location Unix`
#'     \item `File Type`
#'     \item `Owner`
#'     \item `Source`
#'     \item `Resource Table`
#'     \item `Resource Key`
#' }
#' @export
#' @md
getFileList <- function(dbSettings, prrunid) {

  if ( prrunid != 0) {

    stmt <- paste("exec dbo.getFileViewerTable @ProcessRunID = ", prrunid)
    res <- executeDbQuery(dbSettings, stmt)

  } else {

    warning("process run id is 0; returning NULL")
    res <- NULL

  }

  return(res)
}

#' Get the list of output presets
#' @inheritParams executeDbQuery
#' @return output options
#' @export
getOutputOptions <- function(dbSettings, simplify = TRUE) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getOutputOptions"),
      simplify = simplify)

  return(res)
}

#' Get the event set list
#' @inheritParams executeDbQuery
#' @param prgoasisid oasis programme id
#' @return event set
#' @export
getEventSet <- function(dbSettings, prgoasisid, simplify = TRUE){

  res <- executeDbQuery(dbSettings, buildDbQuery("getEventSet", prgoasisid),
      simplify = simplify)

  return(res)
}

#' Get the event occurrence list
#' @inheritParams getEventSet
#' @param prgoasisid oasis programme id
#' @return event occurrence
#' @export
getEventOccurrence <- function(dbSettings, prgoasisid, simplify = TRUE) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getEventOccurrence", prgoasisid),
      simplify = simplify)

  return(res)
}

#' Fetch resource type name and ID
#' @inheritParams executeDbQuery
#' @return resource types; `data.frame` of 3 variables:
#' \itemize{
#' 		\item `ResourceTypeID`
#' 		\item `ResourceTypeName`
#'    \item `Source`
#' }
#' @export
#' @md
getResourceType <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getResourceType"))

  return(res)
}

#' Get Model Data
#' @inheritParams executeDbQuery
#' @return res `data.frame` of 3 variables:
#' \itemize{
#' 		\item `Model ID`
#' 		\item `Model Name`
#'    \item `Model Description`
#' }
#' @export
#' @md
getModelList <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getmodel"))

  return(res)
}

#' Get Source Account Files
#' @inheritParams executeDbQuery
#' @return res `data.frame` of 2 variables:
#' \itemize{
#' 		\item `FileName`
#' 		\item `FileId`
#' }
#' @export
#' @md
getFileSourceAccountFile <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceAccountFile"))

  return(res)
}

#' Get Transforms from Source to Canonical
#' @inheritParams executeDbQuery
#' @return transforms; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `TransformName`
#' 		\item `TransformID`
#' }
#' @export
#' @md
getTransformNameSourceCan <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getTransformNameSourceCan"))

  return(res)
}

#' Get Account Names
#' @inheritParams executeDbQuery
#' @return account name; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `Account ID`
#' 		\item `progOasisId`
#' }
#' @export
#' @md
getAccountName <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getAccount"))

  return(res)
}

#' Get Transforms from Canonical to Model
#' @inheritParams executeDbQuery
#' @return transforms; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `TransformName`
#' 		\item `TransformID`
#' }
#' @export
#' @md
getTransformNameCanModel <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, paste("exec dbo.getTransformNameCanModel"))

  return(res)
}

#' Get File Source Location File
#' @inheritParams executeDbQuery
#' @return  file source`data.frame` of 2 variables:
#' \itemize{
#' 		\item `FileName`
#' 		\item `FileId`
#' }
#' @export
#' @md
getFileSourceLocationFile <- function(dbSettings){

  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceLocationFile"))

  return(res)
}

#' Get Programme Oasis for a given Programme Id
#' @description Run getProgOasisForProg against the database.
#' @inheritParams executeDbQuery
#' @return `data.frame` of 11 variables:
#' \itemize{
#' 		\item `ProgOasisId`
#' 		\item `ProgName`
#' 		\item `ModelName`
#' 		\item `TransformName`
#' 		\item `SourceFileId`
#' 		\item `FileID`
#' 		\item `Status`
#' 		\item `API1aDateTime`
#' 		\item `API1bDateTime`
#' 		\item `API1cDateTime`
#' 		\item `SessionId`
#' }
#' @export
#' @md
getProgOasisForProgdata <- function(dbSettings, progId) {

  stmt <- buildDbQuery("getProgOasisForProg", progId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Get Programme List
#' @description Get a list of programme data.
#' @inheritParams executeDbQuery
#' @return `data.frame` of 7 variables:
#' \itemize{
#'    \item `Programme ID`
#'    \item `Programme Name`
#'    \item `Account ID`
#'    \item `Account Name`
#'    \item `Transform ID`
#'    \item `Transform`
#'    \item `Status`
#' }
#' @export
#' @md
getProgrammeList <- function (dbSettings) {

  res <- executeDbQuery(dbSettings, paste("exec dbo.getProgData"))

  return(res)
}

#' Get User Department
#' @description queries the database to retrieve department login and password
#' details for the designated user.
#' @inheritParams executeDbQuery
#' @param userId user id
#' @export
#' @md
getDeptData <- function(dbSettings, userId) {

  stmt <- buildDbQuery("getUserDepartment", userId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Get Security Groups
#' @inheritParams executeDbQuery
#' @return security groups; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `Security Group ID`
#' 		\item `Security Group Name`
#' }
#' @export
#' @md
getSecurityGroups <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getSecurityGroups"))

  return(res)
}

#' Get Oasis Users
#' @inheritParams executeDbQuery
#' @return oasis users; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `OasisUserID`
#' 		\item `OasisUserName`
#' }
#' @export
#' @md
getOasisUsers <- function(dbSettings) {

  res <- executeDbQuery(dbSettings,
      paste("select OasisUserID, OasisUserName from dbo.OasisUser"))

  return(res)
}

#' Get Process Runtime Param Details
#' @inheritParams executeDbQuery
#' @param processRunId process run id
#' @return param details
#' @export
getProcRunParamFileOutput <- function(dbSettings, processRunId){

  stmt <- buildDbQuery("getUserParamsForProcessRun", processRunId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Get Process Run Details
#' @inheritParams executeDbQuery
#' @param processRunId process run id
#' @return process run details
#' @export
getProcRunDetForFileOutput <- function(dbSettings, processRunId) {

  stmt <- buildDbQuery("getProcessRunDetailsForFileOutput", processRunId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Get location path for a given fileType
#' @inheritParams executeDbQuery
#' @param fileType file type
#' @return path
#' @export
getFileLocationPath <- function(dbSettings, fileType){
  res <- executeDbQuery(dbSettings,
      buildDbQuery("getFileLocationPath", fileType))
  return(res[1,1])
}

### Create

#' Create file record
#' @description document args better
#' @inheritParams executeDbQuery
#' @param fileName file name
#' @param fileDesc file descriptor
#' @param fileType file type
#' @param locPathUnix locPathUnix
#' @param BFEUserID BFEUserID
#' @param ResourceTable ResourceTable
#' @param ResourceKey ResourceKey
#' @return file record id (integer)
#' @export
createFileRecord <- function(dbSettings, fileName, fileDesc,
    fileType, locPathUnix, BFEUserID, ResourceTable, ResourceKey) {
  res <- executeDbQuery(dbSettings,
      buildDbQuery("createFileRecord", fileName, fileDesc, fileType,
          locPathUnix, BFEUserID, ResourceTable, ResourceKey))
  return(unlist(res))
}

#' Create Prog Oasis
#' @inheritParams executeDbQuery
#' @param progId programme id
#' @param modelId model id
#' @param transformId transform id
#' @return Prog Oasis id
#' @export
createProgOasis <- function(dbSettings, progId, modelId, transformId) {

  stmt <- buildDbQuery("createProgOasis", progId, modelId, transformId)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' Create Model Resource
#' @description TODO document
#' @inheritParams executeDbQuery
#' @param modresname `character()`; name of the model resource
#' @param restype resource type
#' @param oasissys TODO document
#' @param modelid model id
#' @param modresval TODO document
#' @return id
#' @export
#' @md
createModelResource <- function(
    dbSettings,
    modresname,
    restype,
    oasissys,
    modelid,
    modresval) {

  stmt <- buildDbQuery("createModelResource", modresname, restype, oasissys,
      modelid, modresval)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}



### Update

#' Update Model Resource
#' @inheritParams executeDbQuery
#' @inheritParams createModelResource
#' @param modresid model resource id
#' @return model resource id
updateModelResource <- function(
    dbSettings,
    modresid,
    modresname,
    restype,
    oasissys,
    modelid,
    modresval) {

  stmt <- buildDbQuery("updateModelResource", modresid, modresname, restype,
      oasissys, modelid, modresval)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}



### Delete

#' Delete Model Resource
#' @inheritParams updateModelResource
#' @return model resource id
#' @export
deleteModelResource <- function(dbSettings, modresid) {

  stmt <- buildDbQuery("deleteModelResource", modresid)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}


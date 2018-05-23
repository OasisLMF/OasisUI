
#' Convenience function to execute database queries
#' @description Opens a new connection to the database, executes the given
#' query statement and then closes the connection again. 
#' @param dbSettings setting object as returned by e.g. \link{flamingoDB}
#' @param statement query statement to execute, e.g. as returned by
#' \link{buildDbQuery}
#' @param simplify if \code{TRUE}, a vector will be returned instead of
#' a \code{data.frame} only a single column is returned.
#' @param verbose print query statement
#' @param logMessage function to call with log message
#' @param logError function to call with error log message
#' @param ... further arguments to \link{dbGetQuery}
#' @return result of the query as a \code{data.frame} or a vector if
#' \code{simplify} is set to \code{TRUE}. \code{NULL} if no result
#' can be returned.
#' @importFrom DBI dbGetQuery dbConnect dbDisconnect
#' @export 
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
#' \link{dbExecute}. 
#' @param dbFunc database function to call
#' @param ... arguments to \code{dbFunc}. \code{character()} args are quoted
#' with single quotes (').
#' @param dboPrefix prefix \code{dbFunc} with \code{"dbo."}
#' @param squareBrackets surround the argument list with [ and ]
#' @export
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
#' @param dbSettings object as returned by \link{flamingoDB}
#' @param uid user name
#' @param pwd user password
#' @return user id if succesful, -1 otherwise
#' @export
flamingoDBLogin <- function(dbSettings, uid, pwd) {
  
  stmt <- buildDbQuery("BFELogin", uid, pwd)
  res <- executeDbQuery(dbSettings, stmt)
  
  return(as.numeric(res))
}

#' Check interface permissions for a user against the database
#' @description Consult the interface permissions for the user with given
#' user identifier by creating and executing a database query.
#' @param dbSettings object as returned by \link{flamingoDB}
#' @param userId user id as returned by \link{flamingoDBLogin}
#' @param resourceId resource identifier, e.g. \code{c("1000"}
#' @return character vector of permission Modes, e.g. #' \code{c("CRUD" "R")}
#' @export
flamingoDBCheckPermissions <- function(dbSettings, userId, resourceId) {

  stmt <- buildDbQuery("getResourceModeUser", userId, as.numeric(resourceId))
  res <- executeDbQuery(dbSettings, stmt)
  
  return(res[,1])
}



### Get

#' Get the list of companies from the database
#' @inheritParams executeDbQuery
#' @return companies; \code{data.frame} of 5 variables:
#' \itemize{
#' 		\item \code{Company ID}
#' 		\item \code{Company Name}
#'    \item \code{Company Domicile}
#' 		\item \code{Company Legal Name}
#' 		\item \code{Company Registration Number}
#' }
#' @export
getCompanyList <- function(dbSettings) {
  
  stmt <- paste("exec dbo.getCompanies")
  res <- executeDbQuery(dbSettings, stmt)
  
  return(res)
}

#' Get the process run overview from the database
#' @param ... other arguments to \link{executeDbQuery}
#' @param userId user id as returned by \link{flamingoDBLogin}
#' @inheritParams executeDbQuery
#' @return inbox; \code{data.frame} of 6 variables:
#' \itemize{
#' 		\item \code{ProgOasisID}
#' 		\item \code{RunID}
#'    \item \code{Run Name}
#' 		\item \code{Model}
#' 		\item \code{Status}
#'    \item \code{Completed}
#' }
#' @export
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
#' @return process data; \code{data.frame} of 3 variables:
#' \itemize{
#'   \item \code{ProgOasisId}
#'   \item \code{ProgName}
#'   \item \code{ModelName}
#' }
#' @export
getProcessData <- function(dbSettings, pruser, prmodel, prprogramme,
    prworkflow) {
  
  stmt <- buildDbQuery("getProcessData", pruser, prmodel, prprogramme,
      prworkflow)
  
  res <- executeDbQuery(dbSettings, stmt)
  
  return(res)
}

#' Get Oasis Systems
#' @inheritParams executeDbQuery
#' @return oasis systems; \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{OasisSystemID}
#' 		\item \code{OasisSystemName}
#' }
#' @export
getOasisSystemId <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getOasisSystemID"))
  
  return(res)
}

#' Get Process Run
#' @inheritParams executeDbQuery
#' @param procid process id
#' @param prstatus process status
#' @export
getProcessRun <- function(dbSettings, procid, prstatus) {
  
  stmt <- buildDbQuery("ListProcessRun", procid, prstatus)
  
  res <- executeDbQuery(dbSettings, stmt)
  
  return(res)
}

#' Get log details for a run
#' @inheritParams executeDbQuery
#' @param wfid workflow id
#' @return log details; \code{data.frame} of 5 variables:
#' \itemize{
#'     \item \code{Element ID}
#'     \item \code{Element Name}
#'     \item \code{Status}
#'     \item \code{Description}
#'     \item \code{CompletedAt}
#' }
#' @export
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
#' @return output files; \code{data.frame} of 10 variables:
#' \itemize{
#'     \item \code{FileID} 
#'     \item \code{File Name} 
#'     \item \code{Description} 
#'     \item \code{Location} 
#'     \item \code{Location Unix} 
#'     \item \code{File Type} 
#'     \item \code{Owner} 
#'     \item \code{Source} 
#'     \item \code{Resource Table} 
#'     \item \code{Resource Key} 
#' }
#' @export
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
#' @return resource types; \code{data.frame} of 3 variables:
#' \itemize{
#' 		\item \code{ResourceTypeID}
#' 		\item \code{ResourceTypeName}
#'    \item \code{Source}
#' }
#' @export
getResourceType <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getResourceType"))
  
  return(res)
}

#' Get Model Data
#' @inheritParams executeDbQuery
#' @return \code{data.frame} of 3 variables:
#' \itemize{
#' 		\item \code{Model ID}
#' 		\item \code{Model Name}
#'    \item \code{Model Description}
#' }
#' @export
getModelList <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getmodel"))
  
  return(res)
}

#' Get Source Account Files
#' @inheritParams executeDbQuery
#' @return \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{FileName}
#' 		\item \code{FileId} 
#' }
#' @export
getFileSourceAccountFile <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceAccountFile"))
  
  return(res)
}

#' Get Transforms from Source to Canonical
#' @inheritParams executeDbQuery
#' @return transforms; \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{TransformName}
#' 		\item \code{TransformID} 
#' }
#' @export
getTransformNameSourceCan <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getTransformNameSourceCan"))
  
  return(res)
}

#' Get Account Names
#' @inheritParams executeDbQuery
#' @return \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{Account ID}
#' 		\item \code{progOasisId} 
#' }
#' @export
getAccountName <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getAccount"))
  
  return(res)
}

#' Get Transforms from Canonical to Model
#' @inheritParams executeDbQuery
#' @return transforms; \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{TransformName}
#' 		\item \code{TransformID}
#' }
#' @export
getTransformNameCanModel <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, paste("exec dbo.getTransformNameCanModel"))
  
  return(res)
}

#' Get File Source Location File
#' @inheritParams executeDbQuery
#' @return \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{FileName}
#' 		\item \code{FileId}
#' }
#' @export
getFileSourceLocationFile <- function(dbSettings){
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceLocationFile"))
  
  return(res)
}

#' Get Programme Oasis for a given Programme Id
#' @description Run getProgOasisForProg against the database.
#' @inheritParams executeDbQuery
#' @return \code{data.frame} of 11 variables:
#' \itemize{
#' 		\item \code{ProgOasisId}
#' 		\item \code{ProgName}
#' 		\item \code{ModelName}
#' 		\item \code{TransformName} 
#' 		\item \code{SourceFileId}
#' 		\item \code{FileID} 
#' 		\item \code{Status}
#' 		\item \code{API1aDateTime} 
#' 		\item \code{API1bDateTime}
#' 		\item \code{API1cDateTime} 
#' 		\item \code{SessionId}
#' }
#' @export
getProgOasisForProgdata <- function(dbSettings, progId) {
  
  stmt <- buildDbQuery("getProgOasisForProg", progId)
  res <- executeDbQuery(dbSettings, stmt)
  
  return(res)
}

#' Get Programme List
#' @description Get a list of programme data.
#' @inheritParams executeDbQuery
#' @return \code{data.frame} of 7 variables:
#' \itemize{
#'    \item \code{Programme ID}
#'    \item \code{Programme Name}
#'    \item \code{Account ID}
#'    \item \code{Account Name}
#'    \item \code{Transform ID}
#'    \item \code{Transform}
#'    \item \code{Status}
#' }
#' @export
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
getDeptData <- function(dbSettings, userId) {
  
  stmt <- buildDbQuery("getUserDepartment", userId)
  res <- executeDbQuery(dbSettings, stmt)
  
  return(res)
}

#' Get Security Groups
#' @inheritParams executeDbQuery
#' @return security groups; \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{Security Group ID}
#' 		\item \code{Security Group Name}
#' }
#' @export
getSecurityGroups <- function(dbSettings) {
  
  res <- executeDbQuery(dbSettings, buildDbQuery("getSecurityGroups"))
  
  return(res)
}

#' Get Oasis Users
#' @inheritParams executeDbQuery
#' @return oasis users; \code{data.frame} of 2 variables:
#' \itemize{
#' 		\item \code{OasisUserID}
#' 		\item \code{OasisUserName}
#' }
#' @export
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
getProcRunDetForFileOutput <- function(processRunId) {
  
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
#' @param modresname \code{character()}; name of the model resource
#' @param restype resource type
#' @param oasissys TODO document
#' @param modelid model id
#' @param modresval TODO document
#' @return id
#' @export
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


#' executeDbQuery
#'
#' @rdname executeDbQuery
#'
#' @description Opens a new connection to the database, executes the given query
#'   statement and then closes the connection again.
#'
#' @param dbSettings Setting object as returned by e.g. [flamingoDB()].
#' @param statement Query statement to execute, e.g. as returned by
#'   [buildDbQuery()].
#' @param simplify If `TRUE`, a vector will be returned instead of a
#'   `data.frame` only a single column is returned.
#' @param verbose Print query statement.
#' @param logMessage Function to call with log message.
#' @param logError Function to call with error log message.
#' @param ... Further arguments to [dbGetQuery()].
#'
#' @return Result of the query as a `data.frame` or a vector if `simplify` is
#'   set to `TRUE`. `NULL` if no result can be returned.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#'
#' @export
#'
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


#' buildDbQuery
#'
#' @rdname buildDbQuery
#'
#' @description Constructs query statements that can be executed with
#'   [DBI::dbExecute()].
#'
#' @param dbFunc Database function to call.
#' @param ... Arguments to `dbFunc`. `character()` args are quoted with single
#'   quotes (').
#' @param dboPrefix Prefix `dbFunc` with `"dbo."`.
#' @param squareBrackets Surround the argument list with square brackets.
#'
#' @export
#'
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

#' flamingoDB
#'
#' @rdname flamingoDB
#'
#' @description Creates a database settings object which can then be used
#' to create new connections to the Flamingo Database.
#'
#' @inheritParams companyDefinition
#' @param server Host name.
#' @param port Host port.
#' @param database Database name.
#' @param pwd User pass.
#' @param driver Connection driver.
#'
#' @return A database setttings object (list).
#'
#' @importFrom odbc odbc
#'
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

#' flamingoDBLogin
#'
#' @rdname flamingoDBLogin
#'
#' @description Construct and execute a login query for the given credentials.
#'
#' @inheritParams flamingoDBLogin
#' @inheritParams flamingoDB
#'
#' @return User id if succesful, -1 otherwise.
#'
#' @export
#'
#' @md
flamingoDBLogin <- function(dbSettings, uid, pwd) {

  stmt <- buildDbQuery("BFELogin", uid, pwd)
  res <- executeDbQuery(dbSettings, stmt)

  return(as.numeric(res))
}

#' flamingoDBCheckPermissions
#'
#' @rdname flamingoDBCheckPermissions
#'
#' @description Consult the interface permissions for the user with given
#' user identifier by creating and executing a database query.
#'
#' @inheritParams flamingoDBLogin
#' @inheritParams companyDefinition
#' @param resourceId Resource identifier, e.g. `c("1000")`.
#'
#' @return Character vector of permission Modes, e.g. #' `c("CRUD" "R")`.
#'
#' @export
#'
#' @md
flamingoDBCheckPermissions <- function(dbSettings, userId, resourceId) {

  stmt <- buildDbQuery("getResourceModeUser", userId, as.numeric(resourceId))
  res <- executeDbQuery(dbSettings, stmt)

  return(res[,1])
}



### Get

#' getCompanyList
#'
#' @rdname getCompanyList
#'
#' @description Get the list of companies from the database.
#'
#' @inheritParams executeDbQuery
#'
#' @return Companies; `data.frame` of 5 variables:
#' \itemize{
#' 		\item `Company ID`
#' 		\item `Company Name`
#'    \item `Company Domicile`
#' 		\item `Company Legal Name`
#' 		\item `Company Registration Number`
#' }.
#'
#' @export
#'
#' @md
getCompanyList <- function(dbSettings) {

  stmt <- paste("exec dbo.getCompanies")
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' getInboxData
#'
#' @rdname getInboxData
#'
#' @description Get the process run overview from the database.
#'
#' @param ... Other arguments to [executeDbQuery()].
#' @inheritParams companyDefinition
#'
#' @inheritParams executeDbQuery
#'
#' @return Inbox; `data.frame` of 6 variables:
#' \itemize{
#' 		\item `ProgOasisID`
#' 		\item `RunID`
#'    \item `Run Name`
#' 		\item `Model`
#' 		\item `Status`
#'    \item `Completed`
#' }.
#'
#' @export
#'
#' @md
getInboxData <- function(dbSettings, userId, ...) {

  stmt <- paste0("exec dbo.getUserProcessDetails ", userId)
  res <- executeDbQuery(dbSettings, stmt, ...)

  return(res)
}

#' getProcessData
#'
#' @rdname getProcessData
#'
#' @description Get Process Data.
#'
#' @inheritParams executeDbQuery
#' @param pruser Process user.
#' @param prmodel Process model.
#' @param prprogramme Process programme.
#' @param prworkflow Process workflow.
#'
#' @return Process data; `data.frame` of 3 variables:
#' \itemize{
#'   \item `ProgOasisId`
#'   \item `ProgName`
#'   \item `ModelName`
#' }.
#'
#' @export
#'
#' @md
getProcessData <- function(dbSettings, pruser, prmodel, prprogramme,
    prworkflow) {

  stmt <- buildDbQuery("getProcessData", pruser, prmodel, prprogramme,
      prworkflow)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' getOasisSystemId
#'
#' @rdname getOasisSystemId
#'
#' @description Get Oasis Systems.
#'
#' @inheritParams executeDbQuery
#'
#' @return Oasis systems; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `OasisSystemID`
#' 		\item `OasisSystemName`
#' }.
#'
#' @export
#'
#' @md
getOasisSystemId <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getOasisSystemID"))

  return(res)
}

#' getProcessRun
#'
#' @rdname getProcessRun
#'
#' @description Get Process Run.
#'
#' @inheritParams executeDbQuery
#' @param procid Process id.
#' @param prstatus Process status.
#'
#' @export
#'
#' @md
getProcessRun <- function(dbSettings, procid, prstatus) {

  stmt <- buildDbQuery("ListProcessRun", procid, prstatus)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}

#' getProcessRunDetails
#'
#' @rdname getProcessRunDetails
#'
#' @description Get log details for a run.
#'
#' @inheritParams executeDbQuery
#' @param wfid Workflow id.
#'
#' @return Log details; `data.frame` of 5 variables:
#' \itemize{
#'     \item `Element ID`
#'     \item `Element Name`
#'     \item `Status`
#'     \item `Description`
#'     \item `CompletedAt`
#' }.
#'
#' @export
#'
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
#'
#' @rdname getFileList
#'
#' @description Get the list of output files for a given process run id.
#'
#' @inheritParams executeDbQuery
#' @param prrunid Process run id.
#'
#' @return Output files; `data.frame` of 10 variables:
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
#' }.
#'
#' @export
#'
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

#' getOutputOptions
#'
#' @rdname getOutputOptions
#'
#' @description Get the list of output presets.
#'
#' @inheritParams executeDbQuery
#'
#' @return Output options.
#'
#' @export
getOutputOptions <- function(dbSettings, simplify = TRUE) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getOutputOptions"),
      simplify = simplify)

  return(res)
}

#' getEventSet
#'
#' @rdname getEventSet
#'
#' @description Get the event set list.
#'
#' @inheritParams executeDbQuery
#' @param prgoasisid Oasis programme id.
#'
#' @return Event set.
#'
#' @export
getEventSet <- function(dbSettings, prgoasisid, simplify = TRUE){

  res <- executeDbQuery(dbSettings, buildDbQuery("getEventSet", prgoasisid),
      simplify = simplify)

  return(res)
}

#' getEventOccurrence
#'
#' @rdname getEventOccurrence
#'
#' @description Get the event occurrence list.
#'
#' @inheritParams getEventSet
#' @inheritParams getEventSet
#'
#' @return Event occurrence.
#'
#' @export
getEventOccurrence <- function(dbSettings, prgoasisid, simplify = TRUE) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getEventOccurrence", prgoasisid),
      simplify = simplify)

  return(res)
}

#' getResourceType
#'
#' @rdname getResourceType
#'
#' @description Fetch resource type name and ID.
#'
#' @inheritParams executeDbQuery
#'
#' @return Resource types; `data.frame` of 3 variables:
#' \itemize{
#' 		\item `ResourceTypeID`
#' 		\item `ResourceTypeName`
#'    \item `Source`
#' }.
#'
#' @export
#'
#' @md
getResourceType <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getResourceType"))

  return(res)
}

#' getModelList
#'
#' @rdname getModelList
#'
#' @description Get Model Data.
#'
#' @inheritParams executeDbQuery
#'
#' @return Res `data.frame` of 3 variables:
#' \itemize{
#' 		\item `Model ID`
#' 		\item `Model Name`
#'    \item `Model Description`
#' }.
#'
#' @export
#'
#' @md
getModelList <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getmodel"))

  return(res)
}

#' getFileSourceAccountFile
#'
#' @rdname getFileSourceAccountFile
#'
#' @description Get Source Account Files.
#'
#' @inheritParams executeDbQuery
#'
#' @return res `data.frame` of 2 variables:
#' \itemize{
#' 		\item `FileName`
#' 		\item `FileId`
#' }.
#'
#' @export
#'
#' @md
getFileSourceAccountFile <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceAccountFile"))

  return(res)
}


#' getTransformNameSourceCan
#'
#' @rdname getTransformNameSourceCan
#'
#' @description Get Transforms from Source to Canonical.
#'
#' @inheritParams executeDbQuery
#'
#' @return Transforms; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `TransformName`
#' 		\item `TransformID`
#' }.
#'
#' @export
#'
#' @md
getTransformNameSourceCan <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getTransformNameSourceCan"))

  return(res)
}


#' getAccountName
#'
#' @rdname getAccountName
#'
#' @description Get Account Names.
#'
#' @inheritParams executeDbQuery
#'
#' @return Account name; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `Account ID`
#' 		\item `progOasisId`
#' }.
#'
#' @export
#'
#' @md
getAccountName <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getAccount"))

  return(res)
}


#' getTransformNameCanModel
#'
#' @rdname getTransformNameCanModel
#'
#' @description Get Transforms from Canonical to Model.
#'
#' @inheritParams executeDbQuery
#'
#' @return Transforms; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `TransformName`
#' 		\item `TransformID`
#' }.
#'
#' @export
#'
#' @md
getTransformNameCanModel <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, paste("exec dbo.getTransformNameCanModel"))

  return(res)
}


#' getFileSourceLocationFile
#'
#' @rdname getFileSourceLocationFile
#'
#' @description Get File Source Location File.
#'
#' @inheritParams executeDbQuery
#'
#' @return  file source`data.frame` of 2 variables:
#' \itemize{
#' 		\item `FileName`
#' 		\item `FileId`
#' }.
#'
#' @export
#'
#' @md
getFileSourceLocationFile <- function(dbSettings){

  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceLocationFile"))

  return(res)
}


#' getFileSourceReinsuranceFile
#'
#' @rdname getFileSourceReinsuranceFile
#'
#' @description Get File Source Reinsurance File.
#'
#' @inheritParams executeDbQuery
#'
#' @return \code{data.frame} of 2 variables:
#' \itemize{
#'              \item \code{FileName}
#'              \item \code{FileId}
#' }.
#'
#' @export
getFileSourceReinsuranceFile <- function(dbSettings){

  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceReinsuranceFile"))

  return(res)
}


#' getFileSourceReinsuranceScopeFile
#'
#' @rdname getFileSourceReinsuranceScopeFile
#'
#' @description Get File Source Reinsurance Scope File.
#'
#' @inheritParams executeDbQuery
#'
#' @return \code{data.frame} of 2 variables:
#' \itemize{
#'              \item \code{FileName}
#'              \item \code{FileId}
#' }.
#'
#' @export
getFileSourceReinsuranceScopeFile <- function(dbSettings){

  res <- executeDbQuery(dbSettings, buildDbQuery("getFileSourceReinsuranceScopeFile"))

  return(res)
}


#' getProgOasisForProgdata
#'
#' @rdname getProgOasisForProgdata
#'
#' @description Get Programme Oasis for a given Programme Id.
#'
#' @details Run getProgOasisForProg against the database.
#'
#' @inheritParams executeDbQuery
#'
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
#' }.
#'
#' @export
#'
#' @md
getProgOasisForProgdata <- function(dbSettings, progId) {

  stmt <- buildDbQuery("getProgOasisForProg", progId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}


#' getProgrammeList
#'
#' @rdname getProgrammeList
#'
#' @description Get a list of programme data.
#'
#' @inheritParams executeDbQuery
#'
#' @return `data.frame` of 7 variables:
#' \itemize{
#'    \item `Programme ID`
#'    \item `Programme Name`
#'    \item `Account ID`
#'    \item `Account Name`
#'    \item `Transform ID`
#'    \item `Transform`
#'    \item `Status`
#' }.
#'
#' @export
#'
#' @md
getProgrammeList <- function (dbSettings) {

  res <- executeDbQuery(dbSettings, paste("exec dbo.getProgData"))

  return(res)
}


#' getDeptData
#'
#' @rdname getDeptData
#'
#' @description Queries the database to retrieve department login and password
#' details for the designated user.
#'
#' @inheritParams executeDbQuery
#' @inheritParams companyDefinition
#'
#' @export
#'
#' @md
getDeptData <- function(dbSettings, userId) {

  stmt <- buildDbQuery("getUserDepartment", userId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}


#' getSecurityGroups
#'
#' @rdname getSecurityGroups
#'
#' @description Get Security Groups.
#'
#' @inheritParams executeDbQuery
#'
#' @return security groups; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `Security Group ID`
#' 		\item `Security Group Name`
#' }.
#'
#' @export
#'
#' @md
getSecurityGroups <- function(dbSettings) {

  res <- executeDbQuery(dbSettings, buildDbQuery("getSecurityGroups"))

  return(res)
}


#' getOasisUsers
#'
#' @rdname getOasisUsers
#'
#' @description Get Oasis Users.
#'
#' @inheritParams executeDbQuery
#'
#' @return oasis users; `data.frame` of 2 variables:
#' \itemize{
#' 		\item `OasisUserID`
#' 		\item `OasisUserName`
#' }.
#'
#' @export
#'
#' @md
getOasisUsers <- function(dbSettings) {

  res <- executeDbQuery(dbSettings,
      paste("select OasisUserID, OasisUserName from dbo.OasisUser"))

  return(res)
}


#' getProcRunParamFileOutput
#'
#' @rdname getProcRunParamFileOutput
#'
#' @description Get Process Runtime Param Details.
#'
#' @inheritParams executeDbQuery
#' @param processRunId Reactive string expression for reselected run id from defineProgramme.
#'
#' @return Param details.
#'
#' @export
getProcRunParamFileOutput <- function(dbSettings, processRunId){

  stmt <- buildDbQuery("getUserParamsForProcessRun", processRunId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}


#' getProcRunDetForFileOutput
#'
#' @rdname getProcRunDetForFileOutput
#'
#' @description Get Process Run Details.
#'
#' @inheritParams executeDbQuery
#' @inheritParams getProcRunParamFileOutput
#'
#' @return Process run details.
#'
#' @export
getProcRunDetForFileOutput <- function(dbSettings, processRunId) {

  stmt <- buildDbQuery("getProcessRunDetailsForFileOutput", processRunId)
  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}


#' getFileLocationPath
#'
#' @rdname getFileLocationPath
#'
#' @description Get location path for a given fileType.
#'
#' @inheritParams executeDbQuery
#' @param fileType File type.
#'
#' @return Path.
#'
#' @export
getFileLocationPath <- function(dbSettings, fileType){
  res <- executeDbQuery(dbSettings,
      buildDbQuery("getFileLocationPath", fileType))
  return(res[1,1])
}

### Create

#' createFileRecord
#'
#' @rdname createFileRecord
#'
#' @description Document args better.
#'
#' @inheritParams executeDbQuery
#' @inheritParams getFileLocationPath
#' @param fileName File name.
#' @param fileDesc File describer.
#' @param locPathUnix locPathUnix.
#' @param BFEUserID BFEUserID.
#' @param ResourceTable ResourceTable.
#' @param ResourceKey ResourceKey.
#'
#' @return File record id (integer).
#'
#' @export
createFileRecord <- function(dbSettings, fileName, fileDesc,
    fileType, locPathUnix, BFEUserID, ResourceTable, ResourceKey) {
  res <- executeDbQuery(dbSettings,
      buildDbQuery("createFileRecord", fileName, fileDesc, fileType,
          locPathUnix, BFEUserID, ResourceTable, ResourceKey))
  return(unlist(res))
}


#' createProgOasis
#'
#' @rdname createProgOasis
#'
#' @description Create Prog Oasis.
#'
#' @inheritParams executeDbQuery
#' @param progId Programme id.
#' @param modelId Model id.
#' @param transformId Transform id.
#'
#' @return Prog Oasis id.
#'
#' @export
createProgOasis <- function(dbSettings, progId, modelId, transformId) {

  stmt <- buildDbQuery("createProgOasis", progId, modelId, transformId)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}


#' createModelResource
#'
#' @rdname createModelResource
#'
#' @description Create Model Resource.
#'
#' @inheritParams executeDbQuery
#' @param modresname `character()`; name of the model resource.
#' @param restype Resource type.
#' @param oasissys TODO document.
#' @param modelid Model id.
#' @param modresval TODO document.
#'
#' @return id.
#'
#' @export
#'
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


#' updateModelResource
#'
#' @rdname updateModelResource
#'
#' @description Update Model Resource.
#'
#' @inheritParams executeDbQuery
#' @inheritParams createModelResource
#' @param modresid Model resource id.
#'
#' @return Model resource id.
#'
#' @export
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


#' deleteModelResource
#'
#' @rdname deleteModelResource
#'
#' @description Delete Model Resource.
#'
#' @inheritParams updateModelResource
#'
#' @return Model resource id.
#'
#' @export
deleteModelResource <- function(dbSettings, modresid) {

  stmt <- buildDbQuery("deleteModelResource", modresid)

  res <- executeDbQuery(dbSettings, stmt)

  return(res)
}


#' executeDbQuery
#'
#' @rdname executeDbQuery
#'
#' @description Opens a new connection to the database, executes the given query
#'   statement and then closes the connection again.
#'
#' @param dbSettings Setting object.
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


flamingoDB <- function(...) {
  return(NULL)
}


### Get
getCompanyList <- function(...) {
  return(NULL)
}

getOasisSystemId <- function(...) {
  return(NULL)
}

getResourceType <- function(...) {
  return(NULL)
}

getModelList <- function(...) {
  return(NULL)
}

getDeptData <- function(...) {
  return(NULL)
}

getSecurityGroups <- function(...) {
  return(NULL)
}

getOasisUsers <- function(...) {
  return(NULL)
}

### Create
createModelResource <- function(...) {
  return(NULL)
}

### Update
updateModelResource <- function(...) {
  return(NULL)
}

### Delete
deleteModelResource <- function(...) {
  return(NULL)
}

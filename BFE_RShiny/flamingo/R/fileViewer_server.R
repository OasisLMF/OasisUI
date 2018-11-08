#' File Viewer Module
#'
#' @rdname fileViewer
#'
#' @description Server logic to view files.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#' 
#' @param preselRunId Reactive string expression for reselected run id from landingpage.
#'
#' @importFrom dplyr select
#'
#' @export
fileViewer <- function(
  input,
  output,
  session,
  dbSettings,
  userId,
  preselRunId = reactive(-1),
  active, #= reactive(TRUE),
  logMessage = message) {

  # Reactive Values ------------------------------------------------------------

  result <- reactiveValues(
    FLdata = NULL
  )

  ns <- session$ns

  navigation_state <- reactiveNavigation()

  ### File List Table ----------------------------------------------------------

  # Load Company user list data when the page is loaded
  # queries the database every time to update its dataset
  observe(if (active()) {
    stmt <- buildDbQuery("getFileViewerTable")
    FLdata <- executeDbQuery(dbSettings, stmt)
    result$FLdata <- FLdata %>% select(-c(Source))
  })

  observeEvent(input$refreshtable, {
    stmt <- buildDbQuery("getFileViewerTable")
    FLdata <- executeDbQuery(dbSettings, stmt)
    result$FLdata <- FLdata %>% select(-c(Source))
  })

  # Pre-select the correct runId
  initialSelection <- reactive({
    if (preselRunId() == -1) {
      index <- 1
      initialSelection <- NULL
    } else {
      index <- match(c(paste0("Process:", preselRunId())), result$FLdata[[7]])
      initialSelection <- rownames(result$FLdata)[c(as.integer(index))]
    }
    return(initialSelection)
  })

  ViewFilesModule <- callModule(
    ViewFilesModule,
    id = "ViewFilesModule",
    filesListData =  reactive({result$FLdata}),
    logMessage = logMessage,
    includechkbox = TRUE)


  # Module Outout --------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list()
  )

  moduleOutput
}

# output files Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title outputfiles_ui
#' Run outputfiles UI
#' @rdname outputfilesUI
#' @description output files of a Run
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' @export
outputfilesUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    flamingoPanel(
      id = ns("flamingoPanelViewOutputFiles"),
      collapsible = TRUE,
      show = TRUE,
      heading = "Output Files Table",
      ViewFilesModuleUI(id  = ns("ViewOutputFilesModule"), includechkbox = TRUE)
    ),
    
    flamingoPanel(
      id = ns("flamingoPanelViewInputFiles"),
      collapsible = TRUE,
      show = FALSE,
      heading = "Input Files Table",
      ViewFilesModuleUI(id  = ns("ViewInputFilesModule"), includechkbox = TRUE)
    )
    
  )
}


# Server -----------------------------------------------------------------------

#' @title outputfiles_server
#' Run outputfiles Server
#' @rdname outputfiles
#' @description output files of a Run
#' @inheritParams flamingoModule
#' @return list of tags
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable
#' @importFrom dplyr mutate select contains filter
#' @export
#' @export
outputfiles <- function(input, output, session, dbSettings,
                        apiSettings, userId,
                        filesListDatatoview, active, logMessage = message) {
  
  ns <- session$ns
  
  # list of sub-modules
  sub_modules <- list()
  
  sub_modules$ViewFilesModule <- callModule(
    ViewFilesModule,
    id = "ViewOutputFilesModule",
    filesListData =  filesListDatatoview,
    logMessage = logMessage,
    includechkbox = TRUE)
  
  sub_modules$ViewFilesModule <- callModule(
    ViewFilesModule,
    id = "ViewInputFilesModule",
    filesListData =  reactive(NULL),
    logMessage = logMessage,
    includechkbox = TRUE)
}
# visualization Batch Run Browse Module UI -------------------------------------

#' visualizationBBRUI
#'
#' @rdname visualizationBBR
#'
#' @description UI/View for batchbrowse run page.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#'
#' @export

visualizationBBRUI <- function(id) {
  
  ns <- NS(id)
  
  #Parameters
  w <- 6
  labelconfig <- "Go to Configure Batch Output"
  
  tagList(
    
    panel(
      status = "primary",
      fluidRow(
        defineIDUI(ns("defineID"), w, batch = TRUE),
        column(w,
               align = "right",
               actionButton(inputId = ns("abuttongotoconfig"), label = labelconfig)))
    ),
    
    tabsetPanel(
      id = ns("tabsBBR"),
      
      tabPanel(
        title = "Summary",
        summarytabUI(ns("summarytab")),
        value = ns("tabsummary")
      ),
      
      tabPanel(
        title = "Plots",
        outputplotsUI(ns("outputplots")),
        value = ns("tabplots")
      ),
      
      tabPanel(
        title = "Files",
        outputfilesUI(ns("outputfiles")),
        value = ns("taboutputfiles")
      )
      
    )
  )
  
}

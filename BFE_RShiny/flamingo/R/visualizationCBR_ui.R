# visualization Compare Runs Browse Module UI ----------------------------------

#' visualizationCBRUI
#'
#' @rdname visualizationCBR
#'
#' @description UI/View for comparing runs run page.
#'
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#'
#' @export
visualizationCBRUI <- function(id) {
  
  ns <- NS(id)
  
  #parameters
  w <- 4
  labelconfig <- "Go to Configure Output"
  
  tagList(
    
    #defineCompareRunIDs
    panel(
      status = "primary",
      fluidRow(
        defineIDUI(ns("defineID-1"), w, batch = FALSE),
        defineIDUI(ns("defineID-2"), w, batch = FALSE),
        column(w,
               align = "right",
               actionButton(inputId = ns("abuttongotoconfig"), label = labelconfig)))
    ),
    
    tabsetPanel(
      id = ns("tabsCBR"),
      
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

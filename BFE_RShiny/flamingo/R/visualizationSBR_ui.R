# visualization Single Run Browse Module UI ------------------------------------

#' visualizationSBRUI
#' 
#' @rdname visualizationSBR
#' 
#' @description UI/View for viewing results of a single run.
#' 
#' @template params-module-ui
#' 
#' @return List of tags.
#' 
#' @importFrom shinyWidgets panel
#' 
#' @export
visualizationSBRUI <- function(id) {
 
  ns <- NS(id)
  
  #Params
  w <- 6
  labelconfig <- "Go to Configure Output"
  
  tagList(
    
    # h4("Browse Run Outputs", class = "flamingo-page-title") ,
    panel(
      status = "primary",
      fluidRow(
        defineIDUI(ns("defineID"), w, batch = FALSE),
        column(w,
               align = "right",
               actionButton(inputId = ns("abuttongotoconfig"), label = labelconfig)))
    ),
    
    tabsetPanel(
      id = ns("tabsSBR"),
      
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

# visualization Batch Run Browse Module UI ------------------------------------

#' @title visualizationBBR_ui
#' visualization Batch Browse Run UI
#' @rdname visualizationBBRUI
#' @description UI/View for the batch process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @export
visualizationBBRUI <- function(id) {

  ns <- NS(id)
  
  tagList(
    
    panelDefineBatchRunID(id),
    
    tabsetPanel(
      id = ns("tabsBBR"),
      
      tabPanel(
        title = "Summary",
        summaryUI(ns("summary")),
        value = ns("tabsummary")
      ),
      
      tabPanel(
        title = "Plots",
        outputplotsUI(ns("outputplots")),
        value = ns("tabplots")
      ),
      
      tabPanel(
        title = "Output Files",
        outputfilesUI(ns("outputfiles")),
        value = ns("taboutputfiles")
      )
      
    )
  )

}

# UI Functions -----------------------------------------------------------------
#' @title panelDefineBatchRunID
#' @rdname panelDefineBatchRunID
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' @export
panelDefineBatchRunID <-  function(id){
  ns <- NS(id)
  panel(
    status = "primary",
    fluidRow(
      column(6,
             selectInput(inputId =  ns("selectBatchRunID"), label = "Batch Run ID", choices = "", selected = NULL) %>%
               bs_embed_tooltip(title = browse_programmes$selectBatchRunID, placement = "right")),
      column(6,
             align = "right",
             actionButton(inputId = ns("abuttongotobatchconfig"), label = "Go to Batch Configure Output"))
    )
  )
}

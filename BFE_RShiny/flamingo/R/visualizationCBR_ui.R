# visualization Compare Runs Browse Module UI ------------------------------------

#' @title visualizationCBR_ui
#' visualization Compare Browse Run UI
#' @rdname visualizationCBRUI
#' @description UI/View for the compare process runs page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @export
visualizationCBRUI <- function(id) {

  ns <- NS(id)
  
  tagList(
    
    panelDefineCompareRunIDs(id),
    
    tabsetPanel(
      id = ns("tabsCBR"),
      
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
#' @title panelDefineCompareRunIDs
#' @rdname panelDefineCompareRunIDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' @export
panelDefineCompareRunIDs <-  function(id){
  ns <- NS(id)
  panel(
    status = "primary",
    fluidRow(
      column(4,
             selectInput(inputId =  ns("selectRunID1"), label = "Run ID 1", choices = "", selected = NULL) %>%
               bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right")),
      column(4,
             selectInput(inputId =  ns("selectRunID2"), label = "Run ID 2", choices = "", selected = NULL) %>%
               bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right")),
      column(4,
             align = "right",
             actionButton(inputId = ns("abuttongotoconfig"), label = "Go to Batch Configure Output"))
    )
  )
}

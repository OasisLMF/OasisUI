# visualization Single Run Browse Module UI ------------------------------------

#' @title visualizationSBR_ui
#' visualization Single Browse Run UI
#' @rdname visualizationSBRUI
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @export
visualizationSBRUI <- function(id) {
 
  ns <- NS(id)
  
  tagList(
    
    # h4("Browse Run Outputs", class = "flamingo-page-title") ,
    panelDefineRunID(id),
    
    tabsetPanel(
      id = ns("tabsSBR"),
      
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
        title = "Files",
        outputfilesUI(ns("outputfiles")),
        value = ns("taboutputfiles")
      )
      
    )
  )
  
}

# UI Functions -----------------------------------------------------------------
#' @title panelDefineRunID
#' @rdname panelDefineRunID
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' @export
panelDefineRunID <-  function(id){
  ns <- NS(id)
  panel(
    status = "primary",
    fluidRow(
      column(6,
             selectInput(inputId =  ns("selectRunID"), label = "Run ID", choices = "", selected = NULL) %>%
               bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right")),
      column(6,
             align = "right",
             actionButton(inputId = ns("abuttongotoconfig"), label = "Go to Configure Output"))
    )
  )
}
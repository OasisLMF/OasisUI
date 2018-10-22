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
    
    defineBatchRunID(id),
    
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


# UI Function ------------------------------------------------------------------
#' @title defineBatchRunID
#' @rdname defineBatchRunID
#' @inheritParams flamingoModuleUI
#' @importFrom bsplus bs_embed_tooltip
#' @export

defineBatchRunID <- function(id){
  
  ns <- NS(id)
  w <- 6
  labelconfig <- "Go to Configure Batch Output"
  
  panel(
    status = "primary",
    fluidRow(
      defineIDUI(ns("defineID"), w, batch = TRUE),
      column(w,
             align = "right",
             actionButton(inputId = ns("abuttongotoconfig"), label = labelconfig)))
  )
}
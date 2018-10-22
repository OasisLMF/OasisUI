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
    
    defineCompareRunIDs(id),
    
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


# UI Function ------------------------------------------------------------------
#' @title defineCompareRunIDs
#' @rdname defineCompareRunIDs
#' @inheritParams flamingoModuleUI
#' @importFrom bsplus bs_embed_tooltip
#' @export

defineCompareRunIDs <- function(id){
  
  ns <- NS(id)
  w <- 4
  labelconfig <- "Go to Configure Output"
  
  panel(
    status = "primary",
    fluidRow(
      defineIDUI(ns("defineID-1"), w, batch = FALSE),
      defineIDUI(ns("defineID-2"), w, batch = FALSE),
      column(w,
             align = "right",
             actionButton(inputId = ns("abuttongotoconfig"), label = labelconfig)))
  )
}
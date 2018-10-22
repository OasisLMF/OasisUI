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
    defineRunID(id),
    
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

# UI Function ------------------------------------------------------------------
#' @title defineRunID
#' @rdname defineRunID
#' @inheritParams flamingoModuleUI
#' @importFrom bsplus bs_embed_tooltip
#' @export

defineRunID <- function(id){

  ns <- NS(id)
  w <- 6
  labelconfig <- "Go to Configure Output"
 
  panel(
    status = "primary",
    fluidRow(
      defineIDUI(ns("defineID"), w, batch = FALSE),
      column(w,
             align = "right",
             actionButton(inputId = ns("abuttongotoconfig"), label = labelconfig)))
    )
}
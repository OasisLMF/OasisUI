#' accountDefinitionUI
#'
#' @rdname accountDefinition
#'
#' @description UI/View to define an account.
#'
#' @template params-module-ui
#' 
#' @return List of tags.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
accountDefinitionUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(12,
             div(class = "flamingo-page-division",
                 
                 helpText(h4("Account Table", class = "flamingo-table-title")),
                 
                 DTOutput(ns("tableDAAccount")),
                 
                 downloadButton(ns("DAAdownloadexcel"), label= " Export to csv"),
                 
                 flamingoButton(ns("abuttoncreateac"), "Create Account",
                                align = "left"),
                 flamingoButton(ns("abuttonamendac"), "Amend Account",
                                align = "centre") %>%
                   bs_embed_tooltip(title = define_account$abuttonamendac, placement = "right"),
                 flamingoButton(ns("abuttondeleteac"), "Delete Account",
                                align = "right") %>%
                   bs_embed_tooltip(title = define_account$abuttondeleteac, placement = "right")
             )
      )
    )
    
  )
  
}

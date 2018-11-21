#' companyDefinitionUI
#'
#' @rdname companyDefinition
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
companyDefinitionUI <- function(id) {

  ns <- NS(id)

  tagList(

    fluidRow(
      column(12,
             div(class = "flamingo-page-division",

                 helpText(h4("Company List", class = "flamingo-table-title")),

                 DTOutput(ns("dt_companylist")),

                 flamingoButton(ns("abuttoncompcrt"),
                                label = "Create", align = "left"),
                 flamingoButton(ns("abuttoncompupdate"),
                                label = "Update", align = "left") %>%
                   bs_embed_tooltip(title = define_company$abuttoncompupdate, placement = "right"),
                 flamingoButton(ns("abuttoncompdel"),
                                label = "Delete", align = "left") %>%
                   bs_embed_tooltip(title = define_company$abuttoncompdel, placement = "right")
             )
      )
    )

  )

}

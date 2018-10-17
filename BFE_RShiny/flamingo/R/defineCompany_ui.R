#' @rdname companyDefinitionUI
#' @title companyDefinitionUI
#' @description UI/View to define an account.
#' @param id account id
#' @return list of tags
#' @rdname companyDefinition
#' @importFrom DT DTOutput
#' @importFrom htmltools tags
#' @export
companyDefinitionUI <- function(id) {

  ns <- NS(id)

  tagList(

    h3("Company", class = "flamingo-page-title"),

    fluidRow(
      column(12,
             div(class = "flamingo-page-division",

                 helpText(h4("Company List", class = "flamingo-table-title")),

                 DTOutput(ns("tablecompanylist")),

                 flamingoButton(ns("abuttoncompcrt"),
                              label = "Create", align = "left"),
                 flamingoButton(ns("abuttoncompupdate"),
                              label = "Update", align = "left"),
                 flamingoButton(ns("abuttoncompdel"),
                              label = "Delete", align = "left")
             )
      )
    )


  )

}

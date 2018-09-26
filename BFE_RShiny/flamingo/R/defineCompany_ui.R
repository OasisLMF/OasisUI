
#' @description UI/View to define an account.
#' @param id account id
#' @return list of tags
#' @rdname companyDefinition
#' @importFrom DT DTOutput
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

                  actionButton(ns("abuttoncompcrt"),  class="btn btn-primary",
                      label = "Create", align = "left"),
                  actionButton(ns("abuttoncompupdate"),  class="btn btn-primary",
                      label = "Update", align = "left"),
                  actionButton(ns("abuttoncompdel"),  class="btn btn-primary",
                      label = "Delete", align = "left")
              )
          )
      )
      

  )

}

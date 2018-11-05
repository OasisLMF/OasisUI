#' userAdminDefinition
#'
#' @rdname userAdminDefinition
#'
#' @description UI/View for accessing the Company User List for Flamingo
#' in association with OASIS LMF.
#'
#' @inheritParams accountDefinitionUI
#'
#' @return List of tags.
#'
#' @importFrom shiny helpText
#' @importFrom shiny textInput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny downloadButton
#' @importFrom DT DTOutput
#' @importFrom shinyjs hidden
#'
#' @export
userAdminDefinitionUI <- function(id) {

  ns <- NS(id)

  tagList(

    # h3("Company User Administration", class = "flamingo-page-title"),

    fluidRow(
      column(12,
             div(class = "flamingo-page-division",

                 helpText(h4("Company User List", class = "flamingo-table-title")),
                 DTOutput(ns("tablecompanyuserlist")),
                 downloadButton(ns("CUACULdownloadexcel"),label="Export to csv"),
                 flamingoButton(ns("abuttonnewUser"),
                              label = "Create", align = "left"),
                 flamingoButton(ns("abuttonuserupdate"),
                              label = "Update", align = "center") %>%
                   bs_embed_tooltip(title = landing_page$abuttonuserupdate, placement = "right"),
                 flamingoButton(ns("abuttonuserdelete") ,
                              label = "Delete", align = "right") %>%
                   bs_embed_tooltip(title = landing_page$abuttonuserdelete, placement = "right"),

                 flamingoButton(ns("abuttonusersecurity"),
                              label = "Add/Remove Security Group") %>%
                   bs_embed_tooltip(title = landing_page$abuttonusersecurity, placement = "right"),
                 flamingoButton(ns("abuttonuseroasis"),
                              label = "Add/Remove User License", align = "right") %>%
                   bs_embed_tooltip(title = landing_page$abuttonuseroasis, placement = "right")

             )
      )
    ),

    fluidRow(
      column(12,
             hidden(
               div(id = ns("usgroups"), class = "flamingo-page-division",
                   h4("User Security Groups", class = "flamingo-table-title"),
                   DTOutput(ns("tableusersecuritygroups")),
                   downloadButton(ns("CUAUUSGdownloadexcel"),
                                  label="Export to csv")
               )#End of div usgroups
             )#End of hidden for usgroups
      )
    ),

    fluidRow(
      column(12,
             hidden(
               div(id = ns("ulicenses"), class = "flamingo-page-division",
                   h4("User Licenses", class = "flamingo-table-title"),
                   DTOutput(ns("tableuserlicenses")),
                   downloadButton(ns("CUAULdownloadexcel"),
                                  label="Export to csv")
               )#End of div ulicenses
             )#End of hidden for ulicense
      )
    )

  )

}

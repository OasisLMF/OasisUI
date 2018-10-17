
#' @rdname userAdminDefinition
#' @description UI/View for accessing the Company User List for Flamingo
#' in association with OASIS LMF
#' @param id shiny module id
#' @return list of tags
#' @importFrom shiny actionButton helpText textInput sidebarLayout sidebarPanel
#' downloadButton
#' @importFrom DT DTOutput
#' @importFrom shinyjs hidden
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
                 actionButton(ns("abuttonnewUser"), class="btn btn-primary",
                              label = "Create", align = "left"),
                 actionButton(ns("abuttonuserupdate"), class="btn btn-primary",
                              label = "Update", align = "center"),
                 actionButton(ns("abuttonuserdelete"), class="btn btn-primary",
                              label = "Delete", align = "right"),

                 actionButton(ns("abuttonusersecurity"), class = "btn btn-primary",
                              label = "Add/Remove Security Group"),
                 actionButton(ns("abuttonuseroasis"), class = "btn btn-primary",
                              label = "Add/Remove User License", align = "right")

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

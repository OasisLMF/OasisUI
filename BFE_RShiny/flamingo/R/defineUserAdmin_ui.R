
#' @rdname userAdminDefinition
#' @description UI/View for accessing the Company User List for Flamingo
#' in association with OASIS LMF
#' @param id shiny module id
#' @return list of tags
#' @importFrom shiny actionButton helpText textInput sidebarLayout sidebarPanel
#' downloadButton
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs hidden
#' @export
userAdminDefinitionUI <- function(id) {

  ns <- NS(id)

  tagList(

    h3("Company User Administration", class = "flamingo-page-title"),

    fluidRow(
      column(12,
             div(class = "flamingo-page-division",

                 helpText(h4("Company User List", class = "flamingo-table-title")),
                 dataTableOutput(ns("tablecompanyuserlist")),
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
                   dataTableOutput(ns("tableusersecuritygroups")),
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
                   dataTableOutput(ns("tableuserlicenses")),
                   downloadButton(ns("CUAULdownloadexcel"),
                                  label="Export to csv")
               )#End of div ulicenses
             )#End of hidden for ulicense
      )
    ),

    tagAppendAttributes(class = "modal-footless",
                        bsModal(ns("useradmincrtupmodal"), "User Details",
                                trigger = "", size = "medium",
                                textInput(ns("tinputUserName"), "User Name"),
                                selectInput(ns("sinputCompany"), "Company Name", choices = c("")),
                                textInput(ns("tinputDepartment"), "Department"),
                                textInput(ns("tinputLogin"), "Login"),
                                passwordInput(ns("tinputPassword"), "Password"),
                                actionButton(ns("abuttonusersubmit"), class = "btn btn-primary",
                                             label = "Submit", align = "left"),
                                actionButton(ns("abuttonusercancel"), class = "btn btn-primary",
                                             label = "Cancel", align = "right"))),

    tagAppendAttributes(class = "modal-footless",
                        bsModal(ns("userdelmodal"), "Are you sure you want to delete?",
                                trigger = "", size = "medium",
                                actionButton(ns("abuttonuconfirmdel"), class="btn btn-primary",
                                             label = "Confirm", align = "center"),
                                actionButton(ns("abuttonucanceldel"), class = "btn btn-primary",
                                             label = "Cancel", align = "right"))),

    tagAppendAttributes(class = "modal-footless",
                        bsModal(ns("usersecuritymodal"), "Add/Remove Security Groups",
                                trigger = "", size = "medium",
                                selectInput(ns("sinputSecurity"), "Select Security Group",
                                            choices = c("")),
                                actionButton(ns("abuttonaddsecurity"), class="btn btn-primary",
                                             label = "Add", align = "left"),
                                actionButton(ns("abuttonrmvsecurity"), class="btn btn-primary",
                                             label = "Remove", align = "right"))),

    tagAppendAttributes(class = "modal-footless",
                        bsModal(ns("userlicensemodal"), "Add/Remove User Licenses", trigger = "",
                                size = "medium",
                                selectInput(ns("sinputOasisID"), "Select Oasis User ID",
                                            choices = c("")),
                                actionButton(ns("abuttonaddoasisid"), class="btn btn-primary",
                                             label = "Add", align = "left"),
                                actionButton(ns("abuttonrmvoasisid"), class="btn btn-primary",
                                             label = "Remove", align = "right")))

  )

}

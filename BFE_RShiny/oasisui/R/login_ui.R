#' loginDialogUI
#'
#' @rdname loginDialog
#'
#' @description UI/View to login an user.
#'
#' @return List of tags.
#'
#' @export
loginDialogUI <- function(id) {

  ns <- NS(id)
  api_type <- getOption("oasisui.settings.api.api_auth_type")
  oidc_api_types <- c("keycloak", "authentik")
  
  if (api_type == "simple") {
    tagList(
      tags$div(align = "center", class = "login-dialog",
              img(src = "img/OASIS_LMF_COLOUR.png", width = "40%",  style = "margin-top:20%; max-width:400px;" ),
              tags$input(id = ns("user"), type = "text",
                          class = "focus",
                          placeholder = "username", size = 15,
                          onkeydown = sprintf(
                            "if (event.keyCode == 13) document.getElementById('%s').select()",
                            ns("password"))),
              tags$input(id = ns("password"), type = "password",
                          placeholder = "password", size = 15,
                          onkeydown = sprintf(
                            "if (event.keyCode == 13) document.getElementById('%s').click()",
                            ns("abuttonloginbutton"))
                          ),
              oasisuiButton(ns("abuttonloginbutton"), "Login", class = "btn btn-success")
      )
    )
  } else if (api_type %in% oidc_api_types) {
    tagList(
      tags$div(align = "center", class = "login-dialog",
              img(src = "img/OASIS_LMF_COLOUR.png", width = "40%",  style = "margin-top:20%; max-width:400px;" ),
              tags$br(),
              oasisuiButton(ns("oidc_login"), "Login via OIDC Provider", class = "btn btn-primary")
      )
    )
  } else {
    tagList(
      tags$div(align = "center", class = "login-dialog",
              img(src = "img/OASIS_LMF_COLOUR.png", width = "40%",  style = "margin-top:20%; max-width:400px;" ),
              tags$br(),
              tags$p(paste("Unknown api_type:", api_type))
      )
    )
  }
}

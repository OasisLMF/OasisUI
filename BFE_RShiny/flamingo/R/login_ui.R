#' @rdname loginDialog
#' @description UI/View to login an user
#' @param id account id
#' @rdname loginDialog
#' @importFrom shinyjs hidden
#' @importFrom htmltools tags
#' @export
loginDialogUI <- function(id) {

  ns <- NS(id)

  tagList(

    tags$div(align = "center", class = "login-dialog",
             img(src = "img/Flamingo.jpg" ),
             tags$input(id = ns("userid"), type = "text",
                        placeholder = "username", size = 15),
             tags$input(id = ns("password"), type = "password",
                        placeholder = "password", size = 15,
                        onkeydown = sprintf(
                          "if (event.keyCode == 13) document.getElementById('%s').click()",
                          ns("loginbutton"))),
             flamingoButton(ns("loginbutton"), "Login", class = "btn btn-success")
    )
  )
}

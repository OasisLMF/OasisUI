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

  tagList(

    tags$div(align = "center", class = "login-dialog",
             img(src = "img/Flamingo.jpg" ),
             tags$input(id = ns("user"), type = "text",
                        placeholder = "username", size = 15),
             tags$input(id = ns("password"), type = "password",
                        placeholder = "password", size = 15,
                        onkeydown = sprintf(
                          "if (event.keyCode == 13) document.getElementById('%s').click()",
                          ns("loginbutton"))),
             flamingoButton(ns("abuttonloginbutton"), "Login", class = "btn btn-success")
    )
  )
}

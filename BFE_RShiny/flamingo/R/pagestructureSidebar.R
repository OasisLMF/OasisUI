#' pagestructureSidebar
#'
#' @rdname pagestructureSidebar
#'
#' @param ns ns.
#' @param collapsed FALSE.
#'
#' @return Side bar in pagestructure.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom shinyWidgets dropdownButton
#'
#' @export
pagestructureSidebar <- function(ns = identity, collapsed = FALSE) {
  flamingoPanel(
    "sidebar",
    heading = div(
      flamingoButton(inputId = ns("abuttonhome"), icon = icon("home"),
                   label = NULL),
      actionButton(inputId = ns("abuttoncollapsesidebar"), icon = icon("ellipsis-v"),
                   label = NULL, style = "float:right;")
    ),
    dropdownButton(
      inputId = ns("abuttonrun"),
      status = "dropdown",
      label = if (!collapsed) "Process",
      icon = if (collapsed) icon("cog", lib = "glyphicon"),
      circle = FALSE,
      right = FALSE,
      width = "100%",
      flamingoButton(ns("abuttondefineprogrammesingle"),
                   label = if (!collapsed) "Single Process" else "Single",
                   icon = if (collapsed) icon("cog", lib = "glyphicon"),
                   align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttondefineprogrammesingle, placement = "right"),
      flamingoButton(ns("abuttondefineprogrammebatch"),
                   label = if (!collapsed) "Batch Process" else "Batch",
                   icon = if (collapsed) icon("cog", lib = "glyphicon"),
                   align = "left",  width = "100%", disabled = TRUE) %>%
        bs_embed_tooltip(title = landing_page$abuttondefineprogrammebatch, placement = "right")
    ) %>% bs_embed_tooltip(title = landing_page$abuttonrun, placement = "right"),

    dropdownButton(
      inputId = ns("abuttonbrowse"),
      status = "dropdown",
      label = if (!collapsed) "Browse",
      icon = if (collapsed) icon("eye"),
      circle = FALSE,
      right = FALSE,
      width = "100%",
      flamingoButton(ns("abuttonbrowseSBR"),
                   label = if (!collapsed) "Single Browse" else "Single",
                   icon = if (collapsed) icon("eye"),
                   align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseSBR, placement = "right"),
      flamingoButton(ns("abuttonbrowseBBR"),
                   label = if (!collapsed) "Batch Browse" else "Batch",
                   icon = if (collapsed) icon("eye"),
                   align = "left",  width = "100%", disabled = TRUE) %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseBBR, placement = "right"),
      flamingoButton(ns("abuttonbrowseCBR"),
                   label = if (!collapsed) "Compare runs" else "Runs",
                   icon = if (collapsed) icon("eye"),
                   align = "left",  width = "100%", disabled = TRUE) %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseCBR, placement = "right")
    ) %>% bs_embed_tooltip(title = landing_page$abuttonbrowse, placement = "right"),

    flamingoButton(
      ns("abuttonfilemngt"),
      label = if (!collapsed) "File Management",
      icon = if (collapsed) icon("file", lib = "glyphicon"),
      align = "left",  width = "100%"
    ) %>%
      bs_embed_tooltip(title = landing_page$abuttonfilemngt, placement = "right")
  )
}

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
  oasisuiPanel(
    "sidebar",
    heading = div(
      oasisuiButton(inputId = ns("abuttonhome"), icon = icon("home"),
                   label = NULL),
      actionButton(inputId = ns("abuttoncollapsesidebar"), icon = icon("ellipsis-v"),
                   label = NULL, style = "float:right;")
    ),
    dropdownButton(
      inputId = ns("abuttonanalysis"),
      status = "dropdown",
      label = if (!collapsed) "Analysis",
      icon = if (collapsed) icon("cog", lib = "glyphicon"),
      circle = FALSE,
      right = FALSE,
      width = "100%",
      oasisuiButton(ns("abuttondefineanasingle"),
                   label = if (!collapsed) "Single Analysis" else "Single",
                   icon = if (collapsed) icon("cog", lib = "glyphicon"),
                   align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttondefineanasingle, placement = "right"),
      oasisuiButton(ns("abuttondefineanabatch"),
                   label = if (!collapsed) "Batch Analysis" else "Batch",
                   icon = if (collapsed) icon("cog", lib = "glyphicon"),
                   align = "left",  width = "100%", disabled = TRUE) %>%
        bs_embed_tooltip(title = landing_page$abuttondefineanabatch, placement = "right")
    ) %>% bs_embed_tooltip(title = landing_page$abuttonanalysis, placement = "right"),

    dropdownButton(
      inputId = ns("abuttonbrowse"),
      status = "dropdown",
      label = if (!collapsed) "Dashboard",
      icon = if (collapsed) icon("eye"),
      circle = FALSE,
      right = FALSE,
      width = "100%",
      oasisuiButton(ns("abuttonbrowseSBR"),
                   label = if (!collapsed) "Single Analysis Dashboard" else "Single",
                   icon = if (collapsed) icon("eye"),
                   align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseSBR, placement = "right"),
      oasisuiButton(ns("abuttonbrowseBBR"),
                   label = if (!collapsed) "Batch Analysis Dashboard" else "Batch",
                   icon = if (collapsed) icon("eye"),
                   align = "left",  width = "100%", disabled = TRUE) %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseBBR, placement = "right"),
      oasisuiButton(ns("abuttonbrowseCBR"),
                   label = if (!collapsed) "Compare Analyses Dashboard" else "Compare",
                   icon = if (collapsed) icon("eye"),
                   align = "left",  width = "100%", disabled = TRUE) %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseCBR, placement = "right")
    ) %>% bs_embed_tooltip(title = landing_page$abuttonbrowse, placement = "right")
  )
}

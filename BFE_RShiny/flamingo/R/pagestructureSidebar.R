pagestructureSidebar <- function(ns = identity, collapsed = FALSE) {
  flamingoPanel(
    "sidebar",
    heading = div(
      actionButton(inputId = ns("abuttonhome"), class = "btn btn-primary", icon = icon("home"),
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
      tooltip = tooltipOptions(title = landing_page$abuttonrun, placement = "right"),
      actionButton(ns("abuttondefineprogrammesingle"),
                   label = if (!collapsed) "Single Process" else "Single",
                   icon = if (collapsed) icon("cog", lib = "glyphicon"),
                   class = "btn btn-primary", align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttondefineprogrammesingle, placement = "right"),
      actionButton(ns("abuttondefineprogrammebatch"),
                   label = if (!collapsed) "Batch Process" else "Batch",
                   icon = if (collapsed) icon("cog", lib = "glyphicon"),
                   class = "btn btn-primary", align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttondefineprogrammebatch, placement = "right")
    ),

    dropdownButton(
      inputId = ns("abuttonbrowse"),
      status = "dropdown",
      label = if (!collapsed) "Browse",
      icon = if (collapsed) icon("eye"),
      circle = FALSE,
      right = FALSE,
      width = "100%",
      actionButton(ns("abuttonbrowseSBR"),
                   label = if (!collapsed) "Single Browse" else "Single",
                   icon = if (collapsed) icon("eye"),
                   class = "btn btn-primary", align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseSBR, placement = "right"),
      actionButton(ns("abuttonbrowseBBR"),
                   label = if (!collapsed) "Batch Browse" else "Batch",
                   icon = if (collapsed) icon("eye"),
                   class = "btn btn-primary", align = "left",  width = "100%") %>%
        bs_embed_tooltip(title = landing_page$abuttonbrowseBBR, placement = "right")
    ),

    actionButton(
      ns("abuttonfilemngt"),
      label = if (!collapsed) "File Management",
      icon = if (collapsed) icon("file", lib = "glyphicon"),
      class = "btn btn-primary", align = "left",  width = "100%"
    ) %>%
      bs_embed_tooltip(title = landing_page$abuttonfilemngt, placement = "right")
  )
}

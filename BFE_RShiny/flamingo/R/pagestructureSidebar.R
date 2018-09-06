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
      icon = if (TRUE) icon("cog", lib = "glyphicon"),
      circle = FALSE,
      right = FALSE,
      width = "100%",
      tooltip = tooltipOptions(title = landing_page$abuttonrun, placement = "right"),
      actionButton(ns("abuttondefineprogrammesingle"), "Single Process",
                   class = "btn btn-primary", align = "left",  width = "100%"),
      bsTooltip(ns("abuttondefineprogrammesingle"),
                landing_page$abuttondefineprogrammesingle,
                placement = "right",
                options   = list(container = "body")),
      actionButton(ns("abuttondefineprogrammebatch"), "Batch Process",
                   class = "btn btn-primary", align = "left",  width = "100%"),
      bsTooltip(ns("abuttondefineprogrammebatch"),
                landing_page$abuttondefineprogrammebatch,
                placement = "right",
                options   = list(container = "body"))
    ),

    dropdownButton(
      inputId = ns("abuttonbrowse"),
      status = "dropdown",
      label = if (!collapsed) "Browse",
      icon = if (TRUE) icon("eye"),
      circle = FALSE,
      right = FALSE,
      width = "100%",
      tooltip = tooltipOptions(title = landing_page$abuttonbrowse, placement = "right"),
      actionButton(ns("abuttonbrowseSBR"), "Single Browse",
                   class = "btn btn-primary", align = "left",  width = "100%"),
      bsTooltip(ns("abuttonbrowseSBR"),
                landing_page$abuttonbrowseSBR,
                placement = "right",
                options   = list(container = "body")),
      actionButton(ns("abuttonbrowseBBR"), "Batch Browse",
                   class = "btn btn-primary", align = "left",  width = "100%"),
      bsTooltip(ns("abuttonbrowseBBR"),
                landing_page$abuttonbrowseBBR,
                placement = "right",
                options   = list(container = "body"))
    ),

    actionButton(
      ns("abuttonfilemngt"),
      label = if (!collapsed) "File Management",
      icon = if (TRUE) icon("file", lib = "glyphicon"),
      class = "btn btn-primary", align = "left",  width = "100%"
    ),
    bsTooltip(ns("abuttonfilemngt"),
              landing_page$abuttonfilemngt,
              placement = "right",
              options   = list(container = "body"))#,
    # sidebar_button(ID = ns("abuttonsysconf"),  Label =  if (collapsed) "SC" else "System Configuration"),
    # bsTooltip(ns("abuttonsysconf"),
    #           landing_page$abuttonsysconf,
    #           placement = "right",
    #           options   = list(container = "body"))
  )
}

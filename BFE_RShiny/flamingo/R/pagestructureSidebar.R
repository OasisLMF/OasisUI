pagestructureSidebar <- function(ns = identity, collapsed = FALSE) {
  panel(
    heading = sidebar_button(ID = ns("abuttonhome"),  Icon = icon("home"), Block = FALSE),
    dropdownButton(
      inputId = ns("abuttonrun"),
      status = "primary",
      label = if (!collapsed) "Process",
      icon = if (collapsed) icon("cog", lib = "glyphicon"),
      circle = FALSE,
      right = TRUE,
      tooltip = tooltipOptions(title = landing_page$abuttonrun, placement = "right"),
      actionButton(ns("abuttondefineprogrammesingle"), "Single Process",
                   class = "btn btn-primary", align = "right",  width = "100%"),
      bsTooltip(ns("abuttondefineprogrammesingle"),
                landing_page$abuttondefineprogrammesingle,
                placement = "right",
                options   = list(container = "body")),
      actionButton(ns("abuttondefineprogrammebatch"), "Batch Process",
                   class = "btn btn-primary", align = "right",  width = "100%"),
      bsTooltip(ns("abuttondefineprogrammebatch"),
                landing_page$abuttondefineprogrammebatch,
                placement = "right",
                options   = list(container = "body"))
    ),
    sidebar_button(
      ID = ns("abuttonbrowse"),
      Label = if (!collapsed) "Browse",
      Icon = if (collapsed) icon("eye")
    ),
    bsTooltip(ns("abuttonbrowse"),
              landing_page$abuttonbrowse,
              placement = "right",
              options   = list(container = "body")),
    sidebar_button(
      ID = ns("abuttonexpmngt"),
      Label = if (collapsed) "EM" else "Exposure Management"
    ),
    bsTooltip(ns("abuttonexpmngt"),
              landing_page$abuttonexpmngt,
              placement = "right",
              options   = list(container = "body")),
    sidebar_button(
      ID = ns("abuttonprmngt"),
      Label = if (collapsed) "PM" else "Process Management"
    ),
    bsTooltip(ns("abuttonprmngt"),
              landing_page$abuttonprmngt,
              placement = "right",
              options   = list(container = "body")),
    sidebar_button(
      ID = ns("abuttonfilemngt"),
      Label = if (collapsed) "FM" else "File Management"
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

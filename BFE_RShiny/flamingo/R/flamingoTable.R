# Flamingo Table Module --------------------------------------------------------

# UI ---------------------------------------------------------------------------

#' flamingoTableUI
#'
#' @rdname flamingoTable
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom DT DTOutput
#'
#' @export
flamingoTableUI <-  function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("dt_flamingoTable"))
  )
}

# Server -----------------------------------------------------------------------

#' flamingoTable
#'
#' @rdname flamingoTable
#'
#' @description Server logic to show the flamingo table output
#'
#' @template params-module
#'
#' @param data dataframe to show in table.
#'
#' @param selection param of datatable, default"none".
#'
#' @param escape param of datatable, default TRUE.
#'
#' @param scrollX param of datatable, default FALSE.
#'
#' @param filter param of datatable, default FALSE.
#'
#' @param rownames param of datatable, default FALSE.
#'
#' @param colnames param of datatable, default TRUE.
#'
#' @param preselRow reactive of preselected row default reactive({NULL}).
#'
#' @param maxrowsperpage param of datatable, default 10.
#'
#' @return rows_selected reactive of selected rows as returned from datatable.
#'
#' @return  rows_current reactive of current rows as returned from datatable.
#'
#' @importFrom DT renderDT
#'
#' @importFrom DT datatable
#'
#' @export
flamingoTable <- function(input, output, session,
                          data,
                          selection = "none",
                          escape = TRUE,
                          scrollX = FALSE,
                          filter = FALSE,
                          rownames = FALSE,
                          colnames = TRUE,
                          preselRow = reactive({NULL}),
                          maxrowsperpage = 10,
                          logMessage = message ) {

  ns <- session$ns

    output$dt_flamingoTable <- renderDT({

      if (!is.null(data())) {
        tbl_flamingoTable <- data()
      } else {
        tbl_flamingoTable <- data.frame(content = "nothing to show")
      }

      colnamesToUse <- ""
      if (colnames) {
        colnamesToUse <- names(tbl_flamingoTable)
        if (rownames) {
          colnamesToUse <- c('Row Number', colnamesToUse)
        }
      }

      datatable(
        tbl_flamingoTable,
          class = "flamingo-table display",
          rownames = rownames,
          selection = list(mode = selection,
                           selected = preselRow(),
                           target = 'row'),
          escape = escape,
          colnames = colnamesToUse,
          options = .getPRTableOptions(scrollX, maxrowsperpage, filter)
        )

    })

  # Helper functions -----------------------------------------------------------

  #table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function(scrollX, maxrowsperpage, filter) {
    options <- list(
      #columnDefs = list(list(visible = FALSE, targets = c(0,5,6))),
      processing = 0,
      scrollX = scrollX,
      pageLength = maxrowsperpage
      #autoWidth = TRUE
    )
    if (filter) {
      options$dom <- 'ft'
      options$search <- list(caseInsensitive = TRUE)
      options$searchHighlight <- TRUE
    } else {
      options$dom <- 't'
    }
    if (!escape) {
      options$preDrawCallback <- JS('function() { Shiny.unbindAll(this.api().table().node()); }')
      options$drawCallback <- JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    }
    return(options)
  }

    # Module Outout --------------------------------------------------------------
    moduleOutput <- c(
      list(
        rows_selected = reactive({input$dt_flamingoTable_rows_selected}),
        rows_current = reactive({input$dt_flamingoTable_rows_current})
      )
    )
}

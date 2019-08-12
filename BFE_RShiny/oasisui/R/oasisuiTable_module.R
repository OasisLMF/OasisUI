# Oasisui Table Module --------------------------------------------------------

# UI ---------------------------------------------------------------------------

#' oasisuiTableUI
#'
#' @rdname oasisuiTable
#'
#' @return List of tags.
#'
#' @importFrom DT DTOutput
#'
#' @export
oasisuiTableUI <-  function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("dt_oasisuiTable"))
  )
}

# Server -----------------------------------------------------------------------

#' oasisuiTable
#'
#' @rdname oasisuiTable
#'
#' @description Server logic to show the oasisui table output
#'
#' @template params-module
#'
#' @param data dataframe to show in table.
#' @param selection param of datatable, default"none".
#' @param escape param of datatable, default TRUE.
#' @param scrollX param of datatable, default TRUE
#' @param filter param of datatable, default FALSE.
#' @param rownames param of datatable, default TRUE.
#' @param preselRow reactive of preselected row default reactive({NULL}).
#' @param maxrowsperpage param of datatable, default 10.
#' @param simple TRUE for simplified version of table, FALSE otherwise.
#'
#' @return rows_selected reactive of selected rows as returned from datatable.
#' @return  rows_current reactive of current rows as returned from datatable.
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#'
#' @export
oasisuiTable <- function(input, output, session,
                         data,
                         selection = "none",
                         escape = TRUE,
                         scrollX = TRUE,
                         filter = FALSE,
                         rownames = TRUE,
                         preselRow = reactive({NULL}),
                         maxrowsperpage = 10,
                         simple = FALSE) {

  ns <- session$ns

  output$dt_oasisuiTable <- renderDT({

    if (!is.null(data())) {
      tbl_oasisuiTable <- data()
    } else {
      tbl_oasisuiTable <- data.frame(content = "nothing to show")
    }

    if (!simple) {
      dt <- datatable(
        tbl_oasisuiTable %>% capitalize_names_df(),
        class = "oasisui-table display",
        rownames = rownames,
        selection = list(mode = selection,
                         selected = preselRow(),
                         target = 'row'),
        escape = escape,
        options = getTableOptions(scrollX,
                                  maxrowsperpage = maxrowsperpage,
                                  escape = escape)
      )
    } else {
      if (length(tbl_oasisuiTable) < 10) {
        dt <- datatable(tbl_oasisuiTable, options = list(dom = 't'))
      } else {
        dt <- datatable(tbl_oasisuiTable, options = list(dom = 'p'))
      }
    }
    dt
  })

  observeEvent(data(), ignoreNULL = FALSE, {
    if (is.null(data()) || length(nrow(data())) == 0) {
      selectRows(dataTableProxy("dt_oasisuiTable"), NULL)
    }
  })

  # Module Outout --------------------------------------------------------------
  moduleOutput <- c(
    list(
      rows_selected = reactive({input$dt_oasisuiTable_rows_selected}),
      rows_current = reactive({input$dt_oasisuiTable_rows_current})
    )
  )
}

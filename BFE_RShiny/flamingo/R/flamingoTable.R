# Flamingo Table Module --------------------------------------------------------

# UI ---------------------------------------------------------------------------

#' @title flamingoTableUI
#' @rdname flamingoTableUI
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
flamingoTableUI <-  function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("flamingoTable"))
  )
}

# Server -----------------------------------------------------------------------
#' Module for flamingoTable
#' @rdname flamingoTable
#' @description Server logic to show the flamingo table output
#' @inheritParams flamingoModule
#' @param data
#' @return null
#' @importFrom DT renderDT datatable
#' @export
flamingoTable <- function(input, output, session,
                          data,
                          selection = "none",
                          escape = TRUE,
                          scrollX = FALSE,
                          filter = FALSE,
                          rownames = FALSE,
                          colnames = TRUE,
                          maxrowsperpage = 10,
                          logMessage = message ) {
  
  ns <- session$ns
  
    output$flamingoTable <- renderDT({
      
      if (!is.null(data())) {
        data <- data()
      } else {
        data <- data.frame(content = "nothing to show")
      }
      
      colnamesToUse <- ""
      if (colnames) {
        colnamesToUse <- names(data)
        if (rownames) {
          colnamesToUse <- c('Row Number', colnamesToUse)
        }
      }
      
      datatable(
          data,
          class = "flamingo-table display",
          rownames = rownames,
          selection = selection,
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
        rows_selected = reactive({input$flamingoTable_rows_selected}),
        rows_current = reactive({input$flamingoTable_rows_current})
      )
    )
}

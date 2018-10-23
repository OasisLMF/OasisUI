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
                          maxrowsperpage = 10,
                          logMessage = message ) {
  
  ns <- session$ns
  
  
    output$flamingoTable <- renderDT({
      if (!is.null(data())) {
        datatable(
          data(),
          class = "flamingo-table display",
          rownames = TRUE,
          selection = selection,
          escape = escape,
          colnames = c('Row Number' = 1),
          options = if(escape){.getPRTableOptions(scrollX, maxrowsperpage)} else {.getFLTableOptions(scrollX, maxrowsperpage)}
        )
      } else {
        datatable(
          data.frame(content = "nothing to show"),
          class = "flamingo-table display",
          rownames = FALSE,
          selection = selection,
          escape = escape,
          colnames = c('Row Number' = 1),
          options = .getPRTableOptions(scrollX, maxrowsperpage)
        )
      }
      
    })

  # Helper functions -----------------------------------------------------------
  
  #table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function(scrollX, maxrowsperpage) {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      processing = 0,
      scrollX = scrollX,
      pageLength = maxrowsperpage,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    return(options)
  }
  
  .getFLTableOptions <- function(scrollX, maxrowsperpage) {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      #columnDefs = list(list(visible = FALSE, targets = c(0,5,6))),
      processing = 0,
      scrollX = scrollX,
      pageLength = maxrowsperpage,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      #autoWidth = TRUE
    )
    return(options)
  }
  
  # Module Output --------------------------------------------------------------
  invisible()
}

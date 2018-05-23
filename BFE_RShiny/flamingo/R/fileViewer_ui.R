
#' @rdname fileViewer
#' @description UI/View to view files
#' @import leaflet
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs hidden disabled
#' @export
fileViewerUI <- function(id) {
  
  ns <- NS(id)
  
  jsPanelCondition <- function(id, value) {
    paste0("output['", ns(id), "']", " == ", "'", value, "'")
  }
  
  attachDependencies(value = flamingoHtmlDependencies(), tagList(
      
      h3("File Viewer", class = "flamingo-page-title"),
      
      hidden(
          verbatimTextOutput(ns("FVPanelSwitcher"))
      ),
      
      actionButton(ns(fileMgmtButtons[["FOBtnShowTable"]]), "Table", 
          class = "btn btn-primary"),
      disabled(
          actionButton(ns(fileMgmtButtons[["FOBtnShowRawContent"]]), "File",
              class = "btn btn-primary")),
      disabled(
          actionButton(ns(fileMgmtButtons[["FOBtnShowMap"]]), "Map",
              class = "btn btn-primary")),
      # actionButton(ns(noteButtonID(FOBtnShowAEPCurve)), "AEP Curve", class = "btn btn-primary"),
      # actionButton(ns(noteButtonID(FOBtnShowGeocode)),        "Geocode",         class = "btn btn-primary"),
      # actionButton(ns(noteButtonID(FOBtnShowEventFootprint)), "Event Footprint", class = "btn btn-primary"),
      
      conditionalPanel(
          condition = jsPanelCondition("FVPanelSwitcher", "FO_div_table"),
          
          h4("File List", class = "flamingo-table-title"),
#            column(12, actionButton("abuttonprocessinfo", "Process Run Details", class="btn btn-primary"), align = "right"),
#            #bsModal("modalExample", "Data Table", "tabBut", size = "large",dataTableOutput("distTable")),
          dataTableOutput(ns("tableFVfileList")),
          hidden(                           
              div(id = ns("processruninfodiv"),  
                  h4("Process Run Details", class = "flamingo-table-title"),
                  htmlOutput(ns("textprcruninfo")),
                  htmlOutput(ns("textprcrunparaminfo"))
              ))
      ),
      
      
      conditionalPanel(
          condition = jsPanelCondition("FVPanelSwitcher", "FO_div_filecontents"),
          
          h4("File Contents", class = "flamingo-table-title"),
          dataTableOutput(ns("tableFVExposureSelected")),
          br(),
          downloadButton(ns("FVEdownloadexcel"), label = "Export to Excel")
      ),
      
      
      conditionalPanel(
          condition = jsPanelCondition("FVPanelSwitcher", "FO_div_plainmap"),
    
          h4("Map", class = "flamingo-table-title"),
          leafletOutput(ns("plainmap"))
      ),
      
      
      conditionalPanel(
          condition = jsPanelCondition("FVPanelSwitcher", "FO_div_AEPcurve"),
          h4("AEP Curve", class = "flamingo-table-title"),
          plotOutput(ns("plotFVAEPCurve"))
          #,
          #    h4("AEP Curve Data"),
          # TODO XXX commented out for serious login-page-not-appearing shiny bug 
          #    DT::dataTableOutput("tableFVAEPdata"),
          #    downloadButton("FVAEPdownloadexcel",label="Export to Excel")
      ),
      
      conditionalPanel(
          condition = jsPanelCondition("FVPanelSwitcher", "FO_div_geocode"),
          h4("Geocoded Data", class = "flamingo-table-title"),
          dataTableOutput(ns("tableGeocodeData"))
          # ,
          # TODO XXX commented out for serious login-page-not-appearing shiny bug 
          #    downloadButton("FVGdownloadexcel",label="Export to Excel")
      ),
      
      conditionalPanel(
          condition = jsPanelCondition("FVPanelSwitcher", "FO_div_EventFootprint"),
          h4("Event Footprint", class = "flamingo-table-title"),
          leafletOutput(ns("EventFootprintMap"))
      )
  
  ))
  
}
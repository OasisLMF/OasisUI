
#' @description UI/View to define programmes
#' @inheritParams accountDefinition
#' @return list of tags
#' @importFrom shinyjs hidden
#' @importFrom shinyBS tipify
#' @importFrom DT dataTableOutput
#' @rdname programmeDefinition
#' @export
programmeDefinitionUI <- function(id) {
  
  ns <- NS(id)
  
  attachDependencies(value = flamingoHtmlDependencies(), tagList(
      
      h3("Define Programme", class = "flamingo-page-title"),
      
      tagAppendAttributes(class = "modal-footless",
          bsModal(ns("prog-crtupModal"), "Programme",
              trigger = "", size = "medium",
              
              programmeDefinitionAmendUI(id)
      )),
  
      tagAppendAttributes(class = "modal-footless",
          bsModal(ns("progModel-crtModal"), "Programme",
              trigger = "", size = "medium",
              
              programmeModelCreateUI(id)
      )),
  
      fluidRow(
          column(12,
              
              div(class = "flamingo-page-division",
                  helpText(h4("Programme Table",
                          class = "flamingo-table-title")),
                  
                  column(12, align = "right",
                      actionButton(ns("abuttonprgtblrfsh"), "Refresh",
                          class = "btn btn-primary btn-refresh")),
                  
                  dataTableOutput(ns("tableDPprog")),
                  
                  actionButton(ns("buttoncreatepr"), "Create Programme",
                      class = "btn btn-primary", align = "left"),
                  actionButton(ns("buttonamendpr"), "Amend Programme",
                      class = "btn btn-primary", align = "centre"),
                  actionButton(ns("buttondeletepr"), "Delete Programme",
                      class = "btn btn-primary", align = "right"),
                  actionButton(ns("buttonloadcanmodpr"), "Load Programme",
                      class = "btn btn-primary", align = "right")),
              
              hidden(
                  div(id = ns("divdefprogdetails"),
                      class = "flamingo-page-division",
                      
                      helpText(h4("Programme Details",
                              class = "flamingo-table-title")),
                      column(12,
                          actionButton(ns("abuttondefprogrfsh"),
                              "Refresh", class="btn btn-primary"),
                          align = "right"),
                      
                      dataTableOutput(ns("tableprogdetails"))
                  ))
  
          ),
              
          column(12,
              
              hidden(
                  div(id = ns("divprogmodeltable"),
                      class = "flamingo-page-division",
                      
                      helpText(h4("Programme Model Table",
                              class = "flamingo-table-title")),
                      column(12,
                          actionButton(ns("abuttonookrefresh"), "Refresh",
                              class="btn btn-primary"), align = "right"),
                      
                      dataTableOutput(ns("tableProgOasisOOK")),
                      
                      actionButton(ns("abuttonmaincreateprog"),
                          label = "Create Programme Model",
                          class = "btn btn-primary",
                          align = "left"),
                      actionButton(ns("abuttonloadprogmodel"),
                          label = "Load Programme Model",
                          class = "btn btn-primary",
                          align = "left"),
                      actionButton(ns("abuttongotoprocessrun"),
                          "Go to Process Run", class = "btn btn-primary")
                  )),
              
              hidden(
                  div(id = ns("divprogoasisfiles"),
                      class = "flamingo-page-division",
                      
                      helpText(h4("Programme Model Details",
                              class = "flamingo-table-title")),
                      
                      column(12,
                          actionButton(ns("abuttonprgoasisrfsh"),
                              label = "Refresh",
                              class="btn btn-primary"),
                          align = "right"),
                      
                      dataTableOutput(ns("tabledisplayprogoasisfiles"))
                  ))
          )
      )
  ))
}

#' @description UI/View to create a programme model
#' @inheritParams programmeDefinitionUI
#' @rdname programmeDefinition
#' @export
programmeModelCreateUI <- function(id) {
  
  ns <- NS(id)
  
  div(id = ns("obtainoasiscrtprgdiv"),
      h4("Create Programme Model"),
      selectInput(ns("sinputookprogid"), "Programme:",
          choices = c("")),
      selectInput(ns("sinputookmodelid"), "Model:",
          choices = c("")),
      
      tipify(
          selectInput(ns("sinputProgModTransform"),
              "Transform Name", choices = ""),
          title = "Transformation Description to be added here",
          placement = "right",
          trigger = "hover",
          options = NULL),
      
      actionButton(ns("abuttoncrprogoasis"), "Create",
          class = "btn btn-primary"),
      actionButton(ns("abuttoncancreateprog"), "Cancel",
          class="btn btn-primary")
  )
  
}

#' @description UI/View to amend a programme
#' @inheritParams programmeDefinitionUI
#' @rdname programmeDefinition
#' @export
programmeDefinitionAmendUI <- function(id) {
  
  ns <- NS(id)
  
  div(id = ns("divamdprog"),
      
      helpText(h4("Create/Amend Programme", class = "flamingo-table-title")),
      
      selectInput(ns("sinputDPAccountName"), label = "Account Name",
          choices = ""),
      
      textInput(ns("tinputDPProgName"), label = "Programme Name"),
      
      # tipify is an extract of bsTooltip and would wrap
      # any shiny element and it will create a tooltip
      # on the wrapped element.
      tipify(
          selectInput(ns("sinputTransformname"), label = "Transform Name",
              choices=""),
          title = "Transformation Description to be added here",
          placement = "right",
          trigger = "hover",
          options = NULL),
      
      actionButton(ns("abuttonProgSubmit"), class = "btn btn-primary",
          label = "Submit", align = "left"),
      
      actionButton(ns("abuttonProgCancel"), class = "btn btn-primary",
          label = "Cancel", align = "right"),
      
      hidden(
          div(id = ns("divFileUpload"),
              
              ## SL File
              
              helpText(h4("Source Location File")),
              
              selectInput(ns("sinputSLFile"), "Select Option", c("Select" = "",
                      "Upload New File" = "U",
                      "Select existing file" = "S")),
              
              hidden(div(id = ns("divSLFileUpload"),
                      fileInput(ns("SLFile"), 'Choose a file to upload:',
                          accept = c(
                              'csv',
                              'comma-separated-values',
                              '.csv'
                          )),
                      actionButton(ns("abuttonSLFileUpload"),
                          class = "btn btn-primary", label = "Upload File",
                          align = "left", enable = FALSE)
                  )),
              
              hidden(div(id = ns("divSLFileSelect"),                    
                      selectInput(ns("sinputselectSLFile"),
                          label = "Select existing File", choices = ""),
                      actionButton(ns("abuttonSLFileLink"),
                          class = "btn btn-primary", label = "Link", align = "left")
                  )),
              
              ## SA File
              
              helpText(h4("Source Account File")),
              
              selectInput(ns("sinputSAFile"), "Select Option", c("Select" = "",
                      "Upload New File" = "U",
                      "Select existing file" = "S")),
              
              hidden(div(id = ns("divSAFileUpload"),
                      fileInput(ns("SAFile"), 'Choose a file to upload:',
                          accept = c(
                              'csv',
                              'comma-separated-values',
                              '.csv'
                          )),
                      actionButton(ns("abuttonSAFileUpload"), 
                          class = "btn btn-primary", label = "Upload File", align = "left")
                  )),
              
              hidden(div(id = ns("divSAFileSelect"),                    
                      selectInput(ns("sinputselectSAFile"),
                          label = "Select existing File", choices = ""),
                      actionButton(ns("abuttonSAFileLink"), 
                          class = "btn btn-primary", label = "Link",
                          align = "left")
                  ))
          
          ))
  
  )
  
  
}
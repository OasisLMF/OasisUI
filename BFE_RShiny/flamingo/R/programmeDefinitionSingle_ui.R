#' @rdname programmeDefinitionSingle
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets sliderTextInput
#' @export
programmeDefinitionSingleUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    h3("Define Process", class = "flamingo-page-title"),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #8b2129}")),
    sliderTextInput(inputId = ns("sliderdefprogsteps"),
                    label = "Process Definition Steps" , 
                    choices = c("Create Programme", "Select Programme & Associate Model", "Configure Workflow Output", "Browse & re-run"), 
                    selected = c("Create Programme"), #incase you want all values by default 
                    animate = FALSE, grid = TRUE, 
                    hide_min_max = TRUE, from_fixed = FALSE,
                    to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                    to_max = NULL, force_edges = TRUE, width = '100%', pre = NULL,
                    post = NULL, dragRange = FALSE),
    bsModal(ns("bsmodalsaveoutput"), "Save Configuration", trigger = "", size = "small",
            textInput(ns("tinputoutputname"), label = "Configuration Name:", value = ""),
            actionButton(ns("abuttonsubmitoutput"), "Submit", class = "btn btn-primary")
    ),
    hidden( div(id = ns("panelamendprogramme"), amendProgrammeUI(id))),
    div(id = ns("panelcreateprogramme"), createProgrammeUI(id)),
    hidden(div(id = ns("paneldefineids"), defineIDsUI(id))),
    hidden(div(id = ns("panelamendmodel"), amendModelUI(id))),
    hidden( div(id = ns("panelassociatemodel"), associateModelUI(id))),
    hidden( div(id = ns("panelconfigureoutput"), configureOutputUI(id))),
    hidden( div(id = ns("panelrunprogramme"), runProgrammeUI(id)))
  )
}

#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
createProgrammeUI <- function(id) {
  ns <- NS(id)
  
  panel(
    helpText(h4("Define Programme", class = "flamingo-table-title")),
    createammendProgrammeUI(id)   
  )
}


#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
createammendProgrammeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             selectInput(ns("sinputDPAccountName"), label = "Account Name",
                         choices = "")
      ),
      column(4,
             textInput(ns("tinputDPProgName"), label = "Programme Name")
      ),
      column(4,
             selectInput(ns("sinputTransformname"), label = "Transform Name",
                         choices = ""),
             bsTooltip(ns("sinputTransformname"), 
                       programme_Definition_Single$sinputTransformname, 
                       placement = "right", 
                       options   = list(container = "body"))
      )
    ),
    
    fluidRow(
      column(4,
             actionButton(ns("abuttonProgSubmit"), "Create Programme", class = "btn btn-primary")
      )
    ),
    
    fluidRow(
      # Source Location File
      column(4,
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
             ))
      ),
      
      column(4,
             ## Source Account File
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
      )
    ),
    
    fluidRow(
      column(4,
             offset = 8,
             actionButton(ns("buttonloadcanmodpr"), label = "Load Programme", class = "btn btn-primary", align = "left"),
             actionButton(ns("abuttonProgCancel"), label = "Cancel", class = "btn btn-primary", align = "left")
      )
    )
  )
  
}

#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
amendProgrammeUI <- function(id) {
  ns <- NS(id)
  panel(
    helpText(h4("Programme Table",
                class = "flamingo-table-title")),
    
    column(12, align = "right",
           actionButton(ns("abuttonprgtblrfsh"), "Refresh",
                        class = "btn btn-primary btn-refresh")),
    
    dataTableOutput(ns("tableDPprog")),
    
    actionButton(ns("buttonamendpr"), "Amend Programme",
                 class = "btn btn-primary", align = "centre"),
    actionButton(ns("buttondeletepr"), "Delete Programme",
                 class = "btn btn-primary", align = "right"),
    actionButton(ns("buttonprogdetails"), "Show Programme Details",
                 class = "btn btn-primary", align = "right"),
    hidden(actionButton(ns("buttonhideprogdetails"), "Hide Programme Details",
                 class = "btn btn-primary", align = "right")),
    
    hidden(
      div(id = ns("divdefprogdetails"),
          class = "flamingo-page-division",
          
          helpText(h4("Programme Details",
                      class = "flamingo-table-title")),
          column(12,
                 actionButton(ns("abuttondefprogrfsh"),
                              "Refresh", class = "btn btn-primary"),
                 align = "right"),
          
          dataTableOutput(ns("tableprogdetails"))
      )
    )
  )
}

#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
amendModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    panel(

      hidden(
        div(id = ns("divprogmodeltable"),
            class = "flamingo-page-division",
            
            helpText(h4("Programme Model Table",
                        class = "flamingo-table-title")),
            column(12,
                   actionButton(ns("abuttonookrefresh"), "Refresh",
                                class = "btn btn-primary"), align = "right"),
            
            dataTableOutput(ns("tableProgOasisOOK")),
            
            column(12,
                   hidden(actionButton(ns("buttonhidemodeldetails"), "Hide Programme Model Details",
                                       class = "btn btn-primary")),
                   actionButton(ns("buttonmodeldetails"), "Show Programme Model Details",
                                class = "btn btn-primary")
                   , align = "right")
        )
      )
    ),
    
    hidden(
      div(id = ns("divprogoasisfiles"),
          panel(
            class = "flamingo-page-division",
            
            helpText(h4("Programme Model Details",
                        class = "flamingo-table-title")),
            
            column(12,
                   actionButton(ns("abuttonprgoasisrfsh"),
                                label = "Refresh",
                                class = "btn btn-primary"),
                   align = "right"),
            
            dataTableOutput(ns("tabledisplayprogoasisfiles")) 
          )
      ))
  )
}


#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
associateModelUI <- function(id) {
  ns <- NS(id)
  panel(
    helpText(h4("Associate Model", class = "flamingo-table-title")),
    fluidRow(
      column(4,
             selectInput(ns("sinputookprogid"), "Programme:",
                         choices = c(""))),
      column(4,
             selectInput(ns("sinputookmodelid"), "Model:",
                         choices = c(""))),
      column(4,
             selectInput(ns("sinputProgModTransform"),
                         "Transform Name", choices = ""),
             bsTooltip(ns("sinputProgModTransform"), 
                       programme_Definition_Single$sinputProgModTransform, 
                       placement = "right", 
                       options   = list(container = "body")))
    ),
    
    actionButton(ns("abuttoncrprogoasis"), "Create",
                 class = "btn btn-primary")
  )
}

#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
configureOutputUI <- function(id) {
  ns <- NS(id)
  panel(
    helpText(h4("Configure Wookflow Output", class = "flamingo-table-title")),

      fluidRow(
        column(4, 
               panel(
                 h4("Configuration Details"),
                 selectInput(ns("sinoutputoptions"), "Select Custom Configuration:", choices = ""),
                 textInput(ns("tinputprocessrunname"), label = "Process Run Name:", value = "")
               ),
               panel(
                 h4("Model Parameters"),
                 div( id = ns("noofsample"), style = "width:100%; margin: 0 auto;", textInput(ns("tinputnoofsample"), label = "Number of Samples:", value = "10")),
                 hidden(div(id = ns("configureOutputAdvancedUIOutput"), style = "width:90%; margin: 0 auto;", configureOutputAdvancedUI(id))),
                 hidden(div(id = ns("configureOutputPerilAdvancedUIOutput"), style = "width:90%; margin: 0 auto;", configureOutputPerilAdvancedUI(id)))
               )),
        
        column(8,
               panel(
                 h4("Output Configuration"),
                 checkboxInput(ns("chkinputGUL"), label = "Ground Up Loss", value = TRUE),
                 hidden(div(id = ns("configureOutputAdvancedGULUIOutput"), configureOutputAdvancedGULUI(id))),
                 checkboxInput(ns("chkinputIL"), label = "Insured Loss", value = FALSE), 
                 hidden(div(id = ns("configureOutputAdvancedILUIOutput"), configureOutputAdvancedILUI(id))),
                 div(id = ns("advanced"), style = "display: inline-block;margin-right:2%", align = "right", actionButton(ns("abtnadvanced"), label = "Advanced", class = "btn btn-primary")),
                 hidden(div(id = ns("basic"), style = "display: inline-block;margin-right:2%", align = "right", actionButton(ns("abtnbasic"), label = "Basic", class = "btn btn-primary"))),
                 hidden(div(id = ns("saveoutput"), style = "display: inline-block", actionButton(ns("abuttonsaveoutput"), label = "Save Configuration", class = "btn btn-primary"))),
                 hidden(div(id = ns("clroutopt"), style = "display: inline-block", actionButton(ns("abtnclroutopt"), label = "Default", class = "btn btn-primary")))
               ))
        
      ),
      
      fluidRow(
        column(12,
               hidden(div(id = ns("hidepanelconfigureoutput"), style = "display: inline-block", actionButton(ns("abuttonehidepanelconfigureoutput"), label = "Hide Configure Output", class = "btn btn-primary"))),
               div(id = ns("executeprrun"), style = "display: inline-block", actionButton(ns("abuttonexecuteprrun"), label = "Execute Run", class = "btn btn-primary")), align = "right"))
  )
}


#' UI/View to define the Advanced  Model parameters of a process run 
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
configureOutputAdvancedUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    textInput(ns("tinputthreshold"), label = "Loss Threshold:", value = "0"),
    selectInput(ns("sinputeventset"), label = "Event Set:", choices = "Probabilistic"),
    selectInput(ns("sinputeventocc"), label = "Event Occurrence Set:", choices = "Long Term"),
    checkboxInput(ns("chkinputsummaryoption"), "Summary Reports", value = TRUE)
  )
}

#' UI/View to define the Advanced  Model perils of a process run 
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
configureOutputPerilAdvancedUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    h5("Available Perils"),
    hidden(div(id = ns("perilwind"),
               checkboxInput(ns("chkinputprwind"), label = "Peril: Wind", value = TRUE))),
    hidden(div(id = ns("perilsurge"),
               checkboxInput(ns("chkinputprstsurge"), label = "Peril: Surge", value = TRUE))),
    hidden(div(id = ns("perilquake"),
               checkboxInput(ns("chkinputprquake"), label = "Peril: Quake", value = TRUE))),
    hidden(div(id = ns("perilflood"),
               checkboxInput(ns("chkinputprflood"), label = "Peril: Flood", value = TRUE))),
    hidden(div(id = ns("demandsurge"),
               checkboxInput(ns("chkinputdsurge"), label = "Demand Surge", value = TRUE))),
    hidden(div(id = ns("leakagefactor"),
               sliderInput(ns("sliderleakagefac"), label = "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)))
  )
}

#' UI/View to define the Advanced  GUL parameters of a process run 
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
configureOutputAdvancedGULUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    # Few outputs commented/disabled for the first release. To be enabled for later releases.  
    column(4,
           h4("Ground Up Loss", style = "font-size: 18px; font-weight: bold;"),
           h5("Full Sample", style = "font-size: 16.5px;"), 
           h5("ELT", style = "font-size: 16.5px;"),
           tags$div(class = "h5-align", h5("AEP", style = "font-size: 16.5px;")), 
           tags$div(class = "h5-align", h5("OEP", style = "font-size: 16.5px;")),
           tags$div(class = "h5-align", h5("Multi AEP", style = "font-size: 16.5px;")),
           h5("Multi OEP", style = "font-size: 16.5px;"),
           # h5("WS Mean AEP", style = "font-size: 16.5px;"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", style = "font-size: 16.5px;")),
           # tags$div(class = "h5-align",h5("Sample Mean AEP", style = "font-size: 16.5px;")),
           # h5("Sample Mean OEP", style = "font-size: 16.5px;"), 
           h5("AAL", style = "font-size: 16.5px;"),
           tags$div(class = "h5-align",h5("PLT", style = "font-size: 16.5px;"))),
    
    tags$div(class = "multicol",
             checkboxGroupInput(ns("chkgulprog"),
                                label = h4("Prog", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "gulprogSummary",
                                  " " = "gulprogELT",
                                  " " = "gulprogFullUncAEP",
                                  " " = "gulprogFullUncOEP",
                                  " " = "gulprogAEPWheatsheaf",
                                  " " = "gulprogOEPWheatsheaf", 
                                  # " " = "gulprogMeanAEPWheatsheaf",
                                  # " " = "gulprogMeanOEPWheatsheaf",
                                  # " " = "gulprogSampleMeanAEP",
                                  # " " = "gulprogSampleMeanOEP",
                                  " " = "gulprogAAL",
                                  " " = "gulprogPLT"),
                                selected = defaultSelectChoicesGUL),
             
             checkboxGroupInput(ns("chkgulstate"),
                                label = h4("State", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "gulstateSummary",
                                  " " = "gulstateELT",
                                  " " = "gulstateFullUncAEP",
                                  " " = "gulstateFullUncOEP",
                                  " " = "gulstateAEPWheatsheaf",
                                  " " = "gulstateOEPWheatsheaf", 
                                  # " " = "gulstateMeanAEPWheatsheaf",
                                  # " " = "gulstateMeanOEPWheatsheaf",
                                  # " " = "gulstateSampleMeanAEP",
                                  # " " = "gulstateSampleMeanOEP",
                                  " " = "gulstateAAL",
                                  " " = "gulstatePLT"),
                                selected = defaultSelectChoicesGUL),
             
             checkboxGroupInput(ns("chkgulcounty"),
                                label = h4("County", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "gulcountySummary",
                                  " " = "gulcountyELT",
                                  " " = "gulcountyFullUncAEP",
                                  " " = "gulcountyFullUncOEP",
                                  " " = "gulcountyAEPWheatsheaf",
                                  " " = "gulcountyOEPWheatsheaf", 
                                  # " " = "gulcountyMeanAEPWheatsheaf",
                                  # " " = "gulcountyMeanOEPWheatsheaf",
                                  # " " = "gulcountySampleMeanAEP",
                                  # " " = "gulcountySampleMeanOEP",
                                  " " = "gulcountyAAL",
                                  " " = "gulcountyPLT"),
                                selected = defaultSelectChoicesGUL),
             
             checkboxGroupInput(ns("chkgulloc"),
                                label = h4("Location", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "gullocSummary",
                                  " " = "gullocELT",
                                  " " = "gullocFullUncAEP",
                                  " " = "gullocFullUncOEP",
                                  " " = "gullocAEPWheatsheaf",
                                  " " = "gullocOEPWheatsheaf",
                                  # " " = "gullocMeanAEPWheatsheaf", 
                                  # " " = "gullocMeanOEPWheatsheaf", 
                                  # " " = "gullocSampleMeanAEP",
                                  # " " = "gullocSampleMeanOEP",
                                  " " = "gullocAAL",
                                  " " = "gullocPLT"),
                                selected = defaultSelectChoicesGUL),
             
             checkboxGroupInput(ns("chkgullob"),
                                label = h4("LOB", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "gullobSummary",
                                  " " = "gullobELT",
                                  " " = "gullobFullUncAEP",
                                  " " = "gullobFullUncOEP",
                                  " " = "gullobAEPWheatsheaf",
                                  " " = "gullobOEPWheatsheaf", 
                                  # " " = "gullobMeanAEPWheatsheaf",
                                  # " " = "gullobMeanOEPWheatsheaf",
                                  # " " = "gullobSampleMeanAEP", 
                                  # " " = "gullobSampleMeanOEP",
                                  " " = "gullobAAL",
                                  " " = "gullobPLT"),
                                selected = defaultSelectChoicesGUL),
             
             checkboxGroupInput(ns("chkgulpolicy"),
                                label = h4("Policy", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "gulpolicySummary",
                                  " " = "gulpolicyELT",
                                  " " = "gulpolicyFullUncAEP",
                                  " " = "gulpolicyFullUncOEP",
                                  " " = "gulpolicyAEPWheatsheaf",
                                  " " = "gulpolicyOEPWheatsheaf",  
                                  # " " = "gulpolicyMeanAEPWheatsheaf",
                                  # " " = "gulpolicyMeanOEPWheatsheaf",
                                  # " " = "gulpolicySampleMeanAEP",
                                  # " " = "gulpolicySampleMeanOEP",
                                  " " = "gulpolicyAAL", " " = "gulpolicyPLT"),
                                selected =NULL)
    )
  )
}


#' UI/View to define the Advanced  IL parameters of a process run 
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
configureOutputAdvancedILUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           h4("Insured Loss", style = "font-size: 18px; font-weight: bold;"),
           h5("Full Sample", style = "font-size: 16.5px;"), 
           h5("ELT", style = "font-size: 16.5px;"),
           tags$div(class = "h5-align", h5("AEP", style = "font-size: 16.5px;")), 
           tags$div(class = "h5-align", h5("OEP", style = "font-size: 16.5px;")),
           tags$div(class = "h5-align", h5("Multi AEP", style = "font-size: 16.5px;")),
           h5("Multi OEP", style = "font-size: 16.5px;"),  
           # h5("WS Mean AEP", style = "font-size: 16.5px;"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", style = "font-size: 16.5px;")),  
           # tags$div(class = "h5-align",h5("Sample Mean AEP", style = "font-size: 16.5px;")),
           # h5("Sample Mean OEP", style = "font-size: 16.5px;"), 
           h5("AAL", style = "font-size: 16.5px;"),
           tags$div(class = "h5-align",h5("PLT", style = "font-size: 16.5px;"))),
    
    tags$div(class = "multicol", 
             checkboxGroupInput(ns("chkilprog"),
                                label = h5("Prog", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "ilprogSummary",
                                  " " = "ilprogELT",
                                  " " = "ilprogFullUncAEP",
                                  " " = "ilprogFullUncOEP",
                                  " " = "ilprogAEPWheatsheaf",
                                  " " = "ilprogOEPWheatsheaf",  
                                  # " " = "ilprogMeanAEPWheatsheaf",
                                  # " " = "ilprogMeanOEPWheatsheaf",
                                  # " " = "ilprogSampleMeanAEP",
                                  # " " = "ilprogSampleMeanOEP",
                                  " " = "ilprogAAL",
                                  " " = "ilprogPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkilstate"),
                                label = h5("State", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "ilstateSummary",
                                  " " = "ilstateELT",
                                  " " = "ilstateFullUncAEP",
                                  " " = "ilstateFullUncOEP",
                                  " " = "ilstateAEPWheatsheaf",
                                  " " = "ilstateOEPWheatsheaf", 
                                  # " " = "ilstateMeanAEPWheatsheaf", 
                                  # " " = "ilstateMeanOEPWheatsheaf", 
                                  # " " = "ilstateSampleMeanAEP", 
                                  # " " = "ilstateSampleMeanOEP",
                                  " " = "ilstateAAL", " " = "ilstatePLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkilcounty"),
                                label = h5("County", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "ilcountySummary",
                                  " " = "ilcountyELT",
                                  " " = "ilcountyFullUncAEP",
                                  " " = "ilcountyFullUncOEP",
                                  " " = "ilcountyAEPWheatsheaf",
                                  " " = "ilcountyOEPWheatsheaf", 
                                  # " " = "ilcountyMeanAEPWheatsheaf",
                                  # " " = "ilcountyMeanOEPWheatsheaf",
                                  # " " = "ilcountySampleMeanAEP",
                                  # " " = "ilcountySampleMeanOEP",
                                  " " = "ilcountyAAL", " " = "ilcountyPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkilloc"),
                                label = h5("Location", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "illocSummary",
                                  " " = "illocELT",
                                  " " = "illocFullUncAEP",
                                  " " = "illocFullUncOEP",
                                  " " = "illocAEPWheatsheaf",
                                  " " = "illocOEPWheatsheaf", 
                                  # " " = "illocMeanAEPWheatsheaf",
                                  # " " = "illocMeanOEPWheatsheaf",
                                  # " " = "illocSampleMeanAEP",
                                  # " " = "illocSampleMeanOEP",
                                  " " = "illocAAL",
                                  " " = "illocPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkillob"),
                                label = h5("LOB", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "illobSummary",
                                  " " = "illobELT",
                                  " " = "illobFullUncAEP",
                                  " " = "illobFullUncOEP",
                                  " " = "illobAEPWheatsheaf",
                                  " " = "illobOEPWheatsheaf", 
                                  # " " = "illobMeanAEPWheatsheaf",
                                  # " " = "illobMeanOEPWheatsheaf",
                                  # " " = "illobSampleMeanAEP",
                                  # " " = "illobSampleMeanOEP",
                                  " " = "illobAAL",
                                  " " = "illobPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkilpolicy"),
                                label = h5("Policy", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "ilpolicySummary",
                                  " " = "ilpolicyELT",
                                  " " = "ilpolicyFullUncAEP",
                                  " " = "ilpolicyFullUncOEP",
                                  " " = "ilpolicyAEPWheatsheaf",
                                  " " = "ilpolicyOEPWheatsheaf", 
                                  # " " = "ilpolicyMeanAEPWheatsheaf",
                                  # " " = "ilpolicyMeanOEPWheatsheaf",
                                  # " " = "ilpolicySampleMeanAEP",
                                  # " " = "ilpolicySampleMeanOEP",
                                  " " = "ilpolicyAAL",
                                  " " = "ilpolicyPLT"),
                                selected = NULL)
    )
  )#end of fluidrow FM
  
}

#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
defineIDsUI <- function(id) {
  ns <- NS(id)
  panel(
    fluidRow(
      div(id = ns("divselectprogammeID"), 
          column(5,
                 selectInput(inputId =  ns("selectprogammeID"), label = "Programme ID", choices = "", selected = NULL),
                 bsTooltip(ns("selectprogammeID"), 
                           programme_Definition_Single$selectprogammeID, 
                           placement = "right", 
                           options   = list(container = "body"))
          )),
      hidden(div(id = ns("divselectprogOasisID"), 
                 column(5,
                        selectInput(inputId =  ns("selectprogOasisID"), label = "Oasis Programme ID", choices = "", selected = NULL),
                        bsTooltip(ns("selectprogOasisID"), 
                                  programme_Definition_Single$selectprogOasisID, 
                                  placement = "right", 
                                  options   = list(container = "body"))
                 )
      )
      )
    )
  )
}

#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
runProgrammeUI <- function(id) {
  ns <- NS(id)
  panel(

    fluidRow(
      column(12,
             hidden(
               div(id = ns("prruntable"), class = "flamingo-page-division",
                   fluidRow(
                     column(6,
                            h4("Process Runs", class = "flamingo-table-title")),
                     
                     column(4,
                            radioButtons(ns("radioprrunsAllOrInProgress"),
                                         "Processes' Status", list("All", "In_Progress"),
                                         inline = TRUE)),
                     column(1,
                            actionButton(ns("abuttonrefreshprrun"), "Refresh",
                                         class = "btn btn-primary"), align = "right")
                   ),#End of fluidrow
                   
                   dataTableOutput(ns("processrundata")),
                   
                   actionButton(ns("abuttondisplayoutput"), "Go To Display Output",
                                class = "btn btn-primary"),
                   actionButton(ns("abuttonrerunpr"), "Rerun",
                                class = "btn btn-primary"),
                   actionButton(ns("abuttonshowlog"), "Show Process Run Log",
                                class = "btn btn-primary", style = "align:right"),
                   hidden(actionButton(ns("abuttonhidelog"), "Hide Process Run Log",
                                       class = "btn btn-primary", style = "align:right"))
               )#End of div prruntable 
             )
      )
    ),
    
    fluidRow( 
      column(12,
             hidden(
               div(id = ns("prrunlogtable"), class = "flamingo-page-division",
                   fluidRow(
                     column(10,
                            h4("Process Run Logs", class = "flamingo-table-title")),
                     column(2, actionButton(ns("abuttonrefreshprrunlogs"),
                                            "Refresh", class = "btn btn-primary"), align = "right")
                   ),#End of fluidrow
                   dataTableOutput(ns("log"))
               )#End of div prrunlogtable
             )
      )
    )
    
  )
}
#' @rdname programmeDefinitionSingle
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets sliderTextInput panel
#' @importFrom shinyjs hidden
#' @importFrom shinyBS bsModal
#' @importFrom shinyWidgets radioGroupButtons
#' @export
programmeDefinitionSingleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sliderInputBlock(id),
    
    #Modals
    bsModal(ns("bsmodalsaveoutput"), "Save Configuration", trigger = "", size = "small",
            textInput(ns("tinputoutputname"), label = "Configuration Name:", value = ""),
            actionButton(inputId = ns("abuttonsubmitoutput"), label = "Submit", class = "btn btn-primary")
    ),
    bsModal(ns("bsmodalviewSLfile"), "Source Location File View", trigger = "", size = "large",
            dataTableOutput(ns("tableviewSLfile"))
    ),
    bsModal(ns("bsmodalviewSAfile"), "Source Account File View", trigger = "", size = "large",
            dataTableOutput(ns("tableviewSAfile"))
    ),
    
    # Hidden/visible panels
    hidden(div(id = ns("panelProgrammeTable"), panelProgrammeTable(id))),
    div(id = ns("panelDefineProgramme"), panelDefineProgramme(id)), 
    hidden(div(id = ns("panelProgrammeDetails"), panelProgrammeDetails(id))),
    hidden(div(id = ns("panelAssociateModel"), panelAssociateModel(id))),
    hidden(div(id = ns("panelDefineIDs"), panelDefineIDs(id))),
    hidden(div(id = ns("panelProgrammeModelTable"), panelProgrammeModelTable(id))),
    hidden(div(id = ns("panelModelDetails"), panelModelDetails(id))),
    hidden(div(id = ns("panelProcessRunTable"), panelProcessRunTable(id))),
    hidden(div(id = ns("panelDefineOutputs"), panelDefineOutputs(id))),
    hidden(div(id = ns("panelProcessRunLogs"), panelProcessRunLogs(id)))
  )
}

# Functions for UI Panels ------------------------------------------------------------------------------  


#' Function wrapping slider input
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel sliderTextInput
#' @export
sliderInputBlock <- function(id){
  ns <- NS(id)
  tagList(
    h4("Process Definition Steps"),
    br(),
    div(id = ns("divradioprogsteps"),
        radioGroupButtons(
          inputId = ns("radioprogsteps"), label = NULL, 
          choices = programmeWorkflowSteps, 
          justified = TRUE, individual = TRUE, status = "primary",
          checkIcon = list(yes = NULL, no = NULL)
        )
    )
  )
}

#'  Function defining panel elements to define a programme
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
defineProgramme <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             selectInput(inputId = ns("sinputDPAccountName"), label = "Account Name", choices = "")),
      column(4,
             textInput(inputId = ns("tinputDPProgName"), label = "Programme Name")),
      column(4,
             selectInput(inputId = ns("sinputTransformname"), label = "Transform Name", choices = ""),
             bsTooltip(ns("sinputTransformname"), 
                       programme_Definition_Single$sinputTransformname, 
                       placement = "right", 
                       options   = list(container = "body")))),
    fluidRow(column(4,
                    actionButton(ns("abuttonProgSubmit"), "Create Programme", class = "btn btn-primary"))),
    fluidRow(
      # Source Location File
      column(4,
             selectInput(inputId = ns("sinputSLFile"), label = "Source Location File",  choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSLFileUpload"),
                        fileInput(inputId = ns("SLFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        actionButton(inputId = ns("abuttonSLFileUpload"), class = "btn btn-primary", label = "Upload File", align = "left", enable = FALSE))),
             hidden(div(id = ns("divSLFileSelect"),
                        selectInput(inputId =  ns("sinputselectSLFile"), label = "Select existing File", choices = ""),
                        actionButton(inputId = ns("abuttonSLFileLink"), class = "btn btn-primary", label = "Link", align = "left"),
                        actionButton(inputId = ns("abuttonSLFileView"), class = "btn btn-primary", label = "View", align = "left")))),
      ## Source Account File
      column(4,
             selectInput(inputId =  ns("sinputSAFile"), label = "Select Option", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSAFileUpload"),
                        fileInput(inputId = ns("SAFile"), label = 'Choose a file to upload:', accept = c('csv','comma-separated-values','.csv')),
                        actionButton(inputId =  ns("abuttonSAFileUpload"), class = "btn btn-primary", label = "Upload File", align = "left"))),
             hidden(div(id = ns("divSAFileSelect"),
                        selectInput(inputId =  ns("sinputselectSAFile"), label = "Select existing File", choices = ""),
                        actionButton(inputId = ns("abuttonSAFileLink"), class = "btn btn-primary", label = "Link", align = "left"),
                        actionButton(inputId = ns("abuttonSAFileView"),class = "btn btn-primary", label = "View", align = "left"))))),
    fluidRow(
      column(11,
             align="right",
             actionButton(inputId = ns("buttonloadcanmodpr"), label = "Load Programme", class = "btn btn-primary", align = "left"),
             actionButton(inputId = ns("abuttonProgCancel"), label = "Clear", class = "btn btn-primary", align = "left")))
  )
}

#' Function wrapping panel to create/ammend programme
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelDefineProgramme <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(11, h4("Define Programme")), 
                       column(1, align = "right",  actionButton(inputId = ns("abuttonhidedefineprogpanel"), label = NULL, icon = icon("times")))),
    defineProgramme(id)
  )
}

#' Function wrapping panel to show created programmes table
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelProgrammeTable <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(10, h4("Programme Table")), 
                       column(2, align = "right", actionButton(inputId = ns("abuttonprgtblrfsh"), label = "Refresh"))),
    dataTableOutput(ns("tableDPprog")),
    actionButton(ns("buttonamendpr"), "Amend Programme", class = "btn btn-primary", align = "centre"),
    actionButton(ns("buttondeletepr"), "Delete Programme", class = "btn btn-primary", align = "right"),
    actionButton(ns("buttonprogdetails"), "Show Programme Details", class = "btn btn-primary", align = "right")
  )
}

#' Function wrapping panel to show programme details table
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelProgrammeDetails <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(10, h4("Programme Details")),
                       column(2, align = "right", 
                              actionButton(inputId = ns("abuttondefprogrfsh"), label = "Refresh"),
                              actionButton(inputId = ns("buttonhideprogdetails"), label = NULL, icon = icon("times")))),
    dataTableOutput(ns("tableprogdetails"))
  )
}

#' Function wrapping panel to associate model
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelAssociateModel <- function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(11, h4("Associate Model"))),
    fluidRow(
      column(4,
             selectInput(ns("sinputookprogid"), "Programme:", choices = c(""))),
      column(4,
             selectInput(ns("sinputookmodelid"), "Model:", choices = c(""))),
      column(4,
             selectInput(ns("sinputProgModTransform"), "Transform Name", choices = ""),
             bsTooltip(ns("sinputProgModTransform"), 
                       programme_Definition_Single$sinputProgModTransform, 
                       placement = "right", 
                       options   = list(container = "body")))),
    
    actionButton(inputId = ns("abuttoncrprogoasis"), label = "Create", class = "btn btn-primary")
  )
}

#' Function wrapping panel to define prgramme and model IDs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelDefineIDs <-  function(id){
  ns <- NS(id)
  
  panel(
    status = "primary",
    #heading = fluidRow(column(11, h4("Filter"))),
    fluidRow(
      div(id = ns("divselectprogrammeID"), 
          column(3,
                 selectInput(inputId =  ns("selectprogrammeID"), label = "Programme ID", choices = "", selected = NULL),
                 bsTooltip(ns("selectprogrammeID"), 
                           programme_Definition_Single$selectprogrammeID, 
                           placement = "right", 
                           options   = list(container = "body")))),
      hidden(div(id = ns("divselectprogOasisID"), 
                 column(3,
                        selectInput(inputId =  ns("selectprogOasisID"), label = "Oasis Programme ID", choices = "", selected = NULL),
                        bsTooltip(ns("selectprogOasisID"), 
                                  programme_Definition_Single$selectprogOasisID, 
                                  placement = "right", 
                                  options   = list(container = "body")))))
    )
  )
}

#' Function wrapping panel to show created programme model table
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelProgrammeModelTable <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(10, h4("Programme Model Table")), 
                       column(2, align = "right",  actionButton(inputId = ns("abuttonookrefresh"), label = "Refresh"))),
    dataTableOutput(ns("tableProgOasisOOK")),
    fluidRow(column(12, actionButton(ns("buttonmodeldetails"), "Show Programme Model Details", class = "btn btn-primary"), align = "right"))
  )
}

#' Function wrapping panel to show programme details table
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelModelDetails <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(10, h4("Programme Model Details")), 
                       column(2, align = "right", 
                              actionButton(inputId = ns("abuttonprgoasisrfsh"), label = "Refresh"),
                              actionButton(inputId = ns("buttonhidemodeldetails"), label = NULL, icon = icon("times")))),
    dataTableOutput(ns("tabledisplayprogoasisfiles"))
  )
}

#' Function wrapping panel to define outputs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelDefineOutputs <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(10, h4("Define Programme Output")), 
                       column(2, align = "right",  hidden(actionButton(inputId = ns("abuttonehidepanelconfigureoutput"), label = NULL, icon = icon("times"))))),
    fluidRow(
      column(4,
             panelDefineOutputsDetails(id)),
      column(8,
             panelDefineOutputConfiguration(id))
    ),
    fluidRow(
      column(12,
             actionButton(inputId = ns("abuttonexecuteprrun"), label = "Execute Run", class = "btn btn-primary"), align = "right"))
  )
}

#' Function wrapping sub-panel to define outputs details
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelDefineOutputsDetails <-  function(id){
  ns <- NS(id)
  tagList(
    panel(
      heading = fluidRow(column(11, h4("Configuration Details"))),
      selectInput(ns("sinoutputoptions"), "Select Custom Configuration:", choices = ""),
      textInput(ns("tinputprocessrunname"), label = "Process Run Name:", value = "")
    ),
    panel(
      heading = fluidRow(column(11, h4("Model Parameters"))),
      div( id = ns("noofsample"), style = "width:100%; margin: 0 auto;", textInput(ns("tinputnoofsample"), label = "Number of Samples:", value = "10")),
      hidden(div(id = ns("configureModelParamsAdvanced"), align = "left",
                 textInput(ns("tinputthreshold"), label = "Loss Threshold:", value = "0"),
                 selectInput(ns("sinputeventset"), label = "Event Set:", choices = "Probabilistic"),
                 selectInput(ns("sinputeventocc"), label = "Event Occurrence Set:", choices = "Long Term"),
                 checkboxInput(ns("chkinputsummaryoption"), "Summary Reports", value = TRUE),
                 h5("Available Perils"),
                 checkboxInput(ns("chkinputprwind"), label = "Peril: Wind", value = TRUE),
                 checkboxInput(ns("chkinputprstsurge"), label = "Peril: Surge", value = TRUE),
                 checkboxInput(ns("chkinputprquake"), label = "Peril: Quake", value = TRUE),
                 checkboxInput(ns("chkinputprflood"), label = "Peril: Flood", value = TRUE),
                 checkboxInput(ns("chkinputdsurge"), label = "Demand Surge", value = TRUE),
                 sliderInput(ns("sliderleakagefac"), label = "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)))
    )
  )
}

#' Function wrapping sub-panel to define outputs configuration
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelDefineOutputConfiguration <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(11, h4("Output Configuration"))),
    checkboxInput(ns("chkinputGUL"), label = "Ground Up Loss", value = TRUE),
    hidden(div(id = ns("configureAdvancedGUL"), configureAdvancedGUL(id))),
    checkboxInput(ns("chkinputIL"), label = "Insured Loss", value = FALSE), 
    hidden(div(id = ns("configureAdvancedIL"), configureAdvancedIL(id))),
    actionButton(inputId = ns("abtnadvanced"), label = "Advanced", class = "btn btn-primary"),
    hidden(actionButton(inputId = ns("abtnbasic"), label = "Basic", class = "btn btn-primary")),
    hidden(actionButton(inputId = ns("abuttonsaveoutput"), label = "Save Configuration", class = "btn btn-primary")),
    hidden(actionButton(inputId = ns("abtnclroutopt"), label = "Default", class = "btn btn-primary"))
  )
}

#' Function wrapping sub-panel to define outputs advanced configuration GUL
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
configureAdvancedGUL <-  function(id){
  ns <- NS(id)
  fluidRow(
    # Few outputs commented/disabled for the first release. To be enabled for later releases.  
    column(4,
           h4("Ground Up Loss", class = "flamingo-loss"),
           h5("Full Sample", class = "flamingo-measure"), 
           h5("ELT", class = "flamingo-measure"),
           tags$div(class = "h5-align", h5("AEP", class = "flamingo-measure")), 
           tags$div(class = "h5-align", h5("OEP", class = "flamingo-measure")),
           tags$div(class = "h5-align", h5("Multi AEP", class = "flamingo-measure")),
           h5("Multi OEP", class = "flamingo-measure"),
           # h5("WS Mean AEP", class = "flamingo-measure"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", class = "flamingo-measure")),
           # tags$div(class = "h5-align", h5("Sample Mean AEP", class = "flamingo-measure")),
           # h5("Sample Mean OEP", class = "flamingo-measure"), 
           h5("AAL", class = "flamingo-measure"),
           tags$div(class = "h5-align", h5("PLT", class = "flamingo-measure"))),
    
    tags$div(class = "multicol",
             checkboxGroupInput(ns("chkgulprog"),
                                label = h6("Prog", class = "flamingo-granularity"), 
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
                                label = h6("State", class = "flamingo-granularity"), 
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
                                label = h6("County", class = "flamingo-granularity"), 
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
                                label = h6("Location", class = "flamingo-granularity"), 
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
                                label = h6("LOB", class = "flamingo-granularity"), 
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
                                label = h6("Policy", class = "flamingo-granularity"), 
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
                                selected = NULL)
    )
  )
}

#' Function wrapping sub-panel to define outputs advanced configuration IL
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
configureAdvancedIL <-  function(id){
  ns <- NS(id)
  fluidRow(
    column(4,
           h4("Insured Loss", class = "flamingo-loss"),
           h5("Full Sample", class = "flamingo-measure"), 
           h5("ELT", class = "flamingo-measure"),
           tags$div(class = "h5-align", h5("AEP", class = "flamingo-measure")), 
           tags$div(class = "h5-align", h5("OEP", class = "flamingo-measure")),
           tags$div(class = "h5-align", h5("Multi AEP", class = "flamingo-measure")),
           h5("Multi OEP", class = "flamingo-measure"),  
           # h5("WS Mean AEP", class = "flamingo-measure"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", class = "flamingo-measure")),  
           # tags$div(class = "h5-align", h5("Sample Mean AEP", class = "flamingo-measure")),
           # h5("Sample Mean OEP", class = "flamingo-measure"), 
           h5("AAL", class = "flamingo-measure"),
           tags$div(class = "h5-align",h5("PLT", class = "flamingo-measure"))),
    
    tags$div(class = "multicol", 
             checkboxGroupInput(ns("chkilprog"),
                                label = h6("Prog", class = "flamingo-granularity"), 
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
                                label = h6("State", class = "flamingo-granularity"), 
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
                                label = h6("County", class = "flamingo-granularity"), 
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
                                label = h6("Location", class = "flamingo-granularity"), 
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
                                label = h6("LOB", class = "flamingo-granularity"), 
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
                                label = h6("Policy", class = "flamingo-granularity"), 
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
  )
}

#' Function wrapping panel to show process run table
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelProcessRunTable <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(10, h4("Process Runs")), 
                       column(2, align = "right",  actionButton(inputId = ns("abuttonrefreshprrun"), label = "Refresh"))),
    div(id = "divProcessRun",
        fluidRow(column(12, 
                        radioButtons(inputId = ns("radioprrunsAllOrInProgress"), "Processes' Status", list("All", "In_Progress"), inline = TRUE))),
        dataTableOutput(ns("tableprocessrundata")),
        fluidRow(column(12, 
                        div(id = ns("divprocessRunButtons"),
                            actionButton(inputId = ns("abuttondisplayoutput"), label = "Go To Display Output", class = "btn btn-primary"),
                            actionButton(inputId = ns("abuttonrerunpr"), label = "Rerun", class = "btn btn-primary"),
                            actionButton(inputId = ns("abuttonshowlog"), label = "Show Process Run Log",class = "btn btn-primary")
                            )))
        )
     )
}

#' Function wrapping panel to show Process Run logs table
#' @inheritParams flamingoModuleUI
#' @importFrom shinyWidgets panel 
#' @importFrom shinyjs hidden
#' @export
panelProcessRunLogs <-  function(id){
  ns <- NS(id)
  panel(
    heading = fluidRow(column(10, h4("Process Run Logs")), 
                       column(2, align = "right", 
                              actionButton(inputId = ns("abuttonrefreshprrunlogs"), label = "Refresh"),
                              actionButton(inputId = ns("abuttonhidelog"), label = NULL, icon = icon("times")))),
    dataTableOutput(ns("tablelog"))
  )
}
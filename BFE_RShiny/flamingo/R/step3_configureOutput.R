# step3_configureOutput Module ------------------

# UI ------------------------------------------
#' step3_configureOutput ui
#' @rdname step3_configureOutput
#' @description UI/View for the step3_configureOutput
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#' @export
step3_configureOutputUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    hidden(div(id = ns("panelProcessRunTable"), panelProcessRunTable(id))),
    hidden(div(id = ns("panelDefineOutputs"), panelDefineOutputs(id))),
    hidden(div(id = ns("panelProcessRunLogs"), panelProcessRunLogs(id)))
  )
}


#' Function wrapping panel to show process run table
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
panelProcessRunTable <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("runs"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitlepanelProcessRunTable"), inline = TRUE),
      actionButton(inputId = ns("abuttonrefreshprrun"), label = "Refresh", style = "float: right;")
    ),
    div(id = "divProcessRun",
        fluidRow(column(12,
                        radioButtons(inputId = ns("radioprrunsAllOrInProgress"), "Processes' Status", list("All", "In_Progress"), inline = TRUE))),
        DTOutput(ns("tableprocessrundata")),
        fluidRow(column(12,
                        div(id = ns("divprocessRunButtons"),
                            actionButton(inputId = ns("abuttonconfigoutput"), label = "New Output Configuration", class = "btn btn-primary"),
                            actionButton(inputId = ns("abuttonrerunpr"), label = "Rerun", class = "btn btn-primary"),
                            actionButton(inputId = ns("abuttonshowlog"), label = "Show Log", class = "btn btn-primary"),
                            div(
                              actionButton(inputId = ns("abuttondisplayoutput"), label = "Browse Run Outputs", class = "btn btn-primary")
                              , style = "inline: true;float: right;")
                        )))
    )
  )
}

#' Function wrapping panel to show log table for specific Process Run
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
panelProcessRunLogs <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("runlogs"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleProcessRunLogs"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelog"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("tablelog"))
  )
}

#' Function wrapping panel to define outputs
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
panelDefineOutputs <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("progout"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitleReDefineProgramme"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidepanelconfigureoutput"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
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
#' @importFrom shinyjs hidden
#' @export
panelDefineOutputsDetails <- function(id) {
  ns <- NS(id)
  tagList(
    flamingoPanel(
      collapsible = FALSE,
      ns("configdtl"),
      heading = h4("Configuration Details"),
      selectInput(ns("sinoutputoptions"), "Select Custom Configuration:", choices = ""),
      textInput(ns("tinputprocessrunname"), label = "Process Run Name:", value = "")
    ),
    flamingoPanel(
      collapsible = FALSE,
      ns("modelpar"),
      heading = h4("Model Parameters"),
      div(id = ns("noofsample"), style = "width:100%; margin: 0 auto;", textInput(ns("tinputnoofsample"), label = "Number of Samples:", value = "10")),
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
#' @importFrom shinyjs hidden
#' @export
panelDefineOutputConfiguration <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("outconfig"),
    heading = h4("Output Configuration"),
    checkboxInput(ns("chkinputGUL"), label = "Ground Up Loss", value = TRUE),
    hidden(div(id = ns("configureAdvancedGUL"), configureAdvancedGUL(id))),
    checkboxInput(ns("chkinputIL"), label = "Insured Loss", value = FALSE),
    hidden(div(id = ns("configureAdvancedIL"), configureAdvancedIL(id))),
    checkboxInput(ns("chkinputRI"), label = "Net RI Loss", value = FALSE),
    hidden(div(id = ns("configureAdvancedRI"), configureAdvancedRI(id))),
    actionButton(inputId = ns("abtnadvanced"), label = "Advanced", class = "btn btn-primary"),
    hidden(actionButton(inputId = ns("abtnbasic"), label = "Basic", class = "btn btn-primary")),
    hidden(actionButton(inputId = ns("abuttonsaveoutput"), label = "Save Configuration", class = "btn btn-primary")),
    hidden(actionButton(inputId = ns("abtnclroutopt"), label = "Default", class = "btn btn-primary"))
  )
}

#' Function wrapping sub-panel to define outputs advanced configuration GUL
#' @inheritParams flamingoModuleUI
#' @export
configureAdvancedGUL <- function(id) {
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
#' @export
configureAdvancedIL <- function(id) {
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
           tags$div(class = "h5-align", h5("PLT", class = "flamingo-measure"))),
    
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

#' Function wrapping sub-panel to define outputs advanced configuration RI
#' @inheritParams flamingoModuleUI
#' @export
configureAdvancedRI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           h4("Net RI Loss", style = "font-size: 18px; font-weight: bold;"),
           h5("Full Sample", style = "font-size: 16.5px;"), 
           h5("ELT", style="font-size: 16.5px;"),
           tags$div(class = "h5-align", h5("AEP", style="font-size: 16.5px;")), 
           tags$div(class = "h5-align", h5("OEP", style="font-size: 16.5px;")),
           tags$div(class = "h5-align", h5("Multi AEP", style="font-size: 16.5px;")),
           h5("Multi OEP", style = "font-size: 16.5px;"),  
           # h5("WS Mean AEP", style="font-size: 16.5px;"),
           # tags$div(class = "h5-align", h5("WS Mean OEP", style="font-size: 16.5px;")),  
           # tags$div(class = "h5-align",h5("Sample Mean AEP", style="font-size: 16.5px;")),
           # h5("Sample Mean OEP", style="font-size: 16.5px;"), 
           h5("AAL", style="font-size: 16.5px;"),
           tags$div(class = "h5-align",h5("PLT", style="font-size: 16.5px;"))),
    
    tags$div(class = "multicol", 
             checkboxGroupInput(ns("chkriprog"),
                                label = h5("Prog", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "riprogSummary",
                                  " " = "riprogELT",
                                  " " = "riprogFullUncAEP",
                                  " " = "riprogFullUncOEP",
                                  " " = "riprogAEPWheatsheaf",
                                  " " = "riprogOEPWheatsheaf",  
                                  " " = "riprogAAL",
                                  " " = "riprogPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkristate"),
                                label = h5("State", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "ristateSummary",
                                  " " = "ristateELT",
                                  " " = "ristateFullUncAEP",
                                  " " = "ristateFullUncOEP",
                                  " " = "ristateAEPWheatsheaf",
                                  " " = "ristateOEPWheatsheaf", 
                                  " " = "ristateAAL", 
                                  " " = "ristatePLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkricounty"),
                                label = h5("County", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "ricountySummary",
                                  " " = "ricountyELT",
                                  " " = "ricountyFullUncAEP",
                                  " " = "ricountyFullUncOEP",
                                  " " = "ricountyAEPWheatsheaf",
                                  " " = "ricountyOEPWheatsheaf", 
                                  " " = "ricountyAAL", 
                                  " " = "ricountyPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkriloc"),
                                label = h5("Location", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "rilocSummary",
                                  " " = "rilocELT",
                                  " " = "rilocFullUncAEP",
                                  " " = "rilocFullUncOEP",
                                  " " = "rilocAEPWheatsheaf",
                                  " " = "rilocOEPWheatsheaf", 
                                  " " = "rilocAAL",
                                  " " = "rilocPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkrilob"),
                                label = h5("LOB", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "rilobSummary",
                                  " " = "rilobELT",
                                  " " = "rilobFullUncAEP",
                                  " " = "rilobFullUncOEP",
                                  " " = "rilobAEPWheatsheaf",
                                  " " = "rilobOEPWheatsheaf", 
                                  " " = "rilobAAL",
                                  " " = "rilobPLT"),
                                selected = NULL),
             
             checkboxGroupInput(ns("chkripolicy"),
                                label = h5("Policy", style = "font-size: 15.0px;"), 
                                choices = list(
                                  " " = "ripolicySummary",
                                  " " = "ripolicyELT",
                                  " " = "ripolicyFullUncAEP",
                                  " " = "ripolicyFullUncOEP",
                                  " " = "ripolicyAEPWheatsheaf",
                                  " " = "ripolicyOEPWheatsheaf", 
                                  " " = "ripolicyAAL",
                                  " " = "ripolicyPLT"),
                                selected = NULL)
    )
  )#end of fluidrow RI
}

# Server --------------------------------------
#' step3_configureOutput server
#' @description Server logic to step3_configureOutput
#' @inheritParams flamingoModule
#' @return For \code{programmeDefinitionSingle()}, list of reactives.
#' @template return-outputNavigation
#' @rdname step3_configureOutput
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDT dataTableProxy selectRows DTOutput selectPage
#' @importFrom dplyr mutate select case_when
#' @importFrom shinyjs onclick js removeClass addClass
#' @export
step3_configureOutput <- function(input, output, session, 
                                  dbSettings,apiSettings, userId, 
                                  active = reactive(TRUE), 
                                  logMessage = message,
                                  currstep = reactive(-1),
                                  selectprogrammeID = reactive(""),
                                  selectprogOasisID = reactive(""),
                                  progOasisName = reactive(""),
                                  progOasisStatus = reactive(""),
                                  POData_rowselected  = reactive(NULL)
) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ------------------------------------------
  
  #number of Rows per Page in a dataable
  pageLength <- 5
  
  # Help function
  '%notin%' <- Negate('%in%')
  
  # Default checkgroup for  GUL, IL and RI
  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")
  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")
  checkrigrplist <- c("chkriprog", "chkristate", "chkricounty", "chkriloc", "chkrilob", "chkripolicy")
  
  # > Reactive Values -------------------------------------------------------
  result <- reactiveValues(
    # reactve value for navigation
    navigationstate = NULL,
    # reactive value for process runs table
    prcrundata = NULL,
    # Id of the Process Run
    prrunid = -1
  )
  
  # Reset Param
  observe(if (active()) {
    result$navigationstate <- NULL
  })
  
  # Panels Visualization ----------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 3 ) {
      .defaultRun()
      .reloadRunData()
    }
  })
  
  # hide process run section if DC returns empty table
  observeEvent(result$prcrundata, {
    if (nrow(result$prcrundata) == 0) {
      hide("abuttondisplayoutput")
    } else {
      show("abuttondisplayoutput")
    }
  })
  
  # If selectprogOasisID changes, reload process run table and set view back to default
  observeEvent(selectprogOasisID(), ignoreInit = TRUE, {
    if (active()) {
      hide("panelDefineOutputs")
      hide("panelProcessRunLogs")
      .reloadRunData()
    }
  })
  
  
  ### > Process Run Table -----
  # reload if radio buttons for 'All' vs 'In_Progress' change
  observeEvent(input$radioprrunsAllOrInProgress, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste0("filter changed to ", input$radioprrunsAllOrInProgress))
      .reloadRunData()
    }
  })
  
  #Content of the process run table
  .getProcessRunWithUserChoices <- function(pruser, prmodel, prprogramme,
                                            prworkflow) {
    logMessage(".getProcessRunWithUserChoices called")
    #prtable <- getProcessData(dbSettings, pruser, prmodel, prprogramme, prworkflow)
    prcid <- selectprogOasisID()
    # For processes in all states (completed, created, in progress etc), pass 'All', for just in progress pass
    # 'In Progress' (not handled by stored procedure in the DB due to bug!)
    prcrundata <- getProcessRun(dbSettings, prcid, input$radioprrunsAllOrInProgress)
    StatusGood <- "Completed"
    StatusBad <- c("Failed", "Cancelled", NA_character_)
    # RSc TODO: should probably allow NULL to clear connections when selecting
    # a ProgOasisID that has no runs
    if (!is.null(prcrundata) && nrow(prcrundata) > 0 ) {
      show("tableprocessrundata")
      show("divprocessRunButtons")
      result$prcrundata <- prcrundata %>%
        mutate(ProcessRunStatus = case_when(ProcessRunStatus %in% StatusGood ~ StatusCompleted,
                                            ProcessRunStatus %in% StatusBad ~ StatusFailed,
                                            ProcessRunStatus %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
        as.data.frame()
      #Handling bug for 'In Progress' 
      if (input$radioprrunsAllOrInProgress == "In_Progress") {
        result$prcrundata <- result$prcrundata %>% filter(ProcessRunStatus == StatusProcessing)
      }
    } else {
      result$prcrundata <- NULL
    }
  }
  
  output$tableprocessrundata <- renderDT(
    
    if (!is.null(result$prcrundata) && nrow(result$prcrundata) > 0) {
      
      # manual refresh button
      invisible(input$abuttonrefreshprrun)
      
      # if (preselRunId() == -1) {
      #   index <- 1
      # } else {
      #   index <- match(c(preselRunId()), result$prcrundata[,prcrundata.ProcessRunID])
      # }
      index <- 1
      logMessage("re-rendering process run table")
      datatable(
        result$prcrundata,
        class = "flamingo-table display",
        rownames = TRUE,
        selection = list(mode = 'single',
                         selected = rownames(result$prcrundata)[c(as.integer(index))]),
        escape = FALSE,
        colnames = c('Row Number' = 1),
        filter = 'bottom',
        options = .getPRTableOptions()
      )
    } else {
      .nothingToShowTable(contentMessage = paste0("no runs available for Model ID ", selectprogOasisID()))
    })
  
  # Process Run Table Title
  output$paneltitlepanelProcessRunTable <- renderUI({
    if (selectprogOasisID() != "") {
      progOasisName <- ifelse(toString(progOasisName()) == " " | toString(progOasisName()) == "" | toString(progOasisName()) == "NA", "", paste0('"',  toString(progOasisName()), '"'))
      paste0('Runs for Model ', progOasisName,' (id: ', toString(selectprogOasisID()), ') ', toString(progOasisStatus()))
    } else {
      paste0("Runs")
    }
    
  })
  
  #Not allow any actions if the process run table is empty
  observeEvent(result$prcrundata, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      if (!is.null(result$prcrundata) && nrow(result$prcrundata) > 0) {
        show("divprocessRunButtons")
      } else {
        hide("divprocessRunButtons")
      }
    }
  })
  
  # > Configure Output --------------------------------------------
  # configuration title
  output$paneltitleReDefineProgramme <- renderUI({
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunID]
      processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunName]
      processRunName <- ifelse(processRunName == " ", "", paste0('"', processRunName, '"'))
      paste0('Re-Define Output Configuration for Run ', processRunName, ' (id: ', processRunId, ')')
    } else {
      "New Output Configuration"
    }
  })
  
  #Show Output Configuration Panel
  onclick("abuttonconfigoutput", {
    if (progOasisStatus() == "- Status: Completed") {
      if (selectprogOasisID() != "") {
        .defaultview(session)
        show("panelDefineOutputs")
        logMessage("showing panelDefineOutputs")
        logMessage(paste("updating tableprocessrundataa select because defining new output configuration"))
        selectRows(dataTableProxy("tableprocessrundata"), selected = NULL)
        selectPage(dataTableProxy("tableprocessrundata"), 1)
        logMessage(paste("selected row is:", input$tableprocessrundata_rows_selected))
      } else {
        showNotification(type = "error", "Please select a Programme Model first")
      }
    } else {
      showNotification(type = "warning", "Please select a completed Programme Model first")
    }
  })
  
  onclick("abuttonrerunpr", {
    if (progOasisStatus() == "- Status: Completed") {
      if (length(input$tableprocessrundata_rows_selected) > 0) {
        .defaultview(session)
        show("panelDefineOutputs")
        logMessage("showing panelDefineOutputs")
        .updateOutputConfig()
      } else {
        showNotification(type = "warning", "Please select Process Run first")
      }
    } else {
      showNotification(type = "warning", "Please select a completed Programme Model first")
    }
  })
  
  ### Hide Output Configuration panel
  onclick("abuttonehidepanelconfigureoutput", {
    hide("panelDefineOutputs")
  })
  
  # simplified view selection
  observe({
    if (length(input$chkgulprog) > 0 |  length(input$chkgulstate) > 0 |
        length(input$chkgulcounty) > 0 |  length(input$chkgulloc) > 0 |
        length(input$chkgullob) > 0 | length(input$chkgulpolicy) > 0) {
      updateCheckboxInput(session, "chkinputGUL", value = TRUE)
      disable("chkgulpolicy")
    }
    if (length(input$chkilprog) > 0 |  length(input$chkilstate) > 0 |
        length(input$chkilcounty) > 0 |  length(input$chkilloc) > 0 |
        length(input$chkillob) > 0 | length(input$chkilpolicy) > 0) {
      updateCheckboxInput(session, "chkinputIL", value = TRUE)
    }
    if (length(input$chkriprog) > 0 |  length(input$chkristate) > 0 |
        length(input$chkricounty) > 0 |  length(input$chkriloc) > 0 |
        length(input$chkrilob) > 0 | length(input$chkripolicy) > 0) {
      updateCheckboxInput(session, "chkinputRI", value = TRUE)
    }
  })
  
  # Select/deselect GUL
  observeEvent(input$chkinputGUL, ignoreInit = TRUE,  {
    if (active()) {
      if (input$chkinputGUL == FALSE) {
        .clearchkboxGULgrp()
      }  else {
        disable("chkgulpolicy")
        gullistlength <- length(input$chkgulprog) + length(input$chkgulstate) +
          length(input$chkgulcounty) + length(input$chkgulloc) +
          length(input$chkgullob) + length(input$chkgulpolicy)
        if (gullistlength == 0) {
          for (i in checkgulgrplist) {
            updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesGUL)
          }
        }
      }
    }
  })
  
  # Select/deselect IL 
  #Note: the ignoreInit = TRUE does not prevent the trigger once logged in
  observeEvent(input$chkinputIL, ignoreInit = TRUE, {
    if (active()) {
      if (input$chkinputIL == FALSE) {
        .clearchkboxILgrp()
      } else {
        if (length(input$chkilprog) == 0 &  length(input$chkilstate) == 0 &
            length(input$chkilcounty) == 0 &  length(input$chkilloc) == 0 &
            length(input$chkillob) == 0 & length(input$chkilpolicy) == 0) {
          for (i in checkilgrplist) {
            updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesIL)
          }
        }
      }
    }
  })
  
  # Select/deselect RI
  observeEvent(input$chkinputRI, ignoreInit = TRUE, {
    if (active()) {
      if (input$chkinputRI == FALSE) {
        .clearchkboxRIgrp()
      } else {
        if (length(input$chkriprog) == 0 &  length(input$chkristate) == 0 &
            length(input$chkricounty) == 0 &  length(input$chkriloc) == 0 &
            length(input$chkrilob) == 0 & length(input$chkripolicy) == 0) {
          for (i in checkrigrplist) {
            updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesRI)
          }
        }
      }
    }
  })
  
  # Update button in sidebar panel to update checkboxes for pre-populated values
  observeEvent(input$sinoutputoptions, {
    if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "") {
      outputlist <- executeDbQuery(dbSettings,
                                   buildDbQuery("getOutputOptionOutputs", input$sinoutputoptions))
      
      if (nrow(outputlist) > 0) {
        # print(paste0("outputlist"))
        # print(outputlist)
        for (i in 1:nrow(outputlist)) {
          grpid <- paste0("chk",outputlist$Group[i])
          grpinputid <- strsplit(toString(grpid), " ")[[1]]
          chkboxid <- outputlist$Parameter[i]
          selchoices <- as.list(strsplit(toString(chkboxid), ",")[[1]])
          updateCheckboxGroupInput(session, inputId = grpinputid, selected = c(selchoices))
        }
      }
    }
  })
  
  # Clear the checkbox groups and preset dropdown - Set back to default
  onclick("abtnclroutopt", {
    .defaultview(session)
  })
  
  # show advanced view
  onclick("abtnadvanced", {
    .advancedview()
  })
  
  # show basic view
  onclick("abtnbasic", {
    .basicview()
  })
  
  # reactive expression yielding the output options as a list
  outputOptionsList <- reactive({paste(collapse = ",", c(
    input$chkinputGUL, input$chkgulprog, input$chkgulpolicy,
    input$chkgulstate, input$chkgulcounty, input$chkgulloc,
    input$chkgullob,
    input$chkinputIL, input$chkilprog, input$chkilpolicy,
    input$chkilstate, input$chkilcounty, input$chkilloc,
    input$chkillob,
    input$chkinputRI, input$chkriprog, input$chkripolicy,
    input$chkristate, input$chkricounty, input$chkriloc,
    input$chkrilob
  ))})
  
  # Save output for later use as presets
  .modalsaveoutput <- function() {
    ns <- session$ns
    modalDialog(label = "modalsaveoutput",
                title = "Save Configuration",
                textInput(ns("tinputoutputname"), label = "Configuration Name:", value = ""),
                footer = tagList(
                  actionButton(inputId = ns("abuttonsubmitoutput"),
                               label = "Submit", class = "btn btn-primary")
                ),
                size = "s",
                easyClose = TRUE
    )
  }
  
  onclick("abuttonsaveoutput", {
    if (outputOptionsList() != "") {
      showModal(.modalsaveoutput())
    } else {
      removeModal()
      showNotification(type = "warning", "Please select Output Configuration")
    }
  })
  
  # Submit output configuration (to be saved)
  onclick("abuttonsubmitoutput", {
    if (input$tinputoutputname == "") {
      showNotification(type = "warning", "Please enter Output Configuration Name")
    } else {
      stmt <- paste0("exec dbo.saveoutputoption @OutputOptionName = '",
                     input$tinputoutputname, "',@OutputOptionsList = '",
                     outputOptionsList(), "'")
      executeDbQuery(dbSettings, stmt)
      showNotification(type = "message", paste0("Output Configuration ", input$tinputoutputname ," saved"))
      updateTextInput(session, "tinputoutputname", value = "")
      removeModal()
      .clearOutputOptions()
      #.defaultview(session)
    }
  })
  
  
  ### > Run Process --------------------------------------------------------
  # A function to generate process run
  .generateRun <- function() {
    
    processrunname <- isolate(input$tinputprocessrunname)
    nosample <- isolate(input$tinputnoofsample)
    sthreshold <- isolate(input$tinputthreshold)
    eventsetid <- isolate(input$sinputeventset)
    eventoccid <- isolate(input$sinputeventocc)
    
    # windperil <- NULL
    # surgeperil <- NULL
    # quakeperil <- NULL
    # floodperil <- NULL
    # dmdsurge <- NULL
    # leakagefactor <- NULL
    
    summaryreports <- tolower(isolate(input$chkinputsummaryoption))
    
    # functionality to handle model resource based metrics
    if (length(POData_rowselected()) > 0) {
      prgId <- selectprogOasisID()
      prgId <- ifelse(is.null(prgId), toString(prgId), -1)
    } else {
      prgId <- -1
    }
    stmt <- buildDbQuery("getRuntimeParamList", prgId)
    runparamlist <- executeDbQuery(dbSettings, stmt)
    
    rows <- nrow(runparamlist)
    if (rows > 0) {
      for (i in 1:rows) {
        switch(runparamlist[i, 1],
               'demand_surge'   = {dmdsurge <- tolower(isolate(input$chkinputdsurge))},
               'peril_wind'     = {windperil <- tolower(isolate(input$chkinputprwind))},
               'peril_surge'    = {surgeperil <- tolower(isolate(input$chkinputprstsurge))},
               'peril_quake'    = {quakeperil <- tolower(isolate(input$chkinputprquake))},
               'peril_flood'    = {floodperil <- tolower(isolate(input$chkinputprflood))},
               'leakage_factor' = {leakagefactor <- isolate(input$sliderleakagefac)}
        )
      }
    }
    
    outputsStringGUL <- paste(collapse = ", ",
                              c(input$chkgulprog, input$chkgulpolicy, input$chkgulstate,
                                input$chkgulcounty, input$chkgulloc, input$chkgullob))
    
    outputsStringIL <- paste(collapse = ", ",
                             c(input$chkilprog, input$chkilpolicy, input$chkilstate,
                               input$chkilcounty, input$chkilloc, input$chkillob))
    
    outputsStringRI <- paste(collapse = ", ",
                             c(input$chkriprog, input$chkripolicy, input$chkristate,
                               input$chkricounty, input$chkriloc, input$chkrilob))
    
    stmt <- paste0("exec dbo.WorkflowFlattener ",
                   "@ProgOasisID= ", prgId, ", ",
                   "@WorkflowID= 1", ", ",
                   "@NumberOfSamples=", nosample, ", ",
                   "@GULThreshold= ", sthreshold, ", ",
                   "@UseRandomNumberFile= 0, ",
                   "@OutputsStringGUL= '", outputsStringGUL, "', ",
                   "@OutputsStringIL= '", outputsStringIL, "', ",
                   "@OutputsStringRI= '", outputsStringRI, "', ",
                   "@EventSetID= '", eventsetid ,"', ",
                   "@EventOccurrenceID= '", eventoccid, "', ",
                   "@PerilWind = '", windperil ,"', ",
                   "@PerilSurge='", surgeperil, "', ",
                   "@PerilQuake='", quakeperil, "', ",
                   "@PerilFlood='", floodperil, "', ",
                   "@DemandSurge= '", dmdsurge, "', ",
                   "@LeakageFactor= '" , leakagefactor, "', ",
                   "@ProcessRunName= '" , processrunname, "', ",
                   "@SummaryReports='", summaryreports , "'")
    
    logMessage(paste("Workflow flattener query: ", stmt))
    runId <- executeDbQuery(dbSettings, stmt)
    logMessage(paste("Process Run ID: ", runId))
    
    return(runId)
  }
  
  # Execute Process run: When "Execute Run" button is clicked - switsches view to Run panel
  onclick("abuttonexecuteprrun", {
    if (outputOptionsList() == "") {
      showNotification(type = "warning", "Please select Output Configuration")
    } else {
      runId <- .generateRun()
      if (is.null(runId)) {
        showNotification(type = "error",
                         "Process Run ID could not be generated. So process run cannot be executed")
      } else {
        status <- runProcess(apiSettings, runId)
        if (grepl("success", status, ignore.case = TRUE)) {
          showNotification(type = "message",
                           sprintf("Created Process Run ID: %s and process run is executing",
                                   runId))
          .reloadRunData()
          #logMessage(paste("colnames are:", paste(colnames(result$prcrundata), collapse = ", ")))
          logMessage(paste("updating tableprocessrundataa select because executing a new run"))
          rowToSelect <- match(runId, result$prcrundata[, prcrundata.ProcessRunID])
          pageSel <- ceiling(rowToSelect/pageLength)
          selectRows(dataTableProxy("tableprocessrundata"), rowToSelect)
          selectPage(dataTableProxy("tableprocessrundata"), pageSel)
          logMessage(paste("selected row is:", input$tableprocessrundata_rows_selected))
        } else {
          showNotification(type = "warning",
                           sprintf("Created Process Run ID: %s. But process run executing failed",
                                   runId))
          hide("abuttondisplayoutput")
          show("panelProcessRunLogs")
          logMessage("showing prrunlogtable")
          hide("abuttonshowlog")
        }
      }
      .defaultview(session)
    }
  })
  
  ### > Logs ---------------------------------------------------------------
  onclick("abuttonshowlog", {
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      show("panelProcessRunLogs")
      logMessage("showing prrunlogtable")
      hide("abuttonshowlog")
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
    }
  })
  
  onclick("abuttonhidelog", {
    hide("panelProcessRunLogs")
    show("abuttonshowlog")
  })
  
  ### Log Table
  output$tablelog <- renderDT({
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      # manual refresh button
      invisible(input$abuttonrefreshprrunlogs)
      
      wfid <- result$prrunid
      
      StatusGood <- "Success"
      StatusBad <- c("Cancelled", "Failed",  NA_character_)
      logdata <- getProcessRunDetails(dbSettings, wfid) %>%
        mutate(Status = case_when(Status %in% StatusGood ~ StatusCompleted,
                                  Status %in% StatusBad ~ StatusFailed,
                                  Status %notin% c(StatusBad, StatusGood) ~ StatusProcessing)) %>%
        as.data.frame()
      logMessage("re-rendering process run log table")
      if (!is.null(logdata)) {
        datatable(
          logdata,
          class = "flamingo-table display",
          rownames = TRUE,
          selection = "none",
          escape = FALSE,
          colnames = c('Row Number' = 1),
          filter = 'bottom',
          options = .getPRTableOptions()
        )
      } else {
        .nothingToShowTable(contentMessage = paste0("no log files associated with Process Run ID ", ifelse(!is.null(result$prrunid), result$prrunid, "NULL")))
      }
    }
  })
  
  # run logs title
  output$paneltitleProcessRunLogs <- renderUI({
    processRunId <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunID]
    processRunName <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunName]
    processRunName <- ifelse(processRunName == " ", "", paste0('"', processRunName, '"'))
    paste0('Logs ', processRunName, ' (id: ', processRunId, ')')
  })
  
  # > Updates dependent on changed: tableprocessrundata_rows_selected ---------
  # Allow display output option only if run successful. Otherwise default view is logs
  observeEvent(input$tableprocessrundata_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste("input$tableprocessrundata_rows_selected is changed to:", input$tableprocessrundata_rows_selected))
      hide("panelDefineOutputs")
      hide("panelProcessRunLogs")
      show("abuttondisplayoutput")
      show("abuttonshowlog")
      ##### TODO: Do I need the second check in this if????
      if (length(input$tableprocessrundata_rows_selected) > 0 && !is.null(result$prcrundata)) {
        result$prrunid <- result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunID]
        if (result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunStatus] != StatusCompleted) {
          hide("abuttondisplayoutput")
          hide("abuttonshowlog")
          # This occurs only by changing process run, which is only possible in panel 3
          show("panelProcessRunLogs")
          logMessage("showing prrunlogtable")
        } else {
          show("abuttondisplayoutput")
          show("abuttonshowlog")
        }
      } else {
        result$prrunid <- -1
      }
    }
  }) 
  
  
  # Navigation --------------------------------------------------------------
  # Go to browse section
  onclick("abuttondisplayoutput", {
    if (length(input$tableprocessrundata_rows_selected) > 0) {
      result$navigationstate <-"SBR"
    } else {
      showNotification(type = "warning", "Please select a Process Run first")
    }
  })
  
  # Help Functions -----------------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs called")
    #Section "Configure Output & Run" = "3"
    hide("panelProcessRunTable")
    hide("panelDefineOutputs")
    hide("panelProcessRunLogs")
  }
  
  #show default view for Section "Configure Output & Run" = "3"
  .defaultRun <- function(){
    logMessage(".defaultRun called")
    show("panelDefineIDs")
    show("panelProcessRunTable")
    disable("chkgulpolicy")
  }
  
  # Reload Process Runs table
  .reloadRunData <- function() {
    logMessage(".reloadRunData called")
    if (selectprogOasisID() != "") {
      .getProcessRunWithUserChoices(userId(), 0, 0, 0)
      logMessage("process run table refreshed")
    }  else {
      result$prcrundata <- NULL
    }
    invisible()
  }
  
  # table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      searchHighlight = TRUE,
      processing = 0,
      pageLength = pageLength,
      #width = "100%",
      #autoWidth = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    return(options)
  }
  
  #empty table
  .nothingToShowTable <- function(contentMessage){
    datatable(
      data.frame(content = contentMessage),
      class = "flamingo-table display",
      selection = "none",
      rownames = FALSE,
      #filter = 'bottom',
      colnames = c(""),
      escape = FALSE,
      options = list(searchHighlight = TRUE)
    )
  }
  
  # Clear checkboxgroups GUL
  .clearchkboxGULgrp <- function() {
    logMessage(".clearchkboxGULgrp called")
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }
  
  # Clear checkboxgroup IL
  .clearchkboxILgrp <- function() {
    logMessage(".clearchkboxILgrp called")
    for (i in checkilgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }
  
  # Clear checkboxgroup RI
  .clearchkboxRIgrp <- function() {
    logMessage(".clearchkboxRIgrp called")
    for (i in checkrigrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = "None")
    }
  }
  
  # Clear other runtime params
  .clearotherparams <- function() {
    logMessage(".clearotherparams called")
    updateSelectInput(session, "sinoutputoptions",
                      choices = c(getOutputOptions(dbSettings)),
                      selected = character(0))
    updateTextInput(session, "tinputprocessrunname", value = "")
    updateSliderInput(session, "sliderleakagefac", "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)
    if (length(POData_rowselected()) > 0) {
      prgId <- selectprogOasisID()
      prgId <- ifelse(is.null(prgId), toString(prgId), -1)
    } else {
      prgId <- -1
    }
    if (prgId != -1) {
      updateSelectInput(session, "sinputeventset",
                        choices = getEventSet(dbSettings, prgId ))
      updateSelectInput(session, "sinputeventocc",
                        choices = getEventOccurrence(dbSettings, prgId ))
    }
    updateCheckboxInput(session, "chkinputprwind", "Peril: Wind", value = TRUE)
    updateCheckboxInput(session, "chkinputprstsurge", "Peril: Surge", value = TRUE)
    updateCheckboxInput(session, "chkinputprquake", "Peril: Quake", value = TRUE)
    updateCheckboxInput(session, "chkinputprflood", "Peril: Flood", value = TRUE)
    updateCheckboxInput(session, "chkinputdsurge", "Demand Surge", value = TRUE)
  }
  
  # Clear Custom Configuration option
  .clearOutputOptions <- function() {
    logMessage(".clearOutputOptions called")
    updateSelectInput(session, "sinoutputoptions",
                      choices = c(getOutputOptions(dbSettings)),
                      selected = character(0))
  }
  
  # Update output configuration for rerun
  .updateOutputConfig <- function() {
    logMessage(".updateOutputConfig called")
    outputlist <- executeDbQuery(dbSettings, paste0("exec dbo.getOutputOptionOutputs @processrunid = ", result$prrunid ))
    runparamsforpr <- executeDbQuery(dbSettings, paste0("exec dbo.getProcessRunParams ", result$prrunid ))
    
    updateTextInput(session, "tinputprocessrunname", value = result$prcrundata[input$tableprocessrundata_rows_selected, prcrundata.ProcessRunName])
    
    if (nrow(runparamsforpr) > 0) {
      for (i in 1:nrow(runparamsforpr)) {
        switch(runparamsforpr[i,1],
               "number_of_samples" = {updateTextInput(session, "tinputnoofsample", value = runparamsforpr[i,2])},
               "gul_threshold" = {updateTextInput(session, "tinputthreshold", value = runparamsforpr[i,2])},
               "event_set" = {updateSelectInput(session, "sinputeventocc", selected = runparamsforpr[i,2])},
               "event_occurrence_id" = {updateSelectInput(session, "sinputeventocc", selected = runparamsforpr[i,2])},
               "peril_wind" = {updateCheckboxInput(session, "chkinputprwind", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
               "peril_surge" = {updateCheckboxInput(session, "chkinputprstsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
               "peril_quake" = {updateCheckboxInput(session, "chkinputprquake", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
               "peril_flood" = {updateCheckboxInput(session, "chkinputprflood", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
               "demand_surge" = {updateCheckboxInput(session, "chkinputdsurge", value = eval(parse(text = toString(runparamsforpr[i,2]))))},
               "leakage_factor" = {updateSliderInput(session, "sliderleakagefac", value = runparamsforpr[i,2])}
        )
      }
    }
    orows <- nrow(outputlist)
    if (orows > 0) {
      for (i in 1:orows) {
        grpid <- paste0("chk",outputlist$Group[i])
        grpinputid <- strsplit(toString(grpid), " ")[[1]]
        chkboxid <- outputlist$Parameter[i]
        selchoices <- as.list(strsplit(toString(chkboxid), ",")[[1]])
        updateCheckboxGroupInput(session, inputId = grpinputid, selected = c(selchoices))
      }
    }
    .clearOutputOptions()
    invisible()
  }
  
  # Output view
  .advancedview <- function() {
    logMessage(".advancedview called")
    show("configureAdvancedGUL")
    show("configureAdvancedIL")
    show("configureAdvancedRI")
    show("configureModelParamsAdvanced")
    show("abtnbasic")
    hide("abtnadvanced")
    show("abuttonsaveoutput")
    show("abtnclroutopt")
  }
  
  .basicview <- function() {
    logMessage(".basicview called")
    hide("configureAdvancedGUL")
    hide("configureAdvancedIL")
    hide("configureAdvancedRI")
    hide("configureModelParamsAdvanced")
    hide("abtnbasic")
    show("abtnadvanced")
    hide("abuttonsaveoutput")
    hide("abtnclroutopt")
  }
  
  # Default output configuration options
  .defaultchkboxGULgrp <- function(session) {
    logMessage(".defaultchkboxGULgrp called")
    for (i in checkgulgrplist) {
      updateCheckboxGroupInput(session, inputId = i, selected = defaultSelectChoicesGUL)
    }
  }
  
  .defaultview <- function(session) {
    logMessage(".defaultview called")
    updateCheckboxInput(session, "chkinputGUL", value = TRUE)
    .defaultchkboxGULgrp(session)
    updateCheckboxInput(session, "chkinputIL", value = FALSE)
    .clearchkboxILgrp()
    updateCheckboxInput(session, "chkinputRI", value = FALSE)
    .clearchkboxRIgrp()
    .clearotherparams()
    .clearOutputOptions()
    .basicview()
  }
  
  # Model Outout ------------------------------------------------------------
  moduleOutput <- c(
    list(
      navigationstate = reactive(result$navigationstate),
      prrunid = reactive({result$prrunid})
    )
  )
  
  moduleOutput
  
}
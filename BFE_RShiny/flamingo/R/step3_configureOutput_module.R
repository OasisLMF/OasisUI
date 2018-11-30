# step3_configureOutput Module -------------------------------------------------

# UI ---------------------------------------------------------------------------
#' step3_configureOutputUI
#'
#' @rdname step3_configureOutput
#'
#' @description UI/View for the step3_configureOutput.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
step3_configureOutputUI <- function(id) {

  ns <- NS(id)

  tagList(
    hidden(div(id = ns("panelAnalysisTable"), panelAnalysisTable(id))),
    hidden(div(id = ns("panelDefineOutputs"), panelDefineOutputs(id))),
    hidden(div(id = ns("panelAnalysisLogs"), panelAnalysisLogs(id)))
  )
}

#' panelAnalysisTable
#'
#' @rdname panelAnalysisTable
#'
#' @description Function wrapping panel to show analysestable.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelAnalysisTable <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_analysis"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_AnalysisTable"), inline = TRUE),
      actionButton(inputId = ns("abuttonanarefresh"), label = "Refresh", style = "float: right;")
    ),
    div(id = "divAnalysis",
        fluidRow(column(12,
                        radioButtons(inputId = ns("radioanaAllOrInProgress"), "Processes' Status", list("All", "In Progress"), inline = TRUE))),
        DTOutput(ns("dt_analyses")),
        fluidRow(column(12,
                        div(id = ns("divAnalysisButtons"),
                            flamingoButton(inputId = ns("abuttonconfigoutput"), label = "New Output Configuration") %>%
                              bs_embed_tooltip(title = defineSingleAna$abuttonconfigoutput, placement = "right"),
                            flamingoButton(inputId = ns("abuttoncancelana"), label = "Cancel Analysis Run") %>%
                              bs_embed_tooltip(title = defineSingleAna$abuttoncancelana, placement = "right"),
                            flamingoButton(inputId = ns("abuttonrerunana"), label = "Rerun") %>%
                              bs_embed_tooltip(title = defineSingleAna$abuttonrerunana, placement = "right"),
                            flamingoButton(inputId = ns("abuttonshowlog"), label = "Show Log") %>%
                              bs_embed_tooltip(title = defineSingleAna$abuttonshowlog, placement = "right"),
                            div(
                              actionButton(inputId = ns("abuttondisplayoutput"), label = "Proceed to Dashboard") %>%
                                bs_embed_tooltip(title = defineSingleAna$abuttondisplayoutput, placement = "right"),
                              style = "inline: true;float: right;")
                        )))
    )
  )
}

#' panelAnalysisLogs
#'
#' @rdname panelAnalysisLogs
#'
#' @description Function wrapping panel to show log table for specific Analysis.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
panelAnalysisLogs <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_analogs"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_AnaLogs"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelog"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_analysesrunlog"))
  )
}

#' panelDefineOutputs
#'
#' @rdname panelDefineOutputs
#'
#' @description Function wrapping panel to define outputs
#'
#' @template params-module-ui
#'
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelDefineOutputs <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_anaoutput"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_defAnaConfigOutput"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidepanelconfigureoutput"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
      column(4,
             panelDefineOutputsDetails(id)),
      column(8,
             panelDefOutputConfiguration(id))
    ),
    fluidRow(
      column(12,
             flamingoButton(inputId = ns("abuttonexecuteanarun"), label = "Execute Run"), align = "right")) %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonexecuteanarun, placement = "right")
  )
}

#' panelDefineOutputsDetails
#'
#' @rdname panelDefineOutputsDetails
#'
#' @description Function wrapping sub-panel to define outputs details.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelDefineOutputsDetails <- function(id) {
  ns <- NS(id)
  tagList(
    flamingoPanel(
      collapsible = FALSE,
      ns("panel_ConfigDetails"),
      heading = h4("Configuration Details"),
      selectInput(ns("sinoutputoptions"), "Select Custom Configuration:", choices = "")
    ),
    flamingoPanel(
      collapsible = FALSE,
      ns("panel_defAnaOutputDetails"),
      heading = h4("Model Parameters"),
      div(id = ns("noofsample"), style = "width:100%; margin: 0 auto;", textInput(ns("tinputnoofsample"), label = "Number of Samples:", value = "10")),
      hidden(div(id = ns("configureAnaParamsAdvanced"), align = "left",
                 textInput(ns("tinputthreshold"), label = "Loss Threshold:", value = "0"),
                 selectInput(ns("sinputeventset"), label = "Event Set:", choices = "Probabilistic"),
                 selectInput(ns("sinputeventocc"), label = "Event Occurrence Set:", choices = "Long Term"),
                 checkboxInput(ns("chkinputsummaryoption"), "Summary Reports", value = TRUE),
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
                            sliderInput(ns("sliderleakagefac"), label = "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)))))
    )
  )
}

#' panelDefOutputConfiguration
#'
#' @rdname panelDefOutputConfiguration
#'
#' @description Function wrapping sub-panel to define outputs configuration.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelDefOutputConfiguration <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_outconfig"),
    heading = h4("Output Configuration"),
    checkboxInput(ns("chkinputGUL"), label = "Ground Up Loss", value = TRUE),
    hidden(div(id = ns("panel_configureAdvancedGUL"), panel_configureAdvancedGUL(id))),
    checkboxInput(ns("chkinputIL"), label = "Insured Loss", value = FALSE),
    hidden(div(id = ns("panel_configureAdvancedIL"), panel_configureAdvancedIL(id))),
    checkboxInput(ns("chkinputRI"), label = "Net RI Loss", value = FALSE),
    hidden(div(id = ns("panel_configureAdvancedRI"), panel_configureAdvancedRI(id))),
    flamingoButton(inputId = ns("abuttonadvanced"), label = "Advanced"),
    hidden(flamingoButton(inputId = ns("abuttonbasic"), label = "Basic")),
    # hidden(flamingoButton(inputId = ns("abuttonsaveoutput"), label = "Save Configuration")) %>%
    #   bs_embed_tooltip(title = defineSingleAna$abuttonsaveoutput, placement = "right"),
    hidden(flamingoButton(inputId = ns("abuttonclroutopt"), label = "Default"))
  )
}

#' panel_configureAdvancedGUL
#'
#' @rdname panel_configureAdvancedGUL
#'
#' @description Function wrapping sub-panel to define outputs advanced configuration GUL.
#'
#' @template params-module-ui
#'
#' @export
panel_configureAdvancedGUL <- function(id) {
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

#' panel_configureAdvancedIL
#'
#' @rdname panel_configureAdvancedIL
#'
#' @description Function wrapping sub-panel to define outputs advanced configuration IL.
#'
#' @template params-module-ui
#'
#' @export
panel_configureAdvancedIL <- function(id) {
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

#' panel_configureAdvancedRI
#'
#' @rdname panel_configureAdvancedRI
#'
#' @description Function wrapping sub-panel to define outputs advanced configuration RI.
#'
#' @template params-module-ui
#'
#' @export
panel_configureAdvancedRI <- function(id) {
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

# Server -----------------------------------------------------------------------

#' step3_configureOutput server
#'
#' @rdname step3_configureOutput
#'
#' @description Server logic to step3_configureOutput.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param currstep current selected step.
#' @param portfolioID selected portfolio ID.
#' @param analysisID selected analysis ID
#'
#' @return anaID id of selected run.
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT selectPage
#' @importFrom shinyjs onclick
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom dplyr filter
#' @importFrom jsonlite write_json
#' @importFrom jsonlite read_json
#'
#' @export
step3_configureOutput <- function(input, output, session,
                                  dbSettings,apiSettings,
                                  active = reactive(TRUE),
                                  logMessage = message,
                                  currstep = reactive(-1),
                                  portfolioID = reactive(""),
                                  analysisID = reactive("")
) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  # Default checkgroup for  GUL, IL and RI
  checkgulgrplist <- c("chkgulprog", "chkgulstate", "chkgulcounty", "chkgulloc", "chkgullob")
  checkilgrplist <- c("chkilprog", "chkilstate", "chkilcounty", "chkilloc", "chkillob", "chkilpolicy")
  checkrigrplist <- c("chkriprog", "chkristate", "chkricounty", "chkriloc", "chkrilob", "chkripolicy")

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # reactve value for navigation
    navigationstate = NULL,
    # reactive value for Analysis table
    tbl_analysesData = NULL,
    # analysis run logs table
    tbl_analysisrunlog = NULL,
    # flag to know if the user is creating a new output configuration or rerunning an analysis
    ana_flag = "C",
    # Id of the Analysis
    anaID = -1,
    # analysis_ setting
    analysis_settings = NULL
  )

  # Reset Param
  observe(if (active()) {
    result$navigationstate <- NULL
    result$anaID <- analysisID()
  })

  # Panels Visualization -------------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 3 ) {
      .defaultstep3()
      .reloadAnaData()
    }
  })

  # If portfolioID changes, reload analysis table and set view back to default
  observeEvent(portfolioID(), ignoreInit = TRUE, {
    if (active()) {
      .hideDivs()
      show("panelAnalysisTable")
      .reloadAnaData()
    }
  })

  # Enable and disable buttons -------------------------------------------------

  #Enabling based on analysis
  observeEvent({
    result$tbl_analysesData
    portfolioID()
    currstep()
    input$dt_analyses_rows_selected}, ignoreNULL = FALSE, ignoreInit = TRUE, {
      disable("abuttonrerunana")
      disable("abuttondisplayoutput")
      disable("abuttonshowlog")
      disable("abuttonconfigoutput")
      disable("abuttoncancelana")
      if (portfolioID() != "") {
        if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 && length(input$dt_analyses_rows_selected) > 0) {
          enable("abuttonrerunana")
          enable("abuttonshowlog")
          enable("abuttonconfigoutput")
          enable("abuttoncancelana")
          if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaStatus] == StatusCompleted) {
            enable("abuttondisplayoutput")
          }
        }
      }
    }
  )


  # Enable and disable buttons based on output confifig
  observeEvent(outputOptionsList(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (outputOptionsList() != "") {
      # enable("abuttonsaveoutput")
      enable("abuttonexecuteanarun")
    } else {
      # disable("abuttonsaveoutput")
      disable("abuttonexecuteanarun")
    }
  })

  # Analyses  Table ------------------------------------------------------------
  # reload if radio buttons for 'All' vs 'In_Progress' change
  observeEvent(input$radioanaAllOrInProgress, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste0("filter changed to ", input$radioanaAllOrInProgress))
      .reloadAnaData()
    }
  })

  output$dt_analyses <- renderDT(

    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0) {
      index <- 1
      logMessage("re-rendering analysis table")
      datatable(
        result$tbl_analysesData,
        class = "flamingo-table display",
        rownames = TRUE,
        selection = list(mode = 'single',
                         selected = rownames(result$tbl_analysesData)[c(as.integer(index))]),
        escape = FALSE,
        colnames = c('row number' = 1),
        filter = 'bottom',
        options = .getPRTableOptions()
      )
    } else {
      .nothingToShowTable(contentMessage = paste0("no analysis available"))
    })

  # Analyses Table Title
  output$paneltitle_AnalysisTable <- renderUI({
    if (portfolioID() != "") {
      paste0('Analyses associated with portfolio id ', portfolioID())
    } else {
      paste0("Analyses")
    }

  })

  # Delete analysis button -----------------------------------------------------
  onclick("abuttoncancelana", {
    showModal(.cancelAnaModal())
  })

  output$cancelAnaModaltitle <- renderUI({
    AnaId <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
    AnaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaName]
    paste0('Cancel ', AnaId, ' ', AnaName)
  })

  .cancelAnaModal <- function(){
    ns <- session$ns
    modalDialog(label = "cancelAnaModal",
                title = uiOutput(ns("cancelAnaModaltitle"), inline = TRUE),
                paste0("Are you sure you want to cancel this analysis?"),
                footer = tagList(
                  flamingoButton(ns("abuttonConfirmDelAna"),
                                 label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = defineSingleAna$abuttonConfirmDel, placement = "right"),
                  actionButton(ns("btnCancelAnaDel"),
                               label = "Go back", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  observeEvent(input$btnCancelAnaDel, {
    removeModal()
  })

  observeEvent(input$abuttonConfirmDelAna, {
    removeModal()

    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
    #should use /v1/analyses/{id}/cancel/
    delete_analyses_id <- api_post_analyses_cancel(analysisID)

    if (delete_analyses_id$status == "Success") {
      flamingoNotification(type = "message",
                           paste0("Analysis id ", analysisID, " cancelled."))
      .reloadAnaData()
      idxSel <- match(analysisID, result$tbl_analysesData[, tbl_analysesData.AnaID])
      pageSel <- ceiling(idxSel/pageLength)
      selectRows(dataTableProxy("dt_analyses"), idxSel)
      selectPage(dataTableProxy("dt_analyses"), pageSel)
    } else {
      flamingoNotification(type = "error",
                           paste0("Error in cancelling analysis ", result$anaID, ". Analysis is not running."))
    }

  })

  # Configure Output -----------------------------------------------------------
  # hide panel
  onclick("abuttonhidepanelconfigureoutput", {
    hide("panelDefineOutputs")
  })

  # configuration title
  output$paneltitle_defAnaConfigOutput <- renderUI({
    if (result$ana_flag  == "R") {
      analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
      analysisName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaName]
      analysisName <- ifelse(analysisName == " ", "", paste0('"', analysisName, '"'))
      paste0('Re-Define Output Configuration for Analysis id ', analysisID, ' ', analysisName)
    } else {
      "New Output Configuration"
    }
  })

  #Show Output Configuration Panel
  onclick("abuttonconfigoutput", {
    .defaultview(session)
    hide("panelAnalysisLogs")
    show("panelDefineOutputs")
    .showPerils()
    logMessage("showing panelDefineOutputs")
    result$ana_flag <- "C"
  })

  # Hide Output Configuration panel
  onclick("abuttonehidepanelconfigureoutput", {
    hide("panelDefineOutputs")
    result$ana_flag <- "C"
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
  # Note: the ignoreInit = TRUE does not prevent the trigger once logged in
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
  #To-Do update output configuration based on analysis setting
  observeEvent(input$sinoutputoptions, {
    # if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "") {
    #   #read the right analysis settings file
    #   analysis_settings <- read_json(paste0("./analysis_settings/",input$sinoutputoptions,".json"),  simplifyVector = TRUE)
    #   #Set inputs
    #   .updateOutputConfig(analysis_settings)
    # }
    # Using analyses names to select the output configuration of a previously posted analyses
    if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "") {
      anaName <- strsplit(input$sinoutputoptions, split = " / ")[[1]][2]
      anaID <- strsplit(input$sinoutputoptions, split = " / ")[[1]][1]
      analysis_settings <-  return_analyses_settings_file_list(anaID)
      if (!is.null(analysis_settings$detail) && analysis_settings$detail == "Not found.") {
        flamingoNotification(type = "error", paste0("No output configuration associated to analysis ", anaName," id ", anaID))
      } else {
        logMessage(paste0("appling the output configuration of analysis ", anaName," id ", anaID))
        #Set inputs
        .updateOutputConfig(analysis_settings) 
      }
    }
  })

  # Clear the checkbox groups and preset dropdown - Set back to default
  onclick("abuttonclroutopt", {
    .defaultview(session)
  })

  # show advanced view
  onclick("abuttonadvanced", {
    .advancedview()
  })

  # show basic view
  onclick("abuttonbasic", {
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

  onclick("abuttonrerunana", {
    .defaultview(session)
    hide("panelAnalysisLogs")
    show("panelDefineOutputs")
    .showPerils()
    logMessage("showing panelDefineOutputs")
    result$ana_flag <- "R"
    analysis_settings <- return_analyses_settings_file_list(result$anaID)
    analysisName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaName]
    if (!is.null(analysis_settings$detail) && analysis_settings$detail == "Not found.") {
      flamingoNotification(type = "error", paste0("No output configuration associated to analysis ", analysisName," id ", result$anaID))
    } else {
      logMessage(paste0("appling the output configuration of analysis ",analysisName," id ", result$anaID))
      #Set inputs
      .updateOutputConfig(analysis_settings) 
    }
  })
  
  # # Save output configuration --------------------------------------------------
  # 
  # # Save output for later use as presets
  # .modalsaveoutput <- function() {
  #   ns <- session$ns
  #   modalDialog(label = "modalsaveoutput",
  #               title = "Save Configuration",
  #               textInput(ns("tinputoutputname"), label = "Configuration Name:", value = ""),
  #               footer = tagList(
  #                 flamingoButton(inputId = ns("abuttonsubmitoutput"),
  #                                label = "Submit")
  #               ),
  #               size = "s",
  #               easyClose = TRUE
  #   )
  # }
  # 
  # onclick("abuttonsaveoutput", {
  #   showModal(.modalsaveoutput())
  # })
  # 
  # # Submit output configuration (to be saved)
  # onclick("abuttonsubmitoutput", {
  #   if (input$tinputoutputname == "") {
  #     flamingoNotification(type = "warning", "Please enter Output Configuration Name")
  #   } else {
  #     dir.create("./analysis_settings")
  #     #write out file to be uploades
  #     analysis_settingsList <- .gen_analysis_settings()
  #     write_json(analysis_settingsList, paste0("./analysis_settings/",input$tinputoutputname,".json"), pretty = TRUE, auto_unbox = TRUE)
  #     flamingoNotification(type = "message", paste0("Output Configuration ", input$tinputoutputname ," saved"))
  #     updateTextInput(session, "tinputoutputname", value = "")
  #     removeModal()
  #     .clearOutputOptions()
  #   }
  # })


  # Run Analyses ---------------------------------------------------------------

  # Execute analysis
  onclick("abuttonexecuteanarun", {
    analysis_settingsList <- .gen_analysis_settings()

    #write out file to be uploades
    write_json(analysis_settingsList, "./analysis_settings.json", pretty = TRUE, auto_unbox = TRUE)

    #post analysis settings
    post_analysis_settings_file <- api_post_analyses_settings_file(result$anaID, "./analysis_settings.json")

    if (post_analysis_settings_file$status == "Success") {
      flamingoNotification(type = "message",
                           paste0("Analysis  settings posted to ", result$anaID ,"."))
    } else {
      flamingoNotification(type = "error",
                           paste0("Analysis settings not posted to ", result$anaID ,"; error ", post_analysis_settings_file$status))
    }

    analyses_run <- return_analyses_run_df(result$anaID)

    if (nrow(analyses_run) > 1) {
      if (analyses_run[[tbl_analysesData.AnaStatus]] == "RUN_STARTED") {
        flamingoNotification(type = "message",
                             paste0("Analysis ", result$anaID ," is executing"))
      } else {
        flamingoNotification(type = "error",
                             paste0("Error in executing analysis ", result$anaID, " status: ", analyses_run[[tbl_analysesData.AnaStatus]] ))
      }
    } else {
      flamingoNotification(type = "error",
                           paste0("Error in executing analysis ", result$anaID, " status: ", analyses_run$detail ))
    }
    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
    idxSel <- match(analysisID, result$tbl_analysesData[, tbl_analysesData.AnaID])
    pageSel <- ceiling(idxSel/pageLength)
    .reloadAnaData()
    hide("panelDefineOutputs")
    .defaultview(session)
    selectRows(dataTableProxy("dt_analyses"), idxSel)
    selectPage(dataTableProxy("dt_analyses"), pageSel)

  })

  # Logs -----------------------------------------------------------------------
  onclick("abuttonshowlog", {
    hide("panelDefineOutputs")
    show("panelAnalysisLogs")
    logMessage("showing analysis run log table")
    .reloadAnaRunLog()
  })

  onclick("abuttonhidelog", {
    hide("panelAnalysisLogs")
  })

  ### Log Table
  output$dt_analysesrunlog <- renderDT({
    if (length(input$dt_analyses_rows_selected) > 0) {
      logMessage("re-rendering analysis log table")
      if (!is.null(result$tbl_analysisrunlog)) {
        datatable(
          result$tbl_analysisrunlog,
          class = "flamingo-table display",
          rownames = TRUE,
          selection = "none",
          escape = FALSE,
          colnames = c('row number' = 1),
          filter = 'bottom',
          options = .getPRTableOptions()
        )
      } else {
        .nothingToShowTable(contentMessage = paste0("no log files associated with analysis ID ", ifelse(!is.null(result$anaID), result$anaID, "NULL")))
      }
    }
  })

  # run logs title
  output$paneltitle_AnaLogs <- renderUI({
    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
    analysisName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaName]
    analysisName <- ifelse(analysisName == " ", "", paste0('"', analysisName, '"'))
    paste0('Run Logs for Analysis id ', analysisID, ' ', analysisName)
  })

  # Refresh Buttons ------------------------------------------------------------
  onclick("abuttonanarefresh", {
    .reloadAnaData()
  } )

  onclick("abuttonanarefreshlogs", {
    .reloadAnaRunLog()
  })

  # Updates dependent on changed: dt_analyses_rows_selected --------------------
  # Allow display output option only if run successful. Otherwise default view is logs
  observeEvent({
    input$dt_analyses_rows_selected
    portfolioID()
    }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste("input$dt_analyses_rows_selected is changed to:", input$dt_analyses_rows_selected))
      hide("panelDefineOutputs")
      hide("panelAnalysisLogs")
      if (length(input$dt_analyses_rows_selected) > 0 && !is.null(result$tbl_analysesData)) {
        result$anaID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
        if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaStatus] == StatusFailed) {
          show("panelAnalysisLogs")
          logMessage("showing analysis run log table")
        }
      } else {
        result$anaID <- -1
      }
    }
  })

  # Navigation -----------------------------------------------------------------
  # Go to browse section
  onclick("abuttondisplayoutput", {
    result$navigationstate <- "SBR"
  })

  # Help Functions -------------------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs called")
    #Section "Configure Output & Run" = "3"
    hide("panelAnalysisTable")
    hide("panelDefineOutputs")
    hide("panelAnalysisLogs")
  }

  #show default view for Section "Configure Output & Run" = "3"
  .defaultstep3 <- function(){
    logMessage(".defaultstep3 called")
    show("panelAnalysisTable")
    disable("chkgulpolicy")
    disable("abuttonrerunana")
    disable("abuttondisplayoutput")
    disable("abuttonshowlog")
    disable("abuttonconfigoutput")
    disable("abuttoncancelana")
  }

  # Reload Analyses table
  .reloadAnaData <- function() {
    logMessage(".reloadAnaData called")
    if (portfolioID()  != "") {
      tbl_analysesData  <- return_tbl_analysesData()
      if (!is.null(tbl_analysesData)  && nrow(tbl_analysesData) > 0) {
        result$tbl_analysesData <- tbl_analysesData %>% filter(!! sym(tbl_analysesData.PortfolioID) == portfolioID())
        #Handling filter for 'In Progress'
        if (input$radioanaAllOrInProgress == "In_Progress") {
          result$tbl_analysesData <- result$tbl_analysesData %>% filter(status == StatusProcessing)
        }
      }
      logMessage("analyses table refreshed")
    }  else {
      result$tbl_analysesData <- NULL
    }
    invisible()
  }

  # Reload Analysis Run Log table
  .reloadAnaRunLog <- function() {
    logMessage(".reloadAnaRunLog called")
    if (!is.null(result$anaID) && result$anaID != "") {
      result$tbl_analysisrunlog <- return_analyses_run_traceback_file_df(result$anaID)
    } else {
      result$tbl_analysisrunlog <-  NULL
    }
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
    .clearOutputOptions()
    updateTextInput(session, "tinputananame", value = "")
    updateSliderInput(session, "sliderleakagefac", "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)

    modelID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.ModelID]
    modelID <- ifelse(modelID == "", -1,modelID)
    # if (modelID != -1) {
    #   updateSelectInput(session, "sinputeventset",
    #                     choices = getEventSet(dbSettings, modelID ))
    #   updateSelectInput(session, "sinputeventocc",
    #                     choices = getEventOccurrence(dbSettings, modelID ))
    # }
    # updateCheckboxInput(session, "chkinputprwind", "Peril: Wind", value = TRUE)
    # updateCheckboxInput(session, "chkinputprstsurge", "Peril: Surge", value = TRUE)
    # updateCheckboxInput(session, "chkinputprquake", "Peril: Quake", value = TRUE)
    # updateCheckboxInput(session, "chkinputprflood", "Peril: Flood", value = TRUE)
    # updateCheckboxInput(session, "chkinputdsurge", "Demand Surge", value = TRUE)
  }

  # Clear Custom Configuration option
  .clearOutputOptions <- function() {
    logMessage(".clearOutputOptions called")
    # updateSelectInput(session, "sinoutputoptions",
    #                   choices = gsub(".json", "", list.files("./analysis_settings")),
    #                   selected = character(0))
    tbl_analysesData  <- return_tbl_analysesData()
    tbl_analysesData <- tbl_analysesData %>% filter(status != StatusProcessing & status != StatusReady)
    namesList <- tbl_analysesData[,tbl_analysesData.AnaName]
    idList <- tbl_analysesData[,tbl_analysesData.AnaID]
    choicesList <- paste(idList, namesList, sep = " / ")
    updateSelectInput(session, "sinoutputoptions",
                      choices = choicesList,
                      selected = character(0))
  }

  #Show available perils
  # To-Do: retrieve perils from model. currently showing all
  .showPerils <- function() {
    # modelID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.ModelID]
    # modelID <- ifelse(modelID == "", -1,modelID)
    #
    # stmt <- buildDbQuery("getRuntimeParamList", modelID)
    # runparamlist <- executeDbQuery(dbSettings, stmt)
    #
    # hide("perilwind")
    # hide("perilsurge")
    # hide("perilquake")
    # hide("perilflood")
    # hide("demandsurge")
    # hide("leakagefactor")
    #
    # if (nrow(runparamlist) > 0) {
    #   for (i in 1:nrow(runparamlist)) {
    #     ctrname <- gsub("_", "", runparamlist[i, 1], fixed = TRUE)
    #     show(ctrname)
    #   }
    # }
  }

  #Generate Analysis settings file
  .gen_analysis_settings <- function(){
    # Assign analysis settings to analysis
    #model data
    modelID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.ModelID]
    modelData <- return_tbl_modelData(modelID)
    
    #Reassigned variables for consistency
    model_version_id <- modelData[[tbl_modelsData.ModelNameId]]
    source_tag <- tolower(model_version_id)
    module_supplier_id <- modelData[[tbl_modelsData.ModelSupplierId]]
    number_of_samples <- as.integer(input$tinputnoofsample)
    prog_id <- as.integer(result$portfolioID)
    event_occurrence_file_id <- 1 # getEventOccurrence(dbSettings, prgId )  as.integer(input$sinputeventocc)
    event_set <- input$sinputeventset
    peril_wind <- input$chkinputprwind
    demand_surge <- input$chkinputdsurge
    peril_quake <- input$chkinputprquake
    peril_flood <- input$chkinputprflood
    peril_surge <- input$chkinputprstsurge
    leakage_factor <- input$sliderleakagefac
    gul_output <- input$chkinputGUL
    il_output <- input$chkinputIL
    ri_output <- input$chkinputRI
    gul_threshold <- as.integer(input$tinputthreshold)
    analysis_tag <- as.integer(result$anaID)
    uniqueItems <- FALSE
    id <- 1
    exposure_location <- "L:"
    use_random_number_file <- FALSE
    return_period_file <- TRUE
    chkinputsummaryoption <- input$chkinputsummaryoption
    outputsGUL <- c(input$chkgulprog, input$chkgulpolicy, input$chkgulstate, input$chkgulcounty, input$chkgulloc, input$chkgullob)
    outputsIL <- c(input$chkilprog, input$chkilpolicy, input$chkilstate, input$chkilcounty, input$chkilloc, input$chkillob)
    outputsRI <- c(input$chkriprog, input$chkripolicy, input$chkristate, input$chkricounty, input$chkriloc, input$chkrilob)
    
    #generate analysis_settings_file
    analysis_settingsList <- construct_tbl_analyses_settings(source_tag, prog_id, number_of_samples,
                                                             module_supplier_id, model_version_id,
                                                             event_occurrence_file_id,use_random_number_file = FALSE, event_set,
                                                             peril_wind, demand_surge, peril_quake, peril_flood, peril_surge, leakage_factor,
                                                             gul_threshold,exposure_location = 'L',
                                                             outputsGUL, outputsIL, outputsRI, chkinputsummaryoption,
                                                             gul_output,  il_output, ri_output,
                                                             return_period_file,
                                                             analysis_tag, uniqueItems = FALSE, id = 1)
    return(analysis_settingsList)
  }
  
  # Update output configuration for rerun
  .updateOutputConfig <- function(analysis_settings) {
    logMessage(".updateOutputConfig called")
    
    #clear checkboxes
    .clearchkboxGULgrp()
    .clearchkboxILgrp()
    .clearchkboxRIgrp()

    settings <- analysis_settings[["analysis_settings"]]
    model_settings <- settings[["model_settings"]]

    SettingsMapping <- list(
      "tinputnoofsample"  = list(
        "UpdateWidget" = "updateTextInput",
        "SettingPath" = "settings",
        "SettingElement" = "number_of_samples"
      ),
      "tinputthreshold"  = list(
        "UpdateWidget" = "updateTextInput",
        "SettingPath" = "settings",
        "SettingElement" = "gul_threshold"
      ),
      "sinputeventset" = list(
        "UpdateWidget" = "updateSelectInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "event_set"
      ),
      "sinputeventocc" = list(
        "UpdateWidget" = "updateSelectInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "event_occurrence_file_id"
      ),
      "chkinputprwind" = list(
        "UpdateWidget" = "updateCheckboxInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "peril_wind"
      ),
      "chkinputprstsurge" = list(
        "UpdateWidget" = "updateCheckboxInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "peril_surge"
      ),
      "chkinputprquake" = list(
        "UpdateWidget" = "updateCheckboxInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "peril_quake"
      ),
      "chkinputprflood" = list(
        "UpdateWidget" = "updateCheckboxInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "peril_flood"
      ),
      "chkinputdsurge" = list(
        "UpdateWidget" = "updateCheckboxInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "demand_surge"
      ),
      "sliderleakagefac" = list(
        "UpdateWidget" = "updateCheckboxInput",
        "SettingPath" = "model_settings",
        "SettingElement" = "leakage_factor"
      )
    )
    
    .updateWidget <- function(inp) {
      curr_setting <- SettingsMapping[[inp]]
      if(curr_setting$UpdateWidget == "updateSelectInput") {
        get(curr_setting$UpdateWidget)(session = session, inputId = inp, selected = get(curr_setting$SettingPath)[[curr_setting$SettingElement]])
      } else {
        get(curr_setting$UpdateWidget)(session = session, inputId = inp, value = get(curr_setting$SettingPath)[[curr_setting$SettingElement]])
      }
    }
    
    
    lapply(names(SettingsMapping), function(i){
      .updateWidget(i)
    } )

    
    #To-do retrieve checkboxes selection from analysis_settings and update inputs accordingly 
    varslist <- list('uniqueItems' = 'Summary',
                     'eltcalc' = 'ELT',
                     'full_uncertainty_aep' = 'FullUncAEP',
                     'full_uncertainty_oep' = 'FullUncOEP',
                     'wheatsheaf_aep' = 'AEPWheatsheaf',
                     'wheatsheaf_oep' = 'OEPWheatsheaf',
                     'wheatsheaf_mean_aep' = 'MeanAEPWheatsheaf',
                     'wheatsheaf_mean_oep' = 'MeanOEPWheatsheaf',
                     'sample_mean_aep' = 'SampleMeanAEP',
                     'sample_mean_oep' = 'SampleMeanOEP',
                     'aalcalc' = 'AAL',
                     'pltcalc' = 'PLT')
    gran <- c('prog', 'policy', 'state', 'county', 'loc', 'lob')
    output_perspectives <- names(analysis_settings[[1]])[grepl("_summaries", names(analysis_settings[[1]]))]
    for (op in output_perspectives) {
      perspective <- gsub("_summaries", "", op)
      nidx <- length(analysis_settings[[1]][[op]])
      for (i in 1:nidx) {
        curr_gran_list <- analysis_settings[[1]][[op]][[i]]
        # index of granularity
        g <- curr_gran_list[["id"]]
        #update summary input
        updateCheckboxInput(session = session, inputId = "chkinputsummaryoption", value = curr_gran_list[["summarycalc"]])
        #checkbox group input name
        chk_persp_gran <- paste0("chk", perspective, gran[g])
        #variables for given granularity
        vars <- names(curr_gran_list)
        leccalcvars <- names(curr_gran_list[["leccalc"]][["outputs"]])
        vars <- vars[which(vars != "id" & vars != "summarycalc" & vars != "lec_output" & vars != "leccalc")]
        vars <- c(vars, leccalcvars)
        #create vector of choices
        choices <- c()
        for (v in vars) {
          choice <- paste0( perspective, gran[g], varslist[[v]])
          choices <- c(choices, choice)
        }
        updateCheckboxGroupInput(session = session, inputId = chk_persp_gran, selected = choices)
      }
    }
    invisible()
  }

  # Output view
  .advancedview <- function() {
    logMessage(".advancedview called")
    show("panel_configureAdvancedGUL")
    show("panel_configureAdvancedIL")
    show("panel_configureAdvancedRI")
    show("configureAnaParamsAdvanced")
    show("abuttonbasic")
    hide("abuttonadvanced")
    # show("abuttonsaveoutput")
    show("abuttonclroutopt")
  }

  .basicview <- function() {
    logMessage(".basicview called")
    hide("panel_configureAdvancedGUL")
    hide("panel_configureAdvancedIL")
    hide("panel_configureAdvancedRI")
    hide("configureAnaParamsAdvanced")
    hide("abuttonbasic")
    show("abuttonadvanced")
    # hide("abuttonsaveoutput")
    hide("abuttonclroutopt")
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

  # Model Outout ---------------------------------------------------------------
  moduleOutput <- c(
    list(
      navigationstate = reactive(result$navigationstate),
      anaID = reactive({result$anaID})
    )
  )

  moduleOutput

}

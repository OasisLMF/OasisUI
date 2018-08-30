
#' @rdname processRunPage
#' @description UI/View for the process run page
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom DT dataTableOutput
#' @importFrom plotly plotlyOutput
#' @export
processRunPageUI <- function(id) {
  
  ns <- NS(id)
  
  attachDependencies(value = flamingoHtmlDependencies(), tagList(
    
    h3("Process Run", class = "flamingo-page-title"),
    
    tagAppendAttributes(class = "modal-footless",
                        bsModal(ns("bsmodalrunparam"), title = "Select Runtime Parameters",
                                trigger = "", size = "large",
                                processRunParamUI(id)
                        )),
    
    bsModal(ns("bsmodalsaveoutput"), "Save Configuration", trigger = "", size = "small",
            textInput(ns("tinputoutputname"), label = "Configuration Name:", value = ""),
            actionButton(ns("abuttonsubmitoutput"), "Submit", class = "btn btn-primary")
    ),
    
    fluidRow(
      column(12,
             div(class = "flamingo-page-division",
                 h4("Prog Oasis", class = "flamingo-table-title"),
                 
                 dataTableOutput(ns("tableprocessdata2")),
                 actionButton(ns("abuttonrunpr"), "Run Process",
                              class = "btn btn-primary")
             )
      ),
      
      column(12,
             hidden(
               div(id = ns("prruntable"), class = "flamingo-page-division",
                   fluidRow(
                     column(6,
                            h4("Process Runs", class = "flamingo-table-title")),
                     
                     column(5,
                            radioButtons(ns("radioprrunsAllOrInProgress"),
                                         "Processes' Status", list("All", "In_Progress"),
                                         inline = TRUE)),
                     
                     column(1,
                            actionButton(ns("abuttonrefreshprrun"), "Refresh",
                                         class = "btn btn-primary"), align = "right")
                   ),#End of fluidrow
                   
                   dataTableOutput(ns("processrundata")),
                   
                   actionButton(ns("abuttondisplayoutput"), "Display Output",
                                class = "btn btn-primary"),
                   hidden(actionButton(ns("abuttonhideoutput"), "Hide Output",
                                class = "btn btn-primary")),
                   actionButton(ns("abuttonrerunpr"), "Rerun",
                                class = "btn btn-primary"),
                   actionButton(ns("abuttonshowlog"), "Show Process Run Log",
                                class = "btn btn-primary", style = "align:right"),
                   hidden(actionButton(ns("abuttonhidelog"), "Hide Process Run Log",
                                       class = "btn btn-primary", style = "align:right"))
               )#End of div prruntable 
             )
      ),
      
      column(12,
             hidden(
               div(id = ns("prrunoutput"), class = "flamingo-page-division",
                   fluidRow(
                     column(10,
                            h4("Output Files", class = "flamingo-table-title")),
                     column(2,
                            actionButton(ns("abuttonrefreshprrunoutputfile"),
                                         "Refresh", class = "btn btn-primary"), align = "right")
                   ),#End of fluidrow
                   tabsetPanel(id = ns("tabsetprrunoutput"),
                               tabPanel("File List", value = "tabprrunfilelist",
                                        dataTableOutput(ns("outputfileslist"))),
                               tabPanel("File Contents", value = "tabprrunfiledata",
                                        htmlOutput(ns("dttableoutputfiledataSelectedInfo")),
                                        dataTableOutput(ns("dttableoutputfiledata")),
                                        downloadButton(ns("PRfiledataIdownloadexcel"),
                                                       label = "Export to csv")
                               ),
                               tabPanel("Summary", value = "tabprrunsummary",
                                        fluidRow(
                                          column(6,
                                                 h3("Summary Exceedance Probability Curves"),
                                                 fluidRow(
                                                   column(12,
                                                          h4("Ground Up Loss Outputs"),
                                                          plotlyOutput(ns("plotGULOutput"))),
                                                   column(12,
                                                          h4("Insured Loss Outputs"),
                                                          plotlyOutput(ns("plotILOutput"))))),
                                          column(6,
                                                 h3("Summary Table"),
                                                 dataTableOutput(ns("dttableoutputsummary")))
                                        )
                               )
                   )
                   
               )#End of div prrunoutput
             )
      ),
      
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
  ))
  
}


#' UI/View to define the parameters of a process run
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
processRunParamUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
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
               hidden(div(id = ns("processRunParamAdvancedUIOutput"), style = "width:90%; margin: 0 auto;", processRunParamAdvancedUI(id))),
               hidden(div(id = ns("processRunParamPerilAdvancedUIOutput"), style = "width:90%; margin: 0 auto;", processRunParamPerilAdvancedUI(id)))
             )),
      
      column(8,
             panel(
               h4("Output Configuration"),
               checkboxInput(ns("chkinputGUL"), label = "Ground Up Loss", value = TRUE),
               hidden(div(id = ns("processRunParamAdvancedGULUIOutput"), processRunParamAdvancedGULUI(id))),
               checkboxInput(ns("chkinputIL"), label = "Insured Loss", value = FALSE), 
               hidden(div(id = ns("processRunParamAdvancedILUIOutput"), processRunParamAdvancedILUI(id))),
               div(id = ns("advanced"), style = "display: inline-block;margin-right:2%", align = "right", actionButton(ns("abtnadvanced"), label = "Advanced", class = "btn btn-primary")),
               hidden(div(id = ns("basic"), style = "display: inline-block;margin-right:2%", align = "right", actionButton(ns("abtnbasic"), label = "Basic", class = "btn btn-primary"))),
               hidden(div(id = ns("saveoutput"), style = "display: inline-block", actionButton(ns("abuttonsaveoutput"), label = "Save Configuration", class = "btn btn-primary"))),
               hidden(div(id = ns("clroutopt"), style = "display: inline-block", actionButton(ns("abtnclroutopt"), label = "Default", class = "btn btn-primary")))
             ))
      
    ),
    
    fluidRow(
      column(6, 
             div(id = ns("executeprrun"), style = "display: inline-block", actionButton(ns("abuttonexecuteprrun"), label = "Execute Run", class = "btn btn-primary"))),
      column(6, actionButton(ns("abuttoncancelrun"), label = "Cancel", class = "btn btn-primary"), align = "right")
    )
  )
}



#' UI/View to define the Advanced  Model parameters of a process run 
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
processRunParamAdvancedUI <- function(id) {
  
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
processRunParamPerilAdvancedUI <- function(id) {
  
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
processRunParamAdvancedGULUI <- function(id) {
  
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
                                selected =NULL)
    )
  )
}


#' UI/View to define the Advanced  IL parameters of a process run 
#' @inheritParams flamingoModuleUI
#' @importFrom shinyjs hidden
#' @export
processRunParamAdvancedILUI <- function(id) {
  
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
  )#end of fluidrow FM
  
}


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
                                                 h3("Summary EP Curves"),
                                                 fluidRow(
                                                   column(12,
                                                          h4("GUL Outputs"),
                                                          plotlyOutput(ns("plotGULOutput"))),
                                                   column(12,
                                                          h4("IL Outputs"),
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
               checkboxInput(ns("chkinputGUL"), "GUL", value = TRUE),
               hidden(div(id = ns("processRunParamAdvancedGULUIOutput"), processRunParamAdvancedGULUI(id))),
               checkboxInput(ns("chkinputIL"), "IL", value = FALSE), 
               hidden(div(id = ns("processRunParamAdvancedILUIOutput"), processRunParamAdvancedILUI(id))),
               div(id = ns("advanced"), style = "display: inline-block;margin-right:2%", align = "right", actionButton(ns("abtnadvanced"), "Advanced", class = "btn btn-primary")),
               hidden(div(id = ns("basic"), style = "display: inline-block;margin-right:2%", align = "right", actionButton(ns("abtnbasic"), "Basic", class = "btn btn-primary"))),
               hidden(div(id = ns("saveoutput"), style = "display: inline-block", actionButton(ns("abuttonsaveoutput"), "Save Output", class = "btn btn-primary"))),
               hidden(div(id = ns("clroutopt"), style = "display: inline-block", actionButton(ns("abtnclroutopt"), "Default", class = "btn btn-primary")))
             ))
      
    ),
    
    fluidRow(
      column(6, 
             div(id = ns("executeprrun"), style = "display: inline-block", actionButton(ns("abuttonexecuteprrun"), "Execute Run", class = "btn btn-primary"))),
      column(6, actionButton(ns("abuttoncancelrun"), "Cancel", class = "btn btn-primary"), align = "right")
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
               checkboxInput(ns("chkinputprwind"), "Peril: Wind", value = TRUE))),
    hidden(div(id = ns("perilsurge"),
               checkboxInput(ns("chkinputprstsurge"), "Peril: Surge", value = TRUE))),
    hidden(div(id = ns("perilquake"),
               checkboxInput(ns("chkinputprquake"), "Peril: Quake", value = TRUE))),
    hidden(div(id = ns("perilflood"),
               checkboxInput(ns("chkinputprflood"), "Peril: Flood", value = TRUE))),
    hidden(div(id = ns("demandsurge"),
               checkboxInput(ns("chkinputdsurge"), "Demand Surge", value = TRUE))),
    hidden(div(id = ns("leakagefactor"),
               sliderInput(ns("sliderleakagefac"), "Leakage factor:", min = 0, max = 100, value = 0.5, step = 0.5)))
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
processRunParamAdvancedILUI <- function(id) {
  
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

# step2_chooseAnalysis Module -----------------------------------------------------

# UI ---------------------------------------------------------------------------

#' step2_chooseAnalysis UI
#'
#' @rdname step2_chooseAnalysis
#'
#' @description UI/View for the step2_chooseAnalysis.
#'
#' @template params-module-ui
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
step2_chooseAnalysisUI <- function(id) {

  ns <- NS(id)

  tagList(
    hidden(div(id = ns("panelCreateAnalysesTable"), panelCreateAnalysesTable(id))),
    hidden(div(id = ns("panelAnalysisDetails"), panelAnalysisDetails(id))),
    hidden(div(id= ns("panelAnalysisLog"), panelAnalysisLog(id))),
    hidden(div(id = ns("panelModelTable"), panelModelTable(id)))#,
    # hidden(div(id = ns("panelModelDetails"), panelModelDetails(id)))
  )
}

#' panelCreateAnalysesTable
#'
#' @rdname panelCreateAnalysesTable
#'
#' @description Function wrapping panel to show analyses table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelCreateAnalysesTable <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_createanalyses"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_CreateAnalysesTable"), inline = TRUE),
      actionButton(inputId = ns("abuttonanarefresh"), label = "Refresh", style = "float: right;")
    ),
    DTOutput(ns("dt_analyses")),
    fluidRow(
      column(12,
             flamingoButton(inputId = ns("abuttoncreateana"), label = "Create Analysis") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttoncreateana, placement = "right"),
             flamingoButton(inputId = ns("abuttongenInput"), label = "Generate Input") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttongenInput, placement = "right"),
             flamingoButton(inputId = ns("abuttoncancelIG"), label = "Cancel Input Generation") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttoncancelIG, placement = "right"),
             flamingoButton(inputId = ns("abuttonshowlog"), label = "Show Log") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttonshowlog, placement = "right"),
             flamingoButton(inputId = ns("abuttonshowanadetails"), label = "Show Details") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttonshowanadetails, placement = "right"),
             actionButton(ns("abuttonpgotonextstep"), "Proceed to Configure Output & Run", style = "float:right")
      )
    )
  )
}

#' panelAnalysisDetails
#'
#' @rdname panelAnalysisDetails
#'
#' @description Function wrapping panel to show analyses details table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
panelAnalysisDetails <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_analysisdetails"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_panelAnalysisDetails"), inline = TRUE),
      actionButton(inputId = ns("abuttonanadetailsrefresh"), label = "Refresh", style = "float: right;"),
      actionButton(inputId = ns("buttonhideanadetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_analysisdetails"))
  )
}

#' panelAnalysisLog
#'
#' @rdname panelAnalysisLog
#'
#' @description Function wrapping panel to show analyses logs table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
panelAnalysisLog <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_analysislog"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_panelAnalysisLog"), inline = TRUE),
      actionButton(inputId = ns("abuttonanalogrefresh"), label = "Refresh", style = "float: right;"),
      actionButton(inputId = ns("buttonhideanalog"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_analysislog"))
  )
}



#' panelModelTable
#'
#' @rdname panelModelTable
#'
#' @description Function wrapping panel to show list of models table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelModelTable <- function(id) {
  ns <- NS(id)
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_model"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_ModelTable"), inline = TRUE),
      actionButton(inputId = ns("abuttonmodelrefresh"), label = "Refresh", style = "float: right;"),
      actionButton(inputId = ns("buttonhidemodel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_models")),
    textInput(inputId = ns("anaName"), label = "Analysis Name"),
    # flamingoButton(ns("abuttonmodeldetails"), "Show Details", align = "centre") %>%
    #   bs_embed_tooltip(title = defineSingleAna$abuttonmodeldetails, placement = "right"),
    flamingoButton(ns("abuttonsubmit"), "Submit", style = "float:right")
  )
}

# #' panelModelDetails
# #'
# #' @rdname panelModelDetails
# #'
# #' @description Function wrapping panel to show details of programme table.
# #'
# #' @template params-module-ui
# #'
# #' @importFrom DT DTOutput
# #'
# #' @export
# panelModelDetails <- function(id) {
#   ns <- NS(id)
#   flamingoPanel(
#     collapsible = FALSE,
#     ns("panel_model_details"),
#     heading = tagAppendChildren(
#       h4(""),
#       uiOutput(ns("paneltitle_ModelDetails"), inline = TRUE),
#       actionButton(inputId = ns("abuttonmodeldetailrfsh"), label = "Refresh", style = "float: right;"),
#       actionButton(inputId = ns("buttonhidemodeldetails"), label = NULL, icon = icon("times"), style = "float: right;")
#     ),
#     DTOutput(ns("dt_modelDetails"))
#   )
# }


# Server -----------------------------------------------------------------------

#' step2_chooseAnalysis Server
#'
#' @rdname step2_chooseAnalysis
#'
#' @description Server logic to step2_chooseAnalysis.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param currstep current selected step.
#' @param portfolioID selected portfolio ID.
#' @param pfName Name of selected portfolio
#' @param pfstatus Status of selected portfolio
#'
#' @return newstep navigation step
#' @return analysisID analysis ID
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs onclick
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT selectPage
#' @importFrom dplyr filter
#' @importFrom dplyr sym
#'
#' @export
step2_chooseAnalysis <- function(input, output, session,
                                 dbSettings,apiSettings,
                                 active = reactive(TRUE),
                                 logMessage = message,
                                 currstep = reactive(-1),
                                 portfolioID = reactive({""}),
                                 pfName = reactive({""}),
                                 pfstatus = reactive({""})

) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  # > Reactive Values ----------------------------------------------------------
  result <- reactiveValues(
    # reactive for portfolioID
    portfolioID = "",
    # reactive for modelID
    modelID = "",
    # reactive value for model table
    tbl_modelsData = NULL,
    # reactive value for detail of model table
    #tbl_modelsDetails = NULL,
    # analyses table
    tbl_analysesData = NULL,
    # analysis details
    tbl_analysisdetails = NULL,
    #analysis log
    tbl_analysislog = NULL,
    #analysis ID
    analysisID = ""
  )

  #Set Params
  observeEvent(portfolioID(), {
    if (!is.null(portfolioID())) {
      result$portfolioID <- portfolioID()
    } else {
      result$portfolioID <- ""
    }

  })

  # Panels Visualization -------------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 2 ) {
      .defaultAssociateModel()
      .reloadAnaData()
      .reloadtbl_modelsData()
    }
  })

  observeEvent(input$dt_models_rows_selected, ignoreNULL = FALSE, {
    hide("panelAnalysisDetails")
    hide("panelAnalysisLog")
    hide("panelModelTable")
  })

  # Analyses  Table ------------------------------------------------------------
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
        colnames = c('Row Number' = 1),
        filter = 'bottom',
        options = .getPRTableOptions()
      )
    } else {
      .nothingToShowTable(contentMessage = paste0("no analysis available"))
    })

  # Create Analyses Table  Title
  output$paneltitle_CreateAnalysesTable <- renderUI({
    if (result$portfolioID != "") {
      pfName <- ifelse(toString(pfName()) == " " | toString(pfName()) == "" | toString(pfName()) == "NA", "", paste0('"', toString(pfName()), '"'))
      paste0('Analyses associated with portfolio ', pfName, ', id ', toString(result$portfolioID),' ', toString(pfstatus()) )
    } else {
      paste0('Analyses')
    }
  })

  observeEvent(result$portfolioID, {
    .reloadAnaData()
  })


  # Analysis ID ----------------------------------------------------------------

  observeEvent(input$dt_analyses_rows_selected, ignoreNULL = FALSE, {
    if (!is.null(input$dt_analyses_rows_selected)) {
      result$analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
    } else {
      result$analysisID <- ""
    }
  })


  # Analysis detais ------------------------------------------------------------

  # Generate input

  observeEvent(input$dt_analyses_rows_selected, ignoreNULL = FALSE, {
    if (length(input$dt_analyses_rows_selected) > 0) {
      enable("abuttongenInput")
    } else {
      disable("abuttongenInput")
    }
  })

  onclick("abuttongenInput", {

    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
    input_generation_id <- api_post_analyses_generate_inputs(analysisID)

    if (input_generation_id$status == "Success") {
      flamingoNotification(type = "message",
                           paste("Input generation id ", analysisID, " created."))
      .reloadAnaData()
    } else {
      flamingoNotification(type = "error",
                           paste("Input generation id ", analysisID, " could not be created."))
    }

  })


  ## Cancel input generation button
  observeEvent(input$dt_analyses_rows_selected, ignoreNULL = FALSE, {
    if (length(input$dt_analyses_rows_selected) > 0) {
      enable("abuttoncancelIG")
    } else {
      disable("abuttoncancelIG")
    }
  })


  onclick("abuttoncancelIG", {

    analysisID <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaID]
    delete_analyses_id <- api_post_analyses_cancel_generate_inputs(analysisID)

    if (delete_analyses_id$status == "Success") {
      flamingoNotification(type = "message",
                           paste("Input Generation id ", analysisID, " deleted."))
      .reloadAnaData()
    } else {
      flamingoNotification(type = "error",
                           paste("Input Generation id ", analysisID, " could not be deleted."))
    }

  })

  onclick("abuttonshowanadetails", {
    hide("panelAnalysisLog")
    hide("panelModelTable")
    #hide("panelModelDetails")
    logMessage("showing panelAnalysisDetails")
    show("panelAnalysisDetails")
    .reloadAnaDetails()
  })

  onclick("buttonhideanadetails", {
    hide("panelAnalysisDetails")
  })

  output$dt_analysisdetails <- renderDT(
    if (!is.null(result$tbl_analysisdetails) && nrow(result$tbl_analysisdetails) > 0 ) {
      logMessage("re-rendering analysis details table")
      datatable(
        result$tbl_analysisdetails,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'none'),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions(pageLengthVal = 10)
      )
    } else {
      .nothingToShowTable(contentMessage = paste0("no files associtated with analysis id ", result$analysisID))
    }
  )

  #  panelAnalysisDetails Table title
  output$paneltitle_panelAnalysisDetails <- renderUI({
    if (result$analysisID != "") {
      anaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaName]
      paste0('Details of analysis id ', toString(result$analysisID), ' ', anaName)
    } else {
      paste0("Analysis Details")
    }
  })

  # Analysis Logs --------------------------------------------------------------
  onclick("abuttonshowlog", {
    hide("panelAnalysisDetails")
    hide("panelModelTable")
    #hide("panelModelDetails")
    logMessage("showing panelAnalysisLog")
    show("panelAnalysisLog")
    .reloadAnaLog()
  })

  onclick("buttonhideanalog", {
    hide("panelAnalysisLog")
  })

  output$dt_analysislog <- renderDT(
    if (!is.null(result$tbl_analysislog) && nrow(result$tbl_analysislog) > 0 ) {
      logMessage("re-rendering analysis log table")
      datatable(
        result$tbl_analysislog,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'none'),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    } else {
      .nothingToShowTable(contentMessage = paste0("no log files associtated with analysis id ", result$analysisID))
    }
  )

  #  panelAnalysisLog Table title
  output$paneltitle_panelAnalysisLog <- renderUI({
    if (result$analysisID != "") {
      anaName <- result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaName]
      paste0('Input generation Logs of analysis id ', toString(result$analysisID), ' ', anaName)
    } else {
      paste0("Input generation Logs")
    }
  })


# Model Table ----------------------------------------------------------------

onclick("abuttoncreateana", {
  hide("panelAnalysisDetails")
  hide("panelAnalysisLog")
  #hide("panelModelDetails")
  logMessage("showing panelModelTable")
  show("panelModelTable")
  .reloadtbl_modelsData()
  .clearinputanaName()
})

onclick("buttonhidemodel", {
  hide("panelModelTable")
})

output$dt_models <- renderDT(
  if (!is.null(result$tbl_modelsData) && nrow(result$tbl_modelsData) > 0 ) {
    logMessage("re-rendering model table")
    datatable(
      result$tbl_modelsData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      escape = FALSE,
      selection = list(mode = 'single',
                       selected = rownames(result$tbl_modelsData)[1]),
      colnames = c('Row Number' = 1),
      options = .getPRTableOptions()
    )
  } else {
    .nothingToShowTable(contentMessage = paste0("no Models associated with Portfolio ID ", result$portfolioID))
  }
)

# Model Table title
output$paneltitle_ModelTable <- renderUI({
  if (result$portfolioID != "") {
    pfName <- ifelse(toString(pfName()) == " " | toString(pfName()) == "" | toString(pfName()) == "NA", "", paste0('"', toString(pfName()), '"'))
    paste0('Pick a model to associate with portfolio id ', toString(result$portfolioID), ' ', pfName)
  } else {
    paste0("List of Models")
  }
})

# Model Details Table --------------------------------------------------------

# # Show/hide Model Details Panel
# onclick("abuttonmodeldetails", {
# hide("panelAnalysisDetails")
# hide("panelAnalysisLog")
#   logMessage("showing panelModelDetails")
#   .reloadtbl_modelsDetails()
#   show("panelModelDetails")
#   logMessage("showing panelModelDetails")
# })

# onclick("buttonhidemodeldetails", {
#   hide("panelModelDetails")
#   logMessage("hiding panelModelDetails")
# })

# output$dt_modelDetails <- renderDT(
#   if (!is.null(result$tbl_modelsDetails) && nrow(result$tbl_modelsDetails) > 0 ) {
#     logMessage("re-rendering model details table")
#     datatable(
#       result$tbl_modelsDetails,
#       class = "flamingo-table display",
#       rownames = TRUE,
#       filter = "none",
#       escape = FALSE,
#       selection = "none",
#       colnames = c('Row Number' = 1),
#       options = .getPRTableOptions()
#     )
#   } else {
#     .nothingToShowTable(contentMessage = paste0("no files associated with Model ID ", result$modelID ))
#   })

# # Details Model title
# output$paneltitle_ModelDetails <- renderUI({
#   modelId <- result$tbl_modelsData[ input$dt_models_rows_selected,tbl_modelsData.ModelId]
#   paste0('Details of Model Association id ', modelId)
# })

# Create new Analysis --------------------------------------------------------

onclick("abuttonsubmit", {
  if (input$anaName != "") {
    modelID <- result$tbl_modelsData[input$dt_models_rows_selected, tbl_modelsData.ModelId]
    post_portfolios_create_analysis <- api_post_portfolios_create_analysis(id = result$portfolioID,
                                                                           name = input$anaName,
                                                                           model = modelID)
    logMessage(paste0("Calling api_post_portfolios_create_analysis with id ", result$portfolioID, " name ", input$anaName, " model ",  modelID))
    if (post_portfolios_create_analysis$status == "Success") {
      flamingoNotification(type = "message",
                           paste("New analysis ", input$anaName, " created."))
      .reloadAnaData()
    } else {
      flamingoNotification(type = "error",
                           paste("Analysis ", input$anaName, " not created."))
    }
  } else {
    flamingoNotification(type = "error",
                         paste("Provide name for analysis creation."))
  }
  hide("panelModelTable")
})

# Enable and disable buttons -------------------------------------------------
observeEvent({
  result$tbl_analysesData
  input$dt_analyses_rows_selected
  result$tbl_modelsData
  input$dt_models_rows_selected}, ignoreNULL = FALSE, ignoreInit = TRUE, {
    disable("abuttonshowlog")
    disable("abuttonshowanadetails")
    disable("abuttondelana")
    disable("abuttoncancelIG")
    disable("abuttongenInput")
    # disable("abuttonmodeldetails")
    disable("abuttonpgotonextstep")
    if (length(input$dt_models_rows_selected) > 0) {
      # enable("abuttonmodeldetails")
    }
    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 && length(input$dt_analyses_rows_selected) > 0) {
      enable("abuttonshowanadetails")
      enable("abuttonshowlog")
      enable("abuttondelana")
      enable("abuttoncancelIG")
      enable("abuttongenInput")
      if (result$tbl_analysesData[input$dt_analyses_rows_selected, tbl_analysesData.AnaStatus] == StatusReady) {
        enable("abuttonpgotonextstep")
      }
    }
  })

#Not allowed creation of an analysis for an incomplete portfolio
observeEvent({
  result$portfolioID
  pfstatus()}, {
    if (pfstatus() == "- Status: Completed") {
      enable("abuttoncreateana")
    } else {
      disable("abuttoncreateana")
    }
  })
# Refresh Buttons ------------------------------------------------------------
onclick("abuttonanarefresh", {
  .reloadAnaData()
} )

onclick("abuttonanadetailsrefresh", {
  .reloadAnaDetails()
})

onclick("abuttonanalogrefresh", {
  .reloadAnaLog()
})

onclick("abuttonmodelrefresh", {
  .reloadtbl_modelsData()
} )

# onclick("abuttonmodeldetailrfsh", {
#   .reloadtbl_modelsDetails()
# } )

# Help Functions -------------------------------------------------------------
# hide all panels
.hideDivs <- function() {
  logMessage(".hideDivs called")
  #Section "Choose Analysis" = "2"
  hide("panelCreateAnalysesTable")
  hide("panelAnalysisDetails")
  hide("panelAnalysisLog")
  hide("panelModelTable")
  #hide("panelModelDetails")
}

#show default view for Section "Choose Analysis" = "2"
.defaultAssociateModel <- function(){
  logMessage(".defaultAssociateModel called")
  show("panelCreateAnalysesTable")
}

# Reload Analysis table
.reloadAnaData <- function() {
  logMessage(".reloadAnaData called")
  if (result$portfolioID  != "") {
    tbl_analysesData  <- return_tbl_analysesData()
    if (!is.null(tbl_analysesData)  && nrow(tbl_analysesData) > 0) {
      result$tbl_analysesData <- tbl_analysesData %>% filter(!! sym(tbl_analysesData.PortfolioID) == result$portfolioID)
    }
    logMessage("analyses table refreshed")
  }  else {
    result$tbl_analysesData <- NULL
  }
  invisible()
}

#clear text input
.clearinputanaName <- function(){
  updateTextInput(session = session, inputId = "anaName", value = "")
}


# Reload Analysis Details table
.reloadAnaDetails <- function() {
  logMessage(".reloadAnaDetails called")
  if (!is.null(result$analysisID) && result$analysisID != "") {
    result$tbl_analysisdetails <- return_tbl_analysisdetails(result$analysisID)
  } else {
    result$tbl_analysisdetails <-  NULL
  }
}

# Reload Analysis Log table
.reloadAnaLog <- function() {
  logMessage(".reloadAnaLog called")
  if (!is.null(result$analysisID) && result$analysisID != "") {
    result$tbl_analysislog <- return_input_generation_traceback_file_df(result$analysisID)
  } else {
    result$tbl_analysislog <-  NULL
  }
}

# Reload Programme Model table
.reloadtbl_modelsData <- function() {
  logMessage(".reloadtbl_modelsData called")
  if (result$portfolioID != "") {
    result$tbl_modelsData <- return_tbl_modelsData()
    logMessage("models table refreshed")
  } else {
    result$tbl_modelsData <- NULL
  }
  invisible()
}

# # Reload Programme Model Details table
# .reloadtbl_modelsDetails <- function() {
#   logMessage(".reloadtbl_modelsDetails called")
#   if (length(input$dt_models_rows_selected) > 0) {
#     prgId <- result$tbl_modelsData[input$dt_models_rows_selected, tbl_modelsData.ModelId]
#     stmt <- buildDbQuery("getProgOasisFileDetails", prgId)
#     tbl_modelsDetails <- executeDbQuery(dbSettings, stmt)
#     if (!is.null(tbl_modelsDetails)) {
#       result$tbl_modelsDetails <-  tbl_modelsDetails %>%
#         replaceWithIcons()
#     }
#     logMessage("files table refreshed")
#   } else {
#     result$tbl_modelsDetails <- NULL
#   }
#   invisible()
# }

# table settings for pr tab: returns option list for datatable
.getPRTableOptions <- function(pageLengthVal = pageLength) {
  options <- list(
    search = list(caseInsensitive = TRUE),
    searchHighlight = TRUE,
    processing = 0,
    pageLength = pageLengthVal,
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

# Model Outout ---------------------------------------------------------------

moduleOutput <- c(
  list(
    analysisID = reactive({result$analysisID}),
    newstep = reactive({input$abuttonpgotonextstep})
  )
)

moduleOutput

}

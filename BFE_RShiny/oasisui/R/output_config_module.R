# Define Output Configuration UI -----------------------------------------------

#' modeldetailsUI
#'
#' @rdname def_out_config
#'
#' @description UI side of function wrapping panel to show oputput configuration.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
def_out_configUI <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = FALSE,
    id = ns("panel_anaoutput"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_defAnaConfigOutput"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidepanelconfigureoutput"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
      column(4,
             panelModelParams(id)),
      column(8,
             fluidRow(panelOutputParams(id)),
             fluidRow(panelOutputParamsDetails(id))
      )
    ),
    fluidRow(
      column(12,
             oasisuiButton(inputId = ns("abuttonexecuteanarun"), label = "Execute Run"), align = "right")) %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonexecuteanarun, placement = "right")
  )
}

#' panelModelParams
#'
#' @rdname panelModelParams
#'
#' @description Function wrapping sub-panel to define model params.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelModelParams <- function(id) {
  ns <- NS(id)
  tagList(
    oasisuiPanel(
      collapsible = FALSE,
      id = ns("panel_ModelParams"),
      heading = h4("Model Parameters"),
      div(id = ns("basic"), style = "width:100%; margin: 0 auto;",
          uiOutput(ns("basic_model_param")),
          uiOutput(ns("chkinputsperils"))
      ),
      hidden(div(id = ns("configureAnaParamsAdvanced"), align = "left",
                 textInput(ns("tinputnoofsample"), label = "Number of Samples:", value = "10"),
                 textInput(ns("tinputthreshold"), label = "Loss Threshold:", value = "0"),
                 checkboxInput(ns("chkinputsummaryoption"), "Summary Reports", value = TRUE),
                 uiOutput(ns("advanced_model_param"))
      ))
    )
  )
}


#' panelOutputParams
#'
#' @rdname panelOutputParams
#'
#' @description Function wrapping sub-panel to define output params.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelOutputParams <- function(id) {
  ns <- NS(id)
  tagList(
    oasisuiPanel(
      collapsible = FALSE,
      id = ns("panel_OutputParams"),
      heading = h4("Output Parameters"),
      fluidRow(
        column(8,
               selectInput(inputId = ns("sintag"), label = "Tag", choices = default_tags, selected = default_tags[1])
        ),
        column(4,
               actionButton(ns(paste0("abuttonchoosetag")), label = NULL, icon = icon("list-alt"),
                            style = " color: rgb(71, 73, 73);
                            background-color: white;
                            padding: 0px;
                            font-size: 24px;
                            background-image: none;
                            border: none;
                            ") %>%
                 bs_embed_tooltip(title = defineSingleAna$abuttonchoosetag, placement = "right")
               )
        )
        )
        )
}

#' panelOutputParamsDetails
#'
#' @rdname panelOutputParamsDetails
#'
#' @description Function wrapping sub-panel to define output params details.
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelOutputParamsDetails <- function(id) {
  ns <- NS(id)
  tagList(
    oasisuiPanel(
      collapsible = TRUE,
      show = FALSE,
      id = ns("panel_OutputParamsDetails"),
      heading = h4("Output Parameters Details"),
      uiOutput(ns("perspective_ui")), # checkboxses for all perspectives; available for all tags
      # hidden(div(id = ns("div_summary_levels_reports_ui"),
      uiOutput(ns("summary_levels_reports_ui")), # combinations of summary levels and reports.
      # ),
      uiOutput(ns("out_params_review_ui")) # review of output configuration in long format. As a collapsible panel. Available for all tags
    )
  )
}


# Define Output Configuration Server -------------------------------------------

#' def_out_config
#'
#' @rdname def_out_config
#'
#' @description Server side of function wrapping panel to show oputput configuration.
#'
#' @param analysisID Selected analysis ID.
#' @param analysisName Selected analysis name.
#' @param ana_flag flag to know if the user is creating a new output configuration or rerunning an analysis.
#' @template params-module
#' @template params-active
#'
#' @return ana_flag flag to know if the user is creating a new output configuration or rerunning an analysis.
#' @return ana_post_status status of posting the analysis.
#'
#' @importFrom shinyjs hide
#' @importFrom dplyr filter
#' @importFrom jsonlite write_json
#'
#' @export
def_out_config <- function(input,
                           output,
                           session,
                           analysisID = reactive(NULL),
                           analysisName = reactive(""),
                           ana_flag = reactive("C"),
                           counter = reactive(NULL),
                           active = reactive(TRUE)) {

  ns <- session$ns

  # max number rows for custom output params
  max_n <- 10

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    # flag to know if the user is creating a new output configuration or rerunning an analysis
    ana_flag = "C",
    # result of posting RUN_analysis
    ana_post_status = "",
    # number of rows of output parameters - dependent on abuttonadd
    n = 0,
    # data for output params review
    out_params_review = data.frame(perspective = c(), summary_level = c(), report = c())
  )

  # Set up ---------------------------------------------------------------------

  #ana_flag
  observeEvent(ana_flag(), {
    if (ana_flag() != result$ana_flag) {
      result$ana_flag <- ana_flag()
    }
  })

  observeEvent(counter(), {
    .clearOutputOptions()
  })

  # Panel infos ----------------------------------------------------------------

  # hide panel
  onclick("abuttonhidepanelconfigureoutput", {
    hide("panelDefineOutputs")
    .defaultview()
    result$ana_flag <- "C"
  })

  # configuration title
  output$paneltitle_defAnaConfigOutput <- renderUI({
    analysisName <- ifelse(analysisName() == " ", "", paste0('"', analysisName(), '"'))
    if (result$ana_flag  == "R") {
      paste0('Re-define output configuration for analysis id ', analysisID(), ' ', analysisName)
    } else {
      paste0('Define output configuration for analysis id ', analysisID(), ' ', analysisName)
    }
  })

  # Select Tag from another analysis -------------------------------------------

  # Choose Tag
  observeEvent(input$abuttonchoosetag, {
    tbl_analysesData  <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
    tbl_analysesData <- tbl_analysesData %>% filter(grepl("run", tolower(status_detailed))) #keep all analyses that have been run, i.e. that have an analysis settings associated.
    namesList <- tbl_analysesData[,tbl_analysesDataNames$name]
    idList <- tbl_analysesData[,tbl_analysesDataNames$id]
    choicesList <- paste(idList, namesList, sep = " / ")
    showModal(AnaList)
    updateSelectInput(inputId = "sinoutputoptions", choices = choicesList, session = session)
  })

  # > Modal Dialogue
  AnaList <- modalDialog(
    easyClose = TRUE,
    size = "l",
    selectInput(ns("sinoutputoptions"), "Select Custom Configuration:", choices = ""),
    footer = tagList(
      oasisuiButton(ns("abuttonselectconf"),
                    label = "Select Configuration", align = "left"),
      actionButton(ns("abuttoncancel"),
                   label = "Cancel", align = "right")
    )
  )

  # update tag based on analysis selection
  observeEvent(input$abuttonselectconf, {
    # Using analyses names to select the output configuration of a previously posted analyses
    logMessage(paste0("updating output configuration because input$sinoutputoptions changed to ",input$sinoutputoptions))
    if (length(input$sinoutputoptions) > 0 && input$sinoutputoptions != "") {
      anaName <- strsplit(input$sinoutputoptions, split = " / ")[[1]][2]
      anaID <- strsplit(input$sinoutputoptions, split = " / ")[[1]][1]
      analysis_settings <- session$userData$data_hub$get_ana_settings_content(anaID, oasisapi = session$userData$oasisapi)
      if (!is.null(analysis_settings$detail) && analysis_settings$detail == "Not found.") {
        oasisuiNotification(type = "error",
                            paste0("No output configuration associated to analysis ", anaName," id ", anaID, "."))
      } else {
        logMessage(paste0("appling the output configuration of analysis ", anaName," id ", anaID))
        # TO DO
        # Get chosen tag out of the analysis settings
        chosen_tag <- default_tags[1]
        # Update tag
        updateSelectInput(inputId = "sintag", selected = chosen_tag, session = session)
        #Set inputs
        .updateOutputConfig(analysis_settings)
      }
    }
    removeModal()
  })

  # > close modal
  observeEvent(input$abuttoncancel, {
    removeModal()
  })

  # Preselected Output Configuration -------------------------------------------

  # Rerun case
  observeEvent({
    result$ana_flag
    analysisID()
  }, {
    if (!is.null(analysisID()) && result$ana_flag == "R") {
      analysis_settings <- session$userData$data_hub$get_ana_settings_content(analysisID(), oasisapi = session$userData$oasisapi)
      if (!is.null(analysis_settings$detail) && analysis_settings$detail == "Not found.") {
        oasisuiNotification(type = "error",
                            paste0("No output configuration associated to analysis ", analysisName()," id ", analysisID(), "."))
      } else {
        logMessage(paste0("appling the output configuration of analysis ", analysisName(), " id ",  analysisID() ))
        #Set inputs
        .updateOutputConfig(analysis_settings)
      }
    }
  })

  # Output Parameters Details --------------------------------------------------

  # checkboxses for all perspectives; available for all tags
  output$perspective_ui <- renderUI(checkboxGroupInput(inputId = ns("chkboxgrplosstypes"), label = "Perspective", choices = output_options$losstypes, inline = TRUE, selected = output_options$losstypes[1]))


  # >> summary levels and reports ----------------------------------------------

  dynamicUI <- function(n) {
    if (n > max_n) {
      oasisuiNotification("Reached maximum number of entries.", type = "warning")
      fluidRow(
        column(5,
               selectInput(inputId = ns(paste0("sinsummarylevels", n)), label = "Summary Levels", choices = output_options$granularities, selected = output_options$granularities[output_options$order][1], multiple = TRUE)
        ),
        column(5,
               selectInput(inputId = ns(paste0("sinreports", n)), label = "Reports", choices = output_options$variables, selected = output_options$variables[output_options$variables_default][1], multiple = TRUE)
        )
      )
    } else {
      fluidRow(
        column(5,
               selectInput(inputId = ns(paste0("sinsummarylevels",n)), label = "Summary Levels", choices = output_options$granularities, selected = output_options$granularities[output_options$order][1], multiple = TRUE)
        ),
        column(5,
               selectInput(inputId = ns(paste0("sinreports", n)), label = "Reports", choices = output_options$variables, selected = output_options$variables[output_options$variables_default][1], multiple = TRUE)
        ),
        column(1,
               actionButton(ns(paste0("abuttonadd", n)), label = "", icon = icon("plus"))
        )
      )
    }
  }

  observeEvent(input$sintag, {
    logMessage(paste0("updating output parameters for ", input$sintag, " configuration"))

    # clean up ui
    logMessage("clean up UI")
    if (any(grepl("sinsummarylevels", input))) {
      removeUI(
        selector = "div:has(> #sinsummarylevels)",
        multiple = TRUE,
        immediate = TRUE
      )
    }
    if (any(grepl("sinreports", input))) {
      removeUI(
        selector = "div:has(> #sinreports)",
        multiple = TRUE,
        immediate = TRUE
      )
    }
    if (any(grepl("abuttonadd", input))) {
      removeUI(
        selector = "div:has(> #abuttonadd)",
        multiple = TRUE,
        immediate = TRUE
      )
    }

    # reset counter
    logMessage(paste0("resetting result$n from ", result$n, " to 0"))
    result$n <- 0
    output$summary_levels_reports_ui <- renderUI({
      if (input$sintag == default_tags[2]) {
        tagList(
          fluidRow(
            column(5,
                   selectInput(inputId = ns(paste0("sinsummarylevels", result$n)), label = "Summary Levels", choices = output_options$granularities, selected = output_options$granularities[output_options$order][1], multiple = TRUE)
            )
          )
        )
      } else if (input$sintag == default_tags[3]) {
        tagList(
          div(id = ns("placeholder")),
          dynamicUI(result$n)
        )
      }
    })
  })

  lapply(seq(0, max_n), function(x) {
    observeEvent({
      input[[paste0("abuttonadd", x)]]}, {
        print(paste0("result$n ", result$n))
        print(paste0("x ", x))
        logMessage(paste0("insert ui because ", paste0("abuttonadd", x), " changed to ",  input[[paste0("abuttonadd", x)]]))
        result$n <<- result$n + 1
        print(paste0("result$n ", result$n))
        insertUI(
          selector = "#placeholder",
          where = "afterEnd",
          immediate = TRUE,
          ui = dynamicUI(result$n)
        )
      })
  })


  # > Output Params Review -----------------------------------------------------

  # review of output configuration in long format. As a collapsible panel. Available for all tags
  output$out_params_review_ui <- renderUI(
    tagList(
      oasisuiPanel(
        collapsible = TRUE,
        show = FALSE,
        id = ns("panel_OutputParamsReview"),
        heading = h4("Output Parameters Review"),
        oasisuiTableUI(ns("out_params_review_tbl")),
        downloadButton(ns("download_out_params_review_tbl"), label = "Export to csv") %>%
          bs_embed_tooltip(title = defineSingleAna$download_out_params_review_tbl, placement = "right")
      )
    )
  )

  observe_output_param <- function(){
    if (is.null(input$chkboxgrplosstypes)){
      perspectives <- output_options$losstypes[1]
    } else {
      perspectives <- input$chkboxgrplosstypes
    }
    if (input$sintag == default_tags[1]) {
      summary_levels <- c(output_options$granularities[output_options$order][1])
      reports <- c(output_options$variables[output_options$variables_default])
    } else if (input$sintag == default_tags[2]) {
      summary_levels <- sapply(seq(0, result$n), function(x){input[[paste0("sinsummarylevels", x)]]})
      reports <- c(output_options$variables[output_options$variables_default])
    } else if (input$sintag == default_tags[3]) {
      summary_levels <- sapply(seq(0, result$n), function(x){input[[paste0("sinsummarylevels", x)]]})
      reports <- sapply(seq(0, result$n), function(x){input[[paste0("sinreports", x)]]})
    }
    if (summary_levels %>% unlist(recursive = TRUE) %>% is.null()) {summary_levels <- c(output_options$granularities[output_options$order][1])}
    if (reports %>% unlist(recursive = TRUE) %>% is.null()) {reports <- c(output_options$variables[output_options$variables_default])}
    result$out_params_review <- expand.grid(perspectives,summary_levels,reports) %>%
      setNames(c("perspective", "summary_level", "report"))
  }

  sinsummarylevels_react_all <- reactive({lapply(seq(0, max_n), function(x){input[[paste0("sinsummarylevels", x)]]})})
  sinreports_react_all <- reactive({lapply(seq(0, max_n), function(x){input[[paste0("sinreports", x)]]})})

  observeEvent({
    input$sintag
    input$chkboxgrplosstypes
    sinsummarylevels_react_all()
    sinreports_react_all()
  }, ignoreInit = TRUE,{
    observe_output_param()
  })

  callModule(
    oasisuiTable,
    id = "out_params_review_tbl",
    data = reactive({result$out_params_review}),
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE
  )

  output$download_out_params_review_tbl <- downloadHandler(
    filename = paste0("outputParams_review_analysis_",analysisID(),".csv"),
    content = function(file) {
      fwrite(result$out_params_review %>% capitalize_names_df(), file, row.names = FALSE, quote = TRUE)
    }
  )

  # Run analysis ---------------------------------------------------------------
  # Execute analysis
  onclick("abuttonexecuteanarun", {
    analysis_settingsList <- .gen_analysis_settings()
    #write out file to be uploades
    currfolder <- session$userData$data_hub$get_user_destdir()
    dest <- file.path(currfolder, "analysis_settings.json")
    write_json(analysis_settingsList, dest, pretty = TRUE, auto_unbox = TRUE)

    #post analysis settings
    post_analysis_settings_file <- session$userData$oasisapi$api_post_file_query(query_path = paste("analyses", analysisID(), "settings_file", sep = "/"), query_body = dest, query_encode = "multipart")

    result$ana_post_status <- post_analysis_settings_file$status
  })


  # Helper Functions -----------------------------------------------------------

  .updateOutputConfig <- function(analysis_settings){

  }

  .gen_analysis_settings <- function(){

  }

  .clearOutputOptions <- function() {
    logMessage(".clearOutputOptions called")

    # Predefined params
    tbl_analysesData  <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)

    # Model Params
    modelID <- tbl_analysesData[tbl_analysesData[, tbl_analysesDataNames$id] == analysisID(), tbl_analysesDataNames$model]
    tbl_modelsDetails <- session$userData$oasisapi$api_return_query_res(query_path = paste( "models", modelID, "resource_file", sep = "/"), query_method = "GET")
    if (!is.null(modelID) && !is.null(tbl_modelsDetails)) {
      model_settings <- tbl_modelsDetails$model_settings #%>% unlist(recursive = FALSE)
      names_settings_type <- lapply(names(model_settings), function(i) {model_settings[[i]][["type"]]}) %>%
        setNames(names(model_settings))

      if (length(names(model_settings)) > 0 ) {
        # Basic model params
        fixed_settings <- c("event_set", "event_occurrence_id")
        basic_model_params <- names(model_settings)[names(model_settings) %in% fixed_settings]
        ui_basic_model_param <- lapply(basic_model_params, function(p){
          curr_param_lst <- model_settings[[p]]
          curr_param_name <- capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
          if (curr_param_lst$type == "boolean") {
            checkboxInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name, value = curr_param_lst$default)
          } else if (curr_param_lst$type == "dictionary") {
            selectInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        choices = SwapNamesValueInList(curr_param_lst$values), selected =  curr_param_lst$default)
          } else if (curr_param_lst$type == "float") {
            sliderInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        min = curr_param_lst$min, max = curr_param_lst$max, value =  curr_param_lst$default)
          }
        })
        output$basic_model_param <- renderUI(ui_basic_model_param)

        # Perils Settings
        model_perils <- names(model_settings)[grepl("peril_", names(model_settings))]
        if (length(model_perils) > 0 ) {
          ui_perils <- lapply(model_perils, function(p){
            curr_param_lst <- model_settings[[p]]
            curr_param_name <- capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
            checkboxInput(ns(paste0("model_params_", p)), label = curr_param_lst$name, value = curr_param_lst$default)
          })
          output$chkinputsperils <- renderUI(list(h5("Available Perils"),ui_perils))
        }

        # Advanced model params
        advanced_model_param <- names(model_settings)[ names(model_settings) %notin% c(basic_model_params, model_perils)]
        ui_advanced_model_param <- lapply(advanced_model_param, function(p){
          curr_param_lst <- model_settings[[p]]
          curr_param_name <- capitalize_first_letter(gsub("_", ": ", curr_param_lst$name))
          if (curr_param_lst$type == "boolean") {
            checkboxInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name, value = curr_param_lst$default)
          } else if (curr_param_lst$type == "dictionary") {
            selectInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        choices = .SwapNamesValueInList(curr_param_lst$values), selected =  curr_param_lst$default)
          } else if (curr_param_lst$type == "float") {
            sliderInput(inputId = ns(paste0("model_params_", p)), label = curr_param_name,
                        min = curr_param_lst$min, max = curr_param_lst$max, value =  curr_param_lst$default)
          }
        })
        output$advanced_model_param <- renderUI(ui_advanced_model_param)
      }
    }
  }

  .defaultview <- function(){

  }

  .advancedview <- function(){

  }

  .basicview <- function(){

  }

  # Module Outout --------------------------------------------------------------

  moduleOutput <- c(
    list(
      ana_flag = reactive(result$ana_flag),
      ana_post_status = reactive(result$ana_post_status)
    )
  )


}

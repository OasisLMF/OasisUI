# step1_choosePortfolio Module -------------------------------------------------

# UI ---------------------------------------------------------------------------
#' step1_choosePortfolioUI
#'
#' @rdname step1_choosePortfolio
#'
#' @description UI/View for the step1_choosePortfolio.
#'
#' @return List of tags.
#'
#' @importFrom shinyjs hidden
#'
#' @export
step1_choosePortfolioUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script('
    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
        var el = $("#" + x);
        el.replaceWith(el = el.clone(true));
        var id = "#" + x + "_progress";
        var text = "#" + x + "-panel_pflink-body";
        var idBar = id + " .bar";
        $(id).css("visibility", "hidden");
        $(idBar).css("width", "0%");
    });
  '),
    # Section "Choose Portfolio" = "1"
    div(id = ns("panelPortfolioTable"), panelPortfolioTable(id)),
    hidden(div(id = ns("panelPortfolioDetails"), panelPortfolioDetails(id))),
    hidden(div(id = ns("panelDefinePortfolio"), panelDefinePortfolio(id))),
    hidden(div(id = ns("panelLinkFiles"), panelLinkFiles(id)))
  )
}

#' panelPortfolioTable
#'
#' @rdname panelPortfolioTable
#'
#' @description Function wrapping panel to show created portfolios table.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelPortfolioTable <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_portfolios"),
    heading = tagAppendChildren(
      h4("Portfolios table"),
      oasisuiRefreshButton(ns("abuttonprgtblrfsh"))
    ),
    DTOutput(ns("dt_Portfolios")),
    fluidRow(
      column(12,
             oasisuiTableButton(ns("abuttonamendpf"), "Amend Portfolio", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonamendpf, placement = "right"),
             oasisuiTableButton(ns("abuttondeletepf"), "Delete Portfolio", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttondeletepf, placement = "right"),
             oasisuiTableButton(ns("abuttonuploadsourcefiles"), "Upload Source Files", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonuploadsourcefiles, placement = "right"),
             oasisuiTableButton(ns("abuttonpfdetails"), "Show Source Files", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonpfdetails, placement = "right")
      )
    ),
    br(),
    fluidRow(
      column(12,
             oasisuiButton(ns("abuttoncreatepf"), "Create Portfolio", align = "centre"),
             actionButton(ns("abuttonpgotonextstep"), "Proceed to Choose Analysis", style = "float:right")
      ),
      style = "margin-top: 10px;"
    )
  )
}

#' panelPortfolioDetails
#'
#' @rdname panelPortfolioDetails
#'
#' @description Function wrapping panel to show details of portfolio.
#'
#' @template params-module-ui
#'
#' @importFrom DT DTOutput
#'
#' @export
panelPortfolioDetails <- function(id) {
  ns <- NS(id)
  portfolio_detailsUI(ns("portfolio_details"))
}

#' panelDefinePortfolio
#'
#' @rdname panelDefinePortfolio
#'
#' @description Function wrapping panel to create/amend portfolio.
#'
#' @template params-module-ui
#'
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
panelDefinePortfolio <- function(id) {
  ns <- NS(id)
  oasisuiPanel(
    collapsible = FALSE,
    ns("panel_pfdef"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_defPortfolio"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidedefpfpanel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
      column(12,
             textInput(inputId = ns("tinputpfName"), label = "Portfolio Name"),
             oasisuiButton(ns("abuttonpfsubmit"), "Submit") %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonpfsubmit, placement = "right"), style = "float:right"
      )
    )
  )
  # fluidRow(
  #   column(4,
  #          textInput(inputId = ns("tinputpfName"), label = "Portfolio Name")),
  #   br(),
  #   column(2,
  #          oasisuiButton(ns("abuttonpfsubmit"), "Submit") %>%
  #            bs_embed_tooltip(title = defineSingleAna_tooltips$abuttonpfsubmit, placement = "right"), style = "float:right"
  #   )
  # )
  # )
}


#' panelLinkFiles
#'
#' @rdname panelLinkFiles
#'
#' @description Function wrapping panel to link files to a portfolio
#'
#' @template params-module-ui
#'
#' @importFrom shinyjs hidden
#'
#' @export
panelLinkFiles <- function(id) {
  ns <- NS(id)

  oasisuiPanel(
    collapsible = FALSE,
    ns("panel_pflink"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_linkFiles"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelinkfilespanel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),

    fluidRow(
      # uiOutputs are used here because of the Clear button where they need to be reset.
      # Shiny does not provide an updatefileInput functionality, hence inputs cannot be reset unless the panel is re-rendered.
      # Source Location File
      column(3,
             uiOutput(ns("SLFile_ui"))),
      # Source Account File
      column(3,
             uiOutput(ns("SAFile_ui"))),

      # Source Reinsurance File
      column(3,
             uiOutput(ns("SRFile_ui"))),
      # Source Reinsurance Scope File
      column(3,
             uiOutput(ns("SRSFile_ui")))
    ),
    fluidRow(
      column(12,
             actionButton(inputId = ns("abuttonpfclear"), label = "Clear", style = "inline: true;float:right;")))
  )
}

# Server -----------------------------------------------------------------------
#' step1_choosePortfolio
#'
#' @rdname step1_choosePortfolio
#'
#' @description Server logic to step1_choosePortfolio
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-active
#'
#' @param currstep current selected step.
#' @param portfolioID selected portfolio ID.
#'
#' @return portfolioID Id of selected portfolioID
#' @return tbl_portfoliosData POData model association table.
#' @return newstep navigation step

#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT DTOutput
#' @importFrom DT selectPage
#' @importFrom dplyr select
#'
#' @export
step1_choosePortfolio <- function(input, output, session,
                                  active = reactive(TRUE),
                                  currstep = reactive(-1),
                                  portfolioID = reactive("")

) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  # number of rows per page in a datatable
  pageLength <- 5

  # values to stop ping pong effect. 2022-12 shouldn't be needed anymore.
  # stop_selPfID <- 0
  # check_selPfID <- 0

  # list of sub-modules
  sub_modules <- list()

  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # reactive for portfolioID
    portfolioID = "",
    # reactive for portfolio name
    portfolioName = "",
    # reactive value for the portfolio table
    tbl_portfoliosData = NULL,
    # flag to know if the user is creating or amending a portfolio
    portfolio_flag = "C",
    # accepted file extensions in file upload
    accepted_ext = NULL
  )

  # Set Params
  observe(if (active()) {
    # this was being isolated previously, but we would rather not have it that way (?)
    result$portfolioID <- portfolioID()
  })

  # Panels Visualization -------------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 1 ) {
      .defaultCreateProg()
      .reloadtbl_portfoliosData()
    }
  })

  # Enable/ Disable buttons ----------------------------------------------------

  # Enable and disable link files buttons

  # Enable and disable buttons
  observeEvent({
    result$portfolioID
    result$tbl_portfoliosData
    #currstep()
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    disable("abuttonpfdetails")
    disable("abuttondeletepf")
    disable("abuttonamendpf")
    disable("abuttonpgotonextstep")
    disable("abuttonuploadsourcefiles")
    result$accepted_ext = c('csv', 'comma-separated-values', '.csv', '.parquet')
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      enable("abuttonpfdetails")
      enable("abuttondeletepf")
      enable("abuttonamendpf")
      enable("abuttonuploadsourcefiles")
      # the row selection will only be updated at the end, so rather use the more complicated lookup
      #if (!is.null(result$tbl_portfoliosData) && result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$status] == Status$Completed) {
      if (!is.null(result$tbl_portfoliosData) && result$tbl_portfoliosData[result$tbl_portfoliosData[, tbl_portfoliosDataNames$id] == result$portfolioID, tbl_portfoliosDataNames$status] == Status$Completed) {
        enable("abuttonpgotonextstep")
      }
    }
  })

  ### Portfolio Table ----------------------------------------------------------
  output$dt_Portfolios <- renderDT({
    logMessage("re-rendering portfolio table")
    if (!is.null(result$tbl_portfoliosData)) {
      if (isolate(result$portfolioID) != "") {
        rowToSelect <- match(isolate(result$portfolioID), result$tbl_portfoliosData[, tbl_portfoliosDataNames$id])
      } else {
        rowToSelect <- 1
      }
      datatable(
        result$tbl_portfoliosData %>% capitalize_names_df(),
        class = "oasisui-table display",
        rownames = FALSE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = rowToSelect,
                         target = 'row'),
        options = getTableOptions(maxrowsperpage = pageLength)
      )
    } else {
      nothingToShowTable("No portfolio available")
    }
  })

  # Portfolio Details Table ----------------------------------------------------

  # Show Portfolio Details
  observeEvent(input$abuttonpfdetails, {
    hide("panelDefinePortfolio")
    hide("panelLinkFiles")
    show("panelPortfolioDetails")
    logMessage("showing panelPortfolioDetails")
  })

  sub_modules$portfolio_details <- callModule(
    portfolio_details,
    id = "portfolio_details",
    refresh_opt = FALSE,
    portfolioID = reactive({
      result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
    }),
    portfolioName = reactive({
      pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name]
      pfName <- ifelse(pfName == " ", "", paste0('"', pfName, '"'))
      pfName
    }),
    counter = reactive({input$abuttonpfdetails}),
    active = active
  )

  # Hide portfolio Details
  observeEvent(input$buttonhidepfdetails, {
    hide("panelPortfolioDetails")
    logMessage("hiding panelPortfolioDetails")
  })

  # Create / Amend portfolio sub-panel -----------------------------------------
  # Create/Amend portfolio title
  output$paneltitle_defPortfolio <- renderUI({
    if (result$portfolio_flag == "C" || is.null(input$dt_Portfolios_rows_selected)) {
      "Create portfolio"
    } else if (result$portfolio_flag == "A") {
      pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
      pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name]
      pfName <- ifelse(pfName == " ", "", paste0('"', pfName, '"'))
      paste0('Amend portfolio id ', pfId, ' ', pfName)
    }
  })

  # Hide portfolio Definition Panel
  observeEvent(input$abuttonhidedefpfpanel, {
    hide("panelDefinePortfolio")
  })

  # Create portfolio
  observeEvent(input$abuttoncreatepf, {
    hide("panelPortfolioDetails")
    hide("panelLinkFiles")
    result$portfolio_flag <- "C"
    #clear fields
    .clearPortfolioName()
    show("panelDefinePortfolio")
    logMessage("showing panelDefinePortfolio")
  })

  ### Amend portfolio
  observeEvent(input$abuttonamendpf, {
    hide("panelPortfolioDetails")
    .defaultCreateProg()
    # TODO: review where/when/how this should be set
    result$portfolio_flag <- "A"
    # clear fields
    .updatePortfolioName()
    show("panelDefinePortfolio")
    logMessage("showing panelDefinePortfolio")
  })

  # to enable and disable submit button for create portfolio
  observeEvent(input$tinputpfName, {
    if (is.null(input$tinputpfName) || input$tinputpfName == "") {
      disable("abuttonpfsubmit")
    } else {
      enable("abuttonpfsubmit")
    }
  })

  ### Submit Button
  observeEvent(input$abuttonpfsubmit, {
    idxSel <- 1
    pageSel <- 1
    if (result$portfolio_flag == "C") {
      post_portfolios <- session$userData$oasisapi$api_body_query(query_path = "portfolios", query_body = list(name = input$tinputpfName),
                                                                  query_method = "POST")
      if (post_portfolios$status == "Success") {
        oasisuiNotification(type = "message",
                            paste0("Portfolio ", input$tinputpfName, " created."))
        .clearUploadFiles()
        show("panelLinkFiles")
      } else {
        oasisuiNotification(type = "error",
                            paste0("Portfolio ", input$tinputpfName, " could not be created."))
      }
    } else if (result$portfolio_flag == "A") {
      idxSel <- input$dt_Portfolios_rows_selected
      pageSel <- ceiling(input$dt_Portfolios_rows_selected/pageLength)
      pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
      put_portfolios_id <- session$userData$oasisapi$api_body_query(query_path = paste("portfolios", pfId, sep = "/"), query_body = list(name = input$tinputpfName), query_method = "PUT")
      if (put_portfolios_id$status == "Success") {
        oasisuiNotification(type = "message",
                            paste0("Portfolio ", pfId, " updated."))
      } else {
        oasisuiNotification(type = "error",
                            paste0("Failed to amend a portfolio ", pfId, " could not be updated."))
      }
    }

    # Reload portfolio Table
    .reloadtbl_portfoliosData()
    logMessage(paste("updating dt_Portfolios select because portfolio table was reloaded"))
    if (result$portfolio_flag == "C") {
      result$portfolioID <- result$tbl_portfoliosData[1, tbl_portfoliosDataNames$id]
    }
    hide("panelDefinePortfolio")
  })

  # Delete portfolio -----------------------------------------------------------
  # title for delete button
  output$pfdelmodaltitle <- renderUI({
    pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name]
    paste0('Delete portfolio id ', pfId, ' "', pfName,'"')
  })

  # Modal dialog of delete button
  .pfdelmodal <- function() {
    ns <- session$ns
    modalDialog(label = "pfdelmodal",
                title = uiOutput(ns("pfdelmodaltitle"), inline = TRUE),
                paste0("Are you sure that you want to delete?"),
                footer = tagList(
                  oasisuiButton(ns("abuttonuconfirmdel"),
                                label = "Confirm", align = "center"),
                  actionButton(ns("abuttoncanceldel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # Delete portfolio
  observeEvent(input$abuttondeletepf,{
    showModal(.pfdelmodal())
  })

  # Confirm delete button
  observeEvent(input$abuttonuconfirmdel, {
    hide("panelPortfolioDetails")
    hide("panelDefinePortfolio")
    hide("panelLinkFiles")
    pfid <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name]
    delete_portfolios_id <- session$userData$oasisapi$api_delete_query(query_path = paste("portfolios", pfid, sep = "/"))
    if (delete_portfolios_id$status == "Success") {
      oasisuiNotification(type = "message",
                          paste0("Portfolio ", pfName, "deleted."))
    } else {
      oasisuiNotification(type = "error",
                          paste0("Portfolio ", pfName, " could not deleted."))
    }
    .reloadtbl_portfoliosData()
    removeModal()
  })

  # Cancel delete button
  observeEvent(input$abuttoncanceldel, {
    removeModal()
  })

  # Link files to portfolio ----------------------------------------------------

  # Show panel
  observeEvent(input$abuttonuploadsourcefiles, {
    .clearUploadFiles()
    hide("panelPortfolioDetails")
    hide("panelDefinePortfolio")
    show("panelLinkFiles")
  })

  # Clear panel
  observeEvent(input$abuttonpfclear, ignoreInit = TRUE, {
    # Re-setting fileInputs
    output$SLFile_ui <- renderUI({
      fileInput(inputId = ns("SLFile"), label = 'Location file:',
                accept = result$accepted_ext)
    })
    output$SAFile_ui <- renderUI({
      fileInput(inputId = ns("SAFile"), label = 'Account file:',
                accept = result$accepted_ext)
    })
    output$SRFile_ui <- renderUI({
      fileInput(inputId = ns("SRFile"), label = 'RI info file:',
                accept = result$accepted_ext)
    })
    output$SRSFile_ui <- renderUI({
      fileInput(inputId = ns("SRSFile"), label = 'RI scope file:',
                accept = result$accepted_ext)
    })
  })

  # Link files to portfolio title
  output$paneltitle_linkFiles <- renderUI({
    pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name]
    pfName <- ifelse(pfName == " ", "", paste0('"', pfName, '"'))
    if (result$portfolio_flag == "C") {
      paste0('Link input files to portfolio id ', pfId, ' ', pfName)
    } else {
      paste0('Amend input files to portfolio id ', pfId, ' ', pfName)
    }
  })

  # Rendering of fileInputs
  output$SLFile_ui <- renderUI({
    fileInput(inputId = ns("SLFile"), label = 'Location file:',
              accept = result$accepted_ext)
  })
  output$SAFile_ui <- renderUI({
    fileInput(inputId = ns("SAFile"), label = 'Account file:',
              accept = result$accepted_ext)
  })
  output$SRFile_ui <- renderUI({
    fileInput(inputId = ns("SRFile"), label = 'RI info file:',
              accept = result$accepted_ext)
  })
  output$SRSFile_ui <- renderUI({
    fileInput(inputId = ns("SRSFile"), label = 'RI scope file:',
              accept = result$accepted_ext)
  })

  observeEvent(input$SLFile, ignoreInit = TRUE, {
    .uploadSourceFile(inFile = input$SLFile, query_path = "location_file")
  })

  observeEvent(input$SAFile, ignoreInit = TRUE, {
    .uploadSourceFile(inFile = input$SAFile, query_path = "accounts_file")
  })

  observeEvent(input$SRFile, ignoreInit = TRUE, {
    .uploadSourceFile(inFile = input$SRFile, query_path = "reinsurance_info_file")
  })

  observeEvent(input$SRSFile, ignoreInit = TRUE, {
    .uploadSourceFile(inFile = input$SRSFile, query_path = "reinsurance_scope_file")
  })

  # Hide link files panel
  observeEvent(input$abuttonhidelinkfilespanel, {
    hide("panelLinkFiles")
  })

  # Define portfolioID ---------------------------------------------------------
  # Add choices to portfolioID, update portfolioID
  observeEvent(result$tbl_portfoliosData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste0("updating portfolioID choices because portfolio table was reloaded - contains ", nrow(result$tbl_portfoliosData), " rows"))
      if (result$portfolioID == "") {
        # initial selection last portfolio created
        pfID <- result$tbl_portfoliosData[1, tbl_portfoliosDataNames$id]
      } else {
        # keep current selection
        pfID <- result$portfolioID
      }
      result$portfolioID <- pfID
    }
  })

  # If portfolioID changes, reload programme model table and set view back to default
  observeEvent(result$portfolioID, ignoreInit = TRUE, {
    if (active()) {
      pfID <- ""

      if (!is.null(input$dt_Portfolios_rows_selected)) {
        pfID <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
      }

      if (result$portfolioID != pfID) {
        # bl_dirty <- stop_selPfID > check_selPfID
        # logMessage(paste("--- stop_selPfID is:", stop_selPfID))
        logMessage(paste("updating dt_Portfolios select because portfolioID changed to", result$portfolioID))
        if (result$portfolioID != "") {
          # if (!is.null(result$tbl_portfoliosData) && nrow(result$tbl_portfoliosData) > 0 && !bl_dirty) {
          if (!is.null(result$tbl_portfoliosData) && nrow(result$tbl_portfoliosData) > 0) {
            rowToSelect <- match(result$portfolioID, result$tbl_portfoliosData[, tbl_portfoliosDataNames$id])
            pageSel <- ceiling(rowToSelect/pageLength)
            # backward propagation
            if (is.null(input$dt_Portfolios_rows_selected)) {
              selectRows(dataTableProxy("dt_Portfolios"), rowToSelect)
              selectPage(dataTableProxy("dt_Portfolios"), pageSel)
              logMessage(paste("selected row is:", input$dt_Portfolios_rows_selected))
            } else if (rowToSelect != input$dt_Portfolios_rows_selected) {
              # re-selecting the same row would trigger event-observers on input$dt_Portfolios_rows_selected
              selectRows(dataTableProxy("dt_Portfolios"), rowToSelect)
              selectPage(dataTableProxy("dt_Portfolios"), pageSel)
              logMessage(paste("selected row is:", input$dt_Portfolios_rows_selected))
            }
          }
        } else {
          selectRows(dataTableProxy("dt_Portfolios"), NULL)
          selectPage(dataTableProxy("dt_Portfolios"), 1)
          logMessage(paste("selected row is:", input$dt_Portfolios_rows_selected))
        }
        # if (bl_dirty) check_selPfID <<- check_selPfID + 1
        # if (bl_dirty) check_selPfID <<- stop_selPfID
      }
    }
  })

  # Refresh Buttons ------------------------------------------------------------
  observeEvent(input$abuttonprgtblrfsh, {
    withModalSpinner(
      .reloadtbl_portfoliosData(),
      "Refreshing...",
      size = "s", t = 0.5
    )
  })

  # Updates dependent on changed: dt_Portfolios_rows_selected ------------------
  observeEvent(input$dt_Portfolios_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste("input$dt_Portfolios_rows_selected is changed to:", input$dt_Portfolios_rows_selected))
      hide("panelPortfolioDetails")
      hide("panelDefinePortfolio")
      hide("panelLinkFiles")

      if (length(input$dt_Portfolios_rows_selected) > 0) {
        # note that dt_Portfolios allows single row selection only
        pfID <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
        # if incomplete show panel to link files
        #currStatus <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$status]
        #if (!is.na(currStatus) && currStatus == Status$Processing) {
        # 287: re-show the panel to upload source files in any case (to avoid
        # having to re-open it when first uploading location file, which will lead
        # to the analysis status being ready)
          .clearUploadFiles()
          show("panelLinkFiles")
        #}
        if (pfID != result$portfolioID) {
          logMessage(paste("updating portfolioID because selection in portfolio table changed to", pfID))
          # re-selecting the same programme ID in the drop-down would not re-trigger
          # any of the observers of the drop-down, however we then also want to be
          # sure not to increase stop_selPfID!
          # stop_selPfID <<- stop_selPfID + 1
          # stop_selPfID <<- check_selPfID + 1
          result$portfolioID <- pfID
        }
      } else {
        result$portfolioID <- ""
      }
    }
  })

  # Help Functions -------------------------------------------------------------
  # hide all panels
  .hideDivs <- function() {
    logMessage(".hideDivs step1 called")
    # Section "Choose portfolio" = "1"
    hide("panelPortfolioTable")
    hide("panelPortfolioDetails")
    hide("panelDefinePortfolio")
    hide("panelLinkFiles")
  }

  #show default view for Section "Choose portfolio" = "1"
  .defaultCreateProg <- function(){
    logMessage(".defaultCreateProg called")
    show("panelPortfolioTable")
  }


  # Reload portfolio table
  .reloadtbl_portfoliosData <- function() {
    logMessage(".reloadtbl_portfoliosData called")
    result$tbl_portfoliosData <- session$userData$data_hub$return_tbl_portfoliosData(Status = Status, tbl_portfoliosDataNames = tbl_portfoliosDataNames)
    logMessage("portfolio table refreshed")
    invisible()
  }

  .clearPortfolioName <- function() {
    logMessage(".clearPortfolioName called")
    updateTextInput(session, "tinputpfName", value = "")
  }

  .clearUploadFiles <- function() {
    session$sendCustomMessage(type = "resetFileInputHandler", message =  session$ns("SLFile"))
    session$sendCustomMessage(type = "resetFileInputHandler", message =  session$ns("SAFile"))
    session$sendCustomMessage(type = "resetFileInputHandler", message =  session$ns("SRFile"))
    session$sendCustomMessage(type = "resetFileInputHandler", message =  session$ns("SRSFile"))
  }

  .updatePortfolioName <- function() {
    logMessage(".updatePortfolioName called")
    updateTextInput(session, "tinputpfName",
                    value = result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name])
  }

  # Upload Location/Account File
  .uploadSourceFile <- function(inFile, query_path) {
    logMessage(paste0("Uploading file ", inFile$datapath))
    if (!is.null(inFile$datapath)) {
      pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
      tmp <- unlist(strsplit(inFile$datapath, split = "/"))
      datapath <- paste(c(tmp[-length(tmp)], ""), collapse = "/")
      newfile <- paste0(datapath, inFile$name)
      file.rename(inFile$datapath, newfile)
      withModalSpinner(
        post_file <- session$userData$oasisapi$api_post_file_query(paste("portfolios", pfId, query_path, sep = "/"),  query_body = newfile),
        "Linking...",
        size = "s"
      )
      if (post_file$status == "Success") {
        oasisuiNotification(type = "message",
                            paste("File linked successfully."))
      } else {
        oasisuiNotification(type = "error",
                            paste("File link failed."))
      }
      # we do this reload to get the portfoliosData table updated (i.e. the status of the portfolio may have changed to "ready")
      .reloadtbl_portfoliosData()
      # panelLinkFiles gets hidden as part of the reload above but is shown again if the status of the portfolio is not "ready"
      # (i.e. if there is no linked location file yet)
      # we cannot change this by doing show("panelLinkFiles") here, it would happen before the reactive hiding above
    }
    invisible()
  }

  # Module output ---------------------------------------------------------------
  moduleOutput <- c(
    list(
      portfolioID = reactive(result$portfolioID),
      tbl_portfoliosData = reactive({result$tbl_portfoliosData}),
      newstep = reactive({input$abuttonpgotonextstep})
    )
  )

  moduleOutput
}

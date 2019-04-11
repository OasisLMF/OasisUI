# step1_choosePortfolio Module -------------------------------------------------

# UI ---------------------------------------------------------------------------
#' step1_choosePortfolioUI
#'
#' @rdname step1_choosePortfolio
#'
#' @description UI/View for the step1_choosePortfolio.
#'
#' @template params-module-ui
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
  flamingoPanel(
    collapsible = TRUE,
    show = TRUE,
    ns("panel_portfolios"),
    heading = tagAppendChildren(
      h4("Portfolios table"),
      flamingoRefreshButton(ns("abuttonprgtblrfsh"))
    ),
    DTOutput(ns("dt_Portfolios")),
    fluidRow(
      column(12,
             flamingoTableButton(ns("abuttonamendpf"), "Amend Portfolio", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttonamendpf, placement = "right"),
             flamingoTableButton(ns("abuttondeletepf"), "Delete Portfolio", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttondeletepf, placement = "right"),
             flamingoTableButton(ns("abuttonuploadsourcefiles"), "Upload Source Files", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttonuploadsourcefiles, placement = "right"),
             flamingoTableButton(ns("abuttonpfdetails"), "Show Source Files", align = "centre") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttonpfdetails, placement = "right")
      )
    ),
    br(),
    fluidRow(
      column(12,
             flamingoButton(ns("abuttoncreatepf"), "Create Portfolio", align = "centre"),
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
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_portfolio_details"),
    heading = tagAppendChildren(
      h4("Source files for portfolio"),
      uiOutput(ns("paneltitle_pfDetails"), inline = TRUE),
      flamingoRefreshButton(ns("abuttondefpfrfsh")),
      actionButton(inputId = ns("buttonhidepfdetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    #DTOutput(ns("dt_portfolioDetails"))
    ViewFilesInTableUI(id  = ns("portfolioDetails"), includechkbox = TRUE)
  )
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
  flamingoPanel(
    collapsible = FALSE,
    ns("panel_pfdef"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_defPortfolio"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidedefpfpanel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    fluidRow(
      column(4,
             textInput(inputId = ns("tinputpfName"), label = "Portfolio Name")),
      br(),
      column(2,
             flamingoButton(ns("abuttonpfsubmit"), "Submit") %>%
               bs_embed_tooltip(title = defineSingleAna$abuttonpfsubmit, placement = "right"), style = "float:right"
      )
    )
  )
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

  flamingoPanel(
    collapsible = FALSE,
    ns("panel_pflink"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_linkFiles"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelinkfilespanel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),

    fluidRow(
      # Source Location File
      column(3,
             fileInput(inputId = ns("SLFile"), label = 'Location file:', accept = c('csv', 'comma-separated-values', '.csv'))),
      column(2,
             flamingoButton(inputId = ns("abuttonSLFileUpload"), label = "Link File", align = "left", enable = FALSE, style = "margin-top: 25px;display-inline: true;")),
      # Source Account File
      column(3,
             fileInput(inputId = ns("SAFile"), label = 'Account file:',
                       accept = c('csv', 'comma-separated-values', '.csv'))),
      column(2,
             flamingoButton(inputId = ns("abuttonSAFileUpload"), label = "Link File", align = "left",
                            style = "margin-top: 25px;display-inline: true;"))),

    fluidRow(
      # Source Reinsurance File
      column(3,
             fileInput(inputId = ns("SRFile"), label = 'RI info file:',
                       accept = c('csv', 'comma-separated-values', '.csv'))),
      column(2,
             flamingoButton(inputId = ns("abuttonSRFileUpload"), label = "Link File", align = "left",
                            style = "margin-top: 25px;display-inline: true;")),
      # Source Reinsurance Scope File
      column(3,
             fileInput(inputId = ns("SRSFile"), label = 'RI scope file:',
                       accept = c('csv', 'comma-separated-values', '.csv'))),
      column(2,
             flamingoButton(inputId = ns("abuttonSRSFileUpload"), label = "Link File", align = "left",
                            style = "margin-top: 25px;display-inline: true;"))),
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
#' @template params-logMessage
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
#' @importFrom shinyjs onclick
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
                                  logMessage = message,
                                  currstep = reactive(-1),
                                  portfolioID = reactive("")

) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  #number of Rows per Page in a dataable
  pageLength <- 5

  #values to stop ping pong effect
  stop_selPfID <- check_selPfID <- 0

  # list of sub-modules
  sub_modules <- list()

  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # reactive for portfolioID
    portfolioID = "",
    # reactive value for the portfolio table
    tbl_portfoliosData = NULL,
    # SL file to load
    SLfile = NULL,
    # SA file to load
    SAfile = NULL,
    # SR file to load
    SRfile = NULL,
    # SA file to load
    SRSfile = NULL,
    # reactive value for details of portfolio table
    tbl_portfolioDetails = NULL,
    # flag to know if the user is creating or amending a portfolio
    portfolio_flag = "C"
  )

  #Set Params
  observe( if (active()) {
    result$portfolioID <- isolate(portfolioID())
  })

  # Panels Visualization -------------------------------------------------------
  observeEvent(currstep(), {
    .hideDivs()
    if (currstep() == 1 ){
      .defaultCreateProg()
      .reloadtbl_portfoliosData()
    }
  })

  # Enable/ Disable buttons ----------------------------------------------------

  # Enable and disable link files buttons
  #Location file
  observeEvent(result$SLFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    .enablingLinkButton(inFileId = "SLFile", abuttonId = "abuttonSLFileUpload")
  })

  #Account file
  observeEvent(result$SAFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    .enablingLinkButton(inFileId = "SAFile", abuttonId = "abuttonSAFileUpload")
  })

  #RI file
  observeEvent(result$SRFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    .enablingLinkButton(inFileId = "SRFile", abuttonId = "abuttonSRFileUpload")
  })

  #Reinsurance Scope file
  observeEvent(result$SRSFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    .enablingLinkButton(inFileId = "SRSFile", abuttonId = "abuttonSRSFileUpload")
  })


  # Enable and disable buttons
  observeEvent({
    result$portfolioID
    result$tbl_portfoliosData
    currstep()
  }, ignoreNULL = FALSE, ignoreInit = TRUE, {
    disable("abuttonpfdetails")
    disable("abuttondeletepf")
    disable("abuttonamendpf")
    disable("abuttonpgotonextstep")
    disable("abuttonuploadsourcefiles")
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      enable("abuttonpfdetails")
      enable("abuttondeletepf")
      enable("abuttonamendpf")
      enable("abuttonuploadsourcefiles")
      if (!is.null(result$tbl_portfoliosData) && result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$status] == Status$Completed) {
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
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = rowToSelect,
                         target = 'row'),
        colnames = c('row number' = 1),
        options = .getPRTableOptions()
      )
    } else {
      .nothingToShowTable(contentMessage = "No portfolio available")
    }
  })

  # Portfolio Details Table ----------------------------------------------------

  sub_modules$portfolioDetails <- callModule(
    ViewFilesInTable,
    id = "portfolioDetails",
    tbl_filesListData =  reactive(result$tbl_portfolioDetails),
    param = reactive(result$portfolioID),
    logMessage = logMessage,
    includechkbox = TRUE)

  # Title Portfolio Details Panel
  output$paneltitle_pfDetails <- renderUI({
    pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name]
    pfName <- ifelse(pfName == " ", "", paste0('"', pfName, '"'))
    paste0('details of portfolio id ', pfId, ' ', pfName)
  })


  # Show Portfolio Details
  onclick("abuttonpfdetails", {
    hide("panelDefinePortfolio")
    hide("panelLinkFiles")
    show("panelPortfolioDetails")
    logMessage("showing panelPortfolioDetails")
    .reloadtbl_portfolioDetails()
  })

  # Hide portfolio Details
  onclick("buttonhidepfdetails", {
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
  onclick("abuttonhidedefpfpanel", {
    hide("panelDefinePortfolio")
  })

  # Create portfolio
  onclick("abuttoncreatepf", {
    hide("panelPortfolioDetails")
    hide("panelLinkFiles")
    result$portfolio_flag <- "C"
    #clear fields
    .clearPortfolioName()
    show("panelDefinePortfolio")
    logMessage("showing panelDefinePortfolio")
  })

  ### Amend portfolio
  onclick("abuttonamendpf", {
    hide("panelPortfolioDetails")
    .defaultCreateProg()
    # TODO: review where/when/how this should be set
    result$portfolio_flag <- "A"
    #clear fields
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
  onclick("abuttonpfsubmit", {
    idxSel <- 1
    pageSel <- 1
    if (result$portfolio_flag == "C") {
      post_portfolios <- api_post_portfolios(input$tinputpfName)
      if (post_portfolios$status == "Success") {
        flamingoNotification(type = "message",
                             paste("portfolio ", input$tinputpfName, " created."))
        .clearUploadFiles()
        show("panelLinkFiles")
      } else {
        flamingoNotification(type = "Error",
                             paste("An error has occurred. Portfolio ", input$tinputpfName, " has not been created."))
      }
    } else if (result$portfolio_flag == "A") {
      idxSel <- input$dt_Portfolios_rows_selected
      pageSel <- ceiling(input$dt_Portfolios_rows_selected/pageLength)
      pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
      put_portfolios_id <- api_put_portfolios_id(id = pfId, name = input$tinputpfName)
      if (put_portfolios_id$status == "Success") {
        flamingoNotification(type = "message",
                             paste("portfolio ",pfId, " updated."))
      } else {
        flamingoNotification(type = "Error",
                             paste("Failed to amend a portfolio ", pfId, " has not been update."))
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
                  flamingoButton(ns("abuttonuconfirmdel"),
                                 label = "Confirm", align = "center"),
                  actionButton(ns("abuttoncanceldel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # Delete portfolio
  onclick("abuttondeletepf",{
    showModal(.pfdelmodal())
  })

  # onclick of confirm delete button
  onclick("abuttonuconfirmdel",{
    hide("panelPortfolioDetails")
    hide("panelDefinePortfolio")
    hide("panelLinkFiles")
    pfid <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$name]
    delete_portfolios_id <- api_delete_portfolios_id(pfid)
    if (delete_portfolios_id$status == "Success") {
      flamingoNotification(type = "message",
                           sprintf("portfolio %s deleted", pfName))
    } else {
      flamingoNotification(type = "error",
                           sprintf("An error occurred. Portfolio %s has not deleted", pfName))
    }
    .reloadtbl_portfoliosData()
    removeModal()
  })

  # onclick of cancel delete button
  onclick("abuttoncanceldel", {
    removeModal()
  })

  # Link files to portfolio ----------------------------------------------------

  #Show panel
  onclick("abuttonuploadsourcefiles", {
    .clearUploadFiles()
    hide("panelPortfolioDetails")
    hide("panelDefinePortfolio")
    show("panelLinkFiles")
  })

  #Clear panel
  observeEvent(input$abuttonpfclear, ignoreInit = TRUE, {
    .clearUploadFiles()
    result$SLFile <- NULL
    result$SAFile <- NULL
    result$SRFile <- NULL
    result$SRSFile <- NULL
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

  # Hide Link files Panel
  onclick("abuttonhidelinkfilespanel", {
    hide("panelLinkFiles")
  })

  # disable link files action button if input file widget is empty
  .enablingLinkButton <- function(inFileId, abuttonId) {
    inFile <- result[[inFileId]]
    if (is.null(inFile) || inFile == "") {
      disable(abuttonId)
    } else {
      enable(abuttonId)
    }
  }

  # Upload Location/Account File
  .uploadSourceFile <- function(inFile, APIfunction){
    logMessage(paste0("Uploading file ", inFile$datapath))
    pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
    if (!is.null(inFile$datapath)) {
      tmp <- unlist(strsplit(inFile$datapath, split = "/"))
      datapath <- paste(c(tmp[-length(tmp)], ""), collapse = "/")
      newfile <- paste0(datapath, inFile$name)
      file.rename(inFile$datapath, newfile)
      withModalSpinner(
        post_file <- APIfunction(pfId, newfile),
        "Linking...",
        size = "s"
      )
      if (post_file$status == "Success") {
        flamingoNotification(type = "message",
                             paste("File linked successfully"))
      } else {
        flamingoNotification(type = "error",
                             paste("File link failed."))
      }
      .reloadtbl_portfoliosData()
    } else {
      flamingoNotification(type = "warning",
                           paste("Select file to link"))
    }
  }

  observeEvent(input$SLFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    result$SLFile <- input$SLFile
  })

  onclick("abuttonSLFileUpload", {
    .uploadSourceFile(inFile = result$SLFile, api_post_portfolios_location_file)
  })

  observeEvent(input$SAFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    result$SAFile <- input$SAFile
  })

  onclick("abuttonSAFileUpload", {
    .uploadSourceFile(inFile = result$SAFile, api_post_portfolios_accounts_file)
  })

  observeEvent(input$SRFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    result$SRFile <- input$SRFile
  })

  onclick("abuttonSRFileUpload", {
    .uploadSourceFile(inFile = result$SRFile, api_post_portfolios_reinsurance_info_file)
  })

  observeEvent(input$SRSFile, ignoreNULL = FALSE, ignoreInit = TRUE, {
    result$SRSFile <- input$SRSFile
  })

  onclick("abuttonSRSFileUpload", {
    .uploadSourceFile(inFile = result$SRSFile, api_post_portfolios_reinsurance_source_file)
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
        bl_dirty <- stop_selPfID > check_selPfID
        logMessage(paste("--- stop_selPfID is:", stop_selPfID))
        logMessage(paste("updating dt_Portfolios select because portfolioID changed to", result$portfolioID))
        if (result$portfolioID != "") {
          if (!is.null(result$tbl_portfoliosData) && nrow(result$tbl_portfoliosData) > 0 && !bl_dirty ) {
            rowToSelect <- match(result$portfolioID, result$tbl_portfoliosData[, tbl_portfoliosDataNames$id])
            pageSel <- ceiling(rowToSelect/pageLength)
            #backward propagation
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
        if (bl_dirty) check_selPfID <<- check_selPfID + 1
      }
    }
  })

  # Refresh Buttons ------------------------------------------------------------
  onclick("abuttonprgtblrfsh", {
    .reloadtbl_portfoliosData()
  } )

  onclick("abuttondefpfrfsh", {
    .reloadtbl_portfolioDetails()
  } )

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
        # #If incomplete show panel to link files
        currStatus <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$status]
        if (!is.na(currStatus) && currStatus == Status$Processing) {
          .clearUploadFiles()
          show("panelLinkFiles")
        }
        if (pfID != result$portfolioID) {
          logMessage(paste("updating portfolioID because selection in portfolio table changed to", pfID))
          # re-selecting the same programme ID in the drop-down would not re-trigger
          # any of the observers of the drop-down, however we then also want to be
          # sure not to increase stop_selPfID!
          stop_selPfID <<- stop_selPfID + 1
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
    logMessage(".hideDivs called")
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
    result$tbl_portfoliosData <- return_tbl_portfoliosData()
    logMessage("portfolio table refreshed")
    invisible()
  }

  # Reload portfolio Details table
  .reloadtbl_portfolioDetails <- function() {
    logMessage(".reloadtbl_portfolioDetails called")
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosDataNames$id]
      result$tbl_portfolioDetails  <- return_tbl_portfolioDetails(pfId)
      logMessage("portfolio details table refreshed")
    } else {
      result$tbl_portfolioDetails  <- NULL
    }
    invisible()
  }

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

  # Model Outout ---------------------------------------------------------------

  moduleOutput <- c(
    list(
      portfolioID = reactive(result$portfolioID),
      tbl_portfoliosData = reactive({result$tbl_portfoliosData}),
      newstep = reactive({input$abuttonpgotonextstep})
    )
  )

  moduleOutput

}

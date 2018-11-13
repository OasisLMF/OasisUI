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
      h4("Portfolios Table"),
      actionButton(inputId = ns("abuttonprgtblrfsh"), label = "Refresh", style = "float: right;")
    ),
    DTOutput(ns("dt_Portfolios")),
    flamingoButton(ns("abuttoncreatepf"), "Create Portfolio", align = "centre"),
    flamingoButton(ns("abuttonamendpf"), "Amend Portfolio", align = "centre") %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonamendpf, placement = "right"),
    flamingoButton(ns("abuttondeletepf"), "Delete Portfolio", align = "centre") %>%
      bs_embed_tooltip(title = defineSingleAna$abuttondeletepf, placement = "right"),
    flamingoButton(ns("abuttonpfdetails"), "Show Details", align = "centre") %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonpfdetails, placement = "right"),
    actionButton(ns("abuttonpgotonextstep"), "Proceed to Choose Model", style = "float:right")
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
      h4("Details of Portfolio"),
      uiOutput(ns("paneltitle_pfDetails"), inline = TRUE),
      actionButton(inputId = ns("abuttondefpfrfsh"), label = "Refresh", style = "float: right;"),
      actionButton(inputId = ns("buttonhidepfdetails"), label = NULL, icon = icon("times"), style = "float: right;")
    ),
    DTOutput(ns("dt_portfolioDetails"))
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
      column(12, h4("Portfolio metadata"))),
    fluidRow(
      # column(4,
      #        selectizeInput(ns("sinputDPAccountName"), "Account Name", choices = c(""), selected = character(0),
      #                       options = list(
      #                         allowEmptyOption = TRUE,
      #                         placeholder = 'Select',
      #                         onInitialize = I('function() { this.setValue(""); }'))
      #        )),
      column(4,
             textInput(inputId = ns("tinputpfName"), label = "Portfolio Name")) #,
      # column(4,
      #        selectizeInput(ns("sinputTransformname"), "Transform Name", choices = c(""), selected = character(0),
      #                       options = list(
      #                         allowEmptyOption = TRUE,
      #                         placeholder = 'Select',
      #                         onInitialize = I('function() { this.setValue(""); }'))
      #        ) %>%
      #          bs_embed_tooltip(title = defineSingleAna$sinputTransformname,
      #                           placement = "right"))
      ),
    fluidRow(column(4,
                    flamingoButton(ns("abuttonpfsubmit"), "Submit")), style = "float:right") %>%
      bs_embed_tooltip(title = defineSingleAna$abuttonpfsubmit, placement = "right")
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
    ns("proglink"),
    heading = tagAppendChildren(
      h4(""),
      uiOutput(ns("paneltitle_linkFiles"), inline = TRUE),
      actionButton(inputId = ns("abuttonhidelinkfilespanel"), label = NULL, icon = icon("times"), style = "float: right;")
    ),

    fluidRow(
      column(12, h4("Link input files to portfolio"))),
    fluidRow(
      # Source Location File
      column(4,
             selectInput(inputId = ns("sinputSLFile"), label = "Source Location File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSLFileUpload"),
                        fileInput(inputId = ns("SLFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSLFileUpload"), label = "Upload File", align = "left", enable = FALSE))),
             hidden(div(id = ns("divSLFileSelect"),
                        selectInput(inputId = ns("sinputselectSLFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSLFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSLFileView"), label = "View", align = "left")))),
      ## Source Account File
      column(4,
             selectInput(inputId = ns("sinputSAFile"), label = "Source Account File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSAFileUpload"),
                        fileInput(inputId = ns("SAFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSAFileUpload"), label = "Upload File", align = "left"))),
             hidden(div(id = ns("divSAFileSelect"),
                        selectInput(inputId = ns("sinputselectSAFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSAFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSAFileView"), label = "View", align = "left"))))),
    fluidRow(
      column(12, h4("Link reinsurance input files to portfolio"))),
    fluidRow(
      ## Source Reinsurance File
      column(4,
             selectInput(inputId = ns("sinputSRFile"), label = "Source Reinsurance File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSRFileUpload"),
                        fileInput(inputId = ns("SRFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSRFileUpload"), label = "Upload File", align = "left"))),
             hidden(div(id = ns("divSRFileSelect"),
                        selectInput(inputId = ns("sinputselectSRFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSRFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSRFileView"), label = "View", align = "left")))),
      ## Source Reinsurance Scope File
      column(4,
             selectInput(inputId = ns("sinputSRSFile"), label = "Source Reinsurance Scope File", choices = c("Select" = "", "Upload New File" = "U", "Select existing file" = "S")),
             hidden(div(id = ns("divSRSFileUpload"),
                        fileInput(inputId = ns("SRSFile"), label = 'Choose a file to upload:', accept = c('csv', 'comma-separated-values', '.csv')),
                        flamingoButton(inputId = ns("abuttonSRSFileUpload"), label = "Upload File", align = "left"))),
             hidden(div(id = ns("divSRSFileSelect"),
                        selectInput(inputId = ns("sinputselectSRSFile"), label = "Select existing File", choices = ""),
                        flamingoButton(inputId = ns("abuttonSRSFileLink"), label = "Link", align = "left"),
                        flamingoButton(inputId = ns("abuttonSRSFileView"), label = "View", align = "left"))))
    ),
    fluidRow(
      column(12,
             actionButton(inputId = ns("abuttonpfclear"), label = "Clear", style = "inline: true;float:right;") #,
             # flamingoButton(inputId = ns("abuttonloadcanmodpr"), label = "Load Portfolio", style = "inline: true;float:right;margin-right: 10px;")
             ))
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
#' @template params-flamingo-module
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
                                  dbSettings,apiSettings, user,
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

  # > Reactive Values ---------------------------------------------------------
  result <- reactiveValues(
    # reactive for portfolioID
    portfolioID = "",
    # reactive value for the portfolio table
    tbl_portfoliosData = NULL,
    # SL file to view
    viewSLfile = NULL,
    # SA file to view
    viewSAfile = NULL,
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

  ### Portfolio Table ----------------------------------------------------------
  output$dt_Portfolios <- renderDT({
    logMessage("re-rendering portfolio table")
    if (!is.null(result$tbl_portfoliosData)) {
      if (isolate(result$portfolioID) != "") {
        rowToSelect <- match(isolate(result$portfolioID), result$tbl_portfoliosData[, tbl_portfoliosData.PortfolioID])
      } else {
        rowToSelect <- 1
      }
      datatable(
        result$tbl_portfoliosData, # %>% select(-c(tbl_portfoliosData.TranformID, tbl_portfoliosData.AccountID)),
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = list(mode = 'single',
                         selected = rowToSelect,
                         target = 'row'),
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions()
      )
    } else {
      .nothingToShowTable(contentMessage = "No portfolio available")
    }
  })

  # Portfolio Details Table ----------------------------------------------------
  output$dt_portfolioDetails <- renderDT({
    if (!is.null(result$tbl_portfolioDetails) && nrow(result$tbl_portfolioDetails) > 0) {
      logMessage("re-rendering portfolio details table")
      datatable(
        result$tbl_portfolioDetails,
        class = "flamingo-table display",
        rownames = TRUE,
        filter = "none",
        escape = FALSE,
        selection = "none",
        colnames = c('Row Number' = 1),
        options = .getPRTableOptions(pageLengthVal = 10) #details max 10 rows, no need for double page
      )
    } else {
      .nothingToShowTable(contentMessage = paste0("No files available for portfolio ID ", result$portfolioID))
    }
  })

  # Title Portfolio Details Panel
  output$paneltitle_pfDetails <- renderUI({
    pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName]
    pfName <- ifelse(pfName == " ", "", paste0('"', pfName, '"'))
    paste0('Details of Portfolio id ', pfId, ' ', pfName)
  })

  # Enable and disable buttons
  observeEvent({
    result$tbl_portfoliosData
    input$dt_Portfolios_rows_selected
    }, ignoreNULL = FALSE, ignoreInit = TRUE, {
      disable("abuttonpfdetails")
      disable("abuttondeletepf")
      disable("abuttonamendpf")
      disable("abuttonpgotonextstep")
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      enable("abuttonpfdetails")
      enable("abuttondeletepf")
      enable("abuttonamendpf")
      if (result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.Status] == StatusCompleted) {
        enable("abuttonpgotonextstep")
      }
    }
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
      "Create Portfolio"
    } else if (result$portfolio_flag == "A") {
      pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
      pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName]
      pfName <- ifelse(pfName == " ", "", paste0('"', pfName, '"'))
      paste0('Amend Portfolio id ', pfId, ' ', pfName)
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
    .clearDPAccountSelection()
    .clearProgrammeName()
    .clearSourceFilesSelection()
    # .clearTransformNameSelection()
    show("panelDefinePortfolio")
    logMessage("showing panelDefinePortfolio")
  })

  ### Amend portfolio
  onclick("abuttonamendpf", {
    hide("panelPortfolioDetails")
    .defaultCreateProg()
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      # TODO: review where/when/how this should be set
      result$portfolio_flag <- "A"
      #clear fields
      .updateDPAccountSelection()
      .updateProgrammeName()
      .clearSourceFilesSelection()
      # .updateTransformNameSelection()
      show("panelDefinePortfolio")
      show("panelLinkFiles")
      logMessage("showing panelDefinePortfolio")
    }
  })

  # to enable and disable submit button for create portfolio
  observeEvent({
    input$tinputpfName
    input$sinputDPAccountName
    # input$sinputTransformname
  }, ignoreInit = TRUE, {
    if (input$tinputpfName == "" || input$sinputDPAccountName == "" ) { # || input$sinputTransformname == ""
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
      # query <- paste0("exec dbo.createProg [", input$tinputpfName,
      #                 "],", input$sinputDPAccountName, ", [",
      #                 input$sinputTransformname, "]")
      res <- executeDbQuery(dbSettings, query)
      if (is.null(res)) {

      } else {
        flamingoNotification(type = "message",
                             paste("portfolio ", input$tinputpfName, " created."))
      }
    } else if (result$portfolio_flag == "A") { # && length(input$dt_Portfolios_rows_selected) > 0
      idxSel <- input$dt_Portfolios_rows_selected
      pageSel <- ceiling(input$dt_Portfolios_rows_selected/pageLength)
      # query <- paste0("exec dbo.updateProg ", result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID],
      #                 ",[", input$tinputpfName,"],", input$sinputDPAccountName,
      #                 ", [", input$sinputTransformname, "]")
      res <- executeDbQuery(dbSettings, query)
      message(paste("A res is:", res))
      if (is.null(res)) {
        flamingoNotification(type = "error",
                             paste("Failed to amend a portfolio - ", result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName]))
      } else {
        flamingoNotification(type = "message",
                             paste("Portfolio ", result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName], " amended."))
      }
    }

    # Reload portfolio Table
    .reloadtbl_portfoliosData()
    logMessage(paste("updating dt_Portfolios select because portfolio table was reloaded"))
    result$portfolioID <- result$tbl_portfoliosData[1, tbl_portfoliosData.PortfolioID]
    hide("panelDefinePortfolio")
    show("panelLinkFiles")
  })

  ### Clear portfolio Definition panel
  onclick("abuttonpfclear", {
    result$portfolio_flag <- "C"
    .clearDPAccountSelection()
    .clearProgrammeName()
    .clearSourceFilesSelection()
    # .clearTransformNameSelection()
  })

  # > Link files to portfolio --------------------------------------------------

  # Link files to portfolio title
  output$paneltitle_linkFiles <- renderUI({
    pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName]
    pfName <- ifelse(pfName == " ", "", paste0('"', pfName, '"'))
    if (result$portfolio_flag == "C") {
      paste0('Provide Inputs to portfolio id ', pfId, ' ', pfName)
    } else {
      paste0('Amend Inputs to portfolio id ', pfId, ' ', pfName)
    }

  })

  # Hide Link files Panel
  onclick("abuttonhidelinkfilespanel", {
    hide("panelLinkFiles")
  })

  # ### Load portfolio Button
  # onclick("abuttonloadcanmodpr", {
  #   pfId = result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
  #   logMessage(paste("loading programme - pfId is:", pfId))
  #   loadprogdata <- loadProgrammeData(apiSettings,
  #                                     pfId = toString(pfId))
  #   if (loadprogdata == 'success' || loadprogdata == 'Success') {
  #     flamingoNotification(type = "message", "Initiating load portfolio data...")
  #   } else {
  #     flamingoNotification(type = "error", "Failed to load portfolio data")
  #   }
  #   .reloadtbl_portfoliosData()
  #   .hideDivs()
  #   .defaultCreateProg()
  # })

  # title for delete button
  output$progdelmodal <- renderUI({
    pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
    pfName <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName]
    paste0('Delete portfolio id ', pfId, ' "', pfName,'"')
    })

  # Modal dialog of delete button
  .progdelmodal <- function() {
    ns <- session$ns
    modalDialog(label = "progdelmodal",
                title = uiOutput(ns("progdelmodal"), inline = TRUE),
                paste0("Are you sure you want to delete?"),
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
    showModal(.progdelmodal())
  })

  # onclick of confirm delete button
  onclick("abuttonuconfirmdel",{
    hide("panelPortfolioDetails")
    hide("panelDefinePortfolio")
    hide("panelLinkFiles")
    stmt <- buildDbQuery("deleteProg", result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID])
    executeDbQuery(dbSettings, stmt)
    flamingoNotification(type = "message",
                         sprintf("portfolio %s deleted", result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName]))
    .reloadtbl_portfoliosData()
    removeModal()
  })

  # onclick of cancel delete button
  onclick("abuttoncanceldel", {
    removeModal()
  })

  # > Source Files -------------------------------------------------------------
  # Upload Location/Account File
  .uploadSourceFile <- function(inFile, recordIdString, recordIdCode){
    #TODO API:
    flc <- getFileLocationPath(dbSettings, "Exposure File")
    flcopy <- file.copy(inFile$datapath, file.path(flc, inFile[1, 1]), overwrite = TRUE)
    logMessage(file.path(flc, inFile[1, 1]))
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      if (flcopy == TRUE) {
        recordId <- createFileRecord(
          dbSettings, inFile[1, 1], recordIdString, recordIdCode, flc, user(),
          "Prog", result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
        )
        if (!is.null(recordId)) {
          flamingoNotification(type = "message",
                               paste("New File record id: ", recordId, " created"))
          #.reloadtbl_portfolioDetails()
        } else {
          flamingoNotification(type = "error", "Could not create file record")
        }
      } else {
        flamingoNotification(type = "error", "File transfer failed")
      }
    }
  }

  onclick("abuttonSLFileUpload", {
    .uploadSourceFile(inFile = input$SLFile, recordIdString = "Source Loc File", recordIdCode = 101)
  })

  onclick("abuttonSAFileUpload", {
    .uploadSourceFile(inFile = input$SAFile, recordIdString = "Source Acc File", recordIdCode = 102)
  })

  onclick("abuttonSRFileUpload", {
    .uploadSourceFile(inFile = input$SRFile, recordIdString = "Source Reinsurance File", recordIdCode = 401)
  })

  onclick("abuttonSRSFileUpload", {
    .uploadSourceFile(inFile = input$SRSFile, recordIdString = "Source Reinsurance Scope File", recordIdCode = 402)
  })

  ### Link Location/Account File
  .linkSourceFile <- function(query, inputID) {
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      res <- executeDbQuery(dbSettings,
                            paste(query,
                                  inputID, ", ",
                                  result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]))
      if (inputID != "") {
        if (is.null(res)) {
          flamingoNotification(type = "error", "Failed to link the File")
        } else {
          flamingoNotification(type = "message",
                               paste("File linked to portfolio", result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName]))
        }
      } else {
        flamingoNotification(type = "warning", "Please select a file to Link")
      }
    }
  }


  onclick("abuttonSLFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceLocationFileForProg ", inputID = input$sinputselectSLFile)
  })

  onclick("abuttonSAFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceAccountFileForProg ", inputID = input$sinputselectSAFile)
  })

  onclick("abuttonSRFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceReinsuranceFileForProg ", inputID = input$sinputselectSRFile)
  })

  onclick("abuttonSRSFileLink", {
    .linkSourceFile(query = "exec dbo.updateSourceReinsuranceScopeFileForProg ", inputID = input$sinputselectSRSFile)
  })

  # File upload or select from dropdown
  # Location file
  observe(if (active()) {
    if (input$sinputSLFile == "U") {
      show("divSLFileUpload")
      disable("abuttonSLFileUpload")
      hide("divSLFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SLFile
      if (!is.null(inFile)) {
        enable("abuttonSLFileUpload")
      }
    } else if (input$sinputSLFile == "S") {
      show("divSLFileSelect")
      hide("divSLFileUpload")
      SLfiles <- getFileSourceLocationFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSLFile",
        choices = createSelectOptions(SLfiles, labelCol = 1, valueCol = 2)
      )
    }
  })

  # Account file
  observe(if (active()) {
    if (input$sinputSAFile == "U") {
      show("divSAFileUpload")
      disable("abuttonSAFileUpload")
      hide("divSAFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SAFile
      if (!is.null(inFile)) {
        enable("abuttonSAFileUpload")
      }
    } else if (input$sinputSAFile == "S") {
      show("divSAFileSelect")
      hide("divSAFileUpload")
      SAfiles <- getFileSourceAccountFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSAFile",
        choices = createSelectOptions(SAfiles, labelCol = 1, valueCol = 2)
      )
    }
  })

  # SR file
  observe(if (active()) {
    if (input$sinputSRFile == "U") {
      show("divSRFileUpload")
      disable("abuttonSRFileUpload")
      hide("divSRFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SRFile
      if (!is.null(inFile)) {
        enable("abuttonSRFileUpload")
      }
    } else if (input$sinputSRFile == "S") {
      show("divSRFileSelect")
      hide("divSRFileUpload")
      SRfiles <- getFileSourceReinsuranceFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSRFile",
        choices = createSelectOptions(SRfiles, labelCol = 1, valueCol = 2)
      )
    }
  })

  # SRS file
  observe(if (active()) {
    if (input$sinputSRSFile == "U") {
      show("divSRSFileUpload")
      disable("abuttonSRSFileUpload")
      hide("divSRSFileSelect")
      options(shiny.maxRequestSize = 1024*1024^2)
      inFile <- input$SRSFile
      if (!is.null(inFile)) {
        enable("abuttonSRSFileUpload")
      }
    } else if (input$sinputSRSFile == "S") {
      show("divSRSFileSelect")
      hide("divSRSFileUpload")
      SRSfiles <- getFileSourceReinsuranceScopeFile(dbSettings)
      updateSelectInput(
        session, "sinputselectSRSFile",
        choices = createSelectOptions(SRSfiles, labelCol = 1, valueCol = 2)
      )
    }
  })

  # View source files
  .modalviewSourcefile <- function(Label, Title, InputID) {
    ns <- session$ns
    modalDialog(label = Label,
                title = Title,
                DTOutput(ns(InputID)),
                size = "l",
                easyClose = TRUE
    )
  }

  .renderDTSourceFile <- function(SourceFile){
    renderDT({
      if (!is.null(SourceFile)) {
        logMessage("re-rendering source file")
        datatable(
          SourceFile,
          class = "flamingo-table display",
          rownames = TRUE,
          filter = "none",
          escape = FALSE,
          selection = list(mode = 'none'),
          colnames = c('Row Number' = 1),
          options = .getPRTableOptions()
        )
      } else {
        .nothingToShowTable(contentMessage = "nothing to show")
      }
    })
  }

  # Source Location
  onclick("abuttonSLFileView", {
    if (input$sinputselectSLFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSLfile", Title = "Source Location File View", InputID = "tableviewSLfile"))
    } else {
      removeModal()
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSLfile <- .renderDTSourceFile(SourceFile = result$viewSLfile)

  # Source Account
  onclick("abuttonSAFileView", {
    if (input$sinputselectSLFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSAfile", Title = "Source Account File View", InputID = "tableviewSAfile"))
    } else {
      removeModal()
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSAfile <- .renderDTSourceFile(SourceFile = result$viewSAfile)

  # Source Reinsurance
  onclick("abuttonSRFileView", {
    if (input$sinputselectSRFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSRfile", Title = "Source Reinsurance File View", InputID = "tableviewSRfile"))
    } else {
      removeModal()
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSRfile <- .renderDTSourceFile(SourceFile = result$viewSRfile)

  # Source Reinsurance Scope
  onclick("abuttonSRSFileView", {
    if (input$sinputselectSRSFile != "") {
      showModal(.modalviewSourcefile(Label = "modalviewSRSfile", Title = "Source Reinsurance Scope File View", InputID = "tableviewSRSfile"))
    } else {
      removeModal()
      flamingoNotification(type = "warning", "Please select source file")
    }
  })

  output$tableviewSRSfile <-  .renderDTSourceFile(SourceFile = result$viewSRSfile)

  # Define portfolioID ---------------------------------------------------------
  # Add choices to portfolioID, update portfolioID
  observeEvent(result$tbl_portfoliosData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (active()) {
      logMessage(paste0("updating portfolioID choices because programme table was reloaded - contains ", nrow(result$tbl_portfoliosData), " rows"))
      if (result$portfolioID == "") {
        # initial selection last portfolio created
        pfID <- result$tbl_portfoliosData[1, tbl_portfoliosData.PortfolioID]
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
        pfID <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
      }

      if (result$portfolioID != pfID) {
        bl_dirty <- stop_selPfID > check_selPfID
        logMessage(paste("--- stop_selPfID is:", stop_selPfID))
        logMessage(paste("updating dt_Portfolios select because portfolioID changed to", result$portfolioID))
        if (result$portfolioID != "") {
          if (!is.null(result$tbl_portfoliosData) && nrow(result$tbl_portfoliosData) > 0 && !bl_dirty ) {
            rowToSelect <- match(result$portfolioID, result$tbl_portfoliosData[, tbl_portfoliosData.PortfolioID])
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
        pfID <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
        # #If incomplete show panel to link files
        currStatus <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.Status]
        if (!is.na(currStatus) && currStatus == StatusProcessing) {
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
    # manual refresh button
    logMessage(".reloadtbl_portfoliosData called")
    # stmt <- buildDbQuery("getProgData")
    # tbl_portfoliosData <- executeDbQuery(dbSettings, stmt)
    tbl_portfoliosData <- return_tbl_portfoliosData()
    if (!is.null(tbl_portfoliosData)) {
      result$tbl_portfoliosData <- tbl_portfoliosData %>%
        replaceWithIcons()
      logMessage("portfolio table refreshed")
    }
    invisible()
  }

  # Reload portfolio Details table
  .reloadtbl_portfolioDetails <- function() {
    logMessage(".reloadtbl_portfolioDetails called")
    if (length(input$dt_Portfolios_rows_selected) > 0) {
      pfId <- result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioID]
      # stmt <- buildDbQuery("getProgFileDetails", pfId)
      # tbl_portfolioDetails <- executeDbQuery(dbSettings, stmt)
      # if (!is.null(tbl_portfolioDetails)) {
      #   result$tbl_portfolioDetails  <- tbl_portfolioDetails %>%
      #     replaceWithIcons()
      # }
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

  .clearDPAccountSelection <- function() {
    logMessage(".clearDPAccountSelection called")
    accounts <- getAccountName(dbSettings)
    updateSelectizeInput(session, "sinputDPAccountName",
                         choices = createSelectOptions(accounts),
                         selected = character(0))
  }

  .clearProgrammeName <- function() {
    logMessage(".clearProgrammeName called")
    updateTextInput(session, "tinputpfName", value = "")
  }

  .clearSourceFilesSelection <- function() {
    logMessage(".clearSourceFilesSelection called")
    updateSelectInput(session, "sinputSLFile", selected = "")
    updateSelectInput(session, "sinputSAFile", selected = "")
    updateSelectInput(session, "sinputSRFile", selected = "")
    updateSelectInput(session, "sinputSRSFile", selected = "")
    hide("divSLFileSelect")
    hide("divSLFileUpload")
    hide("divSAFileSelect")
    hide("divSAFileUpload")
    hide("divSRFileSelect")
    hide("divSRFileUpload")
    hide("divSRSFileSelect")
    hide("divSRSFileUpload")
  }

  # .clearTransformNameSelection <- function() {
  #   logMessage(".clearTransformNameSelection called")
  #   transforms <- getTransformNameSourceCan(dbSettings)
  #   updateSelectizeInput(session, "sinputTransformname",
  #                        choices = createSelectOptions(transforms,
  #                                                      labelCol = 1, valueCol = 2),
  #                        selected = character(0))
  # }

  .updateDPAccountSelection <- function() {
    logMessage(".updateDPAccountSelection called")
    accounts <- getAccountName(dbSettings)
    updateSelectizeInput(session, "sinputDPAccountName",
                         choices = createSelectOptions(accounts),
                         selected = toString(result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.AccountID]))
  }

  .updateProgrammeName <- function() {
    logMessage(".updateProgrammeName called")
    updateTextInput(session, "tinputpfName",
                    value = result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.PortfolioName])
  }

  # .updateTransformNameSelection <- function() {
  #   logMessage(".updateTransformNameSelection called")
  #   transforms <- getTransformNameSourceCan(dbSettings)
  #   updateSelectizeInput(session, "sinputTransformname",
  #                        choices = createSelectOptions(transforms,
  #                                                      labelCol = 1, valueCol = 2),
  #                        selected = toString(result$tbl_portfoliosData[input$dt_Portfolios_rows_selected, tbl_portfoliosData.TranformID]))
  # }

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

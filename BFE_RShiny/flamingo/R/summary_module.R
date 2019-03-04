# Summary Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title summarytab_ui
#'
#' @rdname summarytab
#'
#' @description UI/View of summary elements of an analysis.
#'
#' @return list of tags
#'
#' @importFrom plotly plotlyOutput
#'
#' @export
summarytabUI <- function(id) {

  ns <- NS(id)

  flamingoPanel(
    id = ns("flamingoPanelSummaryTable"),
    collapsible = FALSE,
    heading =  tagAppendChildren(
      h4("Summary table")
    ),

    fluidRow(
      column(6,
             div(
               id = ns("summarypanel"),
               h5("Inputs"),
               flamingoTableUI(ns("summaryInputTable")),
               h5("Parameters"),
               flamingoTableUI(ns("summaryParamsTable")),
               h5("Outputs"),
               flamingoTableUI(ns("summaryOutputTable"))
             )
      ),
      column(6,
             plotlyOutput(ns("summaryGULOutputPlot")),
             plotlyOutput(ns("summaryAALOutputPlot")),
             plotlyOutput(ns("summaryILOutputPlot")),
             plotlyOutput(ns("summaryRIOutputPlot"))
      )
    )
  )

}

# Server -----------------------------------------------------------------------

#' Analysis Summary Server
#' @title summarytab_server
#'
#' @rdname summarytab
#'
#' @description Server logic of summary elements of an analysis.
#'
#' @template params-module
#' @template params-logMessage
#' @template params-active
#' @param selectAnaID1 id of selected analysis
#' @param selectAnaID2 id of selected analysis
#' @param portfolioID1 portfolio id associated with selected analysis
#' @param portfolioID2 portfolio id associated with selected analysis
#' @param tbl_filesListDataana1 dataframe of the output files
#' @param compare logical indicating comparison
#'
#' @return list of tags
#'
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#'
#' @export
summarytab <- function(input, output, session,
                       selectAnaID1 = reactive(""),
                       selectAnaID2 = reactive(""),
                       portfolioID1 = reactive(""),
                       portfolioID2 = reactive(""),
                       model_perils_D = reactive(""),
                       tbl_filesListDataana1 = reactive(NULL),
                       compare = FALSE,
                       active, logMessage = message) {

  ns <- session$ns

  # Reactive Values & Params ---------------------------------------------------
  result <- reactiveValues(
    SummaryData = NULL
  )

  # list of sub-modules
  sub_modules <- list()

  # Extract Summary Data -------------------------------------------------------

  observeEvent({
    selectAnaID1()
    selectAnaID2()
    tbl_filesListDataana1()
    active()}, {

      # Initialize variables
      SummaryData1 <- NULL
      SummaryData2 <- NULL
      SummaryData <- NULL

      if (selectAnaID1() != "" && portfolioID1() != "" && !is.null(tbl_filesListDataana1())) {
        SummaryData1 <- .getSummary(selectAnaID1(), portfolioID1())
      }
      if (selectAnaID2() != "" && portfolioID2() != "" && !is.null(tbl_filesListDataana1())) {
        SummaryData2 <- .getSummary(selectAnaID2(), portfolioID2())
      }

      #define df to use
      if (compare) {
        if (!is.null(SummaryData1) && !is.null(SummaryData2)) {
          idx1 <- which(names(SummaryData1) == "Value")
          names(SummaryData1)[idx1] <- paste0("Analysis Id ", selectAnaID1())
          idx2 <- which(names(SummaryData2) == "Value")
          names(SummaryData2)[idx2] <- paste0("Analysis Id ", selectAnaID2())
          result$SummaryData <- left_join(SummaryData1, SummaryData2, by = c("Specification", "Type"))
        }
      } else {
        result$SummaryData <- SummaryData1
      }

      if (!is.null(result$SummaryData)) {
        show("summarypanel")
        show("summaryAALOutputPlot")
        show("summaryGULOutputPlot")
        show("summaryILOutputPlot")
        show("summaryRIOutputPlot")
      } else {
        hide("summarypanel")
        hide("summaryAALOutputPlot")
        hide("summaryGULOutputPlot")
        hide("summaryILOutputPlot")
        hide("summaryRIOutputPlot")
      }
    })

  # Output Summary Tables ------------------------------------------------------

  # Summary Input tables
  dt_dataInput <- reactive({
    if (!is.null(result$SummaryData)) {
      data <- result$SummaryData %>%
        filter(Type == "input")
      if (!is.null(data) && nrow(data) > 0) {
        data <- data %>%
          select(-Type)
      }
    } else {
      data <- NULL
    }
    data
  })

  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryInputTable",
    data = dt_dataInput,
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE,
    colnames = TRUE,
    maxrowsperpage = 10,
    logMessage = logMessage)

  dt_dataParam <- reactive({
    if (!is.null(result$SummaryData)) {
      data <- result$SummaryData %>%
        filter(Type == "param")
      if (!is.null(data) && nrow(data) > 0) {
        data <- data %>%
          select(-Type)
      }
    } else {
      data <- NULL
    }
    data
  })

  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryParamsTable",
    data = dt_dataParam,
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE,
    colnames = TRUE,
    maxrowsperpage = 10,
    logMessage = logMessage)

  dt_dataOutput <- reactive({
    if (!is.null(result$SummaryData)) {
      data <- result$SummaryData %>%
        filter(Type == "output") %>%
        transform(Value = as.numeric(Value)) %>%
        mutate_if(is.numeric, ~round(., 0))
      if (!is.null(data) && nrow(data) > 0) {
        data <- data %>%
          select(-Type)
      }
    } else {
      data <- NULL
    }
    data
  })

  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryOutputTable",
    data = dt_dataOutput,
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    rownames = FALSE,
    colnames = TRUE,
    maxrowsperpage = 10,
    logMessage = logMessage)


  # Plots ----------------------------------------------------------------------

  # AAL histogram
  output$summaryAALOutputPlot <- renderPlotly({
    if (!is.null(result$SummaryData)) {
    data <- result$SummaryData %>%
      filter(Type == "AALplot")
    } else {
      data <- NULL
    }
    p <- NULL
    if (!is.null(data) && nrow(data) > 0) {
      DFtype <- data.frame("Type" = {lapply(data$Specification, function(s) {
        gsub("Mean AAL ", "", s)
      }) %>%
          unlist()}
      ) %>%
        separate(Type,into = c("perspective", "type"), sep = " ")
      data <- cbind(data, DFtype)
      multipleplots <- FALSE
      if (compare) {
        colnames <- names(select(data, contains("analysis", ignore.case = TRUE)))
        data <- data %>% gather(key = "gridcol", value = "Value", colnames)
        multipleplots <- TRUE
      }
      data <- data  %>%
        rename("colour" = "Specification") %>%
        rename("xaxis" = "perspective") %>%
        rename("value" = "Value") %>%
        mutate(value = as.numeric(value) / 1000000)
      xlabel <- "Perspective"
      ylabel <- "Loss in Millions"
      titleToUse <- "AAL"

      p <- barPlot(xlabel, ylabel, titleToUse, data, multipleplots )
    }
    p
  })

  # OEP / AEP
  output$summaryGULOutputPlot <- renderPlotly({
    p <- NULL
    data <- .prepareDataLinePlot("gul")
    if (!is.null(data) && nrow(data) > 0) {
      xlabel <- "Return Period"
      ylabel <- "Loss in Millions"
      titleToUse <- "GUL EP Curve"
      p <- linePlot(xlabel, ylabel, titleToUse, data)
    }
    p
  })

  output$summaryILOutputPlot <- renderPlotly({
    p <- NULL
    data <- .prepareDataLinePlot("il")
    if (!is.null(data) && nrow(data) > 0) {
      xlabel <- "Return Period"
      ylabel <- "Loss in Millions"
      titleToUse <- "IL EP Curve"
      p <- linePlot(xlabel, ylabel, titleToUse, data)
    }
    p
  })

  output$summaryRIOutputPlot <- renderPlotly({
    p <- NULL
    data <- .prepareDataLinePlot("ri")
    if (!is.null(data) && nrow(data) > 0) {
      xlabel <- "Return Period"
      ylabel <- "Loss in Millions"
      titleToUse <- "RI EP Curve"
      p <- linePlot(xlabel, ylabel, titleToUse, data)
    }
    p
  })



  # Helper functions -----------------------------------------------------------
  .returnData <- function(id, tbl_filesListDataana, filepattern, nonkeycols, variables) {
    perspectives <- c("gul", "il", "ri")
    DFList <- list()
    for (p in 1:length(perspectives)) { #p <- 1
      for (v in 1:length(variables)) { #v <-1
        variable <- variables[v]
        fileName <- tbl_filesListDataana %>%
          filter(summary_level == "Portfolio") %>%
          filter(perspective == perspectives[p]) %>%
          filter(report == variable) %>%
          select(files)
        if (length(fileName$files) > 0) {
          output_file_df <- return_analyses_spec_output_file_df(id, fileName$files %>% as.character())
          if (!is.null(output_file_df)) {
            c <- length(DFList) + 1
            splitvar <- unlist(strsplit(variable, " "))
            var <- splitvar[length(splitvar)]
            DFList[[c]] <- output_file_df %>%
              gather(key = variable, value = value, -nonkeycols) %>%
              mutate(variable = paste0(variable, ".",var, ".",perspectives[p]))
          }
        }
      }
    }
    DF <- do.call(rbind, DFList)
    return(DF)
  }

  .getSummary <- function(selectAnaID, portfolioID) {

    #analyses settings
    analysis_settings <- return_analyses_settings_file_list(selectAnaID)

    #read aal files
    AAL <- .returnData(id = selectAnaID, tbl_filesListDataana =  tbl_filesListDataana1(), filepattern = "aalcalc", nonkeycols = c("summary_id", "type"), variables = c("AAL"))
    if (!is.null(AAL)) {
      #infer params
      tiv <- AAL %>%
        filter(grepl("exposure_value", variable)) %>%
        select(value) %>%
        unique() %>%
        as.character()
      #AAL output
      outputsAALtmp <- AAL %>%
        select(-c("summary_id")) %>%
        filter(grepl("mean", variable)) %>%
        separate(variable, into = c("variables", "report", "perspective"), sep = "\\.")
      outputsAALtmp <- outputsAALtmp %>%
        mutate(type = replace(type, type == "1", paste0("Mean AAL ", outputsAALtmp$perspective[outputsAALtmp$type == "1"], " (NI)"))) %>%
        mutate(type = replace(type, type == "2", paste0("Mean AAL ", outputsAALtmp$perspective[outputsAALtmp$type == "2"], " (Sample)")))
      outputsAAL <- data.frame("Specification" = outputsAALtmp$type, "Value" = outputsAALtmp$value, "Type" = rep("output", nrow(outputsAALtmp)), stringsAsFactors = FALSE)
      # AAL plot
      plotAALtmp <- data.frame("Specification" = outputsAALtmp$type, "Value" = outputsAALtmp$value, "Type" = rep("AALplot", nrow(outputsAALtmp)), stringsAsFactors = FALSE)
    } else {
      tiv <- 0
      outputsAAL <- NULL
      plotAALtmp <- NULL
    }
    #read OEP & aEP files
    leccalc <- .returnData(id = selectAnaID, tbl_filesListDataana =  tbl_filesListDataana1(), filepattern = "leccalc_full_uncertainty", nonkeycols = c("summary_id", "return_period"),variables = c("LEC Full Uncertainty AEP", "LEC Full Uncertainty OEP"))
    if (!is.null(leccalc)) {
      leccalc <- leccalc  %>%
        mutate(variable = paste0(variable, ".", return_period))
      plotleccalc <- data.frame("Specification" = leccalc$variable, "Value" = leccalc$value, "Type" = rep("leccalcplot", nrow(leccalc)), stringsAsFactors = FALSE)
    } else {
      plotleccalc <- NULL
    }
    #Location file
    Location <- return_file_df(api_get_portfolios_location_file, portfolioID)
    if (!is.null(Location)) {
      #infer params
      locnum <- length(unique(Location$LOCNUM))
    } else {
      locnum <- 0
    }

    gul_threshold <- analysis_settings[["analysis_settings"]][["gul_threshold"]]
    gul_threshold <- ifelse(is.null(gul_threshold), 0, gul_threshold)
    number_of_samples <- analysis_settings[["analysis_settings"]][["number_of_samples"]]
    number_of_samples <- ifelse(is.null(number_of_samples), 0, number_of_samples)
    event_set <- analysis_settings[["analysis_settings"]][["model_settings"]][["event_set"]]
    event_set <- ifelse(is.null(event_set), FALSE, event_set)
    peril_wind <- analysis_settings[["model_settings"]][["peril_wind"]]
    peril_wind <- ifelse(is.null(peril_wind), FALSE, peril_wind)
    peril_surge <- analysis_settings[["model_settings"]][["peril_surge"]]
    peril_surge <- ifelse(is.null(peril_surge), FALSE, peril_surge)
    peril_quake <- analysis_settings[["model_settings"]][["peril_quake"]]
    peril_quake <- ifelse(is.null(peril_quake), FALSE, peril_quake)
    peril_flood <- analysis_settings[["model_settings"]][["peril_flood"]]
    peril_flood <- ifelse(is.null(peril_flood), FALSE, peril_flood)
    demand_surge <- analysis_settings[["model_settings"]][["demand_surge"]]
    demand_surge <- ifelse(is.null(demand_surge), FALSE, demand_surge)
    leakage_factor <- analysis_settings[["model_settings"]][["leakage_factor"]]
    leakage_factor <- ifelse(is.null(leakage_factor), FALSE, leakage_factor)

    perils_list <- list("peril_wind", "peril_surge", "peril_quake", "peril_flood")
    PerilsNotInModel <- setdiff(perils_list, model_perils_D())

    #summary DF
    SpecificationRows <- c("exposure location count", "exposure TIV", "gul threshold", "number of samples", "event set", "peril_wind", "peril_surge", "peril_quake", "peril_flood", "demand_surge", "leakage_factor")
    ValueRows <- unlist(c(locnum, tiv, gul_threshold, number_of_samples, event_set, peril_wind, peril_surge, peril_quake, peril_flood, demand_surge, leakage_factor))
    TypeRows <- c("input", "input", "param", "param", "param", "param", "param", "param", "param", "param", "param")
    summary_df <- data.frame("Specification" = SpecificationRows, "Value" =  ValueRows, "Type" = TypeRows, stringsAsFactors = FALSE) %>%
                    filter(Specification %notin% PerilsNotInModel)

    #add AAL outputs
    if (!is.null(outputsAAL)) {
      summary_df <- rbind(summary_df, outputsAAL)
    }
    # add AAL plot
    if (!is.null(plotAALtmp)) {
      summary_df <- rbind(summary_df, plotAALtmp)
    }
    # add oep/aep plot output
    if (!is.null(plotleccalc)) {
      summary_df <- rbind(summary_df, plotleccalc)
    }

    return(summary_df)
  }

  .prepareDataLinePlot <- function(P){
    if (!is.null(result$SummaryData)) {
    data <- result$SummaryData %>%
      filter(Type == "leccalcplot")
    } else {
      data <- NULL
    }
    if (!is.null(data) && nrow(data) > 0) {
      if (compare) {
        data <- data %>%
          gather(key = "gridcol", value = "Value",colnames)
      }
      data <- data %>%
        separate(Specification, into = c("loss", "variable", "perspective", "returnperiod"), sep = "\\.") %>%
        mutate(returnperiod = as.numeric(returnperiod)) %>%
        mutate(variable = as.factor(variable))
      data <- data %>%
        filter(perspective == P)
      if (nrow(data) > 0 ) {
        data <- data %>%
          rename("colour" = "variable") %>%
          rename("xaxis" = "returnperiod") %>%
          rename("value" = "Value") %>%
          mutate(value = as.numeric(value) / 1000000)
      } else {
        data <- NULL
      }
    } else {
      data <- NULL
    }
    data
  }

  # Module output --------------------------------------------------------------
  invisible()
}

# Plot functions ---------------------------------------------------------------

#' basicplot structure
#' @title basicplot
#'
#' @rdname basicplot
#'
#' @description basic plot
#'
#' @param xlabel label x axis
#' @param ylabel label y axis
#' @param titleToUse plot title
#' @param data dataset to plot
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank

#' @export
#'
# Expected DF with columns:
# xaxis : column for aes x
# value : column for aes y
# colour : column for the aes col
# flag multipleplots generates grid over col gridcol

basicplot <- function(xlabel, ylabel, titleToUse, data){
  p <- ggplot(data, aes(x = xaxis, y = value)) +
    labs(title = titleToUse, x = xlabel, y = ylabel) +
    theme(
      plot.title = element_text(color = "grey45", size = 14, face = "bold.italic", hjust = 0.5),
      text = element_text(size = 12),
      panel.background = element_blank(),
      axis.line.x = element_line(color = "grey45", size = 0.5),
      axis.line.y = element_line(color = "grey45", size = 0.5),
      legend.title =  element_blank(),
      legend.position = "right"
    )
  p
}

#' barPlot structure
#' @title barPlot
#'
#' @rdname barPlot
#'
#' @description bar plot
#'
#' @param xlabel label X axis
#' @param ylabel label y axis
#' @param titleToUse title
#' @param data dataframe to plot
#' @param multipleplots flag about facets
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 facet_wrap
#'
#' @export
# Expected DF with columns:
# xaxis : column for aes x
# value : column for aes y
# colour : column for the aes col
# flag multipleplots generates grid over col gridcol

barPlot <- function(xlabel, ylabel, titleToUse, data, multipleplots){
  p <- basicplot(xlabel, ylabel, titleToUse, data) +
    geom_bar(position = "dodge", stat = "identity", aes(fill = as.factor(data$colour)))
    if (multipleplots) {
      p <- p + facet_wrap(.~ gridcol)
    }
  p
}


#' linePlot structure
#' @title linePlot
#'
#' @rdname linePlot
#'
#' @description line plot
#'
#' @param xlabel label X axis
#' @param ylabel label y axis
#' @param titleToUse title
#' @param data dataframe to plot
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#'
#' @export
# Expected DF with columns:
# xaxis : column for aes x
# value : column for aes y
# colour : column for the aes col
linePlot <- function(xlabel, ylabel, titleToUse, data){
  p <- basicplot(xlabel, ylabel, titleToUse, data) +
    geom_line(size = 1, aes(color = colour)) +
    geom_point(size = 2, aes(color = colour))
  p
}

# Summary Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title summarytab_ui
#'
#' @rdname summarytab
#'
#' @description UI/View of summary elements of an analysis.
#'
#' @template params-module-ui
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
      h4("Summary Table")
    ),

    fluidRow(
      column(6,
             div(
               id = ns("summarypanel"),
               h5("Inputs"),
               flamingoTableUI(ns("summaryInputTable")),
               h5("Parameters"),
               flamingoTableUI(ns("summaryParamsTable")),
               h5("Output"),
               flamingoTableUI(ns("summaryOutputTable"))
             ),
             plotlyOutput(ns("summaryAALOutputPlot"))
      ),
      column(6,
             plotlyOutput(ns("summaryGULOutputPlot")),
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
#' @template params-flamingo-module
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
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#'
#' @export
summarytab <- function(input, output, session, dbSettings,
                       apiSettings,
                       selectAnaID1 = reactive(""), 
                       selectAnaID2 = reactive(""),
                       portfolioID1 = reactive(""),
                       portfolioID2 = reactive(""),
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
    active()}, {
      
      # Initialize variables
      SummaryData1 <- NULL
      SummaryData2 <- NULL
      SummaryData <- NULL
      
      if (selectAnaID1() != "" && portfolioID1() != "") {
        SummaryData1 <- .getSummary(selectAnaID1(), portfolioID1())
      }
      if (selectAnaID2() != "" && portfolioID2() != "") {
        SummaryData2 <- .getSummary(selectAnaID2(), portfolioID2())
      }
      
      #define df to use
      if (compare) {
        if (!is.null(SummaryData1) && !is.null(SummaryData2)) {
          idx1 <- which(names(SummaryData1) == "Value")
          names(SummaryData1)[idx1] <- paste0("Analysis Id ", selectAnaID1())
          idx2 <- which(names(SummaryData2) == "Value")
          names(SummaryData2)[idx2] <- paste0("Analysis Id ", selectAnaID2())
          result$SummaryData <- left_join(SummaryData1, SummaryData2, by = c("SummaryType", "Type"))
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
  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryInputTable",
    data = reactive({result$SummaryData %>% 
        filter(Type == "input") %>%
        select(-Type)}),
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE,
    colnames = TRUE,
    maxrowsperpage = 10,
    logMessage = logMessage)

  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryParamsTable",
    data = reactive({result$SummaryData %>% 
        filter(Type == "param") %>%
        select(-Type)}),
    selection = "none",
    escape = TRUE,
    scrollX = FALSE,
    filter = FALSE,
    rownames = FALSE,
    colnames = TRUE,
    maxrowsperpage = 10,
    logMessage = logMessage)

  sub_modules$summaryTable <- callModule(
    flamingoTable,
    id = "summaryOutputTable",
    data = reactive({result$SummaryData %>%
        filter(Type == "output") %>%
        select(-Type)}),
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
    data <- result$SummaryData %>% 
      filter(Type == "AALplot")
    DFtype <- data.frame("Type" = {lapply(data$SummaryType, function(s) {
      gsub("Mean AAL ", "", s)
    }) %>% 
        unlist()}
    ) %>%
      separate(Type,into = c("perspective", "type"), sep = " ")
    data <- cbind(data, DFtype)
    multipleplots <- FALSE
    if (compare) {
      colnames <- names(select(data, contains("analysis", ignore.case = TRUE)))
      data <- data %>% gather(key = "gridcol", value = "Value",colnames)
      multipleplots <- TRUE
    }
    data <- data  %>%
      rename("colour" = "SummaryType") %>%
      rename("xaxis" = "perspective") %>%
      rename("value" = "Value") %>%
      mutate(value = as.numeric(value) / 1000000)
    xlabel <- "Sample Type"
    ylabel <- "Loss in Millions"
    titleToUse <- "AAL"
    
    p <- barPlot(xlabel, ylabel, titleToUse, data, multipleplots )
    
  })
  
  # OEP / AEP
  output$summaryGULOutputPlot <- renderPlotly({
    p <- NULL
    data <- .prepareDataLinePlot("gul")
    if (!is.null(data)) {
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
    if (!is.null(data)) {
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
    if (!is.null(data)) {
      xlabel <- "Return Period"
      ylabel <- "Loss in Millions"
      titleToUse <- "RI EP Curve"
      p <- linePlot(xlabel, ylabel, titleToUse, data) 
    }
    p
  })
  
  

  # Helper functions -----------------------------------------------------------
  .getData <- function(id, filepattern, nonkeycols, variables) {
    perspectives <- c("gul", "il", "ri")
    DFList <- list()
    for (p in 1:length(perspectives)) {
      for (v in 1:length(variables)) {
        if (variables[v] == "aal") {
          output_file_df <- return_analyses_spec_output_file_df(id, paste0(perspectives[p], "_S1_", filepattern, ".csv"))
        } else {
          output_file_df <- return_analyses_spec_output_file_df(id, paste0(perspectives[p], "_S1_", filepattern, "_", variables[v], ".csv"))
        }
        if (!is.null(output_file_df)) {
          c <- length(DFList) + 1
          DFList[[c]] <- output_file_df %>%
            gather(key = variable, value = value, -nonkeycols) %>% 
            mutate(variable = paste0(variable, ".", variables[v], ".",perspectives[p])) 
        }
      }
    }
    DF <- do.call(rbind, DFList)
    return(DF)
  }
  
  .getSummary <- function(selectAnaID, portfolioID) {
    #analyses settings
    analyses_settings <- return_analyses_settings_file_list(selectAnaID)
    #read aal files
    AAL <- .getData(id = selectAnaID, filepattern = "aalcalc", nonkeycols = c("summary_id", "type"), variables = c("aal"))
    #read OEP & aEP files 
    leccalc <- .getData(id = selectAnaID, filepattern = "leccalc_full_uncertainty", nonkeycols = c("summary_id", "return_period"), variables = c("aep", "oep")) %>% 
      mutate(variable = paste0(variable, ".", return_period)) 
    #Location file
    Location <- return_location_file_df(portfolioID)
    #infer params
    locnum <- length(unique(Location$LOCNUM))
    tiv <- AAL %>% 
      filter(grepl("exposure_value", variable)) %>% 
      select(value) %>%
      unique() %>%
      as.character()
    gul_threshold <- analyses_settings[["analysis_settings"]][["gul_threshold"]]
    gul_threshold <- ifelse(is.null(gul_threshold), 0, gul_threshold)
    number_of_samples <- analyses_settings[["analysis_settings"]][["number_of_samples"]]
    number_of_samples <- ifelse(is.null(number_of_samples), 0, number_of_samples)
    event_set <- analyses_settings[["analysis_settings"]][["model_settings"]][["event_set"]]
    event_set <- ifelse(is.null(event_set), FALSE, event_set)
    peril_wind <- analyses_settings[["analysis_settings"]][["model_settings"]][["peril_wind"]]
    peril_wind <- ifelse(is.null(peril_wind), FALSE, peril_wind)
    peril_surge <- analyses_settings[["analysis_settings"]][["model_settings"]][["peril_surge"]]
    peril_surge <- ifelse(is.null(peril_surge), FALSE, peril_surge)
    peril_quake <- analyses_settings[["analysis_settings"]][["model_settings"]][["peril_quake"]]
    peril_quake <- ifelse(is.null(peril_quake), FALSE, peril_quake)
    peril_flood <- analyses_settings[["analysis_settings"]][["model_settings"]][["peril_flood"]]
    peril_flood <- ifelse(is.null(peril_flood), FALSE, peril_flood)
    demand_surge <- analyses_settings[["analysis_settings"]][["model_settings"]][["demand_surge"]]
    demand_surge <- ifelse(is.null(demand_surge), FALSE, demand_surge)
    leakage_factor <- analyses_settings[["analysis_settings"]][["model_settings"]][["leakage_factor"]]
    leakage_factor <- ifelse(is.null(leakage_factor), FALSE, leakage_factor)
    #sumamry DF
    SummaryTypeRows <- c("exposure location count", "exposure TIV", "gul threshold", "number of samples", "event set", "peril_wind", "peril_surge", "peril_quake", "peril_flood", "demand_surge", "leakage_factor")
    ValueRows <- unlist(c(locnum, tiv, gul_threshold, number_of_samples, event_set, peril_wind, peril_surge, peril_quake, peril_flood, demand_surge, leakage_factor))
    TypeRows <- c("input", "input", "param", "param", "param", "param", "param", "param", "param", "param", "param")
    summary_df <- data.frame("SummaryType" = SummaryTypeRows, "Value" =  ValueRows, "Type" = TypeRows, stringsAsFactors = FALSE)
    #add AAL outputs
    outputsAALtmp <- AAL %>%
      select(-c("summary_id")) %>%
      filter(grepl("mean", variable)) %>%
      separate(variable, into = c("variables", "report", "perspective"), sep = "\\.")
    outputsAALtmp <- outputsAALtmp %>%
      mutate(type = replace(type, type == "1", paste0("Mean AAL ", outputsAALtmp$perspective[outputsAALtmp$type == "1"], " (NI)"))) %>%
      mutate(type = replace(type, type == "2", paste0("Mean AAL ", outputsAALtmp$perspective[outputsAALtmp$type == "2"], " (Sample)")))
    outputsAAL <- data.frame("SummaryType" = outputsAALtmp$type, "Value" = outputsAALtmp$value, "Type" = rep("output", nrow(outputsAALtmp)), stringsAsFactors = FALSE)
    summary_df <- rbind(summary_df, outputsAAL)
    # add AAL plot
    plotAALtmp <- data.frame("SummaryType" = outputsAALtmp$type, "Value" = outputsAALtmp$value, "Type" = rep("AALplot", nrow(outputsAALtmp)), stringsAsFactors = FALSE)
    summary_df <- rbind(summary_df, plotAALtmp)
    # add oep/aep plot output
    summary_df <- rbind(summary_df, data.frame("SummaryType" = leccalc$variable, "Value" = leccalc$value, "Type" = rep("leccalcplot", nrow(leccalc)), stringsAsFactors = FALSE))
    return(summary_df)
  }
  
  .prepareDataLinePlot <- function(P){
    data <- result$SummaryData %>% 
      filter(Type == "leccalcplot") 
    if (compare) {
      data <- data %>%
        gather(key = "gridcol", value = "Value",colnames) 
    }
    data <- data %>%
      separate(SummaryType, into = c("loss", "variable", "perspective", "returnperiod"), sep = "\\.") %>%
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

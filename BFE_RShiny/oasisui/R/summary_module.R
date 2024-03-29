# Summary Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title summarytab_ui
#'
#' @rdname summarytab
#'
#' @description UI/View of summary elements of an analysis.
#'
#' @return UI panel element.
#'
#' @importFrom plotly plotlyOutput
#'
#' @export
summarytabUI <- function(id) {

  ns <- NS(id)

  oasisuiPanel(
    id = ns("oasisuiPanelSummaryTable"),
    collapsible = FALSE,
    fluidRow(
      column(6,
             div(
               id = ns("summarypanel"),
               h4("Inputs"),
               oasisuiTableUI(ns("summaryInputTable")),
               h4("Parameters"),
               oasisuiTableUI(ns("summaryParamsTable")),
               h4("Outputs"),
               oasisuiTableUI(ns("summaryOutputTable"))
             )
      ),
      column(6,
             plotlyOutput(ns("summaryGULOutputPlot")),
             br(),
             plotlyOutput(ns("summaryAALOutputPlot")),
             br(),
             plotlyOutput(ns("summaryILOutputPlot")),
             br(),
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
#' @importFrom dplyr case_when
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#'
#' @export
summarytab <- function(input, output, session,
                       selectAnaID1 = reactive(NULL),
                       selectAnaID2 = reactive(NULL),
                       portfolioID1 = reactive(""),
                       portfolioID2 = reactive(""),
                       tbl_filesListDataana1 = reactive(NULL),
                       compare = FALSE,
                       active) {

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
      # SummaryData <- NULL

      if (!is.null(selectAnaID1()) && portfolioID1() != "" && !is.null(tbl_filesListDataana1())) {
        SummaryData1 <- .getSummary(selectAnaID1(), portfolioID1())
      }
      if (!is.null(selectAnaID2()) && portfolioID2() != "" && !is.null(tbl_filesListDataana1())) {
        SummaryData2 <- .getSummary(selectAnaID2(), portfolioID2())
      }

      # define df to use
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
      # insert commas at thousands
      data$Value <- add_commas(as.numeric(data$Value))
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
    oasisuiTable,
    id = "summaryInputTable",
    data = dt_dataInput,
    simple = TRUE)

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
    oasisuiTable,
    id = "summaryParamsTable",
    data = dt_dataParam,
    simple = TRUE)

  dt_dataOutput <- reactive({
    if (!is.null(result$SummaryData)) {
      data <- result$SummaryData %>%
        filter(Type == "output") %>%
        transform(Value = as.numeric(Value)) %>%
        mutate_if(is.numeric, ~round(., 0)) %>%
        transform(Value = as.character(Value))
      # insert commas at thousands
      data$Value <- add_commas(as.numeric(data$Value))
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
    oasisuiTable,
    id = "summaryOutputTable",
    data = dt_dataOutput,
    simple = TRUE)


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
      DFtype <- data.frame("Type" = {
        lapply(data$Specification, function(s) {
          gsub("AAL ", "", s)
        }) %>%
          unlist()
      }) %>%
        separate(Type, into = c("perspective", "type"), sep = " ")
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

      p <- barPlot(xlabel, ylabel, titleToUse, data, multipleplots)
    }
    p
  })

  # OEP / AEP
  output$summaryGULOutputPlot <- renderPlotly({
    p <- NULL
    data <- .prepareDataLinePlot("gul")
    if (!is.null(data) && nrow(data) > 0) {
      xlabel <- "RP"
      ylabel <- "Loss in Millions"
      if (unique(data$type) == 2) {
        titleToUse <- "GUL EP Curve - Sample"
      } else {
        titleToUse <- "GUL EP Curve - Analytical"
      }
      p <- linePlot(xlabel, ylabel, titleToUse, data)
    }
    p
  })

  output$summaryILOutputPlot <- renderPlotly({
    p <- NULL
    data <- .prepareDataLinePlot("il")
    if (!is.null(data) && nrow(data) > 0) {
      xlabel <- "RP"
      ylabel <- "Loss in Millions"
      if (unique(data$type) == 2) {
        titleToUse <- "IL EP Curve - Sample"
      } else {
        titleToUse <- "IL EP Curve - Analytical"
      }
      p <- linePlot(xlabel, ylabel, titleToUse, data)
    }
    p
  })

  output$summaryRIOutputPlot <- renderPlotly({
    p <- NULL
    data <- .prepareDataLinePlot("ri")
    if (!is.null(data) && nrow(data) > 0) {
      xlabel <- "RP"
      ylabel <- "Loss in Millions"
      if (unique(data$type) == 2) {
        titleToUse <- "RI EP Curve - Sample"
      } else {
        titleToUse <- "RI EP Curve - Analytical"
      }

      p <- linePlot(xlabel, ylabel, titleToUse, data)
    }
    p
  })


  # Helper functions -----------------------------------------------------------
  .returnData <- function(id, tbl_filesListDataana, filepattern, nonkeycols, variables) {
    perspectives <- c("gul", "il", "ri")
    DFList <- list()
    for (p in 1:length(perspectives)) {
      for (v in 1:length(variables)) {
        variable <- variables[v]
        fileName <- tbl_filesListDataana %>%
          filter(report == variable) %>%
          filter(summary_level == "All Risks") %>%
          filter(perspective == perspectives[p]) %>%
          select(files)
        if (length(fileName$files) > 0) {
          output_file_df <- session$userData$data_hub$get_ana_outputs_dataset_content(id, fileName$files %>% as.character())
          if (!is.null(output_file_df)) {
            c <- length(DFList) + 1
            splitvar <- unlist(strsplit(variable, " "))
            var <- splitvar[length(splitvar)]
            DFList[[c]] <- output_file_df %>%
              gather(key = variable, value = value, -nonkeycols) %>%
              mutate(variable = paste0(variable, ".", var, ".", perspectives[p]))
          }
        }
      }
    }
    DF <- do.call(rbind, DFList)
    return(DF)
  }

  .getSummary <- function(selectAnaID, portfolioID) {
    # TODO: check overlap of helper functions and plots part here with code in outputplots_module.R
    # analysis settings
    analysis_settings <- session$userData$data_hub$get_ana_settings_content(selectAnaID)
    # read AAL files
    AAL <- .returnData(id = selectAnaID, tbl_filesListDataana =  tbl_filesListDataana1(),
                       filepattern = "aalcalc", nonkeycols = c("summary_id", "type"), variables = c("AAL"))
    if (!is.null(AAL)) {
      # AAL output
      outputsAALtmp <- AAL %>%
        select(-c("summary_id")) %>%
        filter(grepl("mean", variable)) %>%
        separate(variable, into = c("variables", "report", "perspective"), sep = "\\.")
      outputsAALtmp <- outputsAALtmp %>%
        mutate(type = replace(type, type == "1", paste0("AAL ", outputsAALtmp$perspective[outputsAALtmp$type == "1"], " (Analytical)"))) %>%
        mutate(type = replace(type, type == "2", paste0("AAL ", outputsAALtmp$perspective[outputsAALtmp$type == "2"], " (Sample)")))
      outputsAAL <- data.frame("Specification" = outputsAALtmp$type, "Value" = outputsAALtmp$value, "Type" = rep("output", nrow(outputsAALtmp)), stringsAsFactors = FALSE)
      # AAL plot
      plotAALtmp <- data.frame("Specification" = outputsAALtmp$type, "Value" = outputsAALtmp$value, "Type" = rep("AALplot", nrow(outputsAALtmp)), stringsAsFactors = FALSE)
    } else {
      outputsAAL <- plotAALtmp <- NULL
    }

    # read OEP & AEP files
    leccalc <- .returnData(id = selectAnaID, tbl_filesListDataana =  tbl_filesListDataana1(), filepattern = "leccalc_full_uncertainty",
                           nonkeycols = c("summary_id", "return_period", "type"),
                           variables = c("LEC Full Uncertainty AEP", "LEC Full Uncertainty OEP"))
    # REF: make more general, less-hard coded field names e.g.(return_period and others above)
    if (!is.null(leccalc)) {
      leccalc <- leccalc  %>%
        mutate(variable = paste(variable, type, return_period, sep = "."))
      plotleccalc <- data.frame("Specification" = leccalc$variable,
                                "Value" = leccalc$value,
                                "Type" = rep("leccalcplot", nrow(leccalc)), stringsAsFactors = FALSE)
    } else {
      plotleccalc <- NULL
    }
    # Location file
    Location <- session$userData$data_hub$get_pf_location_content(id = portfolioID)
    if (!is.null(Location)) {
      # infer params
      names(Location) <- tolower(names(Location))
      locnum <- length(unique(Location$locnumber))
    } else {
      locnum <- 0
    }

    model_settings <- analysis_settings$model_settings
    model_params_lst <- sapply(model_settings, function(i) {
      paste(i, collapse = ", ")
    })
    modelID <- session$userData$oasisapi$api_return_query_res(
      query_path = paste("analyses", selectAnaID, sep = "/"),
      query_method = "GET"
    )[["model"]]
    tbl_lookup <- session$userData$oasisapi$api_return_query_res(
      query_path = paste("models", modelID, "settings", sep = "/"),
      query_method = "GET"
    )[["lookup_settings"]]
    if (!is.null(tbl_lookup$supported_perils)) {
      # extract the whole files with locations and perils
      locations_all <- check_loc(selectAnaID, portfolioID, data_hub = session$userData$data_hub)
      names(locations_all) <- tolower(names(locations_all))
      # distinguish for unique location
      locs_unique <- distinct(locations_all, locnumber, .keep_all = TRUE)
      # filter by peril
      mod_locations_filter <- lapply(seq(1, length(tbl_lookup$supported_perils)), function(x) {
        perils_id <- tbl_lookup$supported_perils[[x]][["id"]]
        nrow(locs_unique %>% filter(peril_id == perils_id))
      })
      # sum over all unique locations filtered by perils
      mod_locations <- do.call(sum, mod_locations_filter)
    } else {
      mod_locations <- 0
    }

    exposure_rep <- session$userData$data_hub$get_ana_dataset_content(
      id = selectAnaID,
      dataset_identifier = "exposure_summary_report.json",
      type = "input"
    )

    # if total entry is not available, return NA in table
    if (is.null(exposure_rep$total$portfolio$tiv)) {
      perils_tiv <- "NA"
    } else {
      perils_tiv <- exposure_rep$total$portfolio$tiv
    }
    # summary DF
    SpecificationRows <- c("exposure location count", "total TIV", "modelled locations", names(model_settings))
    ValueRows <- unlist(c(locnum, perils_tiv, mod_locations, model_params_lst))
    TypeRows <- c("input", "input", "input", rep("param", length(model_settings)))
    summary_df <- data.frame("Specification" = SpecificationRows, "Value" =  ValueRows, "Type" = TypeRows, stringsAsFactors = FALSE) %>%
      mutate(Specification = gsub(pattern = "_", replacement = " ", x = Specification))

    # add AAL outputs
    if (!is.null(outputsAAL)) {
      summary_df <- rbind(summary_df, outputsAAL)
    }
    # add AAL plot
    if (!is.null(plotAALtmp)) {
      summary_df <- rbind(summary_df, plotAALtmp)
    }
    # add OEP/AEP plot output
    if (!is.null(plotleccalc)) {
      summary_df <- rbind(summary_df, plotleccalc)
    }
    return(summary_df)
  }

  .prepareDataLinePlot <- function(P) {
    if (!is.null(result$SummaryData)) {
      data <- result$SummaryData %>%
        filter(Type == "leccalcplot")
    } else {
      data <- NULL
    }
    if (!is.null(data) && nrow(data) > 0) {
      if (compare) {
        data <- data %>%
          gather(key = "gridcol", value = "Value", colnames)
      }
      data <- data %>%
        separate(Specification, into = c("loss", "variable", "perspective", "type", "returnperiod"), sep = "\\.") %>%
        mutate(returnperiod = as.numeric(returnperiod),
               type = as.numeric(type),
               variable = as.factor(variable)) %>%
        filter(perspective == P,
               type == max(type))
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

#obsolete functions

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
#' @param group Used to create line plot with factors.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#'
#' @export
#'
# Expected DF with columns:
# xaxis : column for aes x
# value : column for aes y
# colour : column for the aes col
# flag multipleplots generates grid over col gridcol

basicplot <- function(xlabel, ylabel, titleToUse, data, group) {
  p <- ggplot(data, aes(x = xaxis, y = value, group = group)) +
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
  Perspective <- data$xaxis
  Loss <- add_commas(data$value * 1000000)

  p <- basicplot(xlabel, ylabel, titleToUse, data, group = Loss) +
    # below throws warnings about unknown aesthetics, which are however used by plotly to prettify the mouse-over labels
    geom_bar(position = "dodge", stat = "identity", aes(fill = colour, prsp = Perspective, loss = Loss))

  if (multipleplots) {
    p <- p + facet_wrap(.~ gridcol)
  }
  ggplotly(p, tooltip = c("colour", "prsp", "loss"))
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
#' @importFrom scales comma
#'
#' @export
# Expected DF with columns:
# xaxis : column for aes x
# value : column for aes y
# colour : column for the aes col
linePlot <- function(xlabel, ylabel, titleToUse, data) {
  RP <- add_commas(data$xaxis)
  #convert value back to the full length from Millions scale
  Loss <- add_commas(data$value * 1000000)
  Report <- data$colour
  data$xaxis <- as.factor(add_commas(data$xaxis))

  p <- basicplot(xlabel, ylabel, titleToUse, data, group = Report) +
    # below throws warnings about unknown aesthetics, which are however used by plotly to prettify the mouse-over labels
    geom_point(size = 2, aes(color = Report, return = RP, loss = Loss), guides = FALSE) +
    geom_line(size = 1, aes(color = colour))

  ggplotly(p, tooltip = c("colour", "return", "loss"))
}

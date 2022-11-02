# outputplots Module -----------------------------------------------------------

# UI ---------------------------------------------------------------------------

#' outputplotsUI
#'
#' @rdname outputplots
#'
#' @description UI/View for output plots of a run.
#'
#' @return List of tags.
#'
#' @export
outputplotsUI <- function(id) {

  ns <- NS(id)

  oasisuiIncrementalPanelUI(
    id = ns("oasisuiIncrementalPanelOutput-0"),
    heading = "New plot",
    collapsible = FALSE, show = FALSE, removable = FALSE)

}

# Server -----------------------------------------------------------------------

#' outputplots
#'
#' @rdname outputplots
#'
#' @description Server logic for outputplots of a run.
#'
#' @template params-module
#' @template params-active
#' @param selectPortfID ID of selected portfolio.
#' @param selectAnaID ID of selected analysis.
#' @param n_panels Number of panels.
#' @param filesListData Table of output files for a given analysis ID.
#'
#' @export
outputplots <- function(input, output, session,
                        selectPortfID,
                        selectAnaID,
                        n_panels,
                        filesListData = reactive(NULL),
                        active) {

  ns <- session$ns

  # list of sub-modules
  sub_modules <- list()

  #incremental panels -----------------------------------------------------------
  panel_IDs <- paste0("oasisuiIncrementalPanelOutput-", seq_len(n_panels))
  content_IDs <- paste0("oasisuiIncrementalPanelOutputcontent-", seq_len(n_panels))
  plotPanels <- callIncrementalPanelModules(
    panel_IDs, "oasisuiIncrementalPanelOutput-0", content_IDs,
    panelOutputModuleUI,
    headings = lapply(seq_len(n_panels), function(i) {
      oasisuiPanelHeadingOutput(ns(paste0("paneltitle", i)))
    }),
    collapsible = TRUE, show = TRUE, ns = ns
  )
  plotsubmodules <- lapply(seq_len(n_panels), function(i) {
    callModule(panelOutputModule, content_IDs[i],
               filesListData =  reactive(filesListData()),
               portfId = selectPortfID,
               anaID = selectAnaID,
               active = reactive(plotPanels$state[[ns(panel_IDs[i])]]))
  })
  lapply(seq_along(plotsubmodules), function(i) {
    output[[paste0("paneltitle", i)]] <- renderOasisuiPanelHeading(plotsubmodules[[i]]())
  })

  observeEvent({
    selectAnaID()
    filesListData()}, {
      plotPanels$remove_all()
    })

  return(invisible())
}


# panelOutputModule Module -----------------------------------------------------

# UI ---------------------------------------------------------------------------
#' panelOutputModuleUI
#'
#' @rdname panelOutputModule
#'
#' @importFrom shinyWidgets panel
#' @importFrom shinyjs hidden
#' @importFrom plotly plotlyOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shinyjs hidden
#'
#' @export
panelOutputModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    oasisuiPanel(
      id = ns("oasisuiPanelOutputModule"),
      collapsible = TRUE,
      heading = "Custom plot",
      fluidRow(
        column(12,
               selectInput(inputId = ns("inputplottype"),
                           label = "Plot type",
                           choices = names(plottypeslist),
                           selected = names(plottypeslist)[1])
        )
      ),
      fluidRow(
        column(6,
               checkboxGroupInput(inputId = ns("chkboxgrplosstypes"),
                                  label = "Perspective",
                                  choices = output_options$losstypes,
                                  inline = TRUE)),
        column(6,
               uiOutput(ns("types_ui")))
      ),
      fluidRow(
        column(6,
               uiOutput(ns("reports_ui"))),
        column(6,
               selectInput(
                 inputId = ns("pltrtnprd"),
                 label = "RP",
                 choices = c(),
                 selected = NULL
               )),
        column(6,
               uiOutput(ns("summary_levels_ui")))
      ),
      br(),
      fluidRow(
        column(3,
               textInput(ns("textinputtitle"), "Title", "")),
        column(4,
               checkboxInput(ns("chkboxmillions"), "Y axis in Millions", TRUE)),
        column(3,
               hidden(checkboxInput(ns("chkboxuncertainty"), "Include Uncertainty", FALSE))),
        oasisuiButton(inputId = ns("abuttondraw"), label = "Draw Plot",  style = "float:right")
      )
    ),

    panel(
      hidden(plotlyOutput(ns("outputplot")))
      ,
      hidden(leafletOutput(ns("outputleaflet")))
    )
  )
}

# Server -----------------------------------------------------------------------

#' panelOutputModule
#'
#' @rdname panelOutputModule
#'
#' @description Server logic to show graphical output such as plots.
#'
#' @template params-module
#' @template params-active
#'
#' @param filesListData Table of output files for a given anaID.
#' @param portfId Portfolio ID.
#' @param anaID Analysis ID.
#'
#' @return Reactive value of the title.
#'
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr intersect
#' @importFrom dplyr between
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' @importFrom tidyr spread
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 label_value
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet awesomeIcons
#' @importFrom leaflet setView
#' @importFrom leaflet addAwesomeMarkers
#' @importFrom leaflet addCircles
#' @importFrom leaflet.extras addFullscreenControl
#' @importFrom stats quantile
#'
#' @export
panelOutputModule <- function(input, output, session,
                              portfId,
                              anaID,
                              filesListData = reactive(NULL), active) {

  ns <- session$ns

  # Reactive values & parameters -----------------------------------------------

  result <- reactiveValues(
    #plot and panel title
    Title = "",
    SumLevel = character(0),
    Losstypes = character(0),
    Report = character(0)
  )

  # reactive values holding checkbox state
  chkbox <- list(
    chkboxgrplosstypes = reactiveVal(NULL),
    pltreports = reactiveVal(NULL),
    pltsummarylevels = reactiveVal(NULL)
  )

  lapply(names(isolate(chkbox)), function(id) {
    observe(chkbox[[id]](input[[id]]))
  })

  #reactive triggered by the existence of the input$plottype and the changes in the data. It hoplds the selected plottype
  inputplottype <- reactive(if (active()) {
    filesListData()
    input$inputplottype
  })

  # Clean up objects------------------------------------------------------------
  #clean up panel objects when inactive
  observe(if (!active()) {
    result$Title <- ""
    result$SumLevel <- character(0)
    result$Losstypes <- character(0)
    result$Report <- character(0)
    # plotlyOutput persists to re-creating the UI
    output$outputplot <- renderPlotly(NULL)
    for (id in names(chkbox)) chkbox[[id]](NULL)
  })

  # > based on analysis ID -----------------------------------------------------
  #Gather the Summary Levels, Reports and Losstypes based on the anaID output presets
  observe(if (active()) {
    if (!is.null(filesListData())) {
      result$SumLevel <- unique(filesListData()$summary_level)
      result$Losstypes <- toupper(unique(filesListData()$perspective))
      result$Report <- unique(filesListData()$report)
    } else {
      result$SumLevel <- character(0)
      result$Losstypes <-  character(0)
      result$Report <-  character(0)
    }
  })

  observeEvent({
    inputplottype()
    result$Losstypes
  }, ignoreNULL = FALSE, {
    if (!is.null(inputplottype())) {
      .reactiveUpdateSelectGroupInput(result$Losstypes,
                                      output_options$losstypes, "chkboxgrplosstypes",
                                      inputplottype())
    }
  })

  # > based on inputs ----------------------------------------------------------

  observeEvent({inputplottype()
    input$chkboxgrplosstypes}, {
      result$Title <- ""
      output$outputplot <- renderPlotly(NULL)
      if (length(plottypeslist[[inputplottype()]]$uncertaintycols) > 0) {
        show("chkboxuncertainty")
      } else {
        updateCheckboxInput(session = session, inputId = "chkboxuncertainty", value = FALSE)
        hide("chkboxuncertainty")
      }

      if (inputplottype() == "loss for return period map" && length(input$chkboxgrplosstypes) > 1) {
        showModal(modalDialog(
          title = "Attention",
          "Only one perspective is allowed for this plot selection",
          footer = tagList(
            modalButton("Ok")
          )
        ))
        disable("abuttondraw")
        # updateCheckboxGroupInput(session = session, inputId = "chkboxgrplosstypes", selected = NULL)
      } else if (length(input$chkboxgrplosstypes) == 1) {
        enable("abuttondraw")
      }

    })

  observeEvent({anaID()
    inputplottype()}, {
      output$outputleaflet <- NULL
      # Update selectInputs based on the choice of plots, for AAL bar plot only AAL will be displayed
      if (inputplottype() == "loss for return period map") {
        filesToPlot <- filesListData() %>% filter(grepl("locnumber", summary_level))
        filesToPlot <- filesToPlot$files[-grep("summary-info", filesToPlot$files)][1]
        data <- .readFile(filesToPlot)
        loc_num <- filesListData()$report[which(grepl("locnumber", filesListData()$summary_level))]
        # display only reports that contain locnumber in list of summary levels
        # without Summary Info and ALL
        loc_num_filter <- loc_num[-which(loc_num == "Summary Info")]

        updateSelectInput(session = session, inputId = "pltrtnprd", choices = unique(data$return_period))

        show("pltrtnprd")
        hide("pltsummarylevels")
        hide("chkboxmillions")
        hide("textinputtitle")
        if ("AAL" %in% loc_num_filter) {
          loc_num_filter <- loc_num_filter[-which(loc_num_filter == "AAL")]
        }
        idx_r <- which(filesListData()$report == unique(loc_num_filter))
        multiple <- FALSE
      } else if(inputplottype() == "loss per return period line plot") {
        hide("pltrtnprd")
        show("pltsummarylevels")
        show("chkboxmillions")
        show("textinputtitle")
        idx_r <- which(filesListData()$report %in% plottypeslist$`loss per return period line plot`$Variables)
        multiple <- TRUE
      } else {
        hide("pltrtnprd")
        show("pltsummarylevels")
        show("textinputtitle")
        idx_r <- which(filesListData()$report == plottypeslist$`AAL bar plot`$Variables)
        multiple <- TRUE
      }

      idx_r_unlist <- unlist(idx_r)
      report <- lapply(idx_r_unlist, function(x) {filesListData()$report[x]})

      output$reports_ui <- renderUI({
        selectInput(
          inputId = ns("pltreports"),
          label = "Report",
          choices = unique(report),
          selected = NULL,
          multiple = multiple
        )
      })

      # remove summary info file from list as it does not have a type
      # Retrieve types from API
      types_list <- unique(unlist(lapply(idx_r_unlist, function(x) {
        session$userData$data_hub$get_ana_outputs_dataset_content(
          id = anaID(),
          dataset_identifier = filesListData()$files[[x]])$type

      })
      ))

      # replace type 1 and 2 with Analytical and Sample resplectively
      types_list <- types_list %>% replace(which(types_list == 1), "Analytical")
      if (length(which(types_list == 2)) != 0) {
        types_list <- types_list %>% replace(which(types_list == 2), "Sample")
      }
      # sort list in descending alphabetical order
      types_list <- sort(types_list)

      output$types_ui <- renderUI({
        selectInput(
          inputId = ns("calctypes"),
          label = "Type",
          choices = types_list,
          selected = tail(types_list, n = 1),
          multiple = multiple
        )
      })
    })

  observeEvent({input$pltreports
    input$pltrtnprd}, ignoreNULL = FALSE, {
      if (length(inputplottype()) != 0 && inputplottype() != "loss for return period map") {
        # display only summary levels that correspond to the selected report
        idx_s <- which(filesListData()$report == input$pltreports)
        summary_level <- filesListData()$summary_level[idx_s]
        output$summary_levels_ui <- renderUI({
          selectInput(
            inputId = ns("pltsummarylevels"),
            label = "Summary Levels",
            choices = unique(summary_level)
          )
        })
      } else {
        if (!is.null(filesListData())) {
          # filter by perspective
          if (!is.null(input$chkboxgrplosstypes)) {
            filesListData <- filesListData() %>% filter(perspective == tolower(input$chkboxgrplosstypes))
          } else {
            filesListData <- filesListData() %>% filter(perspective == "gul")
          }

          if (length(grep("locnumber", filesListData$summary_level)) > 0) {
            filesToPlot <- filesListData %>% filter(grepl("locnumber", filesListData$summary_level))
            filesToPlot <- filesToPlot$files[-grep("summary-info", filesToPlot$files)]
            # filter by report
            if (length(filesToPlot) > 1) {
              if (grepl("aep", tolower(input$pltreports))) {
                filesToPlot <- filesToPlot[grep("aep", filesToPlot)]
              } else {
                filesToPlot <- filesToPlot[grep("oep", filesToPlot)]
              }
            }

            # TODO: review for composite summary levels
            data <- .readFile(filesToPlot[1])
            # set up by type
            data$type <- data$type %>% replace(which(data$type == 1), "Analytical")
            if (length(which(data$type == 2)) != 0) {
              data$type <- data$type %>% replace(which(data$type == 2), "Sample")
            }
            data <- data %>%
              filter(return_period %in% as.numeric(input$pltrtnprd)) %>%
              filter(type == input$calctypes)
            output$summary_levels_ui <- renderUI({
              sliderInput(ns("pltlosses"), "Losses",
                          min = round(min(data$loss)), max = round(max(data$loss)),
                          value = c(round(min(data$loss)), round(max(data$loss))),
                          step = 1000
              )
            })
          } else {
            output$summary_levels_ui <- renderUI({
              sliderInput(ns("pltlosses"), "Losses",
                          min = 0, max = 0,
                          value = 0)
            })
          }
        }
      }
    })

  observeEvent({
    chkbox$chkboxgrplosstypes()
    inputplottype()
  }, ignoreNULL = FALSE, {
    #if losstype = GUL then policy inactive
    if ("GUL" %in% chkbox$chkboxgrplosstypes()) {
      # TODO: GUL does not have policy, more feedback required for development
      SumLevel <- result$SumLevel[which(result$SumLevel != "Policy")]
    } else {
      SumLevel <- result$SumLevel
    }
  })

  # > button based on selection
  observeEvent({
    chkbox$chkboxgrplosstypes()
    chkbox$pltreports()
    chkbox$pltsummarylevels()
    input$calctypes
    input$pltrtnprd
    inputplottype()
  }, ignoreNULL = FALSE, {
    if (length(inputplottype()) != 0) {
      if (inputplottype() != "loss for return period map" && (length(chkbox$chkboxgrplosstypes()) > 0 &&
                                                              length(input$calctypes) > 0 &&
                                                              length(chkbox$pltsummarylevels()) > 0 &&
                                                              length(chkbox$pltreports()) > 0)) {
        enable("abuttondraw")
        show("outputplot")
        hide("outputleaflet")
      } else if (inputplottype() == "loss for return period map" &&
                 (input$pltrtnprd != "" &&
                  input$calctypes != "") &&
                 (length(input$pltrtnprd) > 0 &&
                  length(input$calctypes) > 0 &&
                  length(chkbox$chkboxgrplosstypes()) == 1)) {
        enable("abuttondraw")
        hide("outputplot")
        show("outputleaflet")
      } else if (length(chkbox$chkboxgrplosstypes()) > 1 && length(chkbox$pltreports()) > 1) {
        # if more than one checkbox and report are selected, pop up message and disable "Draw" btn
        showModal(modalDialog(
          title = "Attention!",
          "Only two reports or two perspectives can be selected. Please remove one in either field",
          footer = modalButton("Dismiss"),
          easyClose = TRUE
        ))
        disable("abuttondraw")
      } else {
        disable("abuttondraw")
      }
    }
  })

  # Extract dataframe to plot --------------------------------------------------
  #Logic to filter the files to plot
  #Missing logic in case either variables or granularities are not selected. For the moment not allowed
  observeEvent(input$abuttondraw, {

    # > print current selection
    logMessage(paste0("Plotting ", inputplottype(),
                      " for loss types: ", chkbox$chkboxgrplosstypes(),
                      ", variables: ", input$pltreports,
                      ", granularities: ", input$pltsummarylevels
                      # ", aggregated to Portfolio Level: ", input$chkboxaggregate
    ))

    # > Setup ------------------------------------------------------------------
    # >> clear data
    # Content to plot
    fileData <- NULL
    # List of files to plot
    filesToPlot <- NULL
    # ggplot friendly dataframe to plot
    data <- NULL
    # DF indicating structure of the plot
    plotstrc <- data.frame("Loss" = NULL, "Variable" = NULL, "Granularity" = NULL)
    # single plot or grid
    multipleplots = FALSE

    # >> Plot parameters
    key <- plottypeslist[[inputplottype()]]$keycols
    uncertainty <- plottypeslist[[inputplottype()]]$uncertaintycols
    # 277: this referencecols feature seems unused now, possibly this was added
    # in early days to have both analytical and sample means in one AAL plot?
    reference <- plottypeslist[[inputplottype()]]$referencecols
    keycols <- c(key, uncertainty, reference)
    x <- plottypeslist[[inputplottype()]]$x
    xtickslabels <- plottypeslist[[inputplottype()]]$xtickslabels
    suffix <- c("perspective", "report", "summary_level" )
    extracols <- plottypeslist[[inputplottype()]]$extracols
    xlabel <- plottypeslist[[inputplottype()]]$xlabel
    ylabel <- plottypeslist[[inputplottype()]]$ylabel
    plottype <- plottypeslist[[inputplottype()]]$plottype

    # >> sanity checks
    # something must be selected
    # only one granularity is allowed
    # we can compare either multi-variables or multi-losstypes
    l_losstypes <- length(chkbox$chkboxgrplosstypes())
    l_variables <- length(input$pltreports)
    l_granularities <- length(input$pltsummarylevels)
    sanytyChecks <- FALSE
    if (l_losstypes > 1 && l_variables > 1) {
      oasisuiNotification(type = "error",
                          "Only comparisons among perspectives or reports are allowed.")
    } else {
      logMessage("Sanity checks passed")
      sanytyChecks <- TRUE
      # >> define plot structure
      plotstrc <- data.frame("perspective" = c(l_losstypes), "report" = c(l_variables), "summary_level" = c(l_granularities))
    }

    # >> define dynamic default title
    if (sanytyChecks) {
      if (input$textinputtitle != "") {
        result$Title <- input$textinputtitle
      } else {
        if (l_losstypes > 1) {
          result$Title <- input$pltreports
        } else if (l_variables > 1) {
          result$Title <- chkbox$chkboxgrplosstypes()
        } else {
          result$Title <- paste0(input$pltreports, " of ", chkbox$chkboxgrplosstypes())
        }
      }
    }

    # > filter out files to read -----------------------------------------------
    if (sanytyChecks) {
      if (!is.null(filesListData()) & nrow(plotstrc) > 0 ) {
        if (inputplottype() == "loss for return period map") {
          filesToPlot <- filesListData() %>% filter(grepl("locnumber", summary_level)) %>%
            filter(perspective %in% tolower(chkbox$chkboxgrplosstypes()),
                   report %in% input$pltreports)
          # filter(grepl("uncertainty", files))
        } else {
          filesToPlot <- filesListData() %>% filter(perspective %in% tolower(chkbox$chkboxgrplosstypes()),
                                                    report %in% input$pltreports,
                                                    summary_level %in% input$pltsummarylevels)
          if (nrow(filesToPlot) != prod(plotstrc)) {
            oasisuiNotification(type = "error",
                                "The analysis did not produce the selected output. Please check the logs.")
            filesToPlot <- NULL
          }
        }
      }
    }

    # > read files to plot -----------------------------------------------------
    if (!is.null(filesToPlot)) {
      for (i in seq(nrow(filesToPlot))) {
        currfileData <- .readFile(filesToPlot$files[i])
        if (nrow(currfileData) > 0) {
          # Change column names for joining by adding an extension representing the losstype, the variable or the granularity to compare
          nonkey <- names(currfileData)[ !(names(currfileData) %in% keycols)]
          gridcol <- names(currfileData)[ !(names(currfileData) %in% keycols) &
                                            !(names(currfileData) %in% extracols) &
                                            !(names(currfileData) %in% x)]
          if (any(which(plotstrc > 1))) {
            extension <- filesToPlot[i, suffix[which(plotstrc > 1)]] # losstype or variable
          } else {
            extension <- filesToPlot[i, suffix[3]] # granularity
          }
          for (k in keycols) {
            newnamekey <- paste0(k, ".", extension)
            names(currfileData)[names(currfileData) == k] <- newnamekey
          }
          # Join data
          if (is.null(fileData)) {
            fileData <- currfileData
          } else {
            fileData <- left_join(fileData, currfileData, by = nonkey )
          }
        } else {
          fileData <- NULL
        }
      }
    }

    # Make ggplot friendly -----------------------------------------------------
    if (!is.null(fileData)) {
      data <- fileData
      data$type <- data$type %>% replace(which(data$type == 1), "Analytical")
      if (length(which(data$type == 2)) != 0) {
        data$type <- data$type %>% replace(which(data$type == 2), "Sample")
      }
      data <- data %>% filter(type %in% input$calctypes)

      if (inputplottype() == "loss for return period map") {
        if (TRUE %in% grepl("locnumber", filesListData()$summary_level)) {
          # filter for values related to locnumber
          # TODO: introduce if statement in case return_period is not there
          data <- data %>% filter(return_period == input$pltrtnprd)
        }
      } else {
        data <- data %>% gather(key = variables, value = value, -nonkey) %>%
          separate(variables, into = c("variables", "selection"), sep = "\\.") %>%
          spread(variables, value)

        summary_id_mapfile <- filesListData() %>% filter(grepl("summary-info", files),
                                                         perspective %in% tolower(chkbox$chkboxgrplosstypes()),
                                                         summary_level %in% input$pltsummarylevels)
        summary_id_map <- as.data.frame(.readFile(summary_id_mapfile$files[1]))

        # for "All Risks", replace colname "n/a" and entry "undefined" with "All Risks"
        if (names(summary_id_map)[2] %in% c("n/a", "_not_set_")) {
          summary_id_map[, 2] <- "All Risks"
          names(summary_id_map)[2] <- "All Risks"
        }
        summary_id_title <- paste(colnames(summary_id_map)[-1], collapse = " & ")
        summary_id_map$summary_desc <- do.call("paste", summary_id_map[2:ncol(summary_id_map)])
        # replace summary IDs with descriptions
        data <- data %>% mutate(summary_id = summary_id_map[match(summary_id, summary_id_map$summary_id), "summary_desc"])

        data <- data %>% rename("keyval" = summary_id)

        # rename column for Y axis
        data <- data %>% rename("value" = key)

        # combine type and loss for Title
        data$type <- paste(data$type, "Loss")

        # rename column for x axis
        data <- data %>% rename("xaxis" = x)
        # rename column for granularity. Can be null if granularity level is portfolio
        if (length(gridcol) > 0) {
          data <- data %>% rename("gridcol" = gridcol)
        }

        # rename column for uncertainty. Not all files will have it
        if (length(uncertainty) > 0) {
          data <- data %>% rename("uncertainty" = uncertainty)
        }
        # rename column for reference. Not all files will have it
        # 277: looks unused / obsolete now.
        if (length(reference) > 0) {
          data <- data %>% rename("reference" = reference)
        }

        multipleplots <- TRUE
        data <- data %>% rename("colour" = keyval)
      }

      # > draw plot --------------------------------------------------------------
      if (inputplottype() == "loss for return period map") {
        loss <- which(between(data$loss, min(as.numeric(input$pltlosses)), max(as.numeric(input$pltlosses))))
        pf_loc_content <- session$userData$data_hub$get_pf_location_content(id = portfId())
        names(pf_loc_content) <- tolower(names(pf_loc_content))
        lat <- pf_loc_content$latitude[loss]
        long <- pf_loc_content$longitude[loss]
        loc_num <- pf_loc_content$locnumber[loss]

        popup <- lapply(seq(1, length(data$loss[loss])), function(x) {
          as.character(div(
            strong("Location ID: "), loc_num[x],
            br(), strong("Loss:"), add_commas(round(data$loss[x]))
          ))
        })

        loss_quant <- round(quantile(data$loss[loss], probs = c(1/4, 1/2, 3/4, 6/7)))

        icon_map <- awesomeIcons(
          icon = 'map-marker-alt',
          iconColor = .colorShades(data$loss[loss], loss_quant),
          markerColor = .colorShades(data$loss[loss], loss_quant)
        )

        output$outputleaflet <- renderLeaflet({
          leaflet(session$userData$data_hub$get_pf_location_content(id = portfId())[loss, ]) %>%
            addTiles() %>%
            setView(mean(long), mean(lat), zoom = 12) %>%
            addAwesomeMarkers(
              lng = ~long,
              lat = ~lat,
              icon = icon_map,
              clusterOptions = markerClusterOptions(),
              group = "clustered",
              clusterId = "cluster",
              popup = ~popup[loss]) %>%
            addLegend(colors = c("#0e6199",
                                 "#56d0e8",
                                 "salmon",
                                 "red",
                                 "darkred"),
                      labels = c(paste("<", unname(loss_quant[1])),
                                 paste("<", unname(loss_quant[2])),
                                 paste("<", unname(loss_quant[3])),
                                 paste("<", unname(loss_quant[4])),
                                 paste("<=", round(max(data$loss[loss])))),
                      opacity = 0.8,
                      title = "Loss") %>% # make map full screen
            addFullscreenControl(pseudoFullscreen = TRUE)
        })
      } else {
        if (!is.null(data)) {
          # >> rescale Y axis to millions
          if (input$chkboxmillions) {
            data$value <- data$value / 1000000
            ylabel <- paste(ylabel, "in Millions")
          }

          if (plottype == "line") {
            p <- .linePlotDF(xlabel, ylabel, toupper(result$Title), data,
                             summary_id_title, multipleplots = multipleplots)
            # p <- p + labs("summary_id")
          } else if (plottype == "bar") {
            p <- .barPlotDF(xlabel, ylabel, toupper(result$Title), data,
                            summary_id_title, wuncertainty = input$chkboxuncertainty,
                            multipleplots = multipleplots, xtickslabels = xtickslabels)
          }

          # output$outputplot <- renderPlotly({ggplotly(p)})
          output$outputplot <- renderPlotly({p})
        } else {
          oasisuiNotification(type = "error", "No data to plot.")
        }
      }
    }
  })

  # Helper functions -----------------------------------------------------------
  # Colors for markers
  .colorShades <- function(lossdata, loss_quant) {
    sapply(seq(1, length(lossdata)), function(x) {
      if (lossdata[x] < loss_quant[1]) {
        "darkblue"
      } else if(lossdata[x] < loss_quant[2]) {
        "lightblue"
      } else if(lossdata[x] < loss_quant[3]) {
        "lightred"
      } else if(lossdata[x] < loss_quant[4]) {
        "red"
      } else {
        "darkred"
      }
    })
  }

  # Helper function to enable and disable checkboxes based on condition
  .reactiveUpdateSelectGroupInput <- function(reactivelistvalues, listvalues, inputid, plotType) {
    logMessage(".reactiveUpdateSelectGroupInput called")
    # disable and untick boxes that are not relevant
    selectable <- as.character(reactivelistvalues)
    selection <- intersect(selectable, chkbox[[inputid]]())
    updateCheckboxGroupInput(session = session, inputId = inputid, selected = FALSE)
    # N.B.: JavaScript array indices start at 0
    js$disableCheckboxes(checkboxGroupInputId = ns(inputid),
                         disableIdx = which(listvalues %in% setdiff(listvalues, selectable)) - 1)
    updateCheckboxGroupInput(session = session, inputId = inputid, selected = selection)
  }

  # Helper function to read one file
  .readFile <- function(fileName) {
    if (!is.na(fileName)) {
      logMessage(paste0("Reading file ", fileName))
      tryCatch({
        fileData <- session$userData$data_hub$get_ana_outputs_dataset_content(id = anaID(), dataset_identifier = fileName[1])
      }, error = function(e) {
        oasisuiNotification(type = "error",
                            paste0("Could not read file: ", e$message, "."))
        fileData <- NULL
      })
    } else {
      oasisuiNotification(type = "error",
                          "Invalid file.")
      fileData <- NULL
    }
    return(fileData)
  }
  # > Plot helper functions ----------------------------------------------------
  #Helper functions to plot DF
  #Expected DF with columns:
  # xaxis : column for aes x
  # value : column for aes y
  # colour : column for the aes col
  # flag multipleplots generates grid over col gridcol
  .basicplot <- function(xlabel, ylabel, titleToUse, data, legendtitle, group) {
    p <- ggplot(data, aes(x = xaxis, y = value, col = colour, group = group)) +
      labs(title = titleToUse, x = xlabel, y = ylabel, col = legendtitle) +
      theme(
        plot.title = element_text(color = "grey45", size = 14, face = "bold.italic", hjust = 0.5),
        text = element_text(size = 12),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey45", size = 0.5),
        axis.line.y = element_line(color = "grey45", size = 0.5),
        #legend.title =  element_blank(),
        legend.position = "top"
      )
    p
  }

  # add a horizontal line
  .addRefLine <- function(p, reference){
    if (!is.null(reference)) {
      p <- p + geom_hline(yintercept = reference)
    }
    p
  }

  # add facets
  .multiplot <- function(p, multipleplots = FALSE){
    if (multipleplots) {
      if (inputplottype() == "loss per return period line plot") {
        p <- p + facet_wrap(vars(gridcol, selection), labeller = function(labels, multi_line = FALSE) {
          res <- label_value(labels = labels, multi_line = multi_line)
          # fix for multi_line = FALSE leading to character(0) and y-facets with such label
          if (length(res) == 1 && length(res[[1]]) == 0) {
            res <- list()
          }
          res
        })
      } else {
        p <- p + facet_wrap(~selection)
      }
    }
    p
  }

  # Line plot
  .linePlotDF <- function(xlabel, ylabel, titleToUse, data, legendtitle, multipleplots = FALSE) {
    # add commas to numeric entries and rename them for more user-friendly tooltip
    data$xaxis <- as.factor(add_commas(data$xaxis))
    RP <- data$xaxis
    Loss <- add_commas(data$value * 1000000)
    Summary_Level <- add_commas(data$colour)
    p <- .basicplot(xlabel, ylabel, titleToUse, data, legendtitle, group = data$colour) +
      geom_point(size = 2, aes(color = colour, return = RP, loss = Loss, type = Summary_Level)) +
      geom_line(size = 1)
    p <- .multiplot(p, multipleplots)
    ggplotly(p, tooltip = c("type", "return", "loss"))
  }

  # Bar plot
  .barPlotDF <- function(xlabel, ylabel, titleToUse, data, legendtitle, wuncertainty = FALSE,
                         multipleplots = FALSE, xtickslabels = NULL ) {

    p <- ggplot(data, aes(x = xaxis, y = value, col = colour)) +
      labs(title = titleToUse, x = xlabel, y = ylabel, col = legendtitle) +
      theme(
        plot.title = element_text(color = "grey45", size = 14, face = "bold.italic", hjust = 0.5),
        text = element_text(size = 12),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey45", size = 0.5),
        axis.line.y = element_line(color = "grey45", size = 0.5),
        legend.position = "top"
      )

    # add commas to numeric entries
    data$uncertainty <- add_commas(data$uncertainty)
    data$reference <- add_commas(data$reference)

    # rename tooltip entries
    "Summary_Level" <- data$colour
    "Type" <- data$xaxis
    "Loss" <- add_commas(data$value*1000000)

    p <- p + geom_bar(position = "dodge", stat = "identity", aes(sum_level = Summary_Level, type = Type, loss = Loss))

    if (wuncertainty) {
      p <- p +
        geom_errorbar(aes(ymin = value - uncertainty, ymax = value + uncertainty),
                      size = .3,
                      width = .2, # Width of the error bars
                      position = position_dodge(.9))
      # if ("reference" %in% names(data)) {
      #   p <- .addRefLine(p, unique(data$reference))
      # }
    }
    p <- .multiplot(p, multipleplots)
    ggplotly(p, tooltip = c("sum_level", "type", "loss"))
  }

  # Module Output --------------------------------------------------------------
  reactive(result$Title)
}

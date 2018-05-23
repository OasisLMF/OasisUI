
#' File Management Buttons
fileMgmtButtons <- c(
    FOBtnShowTable 					= "FO_btn_show_table",
    FOBtnShowRawContent 		= "FO_btn_show_raw_content",
    FOBtnShowMap 						= "FO_btn_show_map",
    FOBtnShowAEPCurve 			= "FO_btn_show_AEPCurve",
    FOBtnShowGeocode 				= "FO_btn_show_geocode",
    FOBtnShowEventFootprint = "FO_btn_show_EventFootprint")

#' File Management Divisions
fileMgmtDivs <- c(
    FODivTable              = "FO_div_table",
    FODivFilecontents       = "FO_div_filecontents",
    FODivPlainMap           = "FO_div_plainmap",
    FODivAEPcurve           = "FO_div_AEPcurve",
    FODivGeocode            = "FO_div_geocode",
    FODivEventFootprint     = "FO_div_EventFootprint")

#' File Viewer Module
#' @description Server logic to view files
#' @inheritParams flamingoModule
#' @param userId user id
#' @param preselRunId preselected run id
#' @return empty list
#' @importFrom DT renderDataTable
#' @importFrom shinyjs show hide
#' @rdname fileViewer
#' @export
fileViewer <- function(
    input,
    output,
    session,
    dbSettings,
    userId,
    preselRunId = reactive(-1),
    active = reactive(TRUE),
    logMessage = message) {
  
  result <- reactiveValues(
      FLdata = NULL,
      FVid = -1,
      
      AEPData = NULL,
      
      fileData = NULL
  )
  
  ### File List Table
  
  # Load Company user list data when the page is loaded
  # queries the database every time to update its dataset
  observe(if (active()) {
        
        stmt <- buildDbQuery("getFileViewerTable")
        result$FLdata <- executeDbQuery(dbSettings, stmt)
        
      })
  
  # draw company user list table with custom format options
  output$tableFVfileList <- renderDataTable(if (!is.null(result$FLdata)){
        
        if (preselRunId() == -1) {
          index <- 1 
        } else {
          index <- match(c(paste0("Process:", preselRunId())), result$FLdata[[7]])
        }
        
        datatable(
            result$FLdata,
            class = "flamingo-table display",
            rownames = TRUE,
            selection = list(mode = "single",
                selected = rownames(result$FLdata)[c(as.integer(index))]),
            colnames = c("Row Number" = 1),
            options = list(
                columnDefs = list(list(visible = FALSE, targets = c(0,4,5))),
                pageLength = 20,
                autoWidth=TRUE)
        )
      })
  
  # Export to .csv
  output$FVFLdownloadexcel <- downloadHandler(
      filename = "filelist.csv",
      content = function(file) {
        write.csv(result$FLdata, file)}
  )
  
  # Enable/disable Process details based on the type of file selected
  observe(if (active()) {
        if (length(rows <- input$tableFVfileList_rows_selected) > 0) {
          
          sourcename <- result$FLdata[rows, 7][length(result$FLdata[rows, 7])]
          srcname <- nchar(toString(sourcename))
          prcrunid <- substr(sourcename, 9, srcname)
          prcstr <- toString(substr(sourcename,0,8))
          
          if (prcstr == "Process:") {
            
            show("processruninfodiv")
            prcrunid = as.integer(prcrunid)
            
            showprcinfo <- getProcRunDetForFileOutput(dbSettings, prcrunid)
            
            showruntimeparam <- createSelectOptions(labelCol = 1, valueCol = 2,
                getProcRunParamFileOutput(dbSettings, prcrunid))
            
            output$textprcruninfo <- renderUI({
                  prid <- paste(strong("Process Id: "), showprcinfo[[1]][1])
                  prrnid <- paste(strong("Process Run Id: "), showprcinfo[[2]][1])
                  prname <- paste(strong("Process Name: "), showprcinfo[[3]][1])
                  prgname <- paste(strong("Programme: "), showprcinfo[[4]][1])
                  modname <- paste(strong("Model: "), showprcinfo[[5]][1])
                  wkflow <- paste(strong("Workflow: "), showprcinfo[[6]][1])
                  params <- paste(sep = "<br/>",
                      prid, prrnid, prname, prgname, modname, wkflow)
                  HTML(params)
                })
            
            output$textprcrunparaminfo <- renderUI({
                  paramstype <- names(showruntimeparam)
                  paramvalues <- showruntimeparam
                  params <- paste0(sep = "<br/>",
                      "<b>", paramstype, ": </b>", paramvalues)
                  HTML(params)
                })
            
          } else {
            
            hide("processruninfodiv")
            
          }
        } else {
          
          hide("processruninfodiv")
          
        }
      })
      
  
  
  ### prinfotable
  
  drawprinfotable <- function(prcrunid) {
    
    prinfo <- getProcRunDetForFileOutput(dbSettings, prcrunid)
    
    datatable(
        prinfo,
        class = c("flamingo-table", "display", "flamingo-table-narrow"),
        rownames = TRUE,
        selection = list(mode = "none"),
        colnames = c("Row Number" = 1),
        options = list(
            columnDefs = list(list(visible = FALSE, targets = 0)),
            scrollX = TRUE,
            paging = FALSE,
            searching = FALSE)
    )
  }
  
  
  
  ### FV Table & FVAEP Curve
  
  observe(if (active()) {
        
        stmt <- buildDbQuery("getFileDataForFile", result$FVid)
        result$AEPdata <- executeDbQuery(dbSettings, stmt)
        
      })
  
  output$tableFVAEPdata <- renderDataTable(if (!is.null(result$AEPData)) {
        
        if (is.character(result$AEPData)) {
          validate(
              need(result$AEPData != "", "Data cannot be displayed for this file type.")
          )
        } else {
          datatable(
              result$AEPData,
              class = "flamingo-table display",
              rownames = TRUE,
              colnames = c("Row Number" = 1),
              options = list(
                  columnDefs = list(list(visible = FALSE, targets = 0)),
                  pageLength = 20)
          )
        }
        
      })
  
  output$plotFVAEPCurve <- renderPlot(if (!is.null(result$AEPData)) {
        
        plotAEPCurve(result$AEPData)
        
      })
  
  # Export to .csv
  output$FVAEPdownloadexcel <- downloadHandler(
      filename = "AEPdata.csv",
      content = function(file) {
        write.csv(result$AEPData, file)
      }
  )
  


  ### Plain Map
  
  output$plainmap <- renderLeaflet({
        
        if (length(rows <- c(input$tableFVfileList_rows_selected)) == 1) {
          
          fileName <- file.path(result$FLdata[rows, 5], result$FLdata[rows, 2])
          
          createPlainMap(fileName)
          
        }
        
      })
  
  
  
  ### Foot Print Map
  
  output$EventFootprintMap <- renderLeaflet({
        
        filePath <- createFootprintMap(fileId = result$FVID)
        
      })
  
  
  
  ### FV Exposure / File Contents
  
  output$tableFVExposureSelected <- renderDataTable(
      if (length(rows <- input$tableFVfileList_rows_selected) > 0) {
        fileName <- file.path(result$FLdata[rows, 5], result$FLdata[rows, 2])
        
        tryCatch({
              result$fileData <- read.csv(fileName, header = TRUE, sep = ",",
                  quote = "\"", dec = ".", fill = TRUE, comment.char = "")
            }, error = function(e) {
              showNotification(type = "error",
                  paste("Could not read file:", e$message))
              result$fileData <- NULL
            })
        
        if (!is.null(result$fileData)) {
          datatable(
              result$fileData,
              class = "flamingo-table display",
              rownames = TRUE,
              selection = "none",
              colnames = c("Row Number" = 1))
        } else {
          datatable(
              data.frame(content = "nothing to show"),
              class = "flamingo-table display",
              selection = "none",
              rownames = FALSE,
              colnames = c(""))
        }
        
      })
  
  # Export to .csv
  output$FVEdownloadexcel <- downloadHandler(
      filename = paste0(result$FLdata[c(input$tableFVfileList_rows_selected), 2]),
      content = function(file) {
        write.csv(result$fileData, file)}
  )
  
  
  
  ### Geocode Data
  
  funcForTableGeocodeData <- function() {
    
    ## Only gets the first 50 rows of data.
    
    geocoded <- data.frame()
    filePath <- getFilePath()
    
    stmt <- buildDbQuery("getFileDataForFile", result$FVid)
    GeoData <- executeDbQuery(dbSettings, stmt)
    
    withProgress(message = "Getting Geocode Data",{
          for(n in 1:50) {
            query <- paste(GeoData[n,2], GeoData[n,3], GeoData[n,4])
            logMessage(query)
            result = getGeoDetails(query)
            geocoded <- rbind(geocoded, result)
            geocoded$number[n] <- GeoData[n,2]
            geocoded$street[n] <- paste(GeoData[n,3])
            geocoded$postcode[n] <- paste(GeoData[n,4])
            incProgress(amount = 0.02, detail = paste0(n*2, "%"))
          }
        })
    # note that %>% must follow on immediately from an expression,
    # it can't appear on a line by itself
    datatable(
            geocoded,
            selection = "none",
            colnames = c("Building No.", "Street Name", "Postcode", "Latitude",
                "Longitude", "Returned Address", "Accuracy", "Status"),
            options = list(
                columnDefs = list(list(visible = FALSE, targets = 0)),
                autoWidth=TRUE)
        ) %>%
        formatStyle("lat", backgroundColor = "#96BFD8") %>%
        formatStyle("long", backgroundColor = "#96BFD8") %>%
        formatStyle("accuracy", backgroundColor = "#96BFD8") %>%
        formatStyle("formatted_address", backgroundColor = "#96BFD8") %>%
        formatStyle("status", backgroundColor = "#96BFD8")
  }
  
  output$tableGeocodeData <- renderDataTable(funcForTableGeocodeData())
  
  # Export to .csv
  output$FVGdownloadexcel <- downloadHandler(
      filename = "geocodeddata.csv",
      content = function(file) {
        write.csv(executeDbQuery(dbSettings,
                buildDbQuery("getFileDataForFile", result$FVid)),
            file)
      }
  )
  
  
  
  ### When active (e.g. tab is loaded)
  
  observe(if (active()) {
        
        if (length(rows <- c(input$tableFVfileList_rows_selected)) > 0) {
          
          result$FVid <- result$FLdata[rows, 1]
          
          disableAllButtonsButTable()
          
          validButtons <- executeDbQuery(dbSettings,
              buildDbQuery("TellOperationsValidOnFileID", result$FVid))
          
          for(btnIDs in t(validButtons)) {
            enable(btnIDs)
          }
          
          # Enable/Disable access to other panes based on selected file
          fileName <- file.path(result$FLdata[rows, 5], result$FLdata[rows, 2])
          
          if (!file.exists(fileName)) {
            
            showNotification(type = "warning", "File not found")
            disable(fileMgmtButtons[["FOBtnShowRawContent"]])
            
          } else {
            
            enable(fileMgmtButtons[["FOBtnShowRawContent"]])
            
          }
          
          
        } else {
          
          disableAllButtonsButTable()
          
        }
        
      })
  

  
  ### Panel Switcher
  
  # show the first panel by default
  output$FVPanelSwitcher <- reactive(fileMgmtDivs[[1]])
  outputOptions(output, "FVPanelSwitcher", suspendWhenHidden = FALSE)
  
  displayOnePanel <- function(panelName) {
    logMessage(paste("showing ", panelName))
    output$FVPanelSwitcher <- reactive(panelName)
  }
  
  onclick(fileMgmtButtons[["FOBtnShowTable"]], {
        displayOnePanel(fileMgmtDivs[["FODivTable"]])
      })
  
  onclick(fileMgmtButtons[["FOBtnShowRawContent"]], {
        displayOnePanel(fileMgmtDivs[["FODivFilecontents"]])
      })
  
  onclick(fileMgmtButtons[["FOBtnShowMap"]], {
        displayOnePanel(fileMgmtDivs[["FODivPlainMap"]])
      })
  
  onclick(fileMgmtButtons[["FOBtnShowAEPCurve"]], {
        displayOnePanel(fileMgmtDivs[["FODivAEPcurve"]])
      })
  
  onclick(fileMgmtButtons[["FOBtnShowGeocode"]], {
        displayOnePanel(fileMgmtDivs[["FODivGeocode"]])
      })
  
  onclick(fileMgmtButtons[["FOBtnShowEventFootprint"]], {
        displayOnePanel(fileMgmtDivs[["FODivEventFootprint"]])
      })
  
  
  
  ### Helper Functions
  
  hideAllFileMgmtDivs <- function() {
    for(nm in allFileMgmtDivIDs) {
      hide(nm)
      disable(nm)
    }
  }
  
  disableAllButtonsButTable <- function() {
    for(btnName in names(fileMgmtButtons)) {
      if (!is.null(isolate(input[[btnName]])) &&
          btnName != fileMgmtButtons["FOBtnShowTable"]) {
        disable(btnName)
      }
    }
  }
  
  getFilePath <- function() {
    rows <- c(input$tableFVfileList_rows_selected)
    filePath <- result$FLdata[rows, 3][length(result$FLdata[rows, 4])]
    fileName <- result$FLdata[rows, 4][length(result$FLdata[rows, 4])]
    filePath <- file.path(filePath, fileName)
    return(filePath)
  }
  
  getYears <- function() {
    rows <- c(input$tableFVfileList_rows_selected)
    fileName <- result$FLdata[rows, 6][length(result$FLdata[rows, 6])]
    fileName <- toString(fileName)
    logMessage(paste("selected fileName", typeof(fileName)))
    years <- strsplit(fileName, split = "[_]")
    years <- years[[1]][2]
    return(as.integer(years))
  }
  
  
  
  ### Module Output
  
  moduleOutput <- list()
  
  return(moduleOutput)
  
}
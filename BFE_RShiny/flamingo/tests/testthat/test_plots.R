
test_that("IL plot works", {
      
      x <- seq(10)*1000
      outputPlotData <- data.frame(
          ReturnPeriod = x,
          IL_AEP = sqrt(x),
          IL_OEP = 1.2*sqrt(x) - 10)
      
      plotIL(outputPlotData, interactive = FALSE)
      
      p <- plotIL(outputPlotData, interactive = TRUE)
      
      expect_s3_class(p, "plotly")
      
    })


test_that("GUL plot works", {
      
      x <- seq(10)*1000
      outputPlotData <- data.frame(
          ReturnPeriod = x,
          GUL_AEP = sqrt(x),
          GUL_OEP = 1.2*sqrt(x) - 10)
      
      plotGUL(outputPlotData, interactive = FALSE)
      
      p <- plotGUL(outputPlotData, interactive = TRUE)
      
      expect_s3_class(p, "plotly")
      
    })



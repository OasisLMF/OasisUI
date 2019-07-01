test_that("convert_created_modified works",{
  tbl_analysesData <- data.frame(  id = c(3),
                                   name = "test",
                                   created = "18-12-05T12:58:10.833113+0000",
                                   modified = "8-12-05T12:58:48.417815+0000",
                                   status = "INPUTS_GENERATION_ERROR",
                                   portfolio = c(4 ),
                                   model = c(1),
                                   input_file = "Not Available",
                                   settings_file = "Not Available",
                                   input_errors_file = "Not Available",
                                   input_generation_traceback_file = "http://10.10.0.182:8000/v1/analyses/3/input_generation_traceback_file/",
                                   output_file = "Not Available",
                                   run_traceback_file = "Not Available",
                                   stringsAsFactors = FALSE)
  tbl_analysesData_modified_ref <- data.frame(id = c(3),
                                   name = "test",
                                   created = "5-12-18 12:58:10",
                                   modified = "5-12-08 12:58:48",
                                   status = "INPUTS_GENERATION_ERROR",
                                   portfolio = c(4),
                                   model = c(1),
                                   input_file = "Not Available",
                                   settings_file = "Not Available",
                                   input_errors_file = "Not Available",
                                   input_generation_traceback_file = "http://10.10.0.182:8000/v1/analyses/3/input_generation_traceback_file/",
                                   output_file = "Not Available",
                                   run_traceback_file = "Not Available",
                                   stringsAsFactors = FALSE)
  tbl_analysesData_modified <- convert_created_modified(tbl_analysesData)
  expect_equal(tbl_analysesData_modified, tbl_analysesData_modified_ref)
})
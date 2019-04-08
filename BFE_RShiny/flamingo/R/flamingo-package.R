#' @export
'%>%' <- dplyr::'%>%'


#' @export
'%notin%' <- Negate('%in%')

# Note: readr is not needed directly, but required by httr to parse csv files
#  (httr has it in the 'Suggests' field only).
# Note: shiny >= 1.2.0 updates to Font Awesome 5.
# Note: version specification for shiny-related packages included in relation to
#  the shiny version specification.
# Note: package V8 is included as per documentation of the package shinyjs.
#  It is strongly recommended when using the function extendShinyjs,
#  which allows to include JavaScript functions that can be called from R
#  as if they were regular R functions.
#' @import shiny
#' @importFrom readr read_csv
"_PACKAGE"

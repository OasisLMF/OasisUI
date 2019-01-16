#' @export
'%>%' <- dplyr::'%>%'


#' @export
'%notin%' <- Negate('%in%')

# Note readr is not needed directly, but required by httr to parse csv files
# (httr has it in the 'Suggests' field only).
#' @import shiny
#' @importFrom readr read_csv
"_PACKAGE"

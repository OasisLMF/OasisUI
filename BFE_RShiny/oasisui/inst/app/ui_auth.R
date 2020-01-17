###############################################################################

#' authUI
#'
#' @rdname authUI
#'
#' @param WidthSide This is a specific value.
#' @param WidthMain This is a specific value.
#'
#' @return ui after authentification.
#'
#' @export

authUI <- function(WidthSide = 3, WidthMain = 9) {

  tagList(

    tags$script("Shiny.onInputChange('authUIRenderCallback', true)"),
    # N.B.: this style cannot be easily moved to one of our CSS files.
    # The reasons are:
    # - the same selectors are used in jquery.dataTables.extra.css, which is
    #   loaded after our CSS files
    # - the background-color in jquery.dataTables.extra.css is marked with
    #   !important
    tags$style(
      HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #ebcccc !important;}')
    ),

    # Header ----
    fluidRow(
      column(3,
             a(href = "https://oasislmf.org",
               img(src = "img/OASIS_LMF_COLOUR.png", width = "85%", style = "margin-top:5%"))
      ),
      column(8,
             style = "margin-top:2%"),
      column(1,
             pageheaderUI("pageheader"),
             style = "margin-top:2%")
    ),

    hr(),

    # Main body ----
    fluidRow(

      # Sidebar panel ----
      dynamicColumnUI(
        "sidebar",
        WidthSide,
        style = "min-width: 135px;",
        pagestructureUI("pagestructure")
      ),

      #Main panel -----
      dynamicColumnUI(
        "main",
        WidthMain,
        style = "max-width: calc(100% - 135px);",
        reactiveConditionalPanelsUI(
          "mainPanel",
          list(

            # Landing Page
            LP =
              #img(src = "landingpage.png", width = "70%")
              landingPageUI("landingPage"),

            # Define Single Analysis
            SA =
              singleAnaUI("singleAna"),

            # Define Batch Analysis
            BA =
              batchAnaUI("batchAna"),

            # DefineBrowse Single
            SBR = visualizationSBRUI("visualizationSBR"),

            # DefineBrowse Batch
            BBR = visualizationBBRUI("visualizationBBR"),

            # Compare run Batch
            CBR = visualizationCBRUI("visualizationCBR")

          ) # End of panels list
        ) # End of reactiveConditionalPanelsUI
      ) # End of column
    ), # End of fluidRow

    # Footer ----
    fillRow(
      em(paste("OasisUI", as.character(utils::packageVersion("oasisui"))), class = "oasisui-footer"),
      a(href = "https://shiny.rstudio.com/",
        em("Powered by RShiny",
           class = "rshiny-footer"),
        align = "left")
    )

  )

}

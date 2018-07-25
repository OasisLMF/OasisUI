# Flamingo Shiny
#
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

authUI <- function(WidthSide = 3, WidthMain = 9) {
  
  tagList(
    
    tags$script("Shiny.onInputChange('authUIRenderCallback', true)"),
    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #ebcccc !important;}')),
    # Header ----
    fluidRow(
      column(3,
             a(href = "https://oasislmf.org",
               img(src = "OASIS_LMF_COLOUR.png", width = "85%", style = "margin-top:5%"))
      ),
      column(8,
             actionButton(inputId = "abuttoncollapsesidebar", icon = icon("ellipsis-v"), label = NULL),
             style = "margin-top:2%"),
      column(1,
             pageheaderUI("pageheader"),
             style = "margin-top:2%")
    ),
    
    hr(),
    
    # Main body ###
    fluidRow(
      # Sidebar panel
      column(WidthSide,
             pagestructureUI("pagestructure")),
      
      #Main panel
      column(WidthMain,
             
             ### Landing Page
             conditionalPanel(
               condition = "output.menu == 'LP'",
               #img(src = "landingpage.png", width = "70%")
               landingPageUI("landingPage")
             ),
             
             ### Define Account
             conditionalPanel(
               condition = "output.menu == 'DA'",
               accountDefinitionUI("accountDefinition")
             ),
             
             ### DefineProgramme Single
             conditionalPanel(
               condition = "output.menu == 'PS'",
               programmeDefinitionSingleUI("programmeDefinitionSingle")
             ),
             
             # ### DefineProgramme Batch
             # conditionalPanel(
             #   condition = "output.menu == 'DPB'",
             #   programmeDefinitionBatchUI("programmeDefinitionBatch")
             # ),
             
             ### browseprogrammesUI
             conditionalPanel(
               condition = "output.menu == 'BR'",
               browseprogrammesUI("browseprogrammes")
             ),
             
             ### Exposure Management
             conditionalPanel(
               condition = "output.menu == 'EM'",
               #navbarPage("Exposure Management", id = "em",
               tabsetPanel(id = "em",
                           
                           # tabPanel("Define Account",
                           #          value = "defineAccount",
                           #          accountDefinitionUI("accountDefinition")),
                           
                           tabPanel("Define Programme",
                                    value = "defineProg",
                                    programmeDefinitionUI("programmeDefinition"))
               ) #End of navbarMenu("Exposure Management")
             ), #End of conditional panel Exposure Management
             
             
             ### Process Management
             conditionalPanel(
               condition = "output.menu == 'WF'",
               processRunPageUI("processRunPage")), #End of conditional panel Process Management
             
             #### File Management
             conditionalPanel(
               condition = "output.menu == 'FM'",
               fileViewerUI("fileViewer")), # End of conditional panel File Management
             
             ### System Config
             conditionalPanel(
               condition = "output.menu == 'SC'",
               modelSupplierPageUI("modelSupplierPage")), # End of conditional panel System Config
             
             ### User Admin
             conditionalPanel(
               condition = "output.menu == 'UA'",
               #navbarPage("User Administration", id = "ua",
               tabsetPanel(id = "ua",
                           tabPanel("Company",
                                    value = "definecompany",
                                    companyDefinitionUI("companyDefinition")),
                           
                           tabPanel("Company User Administraton",
                                    value = "defineuser",
                                    userAdminDefinitionUI("userAdminDefinition"))
               ) # End of tabsetpanel
             ) # End of conditional panel User Administration
             
      ) # endo of column
    ), # end of fluidRow
    
    # Footer ----
    fillRow(
      em("Flamingo 1.1 Oasis Business Front End",
         style = "color:gray; font-size:10pt; margin-left:5%"),
      a(href = "https://shiny.rstudio.com/",
        em("Powered by RShiny",
           style = "color:gray; font-size:10pt; float:right; margin-right:2%"),
        align = "left")
    )
    
  )
  
}

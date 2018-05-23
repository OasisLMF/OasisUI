# Flamingo Shiny
# 
# (c) 2013-2017 Oasis LMF Ltd.
# Software provided for early adopter evaluation only.
###############################################################################

authUI <- function() {
  
  tagList(
      
      tags$script("Shiny.onInputChange('authUIRenderCallback', true)"),
      
      conditionalPanel(
          condition = "output.menu == 'LP'",
          landingPageUI("landingPage")
      ),
      
      
      ### Exposure Management
      conditionalPanel(
          condition = "output.menu == 'EM'",                    
          navbarPage("Exposure Management", id = "em", 
              position = "fixed-top",
              tabPanel("", value = "emtemp"),
              
              tabPanel("Define Account",
                  value = "defineAccount",
                  accountDefinitionUI("accountDefinition")),
              
              tabPanel("Define Programme",
                  value = "defineProg",
                  programmeDefinitionUI("programmeDefinition")),
              
              tabPanel("Main Menu",  value = "emlp", id = "em-back"),
              
              shinyFooter <- div(class = "flamingo-footer", align = "right",
                  em("Powered by RShiny", style = "color:gray; font-size:10pt"))                              
          ) #End of navbarMenu("Exposure Management")
      ), #ENd of conditional panel Exposure Management  
      
      
      ### Process Management 
      conditionalPanel(
          condition = "output.menu == 'WF'",                   
          navbarPage("Process Management", id = "pr",
              position = "fixed-top",
              tabPanel("", value = "prtemp"),
              
              tabPanel("Process Run",
                  value = "processrun",
                  processRunPageUI("processRunPage")),
              
              tabPanel("Main Menu",  value = "prlp"),
              
              shinyFooter
          ) #End of navbarMenu("Process Management")
      ), #ENd of conditional panel Process Management
      
      #### File Management
      conditionalPanel(
          condition = "output.menu == 'FM'",
          navbarPage("File Management", id = "fm",
              position = "fixed-top",
              tabPanel("", value = "fmtemp"),
              
              tabPanel("File Viewer",
                  value = "fileviewer",
                  fileViewerUI("fileViewer")),
              
              tabPanel("Main Menu",  value = "fmlp"),
              
              shinyFooter
          ) # End of navbarMenu(File Management")
      ), # ENd of conditional panel File Management
      
      ### System Config
      conditionalPanel(
          condition = "output.menu == 'SC'",
          navbarPage("System Configuration", id = "sc",
              position = "fixed-top",
              tabPanel("", value = "sctemp"),
              
              tabPanel("Model",
                  value = "Model",
                  modelSupplierPageUI("modelSupplierPage")),
              
              tabPanel("Main Menu",  value = "sclp"),
              
              shinyFooter
          ) # End of navbarMenu(System Config)
      ), # ENd of conditional panel System Config
      
      ### User Admin 
      conditionalPanel(
          condition = "output.menu == 'UA'",                    
          navbarPage("User Administration", id = "ua",
              position = "fixed-top",
              tabPanel("", value = "uatemp"),
              
              tabPanel("Company",
                  value = "definecompany",
                  companyDefinitionUI("companyDefinition")),
              
              tabPanel("Company User Administraton",
                  value = "defineuser",
                  userAdminDefinitionUI("userAdminDefinition")),
              
              tabPanel("Main Menu",  value = "ualp"),
              
              shinyFooter                            
          ) # End of navbarMenu("User Administration")
      ) # ENd of conditional panel User Administrationt  
  
  ) # End of conditiopnalPanel Utilities                   
}


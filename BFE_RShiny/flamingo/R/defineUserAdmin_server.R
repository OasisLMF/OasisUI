
#' User Admin Definition Module
#' @description Server logic for accessing the Company User List for Flamingo
#' in association with OASIS LMF
#' @inheritParams flamingoModule
#' @param userId reactive yielding user id (of the logged in user)
#' @return For \code{userAdminDefinition()}, list of reactives.
#' @template return-outputNavigation
#' @rdname userAdminDefinition
#' @importFrom DT renderDT
#' @importFrom shinyjs enable disable
#' @export
userAdminDefinition <- function(input, output, session, dbSettings, userId,
                                active = reactive(TRUE)) {

  navigation_state <- reactiveNavigation()

  result <- reactiveValues(
    CULData = NULL,
    CULDataCounter = 0,

    CUAULData = NULL,
    CUAULDataCounter = 0,

    USGData = NULL,
    USGDataCounter = 0,

    useradminflg = "", # one of c("", "C", "U")

    selUserId = -1 # reset to -1 when no user is selected
  )

  .reloadCULData <- function() {
    result$CULDataCounter <- result$CULDataCounter + 1
    invisible()
  }

  .reloadCUAULData <- function() {
    result$CUAULDataCounter <- result$CUAULDataCounter + 1
    invisible()
  }

  .reloadUSGData <- function() {
    result$USGDataCounter <- result$USGDataCounter + 1
    invisible()
  }


  ### Helper Functions
  .clearCompanySelection <- function() {
    companies <- getCompanyList(dbSettings)
    updateSelectInput(session, "sinputCompany",
                      choices = createSelectOptions(companies, "Select Company"),
                      selected = c("Select Company" = 0))
  }

  .clearSecurityGroupSelection <- function() {
    securityGroups <- getSecurityGroups(dbSettings)
    updateSelectInput(session, "sinputSecurity",
                      choices = createSelectOptions(securityGroups, "All"),
                      selected = c("All" = 0))
  }

  .clearOasisUserSelection <- function() {
    oasisUsers <- getOasisUsers(dbSettings)
    updateSelectInput(session, "sinputOasisID",
                      choices = createSelectOptions(oasisUsers, "Select Oasis User ID"),
                      selected = c("Select Oasis User ID" = 0))
  }

  ### When Activated (e.g. tab is openened)
  observe(if (active()) {
    hide("usgroups")
    hide("ulicenses")
    result$selUserId <- -1
  })


  ### Company User List Table

  # queries the database every time to update its dataset
  observe(if (active()) {

    force(result$CULDataCounter)

    stmt <- buildDbQuery("getUsersForCompany")
    result$CULData <- executeDbQuery(dbSettings, stmt)

  })

  # draw company user list table with custom format options
  output$tablecompanyuserlist <- renderDT({
    datatable(
      result$CULData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      selection = "single",
      colnames = c('Row Number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0))
      )
    )
  })


  ### User Securtiy Group Table

  # queries the database every time to update its dataset
  observe(if (active()) {

    force(result$USGDataCounter)

    stmt <- buildDbQuery("getSecurityGroupsForUser", result$selUserId)
    result$USGData <- executeDbQuery(dbSettings, stmt)

  })

  # draw User Securtiy Group table with custom format options
  output$tableusersecuritygroups <- renderDT({
    if (length(input$tablecompanyuserlist_rows_selected) > 0) {
      datatable(
        result$USGData,
        class = "flamingo-table display",
        rownames = TRUE,
        selection = "none",
        colnames = c('Row Number' = 1),
        filter = 'bottom',
        options = list(
          searchHighlight = TRUE,
          autoWidth=TRUE,
          columnDefs = list(list(visible = FALSE, targets = 0)),
          search = list(caseInsensitive = TRUE),
          processing=0,
          pageLength = 10)
      )
    }
  })


  ### User License Table

  # queries the database every time to update dataset
  observe(if (active()) {

    force(result$CUAULDataCounter)

    stmt <- buildDbQuery("getUserLicenses", result$selUserId)
    result$CUAULData <- executeDbQuery(dbSettings, stmt)

  })

  # draw User License table with custom format options
  output$tableuserlicenses <- renderDT({
    if (length(input$tablecompanyuserlist_rows_selected) > 0){
      datatable(
        result$CUAULData,
        rownames = TRUE,
        selection = "none",
        colnames = c('Row Number' = 1),
        filter = 'bottom',
        options = list(
          searchHighlight = TRUE,
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(list(visible = FALSE, targets = 0)),
          search = list(caseInsensitive = TRUE),
          processing=0,
          pageLength = 10)
      )
    }
  })


  ###  Permission Checking ####################################################

  observe(if(active()) {

    # the user Id here is the user id of the logged in user, not of the
    # currently selected user

    permission <- flamingoDBCheckPermissions(dbSettings, userId(), 905)

    if(length(permission) == 0){

      showNotification(type = "warning",
                       "You do not have the required permissions to view this page")

      updateNavigation(navigation_state, "LP")

    } else if(permission[1] == "CRUD") {

      enable("abuttonnewUser")
      enable("abuttonuserupdate")
      enable("abuttonuserdelete")
      enable("abuttonusersecurity")
      enable("abuttonuseroasis")

    } else if(permission[1] == "R") {

      disable("abuttonnewUser")
      disable("abuttonuserupdate")
      disable("abuttonuserdelete")
      disable("abuttonusersecurity")
      disable("abuttonuseroasis")

    } else {

      showNotification(type = "warning", "Neither CRUD nor R")

    }
  })


  ### Text Input Updating #####################################################

  # Function that:
  # - updates information fields in Modal Dialog
  .getCompUserDetails <- function() {

      result$selUserId <- result$CULData[input$tablecompanyuserlist_rows_selected, 3]
      updateTextInput(session, "tinputUserName",
                      value = result$CULData[input$tablecompanyuserlist_rows_selected, 4])
      updateSelectInput(session, "sinputCompany",
                        selected = result$CULData[(input$tablecompanyuserlist_rows_selected), 1])
      deptData <- getDeptData(dbSettings, result$selUserId)
      updateTextInput(session, "tinputDepartment", value = deptData[[3]])
      updateTextInput(session, "tinputLogin", value = deptData[[1]])
      updateTextInput(session, "tinputPassword", value = deptData[[2]])

  }

  # updates the details on the left hand side every time a row is clicked
  # uses the list of rows selected to attain row index
  # row index is used to access data in table and render it to the left hand side
  observe(if (active()) {
    if (length(input$tablecompanyuserlist_rows_selected) > 0) {
      result$selUserId <- result$CULData[input$tablecompanyuserlist_rows_selected, 3]
      .reloadUSGData()
      show("ulicenses")
      .reloadCUAULData()
      show("usgroups")
    } else {
      hide("usgroups")
      hide("ulicenses")
      result$selUserId <- -1
    }
  })

  ### User Create / Update / Delete ###########################################

  # Modal dialog of create and update buttons

  .useradmincrtupmodal <- function() {
    ns <- session$ns
    modalDialog(label = ".useradmincrtupmodal",
                title = "User Details",
                textInput(ns("tinputUserName"), "User Name"),
                selectInput(ns("sinputCompany"), "Company Name", choices = c("")),
                textInput(ns("tinputDepartment"), "Department"),
                textInput(ns("tinputLogin"), "Login"),
                passwordInput(ns("tinputPassword"), "Password"),
                footer = tagList(
                  actionButton(ns("abuttonusersubmit"), class = "btn btn-primary",
                               label = "Submit", align = "right"),

                  actionButton(ns("abuttonusercancel"), class = "btn btn-default",
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of create button
  onclick("abuttonnewUser", {
    result$useradminflg <- "C"
    showModal(.useradmincrtupmodal())
    result$selUserId <- -1
    .clearCompanySelection()
  })

  # cancel new user
  # clears the fields, hides confirm cancel, shows CRUD buttons
  onclick("abuttonusercancel",{
    removeModal()
    result$selUserId <- -1
    .reloadCULData()
  })

  #on click of update button
  onclick("abuttonuserupdate", {
    if (length(input$tablecompanyuserlist_rows_selected) > 0) {
      result$useradminflg <- "U"
      showModal(.useradmincrtupmodal())
      .clearCompanySelection()
      .getCompUserDetails()
    } else {
      showNotification(type = "warning",
                       "Please select a user to update details.")
    }
  })


  # on click of submit button in pop-up to create/update
  onclick("abuttonusersubmit",{
    if (result$useradminflg == "C"){

      stmt <- paste0("exec dbo.CreateNewUser ",
                     "[",input$tinputUserName,"]", ", ", "[",input$sinputCompany, "]", ", ",
                     "[", input$tinputLogin, "]", ", ",  "[", input$tinputPassword, "]", ", ",
                     "[", input$tinputDepartment,"]")
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        showNotification(type = "error",
                         paste("Failed to create new user - ", input$tinputUserName))
      } else {
        showNotification(type = "message",
                         paste("User ", input$tinputUserName, " created. User id: ", res))
      }

    } else if (result$useradminflg == "U") {

      stmt <- buildDbQuery("updateUser", as.character(result$selUserId),
                           input$tinputUserName, input$sinputCompany, input$tinputLogin,
                           input$tinputPassword, input$tinputDepartment)
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        showNotification(type = "error",
                         paste("Failed to update user - ",input$tinputUserName))
      }else{
        showNotification(type = "message",
                         paste("User -", input$tinputUserName, " updated."))
      }
    }
    result$useradminflg <- ""
    removeModal()
    .reloadCULData()
  })

  # Modal dialog of delete button
  .userdelmodal <- function() {
    ns <- session$ns
    modalDialog(label = ".userdelmodal",
                title = "Delete selection",
                paste0("Are you sure you want to delete?"),
                footer = tagList(
                  actionButton(ns("abuttonuconfirmdel"), class="btn btn-primary",
                               label = "Confirm", align = "center"),
                  actionButton(ns("abuttonucanceldel"), class = "btn btn-default",
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of delete button
  onclick("abuttonuserdelete", {
    if (length(input$tablecompanyuserlist_rows_selected) > 0) {
      showModal(.userdelmodal())
    } else {
      showNotification(type = "warning", "Please select a user to delete")
    }
  })

  # onclick of cancel delete button
  onclick("abuttonucanceldel", {
    removeModal()
    .reloadCULData()
  })

  # onclick of confirm delete button
  onclick("abuttonuconfirmdel",{

    stmt <- buildDbQuery("deleteUser",
                         as.character(result$CULData[input$tablecompanyuserlist_rows_selected, 3]))
    res <- executeDbQuery(dbSettings, stmt)

    if (is.null(res)) {
      showNotification(type = "error", sprintf("Failed to delete user - ",
                                               result$CULData[input$tablecompanyuserlist_rows_selected, 4]))
    } else {
      showNotification(type = "message", sprintf("User - %s deleted.",
                                                 result$CULData[input$tablecompanyuserlist_rows_selected, 4]))
    }

    removeModal()
    result$selUserId <- -1
    .reloadCULData()
  })



  ### Security Group Add/Delete ###############################################

  # modal dialog of add/remove security button
  .usersecuritymodal <- function() {
    ns <- session$ns
    modalDialog(label = ".usersecuritymodal",
                title = "Add/Remove Security Groups",
                selectInput(ns("sinputSecurity"), "Select Security Group",
                            choices = c("")),
                footer = tagList(
                  actionButton(ns("abuttonaddsecurity"), class="btn btn-primary",
                               label = "Add", align = "left"),
                  actionButton(ns("abuttonrmvsecurity"), class="btn btn-primary",
                               label = "Remove", align = "right")),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of add/remove security button
  onclick("abuttonusersecurity", {
    if (length(input$tablecompanyuserlist_rows_selected) > 0) {
      showModal(.usersecuritymodal())
      .clearSecurityGroupSelection()
    } else {
      showNotification(type = "warning",
                       "Please select a user to add security group")
    }
  })

  # onclick of add security button in pop-up
  onclick("abuttonaddsecurity", {
    # as.character ensures query builder will quote args
    stmt <- buildDbQuery("addSecurityGroup",
                         as.character(result$selUserId), as.character(input$sinputSecurity))
    executeDbQuery(dbSettings, stmt)
    .reloadUSGData()
    removeModal()
  })

  # onclick of remove security button
  onclick("abuttonrmvsecurity", {
    stmt <- buildDbQuery("removeSecurityGroup",
                         as.character(result$selUserId), as.character(input$sinputSecurity))
    executeDbQuery(dbSettings, stmt)
    .reloadUSGData()
    removeModal()
  })



  ### Oasis User Id (License) Add/Delete ######################################

  # modal dialog of add/remove license button
  .userlicensemodal <- function() {
    ns <- session$ns
    modalDialog(label = ".userlicensemodal",
                title = "Add/Remove User Licenses",
                selectInput(ns("sinputOasisID"), "Select Oasis User ID",
                            choices = c("")),
                footer = tagList(
                  actionButton(ns("abuttonaddoasisid"), class="btn btn-primary",
                               label = "Add", align = "left"),
                  actionButton(ns("abuttonrmvoasisid"), class="btn btn-primary",
                               label = "Remove", align = "right")),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of add/remove license button
  onclick("abuttonuseroasis", {
    if (length(input$tablecompanyuserlist_rows_selected) > 0) {
      showModal(.userlicensemodal())
      .clearOasisUserSelection()
    } else {
      showNotification(type = "warning",
                       "Please select a user to add license")
    }
  })


  # onclick of add license button in pop-up
  onclick("abuttonaddoasisid", {
    stmt <- buildDbQuery("addUserLicense",
                         as.character(result$selUserId), as.character(input$sinputOasisID))
    executeDbQuery(dbSettings, query)
    .reloadCUAULData()
    removeModal()
  })

  # onclick of remove license button in pop-up
  onclick("abuttonrmvoasisid", {
    stmt <- buildDbQuery("removeUserLicense",
                         as.character(result$selUserId), as.character(input$sinputOasisID))
    executeDbQuery(dbSettings, query)
    .reloadCUAULData()
    removeModal()
  })


  ### Export to CSV ###########################################################
  # User List
  output$CUACULdownloadexcel <- downloadHandler(
    filename ="companyuserlist.csv",
    content = function(file) {
      write.csv(result$CULData, file)
    }
  )
  # User Security Groups
  output$CUAUUSGdownloadexcel <- downloadHandler(
    filename ="usersecuritygroups.csv",
    content = function(file) {
      write.csv(result$USGdata, file)
    }
  )
  # User Licenses
  output$CUAULdownloadexcel <- downloadHandler(
    filename ="userlicenses.csv",
    content = function(file) {
      write.csv(result$CUAULData, file)
    }
  )


  ### Module Output ###########################################################

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list() # placeholder
  )

  moduleOutput

}


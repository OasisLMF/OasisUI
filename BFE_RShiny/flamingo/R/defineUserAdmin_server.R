#' userAdminDefinition
#'
#' @rdname userAdminDefinition
#'
#' @description Server logic for accessing the Company User List for Flamingo
#' in association with OASIS LMF.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-active
#' @param dbSettings Setting object as returned by e.g. [flamingoDB()].
#' @param user current user name
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom shinyjs onclick
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom utils write.csv
#'
#' @export
userAdminDefinition <- function(input, output, session, dbSettings, user,
                                active = reactive(TRUE)) {

  # Reactive Values and parameters ---------------------------------------------
  navigation_state <- reactiveNavigation()

  result <- reactiveValues(

    permission = NULL,

    tbl_CULData = NULL,
    tbl_CULDataCounter = 0,

    tbl_CUAULData = NULL,
    tbl_CUAULDataCounter = 0,

    tbl_USGData = NULL,
    tbl_USGDataCounter = 0,

    useradminflg = "", # one of c("", "C", "U")

    selUserId = -1 # reset to -1 when no user is selected
  )

  .reloadtbl_CULData <- function() {
    result$tbl_CULDataCounter <- result$tbl_CULDataCounter + 1
    invisible()
  }

  .reloadtbl_CUAULData <- function() {
    result$tbl_CUAULDataCounter <- result$tbl_CUAULDataCounter + 1
    invisible()
  }

  .reloadtbl_USGData <- function() {
    result$tbl_USGDataCounter <- result$tbl_USGDataCounter + 1
    invisible()
  }


  # Helper Functions -----------------------------------------------------------
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
                      choices = createSelectOptions(oasisUsers, "Select Oasis User"),
                      selected = c("Select Oasis User" = 0))
  }

  # When Activated (e.g. tab is openened) --------------------------------------
  observe(if (active()) {
    hide("usgroups")
    hide("ulicenses")
    result$selUserId <- -1
  })


  # Company User List Table ----------------------------------------------------

  # queries the database every time to update its dataset
  observe(if (active()) {

    force(result$tbl_CULDataCounter)

    stmt <- buildDbQuery("getUsersForCompany")
    result$tbl_CULData <- executeDbQuery(dbSettings, stmt)

  })

  # draw company user list table with custom format options
  output$dt_companyuserlist <- renderDT({
    datatable(
      result$tbl_CULData,
      class = "flamingo-table display",
      rownames = TRUE,
      filter = "none",
      selection = "single",
      colnames = c('row number' = 1),
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0))
      )
    )
  })


  # User Securtiy Group Table --------------------------------------------------

  # queries the database every time to update its dataset
  observe(if (active()) {

    force(result$tbl_USGDataCounter)

    stmt <- buildDbQuery("getSecurityGroupsForUser", result$selUserId)
    result$tbl_USGData <- executeDbQuery(dbSettings, stmt)

  })

  # draw User Securtiy Group table with custom format options
  output$dt_usersecuritygroups <- renderDT({
    if (length(input$dt_companyuserlist_rows_selected) > 0) {
      datatable(
        result$tbl_USGData,
        class = "flamingo-table display",
        rownames = TRUE,
        selection = "none",
        colnames = c('row number' = 1),
        filter = 'bottom',
        options = list(
          searchHighlight = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(visible = FALSE, targets = 0)),
          search = list(caseInsensitive = TRUE),
          processing = 0,
          pageLength = 10)
      )
    }
  })


  # User License Table ---------------------------------------------------------

  # queries the database every time to update dataset
  observe(if (active()) {

    force(result$tbl_CUAULDataCounter)
    stmt <- buildDbQuery("getUserLicenses", result$selUserId)
    result$tbl_CUAULData <- executeDbQuery(dbSettings, stmt)
  })

  # draw User License table with custom format options
  output$dt_userlicenses <- renderDT({
    if (length(input$dt_companyuserlist_rows_selected) > 0) {
      datatable(
        result$tbl_CUAULData,
        rownames = TRUE,
        selection = "none",
        colnames = c('row number' = 1),
        filter = 'bottom',
        options = list(
          searchHighlight = TRUE,
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(list(visible = FALSE, targets = 0)),
          search = list(caseInsensitive = TRUE),
          processing = 0,
          pageLength = 10)
      )
    }
  })


  # Permission Checking --------------------------------------------------------

  observeEvent({active(); user()}, if (active()) {

    # the user here is the logged in user, not the
    # currently selected user

    result$permission <- "R"

    if (length(result$permission) == 0) {

      flamingoNotification(type = "warning",
                       "You do not have the required permissions to view this page")

      updateNavigation(navigation_state, "LP")

    } else if (result$permission[1] != "CRUD" && result$permission[1] != "R") {

      flamingoNotification(type = "warning", "Neither CRUD nor R")

    }
  })


  # Text Input Updating --------------------------------------------------------

  # Function that:
  # - updates information fields in Modal Dialog
  .getCompUserDetails <- function() {

    result$selUserId <- result$tbl_CULData[input$dt_companyuserlist_rows_selected, 3]
    updateTextInput(session, "tinputUserName",
                    value = result$tbl_CULData[input$dt_companyuserlist_rows_selected, 4])
    updateSelectInput(session, "sinputCompany",
                      selected = result$tbl_CULData[(input$dt_companyuserlist_rows_selected), 1])
    deptData <- getDeptData(dbSettings, result$selUserId)
    updateTextInput(session, "tinputDepartment", value = deptData[[3]])
    updateTextInput(session, "tinputLogin", value = deptData[[1]])
    updateTextInput(session, "tinputPassword", value = deptData[[2]])

  }

  # updates the details oevery time a row is clicked
  # uses the list of rows selected to attain row index
  # row index is used to access data in table and render it
  observeEvent(input$dt_companyuserlist_rows_selected, ignoreNULL = FALSE, {
    if (length(input$dt_companyuserlist_rows_selected) > 0) {
      .getCompUserDetails()
      .reloadtbl_USGData()
      show("ulicenses")
      .reloadtbl_CUAULData()
      show("usgroups")
    } else {
      hide("usgroups")
      hide("ulicenses")
      result$selUserId <- -1
    }
  })

  # User Create / Update / Delete ----------------------------------------------

  # Modal dialog of create and update buttons
  .useradmincrtupmodal <- function() {
    ns <- session$ns
    modalDialog(label = "useradmincrtupmodal",
                title = "User Details",
                textInput(ns("tinputUserName"), "User Name"),
                selectInput(ns("sinputCompany"), "Company Name", choices = c("")),
                textInput(ns("tinputDepartment"), "Department"),
                textInput(ns("tinputLogin"), "Login"),
                passwordInput(ns("tinputPassword"), "Password"),
                footer = tagList(
                  flamingoButton(ns("abuttonusersubmit"),
                               label = "Submit", align = "right"),

                  actionButton(ns("abuttonusercancel"),
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
    .reloadtbl_CULData()
  })

  # Enable and disable buttons
  observeEvent(input$dt_companyuserlist_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
      if (length(input$dt_companyuserlist_rows_selected) > 0 && result$permission[1] == "CRUD") {
        enable("abuttonuserupdate")
        enable("abuttonuserdelete")
        enable("abuttonusersecurity")
        enable("abuttonuseroasis")
      } else {
        disable("abuttonuserupdate")
        disable("abuttonuserdelete")
        disable("abuttonusersecurity")
        disable("abuttonuseroasis")
      }
    })

  #on click of update button
  onclick("abuttonuserupdate", {
      result$useradminflg <- "U"
      showModal(.useradmincrtupmodal())
      .clearCompanySelection()
      .getCompUserDetails()
  })


  # on click of submit button in pop-up to create/update
  onclick("abuttonusersubmit",{
    if (result$useradminflg == "C") {

      stmt <- paste0("exec dbo.CreateNewUser ",
                     "[",input$tinputUserName,"]", ", ", "[",input$sinputCompany, "]", ", ",
                     "[", input$tinputLogin, "]", ", ",  "[", input$tinputPassword, "]", ", ",
                     "[", input$tinputDepartment,"]")
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        flamingoNotification(type = "error",
                         paste("Failed to create new user - ", input$tinputUserName))
      } else {
        flamingoNotification(type = "message",
                         paste("User ", input$tinputUserName, " created. User: ", res))
      }

    } else if (result$useradminflg == "U") {

      stmt <- buildDbQuery("updateUser", as.character(result$selUserId),
                           input$tinputUserName, input$sinputCompany, input$tinputLogin,
                           input$tinputPassword, input$tinputDepartment)
      res <- executeDbQuery(dbSettings, stmt)

      if (is.null(res)) {
        flamingoNotification(type = "error",
                         paste("Failed to update user - ",input$tinputUserName))
      } else {
        flamingoNotification(type = "message",
                         paste("User -", input$tinputUserName, " updated."))
      }
    }
    result$useradminflg <- ""
    removeModal()
    .reloadtbl_CULData()
  })

  # title for delete button
  output$userdelmodaltitle <- renderUI({
    companyId <- result$tbl_CULData[(input$dt_companyuserlist_rows_selected), 1]
    companyName <- result$tbl_CULData[(input$dt_companyuserlist_rows_selected), 2]
    CULId <- result$tbl_CULData[(input$dt_companyuserlist_rows_selected), 3]
    CULName <- result$tbl_CULData[(input$dt_companyuserlist_rows_selected), 4]
    paste0('Delete User ', CULId, ' "', CULName,'" for Company id ', companyId, ' "', companyName, '"')
  })

  # Modal dialog of delete button
  .userdelmodal <- function() {
    ns <- session$ns
    modalDialog(label = "userdelmodal",
                title = uiOutput(ns("userdelmodaltitle"), inline = TRUE),
                paste0("Are you sure you want to delete?"),
                footer = tagList(
                  flamingoButton(ns("abuttonuconfirmdel"),
                               label = "Confirm", align = "center"),
                  actionButton(ns("abuttonucanceldel"),
                               label = "Cancel", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of delete button
  onclick("abuttonuserdelete", {
      showModal(.userdelmodal())
  })

  # onclick of cancel delete button
  onclick("abuttonucanceldel", {
    removeModal()
    .reloadtbl_CULData()
  })

  # onclick of confirm delete button
  onclick("abuttonuconfirmdel",{

    stmt <- buildDbQuery("deleteUser",
                         as.character(result$tbl_CULData[input$dt_companyuserlist_rows_selected, 3]))
    res <- executeDbQuery(dbSettings, stmt)

    if (is.null(res)) {
      flamingoNotification(type = "error", sprintf("Failed to delete user - ",
                                               result$tbl_CULData[input$dt_companyuserlist_rows_selected, 4]))
    } else {
      flamingoNotification(type = "message", sprintf("User - %s deleted.",
                                                 result$tbl_CULData[input$dt_companyuserlist_rows_selected, 4]))
    }

    removeModal()
    result$selUserId <- -1
    .reloadtbl_CULData()
  })



  # Security Group Add/Delete --------------------------------------------------

  # modal dialog of add/remove security button
  .usersecuritymodal <- function() {
    ns <- session$ns
    modalDialog(label = "usersecuritymodal",
                title = "Add/Remove Security Groups",
                selectInput(ns("sinputSecurity"), "Select Security Group",
                            choices = c("")),
                footer = tagList(
                  flamingoButton(ns("abuttonaddsecurity"),
                               label = "Add", align = "left"),
                  flamingoButton(ns("abuttonrmvsecurity"),
                               label = "Remove", align = "right")),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of add/remove security button
  onclick("abuttonusersecurity", {
      showModal(.usersecuritymodal())
      .clearSecurityGroupSelection()
  })

  # onclick of add security button in pop-up
  onclick("abuttonaddsecurity", {
    # as.character ensures query builder will quote args
    stmt <- buildDbQuery("addSecurityGroup",
                         as.character(result$selUserId), as.character(input$sinputSecurity))
    executeDbQuery(dbSettings, stmt)
    .reloadtbl_USGData()
    removeModal()
  })

  # onclick of remove security button
  onclick("abuttonrmvsecurity", {
    stmt <- buildDbQuery("removeSecurityGroup",
                         as.character(result$selUserId), as.character(input$sinputSecurity))
    executeDbQuery(dbSettings, stmt)
    .reloadtbl_USGData()
    removeModal()
  })



  # Oasis User (License) Add/Delete --------------------------------------------

  # modal dialog of add/remove license button
  .userlicensemodal <- function() {
    ns <- session$ns
    modalDialog(label = "userlicensemodal",
                title = "Add/Remove User Licenses",
                selectInput(ns("sinputOasisID"), "Select Oasis User",
                            choices = c("")),
                footer = tagList(
                  flamingoButton(ns("abuttonaddoasisid"),
                               label = "Add", align = "left"),
                  flamingoButton(ns("abuttonrmvoasisid"),
                               label = "Remove", align = "right")),
                size = "m",
                easyClose = TRUE
    )
  }

  # onclick of add/remove license button
  onclick("abuttonuseroasis", {
      showModal(.userlicensemodal())
      .clearOasisUserSelection()
  })


  # onclick of add license button in pop-up
  onclick("abuttonaddoasisid", {
    stmt <- buildDbQuery("addUserLicense",
                         as.character(result$selUserId), as.character(input$sinputOasisID))
    executeDbQuery(dbSettings, query)
    .reloadtbl_CUAULData()
    removeModal()
  })

  # onclick of remove license button in pop-up
  onclick("abuttonrmvoasisid", {
    stmt <- buildDbQuery("removeUserLicense",
                         as.character(result$selUserId), as.character(input$sinputOasisID))
    executeDbQuery(dbSettings, query)
    .reloadtbl_CUAULData()
    removeModal()
  })


  # Export to CSV --------------------------------------------------------------
  # User List
  output$CUACULdownloadexcel <- downloadHandler(
    filename = "companyuserlist.csv",
    content = function(file) {
      write.csv(result$tbl_CULData, file)
    }
  )
  # User Security Groups
  output$CUAUUSGdownloadexcel <- downloadHandler(
    filename = "usersecuritygroups.csv",
    content = function(file) {
      write.csv(result$tbl_USGData, file)
    }
  )
  # User Licenses
  output$CUAULdownloadexcel <- downloadHandler(
    filename = "userlicenses.csv",
    content = function(file) {
      write.csv(result$tbl_CUAULData, file)
    }
  )


  # Module Output --------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list() # placeholder
  )

  moduleOutput

}

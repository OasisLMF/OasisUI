#' Reactive navigation
#'
#' Set of utilities providing a common way to store reactive navigation state
#' and propagate it as or retrieve it from module outputs, supporting navigation
#' across nested modules.
#'
#' @param value
#'
#' @details
#' The details behind how the navigation state is stored, propagated and
#' observed are encapsulate using utilities for initializing the state
#' (`reactiveNavigation()`), updating it (`updateNavigation()`), making it a
#' module output reactive list element (`outputNavigation()`), retrieving it from
#' a module output (`getNavigation()`). Observing and collecting navigation
#' state from a list of (sub-) modules (`observeModuleNavigation()`) allows
#' flexible modular configurations with arbitrarily nested modules.
#'
#' This functionality provides a common, aligned way of handling navigation,
#' abstracting from the specific implementation details. In fact, from the user
#' (shiny app developer) perspective, there is no need to explicitly know how
#' the navigation state is internally stored or called.
#'
#' Enabling navigation in the sever or a module's server function) boils down
#' to:
#' ``` r
#' # [...]
#' # initialize the reactive navigation state
#' navigation_state <- reactiveNavigation()
#' # [...]
#' # possibly observe what is relevant for the update of the navigation state
#' observeEvent(..., {updateNavigation(navigation_state, new_value)})
#' # possibly observe the navigation coming from a list of (sub)modules outout
#' observeModuleNavigation(navigation_state, modules)
#' # [...]
#' # construct the navigation-related part of the module output list
#' result <- c(
#'   outputNavigation(navigation_state),
#'   list(other_reactive_outputs)
#' )
#' [...]
#' # get the reactive value when needed (typically in the top-level module/server)
#' getNavigation(navigation_state)
#' getNavigation(outputNavigation(navigation_state))
#' [...]
#' ```
#'
#' @return TBD
#'
#' @examples
#'
#' @export
#'
#' @md
reactiveNavigation <- function(value = NULL) {
  reactiveValues(
    .navigate_to = value,
    # `.react` allows reactivity upon any observed action, even if $.navigate_to
    # is not changed
    .react = TRUE
  )
}


# update navigation state
#' @rdname reactiveNavigation
#'
#' @export
#'
#' @md
updateNavigation <- function(state, value, force_react = TRUE) {
  state$.navigate_to <- value
  if (force_react) {
    # `.react` gets always invalidated by changing its value
    state$.react <- !state$.react
  }
  invisible()
}

# reactive navigation state output, including or excluding .react invalidation
#' @rdname reactiveNavigation
#'
#' @export
#'
#' @md
outputNavigation <- function(state, force_react = TRUE) {
  if (force_react) {
    list(
      .navigate_to = reactive({
        state$.react
        state$.navigate_to
      })
    )
  } else {
    list(
      .navigate_to = reactive({
        state$.navigate_to
      })
    )
  }
}

# extract the navigation state as a reactive value if present, returining
# reactive(NULL) otherwise
#' @rdname reactiveNavigation
#'
#' @export
#'
#' @md
getNavigation <- function(module) {
  reactive(if (!is.null(module$.navigate_to)) module$.navigate_to())
}

# create observers that update the inpuit navigation `state` based on the
# navigation state of a list of modules
#' @rdname reactiveNavigation
#'
#' @export
#'
#' @md
observeModuleNavigation <- function(state, modules, force_react = TRUE,
                                    logger = NULL) {
  lapply(modules, function(module) {
    observeEvent(getNavigation(module)(), {
      .navigate_to <- getNavigation(module)()
      if (!is.null(.navigate_to)) {
        if (!is.null(logger)) logger(paste0(" => ", .navigate_to))
        updateNavigation(state, .navigate_to, force_react = force_react)
      }
    })
  })
}

#' reactiveNavigation
#'
#' @rdname reactiveNavigation
#'
#' @description Set of utilities for storing reactive navigation state and propagating it as
#' module output or retrieving it from module outputs, supporting navigation
#' across nested modules. This provides a common, aligned way of handling
#' navigation, abstracting from the specific implementation details.
#'
#' @details The details behind how the navigation state is stored, propagated and
#' observed are encapsulated using utilities for initializing the state
#' (`reactiveNavigation()`), updating it (`updateNavigation()`), adding it as
#' part of the module output list (`outputNavigation()`), retrieving it from a
#' module output (`getNavigation()`). Observing and collecting navigation state
#' from a list of (sub-) modules (`observeModuleNavigation()`) allows flexible
#' modular configurations with arbitrarily nested modules.
#'
#' Enabling navigation in the sever (or a module's server function) boils down
#' to:
#' ``` r
#' # [...]
#' # initialize the reactive navigation state
#' navigation_state <- reactiveNavigation()
#' # [...]
#' # observe what is relevant for the update of the navigation state
#' observeEvent(..., {updateNavigation(navigation_state, new_value)})
#' # possibly observe the navigation coming from a list of (sub)modules outputs
#' observeModuleNavigation(navigation_state, modules)
#' # [...]
#' # construct the navigation-related part of the module output list
#' result <- c(
#'   outputNavigation(navigation_state),
#'   list(other_reactive_outputs)
#' )
#' [...]
#' # get the reactive value when needed (typically in the top-level server)
#' getNavigation(outputNavigation(navigation_state))
#' [...]
#' ```
#' @param value Desired value of the navigation state.
#'
#' @return Returns the navigation state to be used by the
#'   other functions, initialized with `value`.
#'
#' @example man-roxygen/ex-reactiveNavigation.R
#' @example man-roxygen/ex-reactiveNavigation.R
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


#' @rdname reactiveNavigation
#'
#' @param state Reactive navigation state created with `reactiveNavigation()`.
#' @param force_react Logical flag specifying whether the observed event for
#'   navigation state update should always invalidate the reactive navigation
#'   state, thus forcing rectivity. This is especially desirable within inner
#'   modules.
#'
#' @return Returns `NULL`, invisibly, and is called for its
#'   side-effect of updating the input `state` with the supplied `value`.
#'
#' @export
#'
#' @md
updateNavigation <- function(state, value, force_react = TRUE) {
  state$.navigate_to <- value
  if (force_react) {
    # always invalidate `.react`  by changing its value
    state$.react <- !state$.react
  }
  invisible()
}

#' outputNavigation
#'
#' @rdname outputNavigation
#'
#' @inheritParams updateNavigation
#'
#' @return `Returns a list containing the reactive
#'   navigation state, to be possibliy `c`ombined with other module outputs. The
#'   reactive navigation state can then be extracted using `getNavigation()`.
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


#' @rdname reactiveNavigation
#'
#' @param output A list possibly containing a reactive navigation state
#'   component (created with `outputNavigation()`).
#'
#' @return Returns the reactive navigation state extracted
#'   from `output` if present, `reactive(NULL)` otherwise.
#'
#' @export
#'
#' @md
getNavigation <- function(output) {
  reactive(if (!is.null(output$.navigate_to)) output$.navigate_to())
}


#' reactiveNavigation
#'
#' @rdname reactiveNavigation
#'
#' @param modules A list of modules output lists, each possibly containing a
#'   reactive navigation state component (created with `outputNavigation()`).
#' @param logger Optional logging function to log the observed navigation state
#'   updates.
#'
#' @return Returns `NULL`, invisibly, and is called
#'   for its side-effect of updating the input navigation `state` by observing
#'   the navigation state of the input `modules`.
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
  invisible()
}

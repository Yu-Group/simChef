#' Arguments that are shared by multiple signal functions.
#'
#' @name shared_error_args
#' @param message Message to include with the condition.
#' @param class Subclass passed to the appropriate `rlang` signal function.
#' @param call The function environment that is most relevant from the user's
#'   perspective. Default is `rlang::caller_env()`, which gives the environment
#'   attached to the function that called this signal function.
#' @param ... Additional arguments to pass to the appropriate `rlang` signal
#'   function.
#'
#' @keywords internal
NULL

#' Signals an error with default subclass "simChef_error".
#'
#' @inheritParams shared_error_args
#'
#' @keywords internal
abort <- function(message = NULL,
                  class = "simChef_error",
                  call = rlang::caller_env(),
                  ...) {
  rlang::abort(message = message, class = class, call = call, ...)
}

#' Signals a warning with default subclass "simChef_warning".
#'
#' @inheritParams shared_error_args
#'
#' @keywords internal
warn <- function(message = NULL,
                 class = "simChef_warning",
                 call = rlang::caller_env(),
                 ...) {
  rlang::warn(message = message, class = class, call = call, ...)
}

#' Signals a message with default subclass "simChef_message".
#'
#' @inheritParams shared_error_args
#'
#' @keywords internal
inform <- function(message = NULL,
                   class = "simChef_message",
                   call = rlang::caller_env(),
                   ...) {
  prefix <- ""
  if (getOption("simChef.debug", FALSE)) {
    prefix <- "[simChef.debug]"
  }
  message <- paste(prefix, message)
  rlang::inform(message = message, class = class, call = call, ...)
}

#' Constructs a helpful message and signals an error when the user takes an
#' invalid action. Can include an optional hint to fix the error.
#'
#' @inheritParams shared_error_args
#' @param cause_string Why the action was invalid.
#' @param hint_string An optional hint to fix the error.
#' @param ... Passed to `simChef::abort()`.
#'
#' @keywords internal
abort_on_invalid_user_action <- function(cause_string,
                                         hint_string = NULL,
                                         call = rlang::caller_env(),
                                         ...) {
  msg <- sprintf("Cause: %s.\n", state_string)
  if (!is.null(how_to_fix_string)) {
    msg <- paste(msg, sprintf("Hint: %s."))
  }
  abort(msg, call = call, ...)
}

#' Check an object for a subclass and signal an error if the subclass is
#' missing.
#'
#' @inheritParams abort_on_invalid_user_action
#' @param obj The object to check for `subclass`.
#' @param sublcass The subclass that `obj` is expected to have.
#'
#' @keywords internal
abort_if_missing_subclass <- function(obj,
                                      subclass,
                                      cause_string,
                                      hint_string = NULL,
                                      call = rlang::caller_env(),
                                      ...) {
  if (!inherits(obj, subclass)) {
    abort_on_invalid_user_action(cause_string,
                                 hint_string,
                                 call = call,
                                 ...)
  }
}

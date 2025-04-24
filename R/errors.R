# todos ------------------------------------------------------------------------

# Get standalone-obj-type from {rlang} for `obj_type_friendly()`
# - alter the error messages in this script

# check ------------------------------------------------------------------------

#' Check that an object is a symbol
#'
#' @param x An object to check.
#' @param x_name Name used in error message.
#' @param call Call used in error condition.
#'
#' @returns A symbol.
#' @keywords internal
#' @noRd
check_is_symbol <- function(
    x,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
  ) {

  if (rlang::is_symbol(x)) {
    return(x)
  }
  typewriter_abort(
    # TODO: Add "not {obj_type_friendly(x)}"
    sprintf("`%s` must be a symbol.", x_name),
    class = "typewriter_error_input_type",
    call = call
  )
}

#' Check that a quosure is a call or a function provided by name
#'
#' @param quosure A quosure.
#' @param quosure_name Name used in error message.
#' @param call Call used in error condition.
#'
#' @returns
#'
#' A call expression (not a quosure!) If `quosure` is a quoted function, then
#' it is converted to a call with `rlang::call2()` prior to returning.
#'
#' @keywords internal
#' @noRd
check_quo_is_callish <- function(
    quosure,
    quosure_name = rlang::caller_arg(quosure),
    call = rlang::caller_env()
  ) {

  if (!rlang::is_quosure(quosure)) {
    typewriter_abort("`quosure` must be a quosure.", internal = TRUE)
  }

  expr <- rlang::quo_get_expr(quosure)
  if (quo_is_named_function(quosure)) {
    return(rlang::call2(expr))
  }
  if (rlang::is_call_simple(expr)) {
    return(expr)
  }
  if (rlang::is_call(expr)) {
    typewriter_abort(
      # TODO: Add more examples of what a simple call is...
      sprintf("`%s` must be a simple call.", quosure_name),
      class = "typewriter_error_input_type",
      call = call
    )
  }
  typewriter_abort(
    # TODO: Add second line, x = `quosure_name = rlang::as_label(expr)` is a BLANK.
    sprintf("`%s` must be a simple call or function provided by name.", quosure_name),
    class = "typewriter_error_input_type",
    call = call
  )
}

# abort ------------------------------------------------------------------------

typewriter_abort <- function(
    message,
    class = character(),
    call = rlang::caller_env(),
    parent = NULL,
    internal = FALSE
  ) {
  rlang::abort(
    message = message,
    class = c("typewriter_error", class),
    call = call,
    parent = parent,
    internal = internal
  )
}

# messaging --------------------------------------------------------------------

at_positions <- function(loc, n_max = 5) {
  loc <- if (is.logical(loc)) which(loc & !is.na(loc)) else loc
  loc <- as.numeric(loc)
  n <- length(loc)
  at <- ngettext(min(n, n_max), "at postion ", "at positions ")
  if (n > n_max) {
    paste0(at, "`", deparse(loc[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(loc), "`")
  }
}
